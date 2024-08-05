use std::env;
use std::fmt::{Debug, Formatter, Write};
use std::io;
use std::iter::Peekable;
use std::process;
use std::str::Chars;

#[derive(PartialEq)]
pub(crate) enum Ast {
    Literal(Count, char),
    Class(Count, Class),
    CharacterSet(CharacterSet),
    Group(Group),
    Alternation(Alternation),
    Backreference(u8),
}

impl Debug for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let write_count = |f: &mut Formatter<'_>, count: &Count| -> std::fmt::Result {
            match count {
                Count::One => Ok(()),
                Count::OneOrMore => f.write_char('+'),
                Count::ZeroOrOne => f.write_char('?'),
            }
        };
        match self {
            Ast::Literal(count, char) => {
                f.write_char(*char)?;
                write_count(f, count)
            }
            Ast::Class(count, class) => {
                match class {
                    Class::Alphanumeric => f.write_str("\\w")?,
                    Class::Digit => f.write_str("\\d")?,
                    Class::Wildcard => f.write_char('.')?,
                }
                write_count(f, count)
            }
            Ast::CharacterSet(set) => {
                f.write_char('[')?;
                f.write_str(&set.chars)?;
                f.write_char(']')?;
                write_count(f, &set.count)
            }
            Ast::Group(group) => {
                f.write_char('(')?;
                write!(f, "[{}]", group.idx)?;
                for item in &group.items {
                    item.fmt(f)?;
                }
                f.write_char(')')
            }
            Ast::Alternation(alternation) => {
                let mut first = true;
                f.write_char('(')?;
                write!(f, "[{}]", alternation.idx)?;
                for alt in &alternation.alternatives {
                    if !first {
                        f.write_char('|')?;
                        first = false;
                    }
                    for item in alt {
                        item.fmt(f)?;
                    }
                }
                f.write_char(')')
            }
            Ast::Backreference(n) => {
                write!(f, "\\{n}")
            }
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Count {
    One,
    OneOrMore,
    ZeroOrOne,
}

#[derive(Debug, PartialEq)]
enum Class {
    Alphanumeric,
    Digit,
    Wildcard,
}

#[derive(Debug, PartialEq)]
struct CharacterSet {
    negated: bool,
    count: Count,
    chars: String,
}

#[derive(Debug, PartialEq)]
struct Group {
    idx: usize,
    items: Vec<Ast>,
}

#[derive(Debug, PartialEq)]
struct Alternation {
    idx: usize,
    alternatives: Vec<Vec<Ast>>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Pattern {
    start: bool,
    end: bool,
    items: Vec<Ast>,
}

struct Parser {
    group_idx: usize,
    nesting_level: usize,
}

fn parse_count(pattern: &mut Peekable<Chars>) -> Count {
    match pattern.next_if(|c| matches!(c, '+' | '?')) {
        Some('+') => Count::OneOrMore,
        Some('?') => Count::ZeroOrOne,
        _ => Count::One,
    }
}

impl Parser {
    fn read_group_items(&mut self, pattern: &mut Peekable<Chars>) -> Result<Vec<Ast>, ParseError> {
        let mut items = vec![];
        loop {
            if pattern.peek().is_none() {
                break;
            }
            items.extend(self.parse(pattern)?)
        }
        Ok(items)
    }
    fn parse(&mut self, pattern: &mut Peekable<Chars>) -> Result<Vec<Ast>, ParseError> {
        let mut items = vec![];
        let Some(char) = pattern.next() else {
            return Ok(items);
        };

        match char {
            '\\' => {
                let c = pattern.next();
                let count: Count = parse_count(pattern);
                match c {
                    Some('\\') => items.push(Ast::Literal(count, '\\')),
                    Some('d') => items.push(Ast::Class(count, Class::Digit)),
                    Some('w') => items.push(Ast::Class(count, Class::Alphanumeric)),
                    Some('0') => return Err(ParseError::BackreferenceZero),
                    Some(n) if n.is_ascii_digit() => {
                        if pattern.peek().is_some_and(|c| c.is_ascii_digit()) {
                            return Err(ParseError::MultiDigitBackreference);
                        }
                        if count != Count::One {
                            return Err(ParseError::BackreferenceCountNotOne);
                        }
                        let n = ((n as u32) - ('0' as u32)) as usize;
                        if n > self.group_idx + 1 {
                            return Err(ParseError::BackreferenceToEarly(n, self.group_idx + 1));
                        }
                        items.push(Ast::Backreference(n as u8));
                    }
                    Some(char) => return Err(ParseError::UnexpectedEscapedChar(char)),
                    None => return Err(ParseError::UnexpectedEnd),
                }
            }
            '[' => {
                let negated = Some(&'^') == pattern.peek();
                if negated {
                    pattern.next();
                }
                let mut chars = String::new();
                loop {
                    match pattern.next() {
                        None => return Err(ParseError::MissingClosingCharacterSet),
                        Some(']') => break,
                        Some(ch) if ch.is_alphanumeric() => chars.push(ch),
                        Some(_) => return Err(ParseError::NonAlphanumericCharacterSet),
                    }
                }
                let count = parse_count(pattern);
                items.push(Ast::CharacterSet(CharacterSet {
                    negated,
                    count,
                    chars,
                }))
            }
            '.' => {
                let count = parse_count(pattern);
                items.push(Ast::Class(count, Class::Wildcard))
            }
            '(' => {
                let mut group_chars = String::new();
                let mut group_items = vec![];
                self.nesting_level += 1;
                self.group_idx += 1;
                let idx = self.group_idx;
                let mut num_open = 0;
                loop {
                    match pattern.next() {
                        None => return Err(ParseError::MissingClosingParenthesis),
                        Some('(') => {
                            num_open += 1;
                            group_chars.push('(')
                        }
                        Some('|') => {
                            if num_open == 0 {
                                group_items.push(
                                    self.read_group_items(&mut group_chars.chars().peekable())?,
                                );
                                group_chars.clear();
                            } else {
                                group_chars.push('|');
                            }
                        }
                        Some(')') => {
                            if num_open == 0 {
                                group_items.push(
                                    self.read_group_items(&mut group_chars.chars().peekable())?,
                                );
                                break;
                            } else {
                                num_open -= 1;
                                group_chars.push(')');
                            }
                        }
                        Some(ch) => group_chars.push(ch),
                    }
                }
                self.nesting_level -= 1;
                match group_items.len() {
                    0 => return Err(ParseError::EmptyGroup),
                    1 => items.push(Ast::Group(Group {
                        idx,
                        items: group_items.into_iter().next().unwrap(),
                    })),
                    _ => items.push(Ast::Alternation(Alternation {
                        idx,
                        alternatives: group_items,
                    })),
                }
            }
            char if (char.is_ascii_alphanumeric()
                || char.is_ascii_punctuation()
                || char.is_ascii_whitespace())
                && !"(|)[]+?.\\^$".contains(char) =>
            {
                let count = parse_count(pattern);
                items.push(Ast::Literal(count, char));
            }
            char => return Err(ParseError::UnexpectedChar(char)),
        }

        Ok(items)
    }
}

#[derive(Debug, PartialEq)]
#[non_exhaustive]
enum ParseError {
    UnexpectedEscapedChar(char),
    UnexpectedChar(char),
    UnexpectedEnd,
    EmptyGroup,
    MissingClosingParenthesis,
    MissingClosingCharacterSet,
    NonAlphanumericCharacterSet,
    BackreferenceZero,
    MultiDigitBackreference,
    BackreferenceCountNotOne,
    BackreferenceToEarly(usize, usize),
}

impl Pattern {
    fn parse(pattern: &str) -> Result<Pattern, ParseError> {
        let start = pattern.starts_with('^');
        let end = pattern.ends_with('$');

        let mut pattern = pattern.chars().peekable();
        if start {
            pattern.next();
        }
        if end {
            pattern.next_back();
        }

        let mut parser = Parser {
            group_idx: 0,
            nesting_level: 0,
        };
        let mut asts = vec![];
        loop {
            if pattern.peek().is_none() {
                break;
            }
            asts.extend(parser.parse(&mut pattern)?)
        }
        Ok(Pattern {
            start,
            end,
            items: asts,
        })
    }
}

impl Ast {
    fn match_count(
        text: &mut Peekable<Chars>,
        count: Count,
        pred: impl Fn(&char) -> bool,
        current_group: &mut String,
    ) -> bool {
        match count {
            Count::One => text
                .next_if(&pred)
                .inspect(|c| {
                    current_group.push(*c);
                })
                .is_some(),
            Count::OneOrMore => {
                let mut k = 0;
                while let Some(c) = text.next_if(&pred) {
                    current_group.push(c);
                    k += 1;
                }
                k >= 1
            }
            Count::ZeroOrOne => {
                if let Some(c) = text.next_if(&pred) {
                    current_group.push(c);
                }
                true
            }
        }
    }

    fn match_at_start(
        &self,
        text: &mut Peekable<Chars>,
        groups: &mut Vec<String>,
        current_group: &mut String,
    ) -> bool {
        return match self {
            Ast::Literal(count, lit) => Ast::match_count(text, *count, |c| c == lit, current_group),
            Ast::Class(count, class) => match class {
                Class::Alphanumeric => {
                    Ast::match_count(text, *count, |c| c.is_alphanumeric(), current_group)
                }
                Class::Digit => {
                    Ast::match_count(text, *count, |c| c.is_ascii_digit(), current_group)
                }
                Class::Wildcard => {
                    Ast::match_count(text, *count, |c| !"[](|)\\".contains(*c), current_group)
                }
            },
            Ast::CharacterSet(set) => Ast::match_count(
                text,
                set.count,
                |c| c.is_alphanumeric() && (set.negated ^ set.chars.contains(*c)),
                current_group,
            ),
            Ast::Group(group) => {
                let mut new_current_group = String::new();
                if group
                    .items
                    .iter()
                    .all(|item| item.match_at_start(text, groups, &mut new_current_group))
                {
                    let i = group.idx - 1;
                    if groups.len() <= i {
                        groups.resize(i + 1, "".into());
                    }
                    current_group.push_str(&new_current_group);
                    groups[i] = new_current_group;
                    return true;
                }
                false
            }
            Ast::Alternation(alternation) => {
                let mut new_current_group = String::new();
                for alt in &alternation.alternatives {
                    let mut text_clone = text.clone();
                    if alt.iter().all(|item| {
                        item.match_at_start(&mut text_clone, groups, &mut new_current_group)
                    }) {
                        let i = alternation.idx - 1;
                        if groups.len() <= i {
                            groups.resize(i + 1, "".into());
                        }
                        current_group.push_str(&new_current_group);
                        groups[i] = new_current_group;
                        *text = text_clone;
                        return true;
                    }
                    new_current_group.clear();
                }
                false
            }
            Ast::Backreference(n) => groups.get(*n as usize - 1).is_some_and(|matched| {
                let chars: String = text.take(matched.len()).collect();
                if matched == &chars {
                    current_group.push_str(&chars);
                    return true;
                }
                false
            }),
        };
    }
}

fn match_pattern(text: &str, pattern: &str) -> bool {
    let Pattern { start, end, items } = Pattern::parse(pattern).unwrap();
    let mut text = text.chars().peekable();
    let mut groups = vec![];
    let mut current_group = String::new();
    if start {
        if items
            .iter()
            .all(|item| item.match_at_start(&mut text, &mut groups, &mut current_group))
        {
            !end || text.peek().is_none()
        } else {
            false
        }
    } else {
        loop {
            let mut text_starting_at = text.clone();
            if items.iter().all(|item| {
                item.match_at_start(&mut text_starting_at, &mut groups, &mut current_group)
            }) {
                if end && text_starting_at.peek().is_some() {
                } else {
                    return true;
                }
            }
            current_group.clear();
            groups.clear();
            if text.next().is_none() {
                return false;
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{match_pattern, Group};

    use crate::{Alternation, Ast, CharacterSet, Class, Count, Pattern};

    fn t(items: impl IntoIterator<Item = Ast>) -> Pattern {
        Pattern {
            start: false,
            end: false,
            items: items.into_iter().collect(),
        }
    }

    fn with_start(mut pattern: Pattern) -> Pattern {
        pattern.start = true;
        pattern
    }

    fn with_end(mut pattern: Pattern) -> Pattern {
        pattern.end = true;
        pattern
    }

    #[test]
    fn test_parse() {
        assert_eq!(Pattern::parse(""), Ok(t([])));
        assert_eq!(Pattern::parse("a"), Ok(t([Ast::Literal(Count::One, 'a')])));
        assert_eq!(
            Pattern::parse("ab"),
            Ok(t([
                Ast::Literal(Count::One, 'a'),
                Ast::Literal(Count::One, 'b')
            ]))
        );
        assert_eq!(
            Pattern::parse("\\d"),
            Ok(t([Ast::Class(Count::One, Class::Digit)]))
        );
        assert_eq!(
            Pattern::parse("\\w"),
            Ok(t([Ast::Class(Count::One, Class::Alphanumeric)]))
        );
        assert_eq!(
            Pattern::parse("a?"),
            Ok(t([Ast::Literal(Count::ZeroOrOne, 'a')]))
        );
        assert_eq!(
            Pattern::parse("a+"),
            Ok(t([Ast::Literal(Count::OneOrMore, 'a')]))
        );
        assert_eq!(
            Pattern::parse("[abc]"),
            Ok(t([Ast::CharacterSet(CharacterSet {
                negated: false,
                count: Count::One,
                chars: "abc".to_string(),
            })]))
        );
        assert_eq!(
            Pattern::parse("[^xyz]"),
            Ok(t([Ast::CharacterSet(CharacterSet {
                negated: true,
                count: Count::One,
                chars: "xyz".to_string(),
            })]))
        );
        assert_eq!(
            Pattern::parse("^abc"),
            Ok(with_start(t([
                Ast::Literal(Count::One, 'a'),
                Ast::Literal(Count::One, 'b'),
                Ast::Literal(Count::One, 'c')
            ])))
        );
        assert_eq!(
            Pattern::parse("abc$"),
            Ok(with_end(t([
                Ast::Literal(Count::One, 'a'),
                Ast::Literal(Count::One, 'b'),
                Ast::Literal(Count::One, 'c')
            ])))
        );
        assert_eq!(
            Pattern::parse("(a|b|c)"),
            Ok(t([Ast::Alternation(Alternation {
                idx: 1,
                alternatives: vec![
                    vec![Ast::Literal(Count::One, 'a')],
                    vec![Ast::Literal(Count::One, 'b')],
                    vec![Ast::Literal(Count::One, 'c')],
                ],
            })]))
        );
        assert_eq!(
            Pattern::parse("(a|b|)"),
            Ok(t([Ast::Alternation(Alternation {
                idx: 1,
                alternatives: vec![
                    vec![Ast::Literal(Count::One, 'a')],
                    vec![Ast::Literal(Count::One, 'b')],
                    vec![],
                ],
            })]))
        );
        assert_eq!(
            Pattern::parse("(\\w+) and \\1"),
            Ok(t([
                Ast::Group(Group {
                    idx: 1,
                    items: vec![Ast::Class(Count::OneOrMore, Class::Alphanumeric)],
                }),
                Ast::Literal(Count::One, ' '),
                Ast::Literal(Count::One, 'a'),
                Ast::Literal(Count::One, 'n'),
                Ast::Literal(Count::One, 'd'),
                Ast::Literal(Count::One, ' '),
                Ast::Backreference(1)
            ]))
        );
        assert_eq!(
            Pattern::parse(r#"((\w)\2)_\1"#),
            Ok(t([
                Ast::Group(Group {
                    idx: 1,
                    items: vec![
                        Ast::Group(Group {
                            idx: 2,
                            items: vec![Ast::Class(Count::One, Class::Alphanumeric)]
                        }),
                        Ast::Backreference(2)
                    ]
                }),
                Ast::Literal(Count::One, '_'),
                Ast::Backreference(1)
            ]))
        );
    }

    #[test]
    fn nested_backreferences() {
        assert!(match_pattern(
            "'cat and cat' is the same as 'cat and cat'",
            r#"('(cat) and \2') is the same as \1"#
        ));
        assert!(!match_pattern(
            "'cat and cat' is the same as 'cat and dog'",
            r#"('(cat) and \2') is the same as \1"#
        ));
    }

    #[test]
    fn multiple_backreferences() {
        assert!(match_pattern(
            "3 red squares and 3 red circles",
            r#"(\d+) (\w+) squares and \1 \2 circles"#
        ));
        assert!(!match_pattern(
            "3 red squares and 4 red circles",
            r#"(\d+) (\w+) squares and \1 \2 circles"#
        ));
        assert!(match_pattern("abc-def is abc-def, not efg, abc, or def", r#"(([abc]+)-([def]+)) is \1, not ([^xyz]+), \2, or \3"#));
        assert!(match_pattern("'howwdy hey there' is made up of 'howwdy' and 'hey'. howwdy hey there", r#"'((how+dy) (he?y) there)' is made up of '\2' and '\3'. \1"#));
        assert!(!match_pattern("'hody hey there' is made up of 'hody' and 'hey'. hody hey there", r#"'((how+dy) (he?y) there)' is made up of '\2' and '\3'. \1"#));
        assert!(!match_pattern("'howwdy heeey there' is made up of 'howwdy' and 'heeey'. howwdy heeey there", r#"'((how+dy) (he?y) there)' is made up of '\2' and '\3'. \1"#));
        assert!(match_pattern("cat and fish, cat with fish, cat and fish", r#"((c.t|d.g) and (f..h|b..d)), \2 with \3, \1"#));
    }

    #[test]
    fn single_backreference() {
        assert!(match_pattern("cat and cat", "(cat) and \\1"));
        assert!(!match_pattern("cat and dog", "(cat) and \\1"));
        assert!(match_pattern("dog and dog", "(cat|dog) and \\1"));
        assert!(match_pattern("cat and cat", "(cat|dog) and \\1"));
        assert!(!match_pattern("cat and dog", "(cat|dog) and \\1"));
        assert!(match_pattern("cat and cat", "(\\w+) and \\1"));
        assert!(match_pattern("dog and dog", "(\\w+) and \\1"));
        assert!(!match_pattern("cat and dog", "(\\w+) and \\1"));

        assert!(match_pattern(
            "grep 101 is doing grep 101 times",
            r#"(\w\w\w\w \d\d\d) is doing \1 times"#
        ));
        assert!(!match_pattern(
            "$?! 101 is doing $?! 101 times",
            r#"(\w\w\w\w \d\d\d) is doing \1 times"#
        ));
        assert!(!match_pattern(
            "grep yes is doing grep yes times",
            r#"(\w\w\w\w \d\d\d) is doing \1 times"#
        ));
        assert!(match_pattern(
            "abcd is abcd, not efg",
            r#"([abcd]+) is \1, not [^xyz]+"#
        ));
    }

    #[test]
    fn alternation() {
        assert!(match_pattern("dog", "(cat|dog)"));
        assert!(match_pattern("cat", "(cat|dog)"));
        assert!(!match_pattern("apple", "(cat|dog)"));
        assert!(match_pattern("aa", "a(|b)a"));
        //assert!(match_pattern("aba", "a(|b)a"));
        assert!(match_pattern("aba", "a(b|bb)a"));
        //assert!(match_pattern("abba", "a(b|bb)a"));
    }

    #[test]
    fn group() {
        assert!(match_pattern("abcd", "((ab)c)d"));
    }

    #[test]
    fn wildcard() {
        assert!(match_pattern("dog", "d.g"));
        assert!(!match_pattern("cog", "d.g"));
    }

    #[test]
    fn match_zero_or_one_times() {
        assert!(match_pattern("dogs", "dogs?"));
        assert!(match_pattern("dog", "dogs?"));
        assert!(!match_pattern("cat", "dogs?"));
    }

    #[test]
    fn match_one_or_more_times() {
        assert!(match_pattern("SaaS", "a+"));
        assert!(!match_pattern("dog", "a+"));
        assert!(match_pattern("caats", "ca+ts"));

        assert!(match_pattern("aaabb", "a+"));
        //assert!(match_pattern("baaabb", "ba+ab"));
        //assert!(match_pattern("aaabb", "aa+ab"));
        //assert!(!match_pattern("aaabb", "aa+aa"));

        assert!(match_pattern("0123", "^\\d+$"));
    }

    #[test]
    fn end_of_string_anchor() {
        assert!(match_pattern("dog", "dog$"));
        assert!(!match_pattern("dogs", "dog$"));
    }

    #[test]
    fn start_of_string_anchor() {
        assert!(match_pattern("log", "^log"));
        assert!(!match_pattern("slog", "^log"));
    }

    #[test]
    fn combining_character_classes() {
        assert!(match_pattern("1 apple", "\\d apple"));
        assert!(!match_pattern("1 orange", "\\d apple"));
        assert!(match_pattern("100 apples", "\\d\\d\\d apple"));
        assert!(!match_pattern("1 apple", "\\d\\d\\d apple"));
        assert!(match_pattern("3 dogs", "\\d \\w\\w\\ws"));
        assert!(match_pattern("4 cats", "\\d \\w\\w\\ws"));
        assert!(!match_pattern("1 dog", "\\d \\w\\w\\ws"));
    }
    #[test]
    fn test() {
        assert!(match_pattern("sally has 3 apples", "\\d apple"));
        assert!(match_pattern("apple", "[^xyz]"));
        assert!(!match_pattern("banana", "[^anb]"));
        assert!(match_pattern("a", "[abcd]"));
        assert!(!match_pattern("efgh", "[abcd]"));
        assert!(match_pattern("word", "\\w"));
        assert!(!match_pattern("$!?", "\\w"));
        assert!(match_pattern("123", "\\d"));
        assert!(!match_pattern("apple", "\\d"));
        assert!(match_pattern("dog", "d"));
        assert!(!match_pattern("dog", "f"));
    }
}

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();
    let mut input_line = String::new();

    io::stdin().read_line(&mut input_line).unwrap();

    if match_pattern(&input_line, &pattern) {
        process::exit(0)
    } else {
        process::exit(1)
    }
}
