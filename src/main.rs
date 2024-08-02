use std::env;
use std::io;
use std::process;

fn match_pattern(input_line: &str, pattern: &str) -> bool {
    if pattern.starts_with('^') {
        return match_from_start(input_line, &pattern[1..]);
    }
    let mut i = 0;
    while i != input_line.len() {
        if match_from_start(&input_line[i..], pattern) { return true; }
        i += input_line[i..].chars().next().unwrap().len_utf8();
    }
    false
}

fn match_from_start(input_line: &str, pattern: &str) -> bool {
    if pattern.is_empty() {
        return true;
    }
    if pattern == "$" {
        return input_line.is_empty();
    }
    if pattern.starts_with("\\d") {
        if input_line.chars().next().is_some_and(|c| matches!(c, '0'..='9')) {
            return match_from_start(&input_line[1..], &pattern[2..]);
        } else {
            return false;
        }
    }
    if pattern.starts_with("\\w") {
        if input_line.chars().next().is_some_and(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
            return match_from_start(&input_line[1..], &pattern[2..]);
        } else {
            return false;
        }
    }
    if pattern.starts_with("[") {
        let (group, suffix) = pattern[1..].split_once(']').expect("no end group");
        let mut chars = input_line.chars();
        let Some((start, input_line)) = chars.next().map(|c| (c, chars.as_str())) else { return false; };
        let group_matches = if group.starts_with('^') {
            !group[1..].contains(start)
        } else {
            group.contains(start)
        };
        if group_matches {
            return match_from_start(input_line, suffix);
        }
    }
    let mut pattern_chars = pattern.chars();
    if let Some(c) = pattern_chars.next() {
        if matches!(c, 'a'..='z' | 'A'..='Z' | ' ') {
            match pattern_chars.next() {
                Some('+') => {
                    let mut chars = input_line.chars();
                    loop {
                        let Some((start, input_line)) = chars.next().map(|c| (c, chars.as_str())) else { return false; };
                        if c == start {
                            if match_from_start(input_line, &pattern[2..]) {
                                return true;
                            }
                        } else {
                            return false;
                        }
                    }
                },
                Some('?') => {
                    if match_from_start(input_line, &pattern[2..]) {
                        return true;
                    }
                    let mut chars = input_line.chars();
                    loop {
                        let Some((start, input_line)) = chars.next().map(|c| (c, chars.as_str())) else { return false; };
                        if c == start {
                            if match_from_start(input_line, &pattern[2..]) {
                                return true;
                            }
                        } else {
                            return false;
                        }
                    }
                }
                _ => {
                    let mut chars = input_line.chars();
                    let Some((start, input_line)) = chars.next().map(|c| (c, chars.as_str())) else { return false; };
                    return c == start && match_from_start(input_line, &pattern[1..]);
                }
            }
        }
    }
    false
}


#[cfg(test)]
mod test {
    use crate::match_pattern;

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
        assert!(match_pattern("aaabb", "a+a"));
        assert!(match_pattern("aaabb", "aa+ab"));
        assert!(!match_pattern("aaabb", "aa+aa"));
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
