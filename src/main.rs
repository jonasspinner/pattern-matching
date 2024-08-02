use std::env;
use std::io;
use std::process;

fn match_pattern(input_line: &str, pattern: &str) -> bool {
    match pattern {
        "\\d" => input_line.chars().any(|c| matches!(c, '0'..='9')),
        "\\w" => input_line.chars().any(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')),
        s if s.starts_with("[") && s.ends_with(']') => {
            let group = &s[1..s.len() - 1];
            if group.starts_with('^') {
                !input_line.chars().all(|c| group[1..].contains(c))
            } else {
                input_line.chars().any(|c| group.contains(c))
            }
        }
        s if s.len() == 1 => {
            let c = s.chars().next().unwrap();
            input_line.contains(c)
        }
        _ => panic!("unrecognized pattern")
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
