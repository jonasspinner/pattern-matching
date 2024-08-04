# Pattern matching


This is a Rust solution to the
["Build Your Own grep" Challenge](https://app.codecrafters.io/courses/grep/overview).


## Supported patterns
+ `\d` Digit
+ `\w` Alphanumeric
+ `.`  Wildcard
+ `a` Literal
+ `?` Zero or one
+ `+` One or more
+ `[abc]` Positive character set
+ `[^xyz]` Negative character set
+ `^` Start of string marker
+ `$` End of string marker
+ `(..)` Group
+ `(..|..)` Alternation
+ `\1` Backreference

### Codecrafters

[Regular expressions](https://en.wikipedia.org/wiki/Regular_expression)
(Regexes, for short) are patterns used to match character combinations in
strings. [`grep`](https://en.wikipedia.org/wiki/Grep) is a CLI tool for
searching using Regexes.

In this challenge you'll build your own implementation of `grep`. Along the way
we'll learn about Regex syntax, how parsers/lexers work, and how regular
expressions are evaluated.

[![progress-banner](https://backend.codecrafters.io/progress/grep/0610c8e4-d849-4b13-b8fc-c926f97a752b)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF)
