pub(crate) fn extract_digits(s: &str) -> Result<(&str, &str), String> {
    take_while_err(|ch| ch.is_ascii_digit(), s, "expected digits".into())
}

pub(crate) fn extract_whitespace(s: &str) -> (&str, &str) {
    take_while(|ch| ch == ' ', s)
}

pub(crate) fn expect_whitespace(s: &str) -> Result<(&str, &str), String> {
    take_while_err(|ch| ch == ' ', s, "expected a space".to_string())
}

pub(crate) fn extract_ident(s: &str) -> Result<(&str, &str), String> {
    let input_starts_with_alphabet = s
        .chars()
        .next()
        .map(|ch| ch.is_ascii_alphabetic())
        .unwrap_or(false);

    if input_starts_with_alphabet {
        Ok(take_while(|ch| ch.is_ascii_alphanumeric(), s))
    } else {
        Err("expected string".to_string())
    }
}

pub(crate) fn take_while(accept: impl Fn(char) -> bool, s: &str) -> (&str, &str) {
    let extracted_ends = s
        .char_indices()
        .find_map(|(idx, c)| if accept(c) { None } else { Some(idx) })
        .unwrap_or_else(|| s.len());

    let extracted = &s[..extracted_ends];
    let remainder = &s[extracted_ends..];

    (remainder, extracted)
}

pub(crate) fn take_while_err(
    accept: impl Fn(char) -> bool,
    s: &str,
    error_msg: String,
) -> Result<(&str, &str), String> {
    let (remainder, extracted) = take_while(accept, s);

    if extracted.is_empty() {
        Err(error_msg)
    } else {
        Ok((remainder, extracted))
    }
}

pub(crate) fn extract_op(s: &str) -> (&str, &str) {
    match &s[0..1] {
        "+" | "-" | "*" | "/" => {}
        _ => panic!("bad operator"),
    }

    (&s[1..], &s[0..1])
}

pub(crate) fn tag<'a, 'b>(starting_text: &'a str, s: &'b str) -> Result<&'b str, String> {
    if s.starts_with(starting_text) {
        Ok(&s[starting_text.len()..])
    } else {
        Err(format!("expected '{}'", starting_text))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_one_digit() {
        assert_eq!(extract_digits("1+2"), Ok(("+2", "1")));
    }

    #[test]
    fn extract_multiple_digits() {
        assert_eq!(extract_digits("10-20"), Ok(("-20", "10")));
    }

    #[test]
    fn do_not_extract_digits_when_input_is_invalid() {
        assert_eq!(extract_digits("abcd"), Err("expected digits".to_string()));
    }

    #[test]
    fn extract_digits_with_no_remainder() {
        assert_eq!(extract_digits("100"), Ok(("", "100")));
    }

    #[test]
    fn extract_plus() {
        assert_eq!(extract_op("+2"), ("2", "+"));
    }

    #[test]
    fn extract_minus() {
        assert_eq!(extract_op("-10"), ("10", "-"));
    }

    #[test]
    fn extract_star() {
        assert_eq!(extract_op("*3"), ("3", "*"));
    }

    #[test]
    fn extract_slash() {
        assert_eq!(extract_op("/4"), ("4", "/"));
    }

    #[test]
    fn extract_spaces() {
        assert_eq!(extract_whitespace("    1"), ("1", "    "));
    }

    #[test]
    fn extract_alphabetic_ident() {
        assert_eq!(extract_ident("abcdEFGH stop"), Ok((" stop", "abcdEFGH")));
    }

    #[test]
    fn extract_alphanumeric_ident() {
        assert_eq!(extract_ident("factor1()"), Ok(("()", "factor1")));
    }

    #[test]
    fn cannot_extract_ident_beginning_with_number() {
        assert_eq!(extract_ident("123abc"), Err("expected string".to_string()));
    }

    #[test]
    fn tag_word() {
        assert_eq!(tag("let", "let a"), Ok(" a"));
    }
}
