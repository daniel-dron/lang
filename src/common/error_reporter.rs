use colored::Colorize;

use crate::{frontend::{ast::Span, lexer::Token}, middle::type_system::{Expected, TypeError, TypeErrorKind}};

fn get_earliest_and_latest_tokens(tokens: &[Token]) -> Option<(&Token, &Token)> {
    if tokens.is_empty() {
        return None;
    }

    // Use iterator methods for a more functional approach
    let earliest = tokens.iter().min_by_key(|token| token.span.start)?;

    let latest = tokens.iter().max_by_key(|token| token.span.end)?;

    Some((earliest, latest))
}

fn get_error_title(kind: &TypeErrorKind) -> String {
    match kind {
        TypeErrorKind::MissmatchedTypes {
            expected: _,
            got_ty: _,
            got_span: _,
        } => "mismatched types".into(),
        TypeErrorKind::TODO(text) => text.clone(),
    }
}

fn get_error_id(kind: &TypeErrorKind) -> u64 {
    match kind {
        TypeErrorKind::MissmatchedTypes {
            expected: _,
            got_ty: _,
            got_span: _,
        } => 100,
        TypeErrorKind::TODO(_) => 0,
    }
}

fn point_zone(tokens: &[Token]) -> Span {
    if tokens.is_empty() {
        return Span { start: 0, end: 0 };
    }

    let earliest = tokens.iter().min_by_key(|token| token.column).unwrap();
    let latest = tokens.iter().max_by_key(|token| token.column).unwrap();

    return Span {
        start: earliest.column,
        end: latest.column + latest.lexeme.len(),
    };
}

fn _point_to(tokens: &[Token]) -> String {
    let got_text = tokens
        .iter()
        .map(|token| token.lexeme.clone())
        .collect::<Vec<String>>()
        .join("");
    format!(
        "{}{}{}",
        " ".repeat(3),
        " ".repeat(tokens.last().unwrap().column - got_text.len()),
        "^".repeat(got_text.len())
    )
}

pub fn report(file: &str, error: TypeError, source: &str, tokens: &[Token]) {
    println!("[ERR] {:#?}", error);
    let lines: Vec<&str> = source.split('\n').collect();
    let span = &tokens[error.span.start..error.span.end];

    // get source span
    let (earliest, latest) = if let Some((earliest, latest)) = get_earliest_and_latest_tokens(span)
    {
        (earliest, latest)
    } else {
        panic!()
    };

    // always print the error code and its type/description first
    let id = get_error_id(&error.ty);
    let title = get_error_title(&error.ty);
    println!("\x1b[1;31merror[E{}]\x1b[0m: {}", id, title);
    println!(
        " \x1b[94m-->\x1b[0m {}:{}:{}",
        file, earliest.line, earliest.column
    );

    let context_end = (latest.line + 1).min(lines.len());
    let line_num_width = context_end.to_string().len();
    println!("{} |", " ".repeat(line_num_width));
    println!(
        "{}{}| {}",
        earliest.line,
        " ".repeat(line_num_width - earliest.line.to_string().len() + 1),
        &lines[earliest.line - 1..latest.line].join(" ")
    );

    match error.ty {
        TypeErrorKind::MissmatchedTypes {
            expected,
            got_ty,
            got_span,
        } => {
            match expected {
                Expected::Span(span) => {
                    let got_tokens = &tokens[got_span.start..got_span.end];
                    let expected_tokens = &tokens[span.start..span.end];

                    let expected_text = expected_tokens
                        .iter()
                        .map(|token| token.lexeme.clone())
                        .collect::<Vec<String>>()
                        .join("");

                    // got zone
                    let got_zone_span = point_zone(got_tokens);
                    let expected_span = point_zone(expected_tokens);

                    // get the length of the wrong expression that fits in a single line
                    let mut single_line_got_length: usize = 0;
                    let line = got_tokens[0].line;
                    for token in got_tokens {
                        if token.line == line {
                            single_line_got_length += token.span.len();
                        }
                    }

                    println!(
                        "{} | {}{}{}{} {}",
                        " ".repeat(line_num_width),
                        " ".repeat(expected_span.start),
                        "-".repeat(expected_span.len()).blue(), // underline the expected
                        " ".repeat(got_zone_span.start - expected_span.end),
                        "^".repeat(single_line_got_length).red(),
                        format!(
                            "expected `{}`, but found `{}`",
                            expected_text,
                            got_ty.to_string()
                        )
                        .red()
                    );

                    println!(
                        "{} | {}{}",
                        " ".repeat(line_num_width),
                        " ".repeat(expected_span.start),
                        "|".blue()
                    );

                    println!(
                        "{} | {}{}",
                        " ".repeat(line_num_width),
                        " ".repeat(expected_span.start),
                        "expected due to this".blue()
                    );
                }
                Expected::Named(ty) => {
                    let got_tokens = &tokens[got_span.start..got_span.end];
                    let got_span = point_zone(got_tokens);

                    println!(
                        "{} | {}{} {}",
                        " ".repeat(line_num_width),
                        " ".repeat(got_span.start),
                        "^".repeat(got_span.len()).red(),
                        format!("expected `{}`, but found `{}`", ty, got_ty.to_string()).red()
                    );
                }
            };
        }
        TypeErrorKind::TODO(text) => println!("{}", text),
    };
}
