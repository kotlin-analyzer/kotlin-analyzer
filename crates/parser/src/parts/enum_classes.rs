use ast::syntax::SyntaxKind::*;
use tokens::Token;

use super::class_members::{class_body, class_member_declarations};
use super::expressions::value_arguments;
use super::identifiers::simple_identifier;
use super::modifiers::{parse_optional_modifiers, starts_modifiers};
use super::utils::starts_simple_identifier;
use crate::{parse_loop, parse_while, Parser};

const ENUM_RECOVERY: &[Token] = &[Token::R_CURL, Token::SEMICOLON, Token::NL, Token::EOF];

pub(crate) fn enum_class_body(parser: &mut Parser<'_, '_>) {
	parser.start_node(ENUM_CLASS_BODY);

	parse_loop! { parser =>
		if !parser.expect_recover(Token::L_CURL, "expected '{'", ENUM_RECOVERY) {
			break;
		}

		parser.skip_trivia_and_newlines();

		if starts_enum_entry(parser) {
			enum_entries(parser);
			parser.skip_trivia_and_newlines();
		}

		if parser.current_token() == Some(&Token::SEMICOLON) {
			parser.bump();
			parser.skip_trivia_and_newlines();
			class_member_declarations(parser);
			parser.skip_trivia_and_newlines();
		}

		if !parser.expect_recover(Token::R_CURL, "expected '}'", ENUM_RECOVERY) {
			break;
		}
		break;
	}

	parser.finish_node(ENUM_CLASS_BODY);
}

pub(crate) fn enum_entries(parser: &mut Parser<'_, '_>) {
	parser.start_node(ENUM_ENTRIES);

	enum_entry(parser);
	parser.skip_trivia_and_newlines();

	parse_while!(parser.current_token() == Some(&Token::COMMA), parser => {
		parser.bump();
		parser.skip_trivia_and_newlines();
		if starts_enum_entry(parser) {
			enum_entry(parser);
			parser.skip_trivia_and_newlines();
		} else {
			break;
		}
	});

	parser.finish_node(ENUM_ENTRIES);
}

pub(crate) fn enum_entry(parser: &mut Parser<'_, '_>) {
	parser.start_node(ENUM_ENTRY);

	parse_loop! { parser =>
		parse_optional_modifiers(parser);
		parser.skip_trivia_and_newlines();

		if starts_simple_identifier(parser) {
			simple_identifier(parser);
		} else {
			parser.error("expected enum entry name");
			parser.recover_until(ENUM_RECOVERY);
			break;
		}

		parser.skip_trivia_and_newlines();
		if parser.current_token() == Some(&Token::L_PAREN) {
			value_arguments(parser);
			parser.skip_trivia_and_newlines();
		}

		if parser.current_token() == Some(&Token::L_CURL) {
			class_body(parser);
		}
		break;
	}

	parser.finish_node(ENUM_ENTRY);
}

fn starts_enum_entry(parser: &mut Parser<'_, '_>) -> bool {
	starts_modifiers(parser) || starts_simple_identifier(parser)
}
