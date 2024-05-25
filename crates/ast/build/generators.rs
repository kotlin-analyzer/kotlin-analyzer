use std::char;

use inflector::Inflector;
use tokens::resolve_token;

use crate::ParseEntry;

enum GenInterface {
    Token {
        name: String,
        is_multiple: bool,
        syntax_name: String,
    },
    Ident {
        name: String,
        ret_type: String,
        is_multiple: bool,
    },
    Enum {
        variants: Vec<String>,
        name: String,
    },
}

fn build_interface(entry: &ParseEntry, is_multiple: bool) -> Vec<GenInterface> {
    match entry {
        ParseEntry::CharLit(_) | ParseEntry::StrLit(_) => {
            let name = match entry {
                ParseEntry::CharLit(ch) => ch.to_string(),
                ParseEntry::StrLit(st) => st.to_string(),
                _ => unreachable!(),
            };
            let token = resolve_token(&name);

            if let Some(token) = token {
                let token_fmt = format!("{:?}", token);
                let method_name = format!("get_{}", token_fmt.to_snake_case());
                let syntax_name = format!("{}", token_fmt);

                vec![GenInterface::Token {
                    name: method_name,
                    is_multiple,
                    syntax_name,
                }]
            } else {
                Vec::new()
            }
        }
        ParseEntry::Ident(id) => {
            if id.starts_with(char::is_lowercase) {
                vec![GenInterface::Ident {
                    name: id.to_snake_case(),
                    ret_type: id.to_pascal_case(),
                    is_multiple,
                }]
            } else {
                vec![GenInterface::Token {
                    name: format!("get_{}", id.to_snake_case()),
                    is_multiple,
                    syntax_name: id.to_string(),
                }]
            }
        }
        ParseEntry::Optional(entries) => entries
            .iter()
            .flat_map(|it| build_interface(it, is_multiple))
            .collect(),
        ParseEntry::Repeated(entries) => entries
            .iter()
            .flat_map(|it| build_interface(it, true))
            .collect(),
        ParseEntry::Choice(_) => todo!(),
        ParseEntry::Group(entries) => entries
            .iter()
            .flat_map(|it| build_interface(it, true))
            .collect(),
    }
}
