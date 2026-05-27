pub(crate) mod annotations;
pub(crate) mod class_members;
pub(crate) mod expressions;
mod identifiers;
pub(crate) mod modifiers;
pub(crate) mod types;

pub(crate) mod classes;
pub(crate) mod enum_classes;
pub(crate) mod statements;

pub(crate) mod general;

pub(crate) mod entry {
    use super::*;

    pub(crate) mod top {
        use crate::Parser;
        use crate::SyntaxKind::*;

        use super::*;

        pub(crate) fn kotlin_file(p: &mut Parser<'_>) {
            general::kotlin_file(p);
        }

        pub(crate) fn script(p: &mut Parser<'_>) {
            general::kotlin_file(p);
        }

        pub(crate) fn type_(p: &mut Parser<'_>) {
            let m = p.start();
            types::ty(p);
            if p.at(EOF) {
                m.abandon(p);
                return;
            }
            while !p.at(EOF) {
                p.bump_any();
            }
            m.complete(p, ERROR);
        }

        pub(crate) fn expr(p: &mut Parser<'_>) {
            let m = p.start();
            expressions::expression(p);
            if p.at(EOF) {
                m.abandon(p);
                return;
            }
            while !p.at(EOF) {
                p.bump_any();
            }
            m.complete(p, ERROR);
        }
    }
}
