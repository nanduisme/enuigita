use crate::{
    env::Env,
    expr::{self, Expr},
    utils,
};

#[derive(Debug, PartialEq)]
pub struct BindingDef {
    pub name: String,
    pub val: Expr,
}

impl BindingDef {
    pub(crate) fn new(s: &str) -> Result<(&str, Self), String> {
        let s = utils::tag("let", s)?;
        let (s, _) = utils::expect_whitespace(s)?;

        let (s, name) = utils::extract_ident(s)?;
        let (s, _) = utils::extract_whitespace(s);

        let s = utils::tag("=", s)?;
        let (s, _) = utils::extract_whitespace(s);

        let (s, expr) = Expr::new(s)?;

        Ok((
            s,
            Self {
                name: name.to_string(),
                val: expr,
            },
        ))
    }

    pub(crate) fn eval(&self, env: &mut Env) -> Result<(), String> {
        env.store_binding(self.name.clone(), self.val.eval(env)?) ;        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::{Number, Op};

    #[test]
    fn parse_binding_def() {
        assert_eq!(
            BindingDef::new("let a = 10 / 2"),
            Ok((
                "",
                BindingDef {
                    name: "a".to_string(),
                    val: Expr::Operation {
                        lhs: Number(10),
                        rhs: Number(2),
                        op: Op::Div,
                    },
                },
            )),
        );
    }

    #[test]
    fn cannot_parse_binding_def_without_space_after_let() {
        assert_eq!(
            BindingDef::new("letaaa=1+2"),
            Err("expected a space".to_string()),
        );
    }
}
