use algorithm_w::{error::InferenceError, infer::infer_type_only, parser};

fn infer(input: &str) -> Result<String, InferenceError> {
    let expr = parser::ExprParser::new().parse(input).expect("parse error");
    infer_type_only(&expr).map(|ty| ty.to_string())
}

fn assert_types_equal(actual: &str, expected: &str) {
    let actual_ty = parser::TypeParser::new()
        .parse(actual)
        .expect(&format!("failed to parse actual type: {}", actual));
    let expected_ty = parser::TypeParser::new()
        .parse(expected)
        .expect(&format!("failed to parse expected type: {}", expected));

    assert!(
        actual_ty.alpha_eq(&expected_ty),
        "Types not alpha-equivalent:\n  actual:   {}\n  expected: {}",
        actual,
        expected
    );
}

mod basic_literals {
    use super::*;

    #[test]
    fn int_literal() {
        let actual = infer("42").unwrap();
        let expected = "Int";
        assert_eq!(actual, expected);
    }

    #[test]
    fn true_literal() {
        let actual = infer("true").unwrap();
        let expected = "Bool";
        assert_eq!(actual, expected);
    }

    #[test]
    fn false_literal() {
        let actual = infer("false").unwrap();
        let expected = "Bool";
        assert_eq!(actual, expected);
    }
}

mod variables_and_identity {
    use super::*;

    #[test]
    fn identity_x() {
        let actual = infer(r"\x -> x").unwrap();
        let expected = "a -> a";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn identity_f() {
        let actual = infer(r"\f -> f").unwrap();
        let expected = "a -> a";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn k_combinator_inline() {
        let actual = infer(r"\x -> \y -> x").unwrap();
        let expected = "a -> b -> a";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn second_arg_selector() {
        let actual = infer(r"\x -> \y -> y").unwrap();
        let expected = "a -> b -> b";
        assert_types_equal(&actual, expected);
    }
}

mod application {
    use super::*;

    #[test]
    fn identity_applied_to_int() {
        let actual = infer(r"(\x -> x) 42").unwrap();
        let expected = "Int";
        assert_eq!(actual, expected);
    }

    #[test]
    fn identity_applied_to_bool() {
        let actual = infer(r"(\x -> x) true").unwrap();
        let expected = "Bool";
        assert_eq!(actual, expected);
    }

    #[test]
    fn nested_application() {
        let actual = infer(r"(\f -> \x -> f x) (\y -> y) 42").unwrap();
        let expected = "Int";
        assert_eq!(actual, expected);
    }
}

mod let_expressions {
    use super::*;

    #[test]
    fn let_int() {
        let actual = infer("let x = 42 in x").unwrap();
        let expected = "Int";
        assert_eq!(actual, expected);
    }

    #[test]
    fn let_identity() {
        let actual = infer(r"let f = \x -> x in f").unwrap();
        let expected = "a -> a";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn let_identity_applied_to_int() {
        let actual = infer(r"let id = \x -> x in id 42").unwrap();
        let expected = "Int";
        assert_eq!(actual, expected);
    }

    #[test]
    fn let_identity_applied_to_bool() {
        let actual = infer(r"let id = \x -> x in id true").unwrap();
        let expected = "Bool";
        assert_eq!(actual, expected);
    }

    #[test]
    fn nested_let() {
        let actual = infer(r"let f = \x -> x in let g = \y -> y in f (g 42)").unwrap();
        let expected = "Int";
        assert_eq!(actual, expected);
    }
}

mod tuples {
    use super::*;

    #[test]
    fn pair() {
        let actual = infer("(42, true)").unwrap();
        let expected = "(Int, Bool)";
        assert_eq!(actual, expected);
    }

    #[test]
    fn triple() {
        let actual = infer("(true, false, 42)").unwrap();
        let expected = "(Bool, Bool, Int)";
        assert_eq!(actual, expected);
    }

    #[test]
    fn duplicate_in_tuple() {
        let actual = infer(r"(\x -> (x, x)) 42").unwrap();
        let expected = "(Int, Int)";
        assert_eq!(actual, expected);
    }

    #[test]
    fn pair_constructor() {
        let actual = infer(r"let pair = \x -> \y -> (x, y) in pair 42 true").unwrap();
        let expected = "(Int, Bool)";
        assert_eq!(actual, expected);
    }
}

mod higher_order_functions {
    use super::*;

    #[test]
    fn apply() {
        let actual = infer(r"\f -> \x -> f x").unwrap();
        let expected = "(a -> b) -> a -> b";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn twice() {
        let actual = infer(r"\f -> \x -> f (f x)").unwrap();
        let expected = "(a -> a) -> a -> a";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn compose() {
        let actual = infer(r"\f -> \g -> \x -> f (g x)").unwrap();
        let expected = "(a -> b) -> (c -> a) -> c -> b";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn let_twice() {
        let actual = infer(r"let twice = \f -> \x -> f (f x) in twice").unwrap();
        let expected = "(a -> a) -> a -> a";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn let_compose() {
        let actual = infer(r"let compose = \f -> \g -> \x -> f (g x) in compose").unwrap();
        let expected = "(a -> b) -> (c -> a) -> c -> b";
        assert_types_equal(&actual, expected);
    }
}

mod complex_examples {
    use super::*;

    #[test]
    fn k_combinator() {
        let actual = infer(r"let K = \x -> \y -> x in K").unwrap();
        let expected = "a -> b -> a";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn s_combinator() {
        let actual = infer(r"let S = \f -> \g -> \x -> f x (g x) in S").unwrap();
        let expected = "(a -> b -> c) -> (a -> b) -> a -> c";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn y_combinator() {
        let actual = infer(r"let Y = \f -> (\x -> f (x x)) (\x -> f (x x)) in Y");
        assert!(matches!(actual, Err(InferenceError::OccursCheck { .. })));
    }
}

mod polymorphic_examples {
    use super::*;

    #[test]
    fn tuple_of_identities() {
        let actual = infer(r"let id = \x -> x in (id, id)").unwrap();
        let expected = "(a -> a, b -> b)";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn const_combinator() {
        let actual = infer(r"let const = \x -> \y -> x in const").unwrap();
        let expected = "a -> b -> a";
        assert_types_equal(&actual, expected);
    }

    #[test]
    fn flip() {
        let actual = infer(r"let flip = \f -> \x -> \y -> f y x in flip").unwrap();
        let expected = "(a -> b -> c) -> b -> a -> c";
        assert_types_equal(&actual, expected);
    }
}

mod error_cases {
    use super::*;

    #[test]
    fn self_application() {
        let actual = infer(r"\x -> x x");
        assert!(matches!(actual, Err(InferenceError::OccursCheck { .. })));
    }

    #[test]
    fn omega() {
        let actual = infer(r"(\x -> x x) (\x -> x x)");
        assert!(matches!(actual, Err(InferenceError::OccursCheck { .. })));
    }
}
