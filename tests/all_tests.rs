mod infra;

// Your tests go here!
success_tests! {
    {
        name: tuple_basic,
        file: "egg_tuple_basic.snek",
        input: "true",
        expected: "(tuple 1 2 3 4 5 6 7 8)"
    },
    {
        name: tuple_nested,
        file: "egg_tuple_nested.snek",
        input: "true",
        expected: "(tuple 1 (tuple 1 2))"
    },
    {
        name: tuple_index_nested,
        file: "egg_tuple_index.snek",
        input: "true",
        expected: "(tuple 3 4)"
    },
    {
        name: tuple_index,
        file: "egg_index.snek",
        input: "true",
        expected: "2"
    },
    {
        name: isnil_true,
        file: "isnil.snek",
        input: "nil",
        expected: "true"
    },
    {
        name: isnil_false,
        file: "isnil.snek",
        input: "2",
        expected: "false"
    },
    {
        name: print_input_num,
        file: "print_input.snek",
        input: "2",
        expected: "2\n2"
    },
    {
        name: print_input_bool,
        file: "print_input.snek",
        input: "true",
        expected: "true\ntrue"
    },
    {
        name: print_nested,
        file: "print_nested.snek",
        input: "0",
        expected: "4\n4"
    },
    {
        name: print_block,
        file: "print_block.snek",
        input: "6",
        expected: "6\n6\n6"
    },
    {
        name: fun_increment,
        file: "fun_increment.snek",
        input: "7",
        expected: "8"
    },
    {
        name: fun_plus,
        file: "fun_plus.snek",
        input: "7",
        expected: "7"
    },
    {
        name: fun_plusplus,
        file: "fun_plusplus.snek",
        input: "true",
        expected: "8"
    },
    {
        name: fun_nestedargs,
        file: "fun_nestedargs.snek",
        input: "true",
        expected: "8"
    },
    {
        name: fun_sum3,
        file: "fun_sum3.snek",
        input: "true",
        expected: "15"
    },
    {
        name: fun_sum4,
        file: "fun_sum4.snek",
        input: "true",
        expected: "10"
    },
    {
        name: fun_nested,
        file: "fun_nested.snek",
        input: "true",
        expected: "10"
    },
    {
        name: fun_weightedsum,
        file: "fun_weightedsum.snek",
        input: "true",
        expected: "321"
    },
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: fact_rec,
        file: "fun_recursive_fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: fun_noarg,
        file: "fun_noarg.snek",
        input: "true",
        expected: "1",
    },
    {
        name: add1,
        file: "boa_add1.snek",
        input: "true",
        expected: "5"
    },
    {
        name: sub1,
        file: "boa_sub1.snek",
        input: "4",
        expected: "4"
    },
    {
        name: plus,
        file: "boa_plus.snek",
        input: "4",
        expected: "5"
    },
    {
        name: minus,
        file: "boa_minus.snek",
        input: "4",
        expected: "3"
    },
    {
        name: times,
        file: "boa_times.snek",
        input: "true",
        expected: "4"
    },
    {
        name: nested_arith_comp,
        file: "boa_nested_arith_comp.snek",
        input: "true",
        expected: "5"
    },
    {
        name: binding,
        file: "boa_binding.snek",
        input: "true",
        expected: "5"
    },
    {
        name: let_nested,
        file: "boa_let_nested.snek",
        input: "true",
        expected: "20"
    },
    {
        name: let_expr,
        file: "boa_let_expr.snek",
        input: "true",
        expected: "12"
    },
    {
        name: let_dependency,
        file: "boa_let_dependency.snek",
        input: "true",
        expected: "21"
    },
    {
        name: let_succ,
        file: "boa_let_succ.snek",
        input: "true",
        expected: "5"
    },
    {
        name: let_inner,
        file: "boa_let_inner.snek",
        input: "true",
        expected: "10"
    },
    {
        name: let_outer,
        file: "boa_let_outer.snek",
        input: "true",
        expected: "7"
    },
    {
        name: boa_let_multiple_bindings,
        file: "boa_let_multiple_bindings.snek",
        input: "true",
        expected: "6"
    },
    {
        name: greater,
        file: "cobra_greater.snek",
        input: "false",
        expected: "false"
    },
    {
        name: greater_equal,
        file: "cobra_greater_equal.snek",
        input: "false",
        expected: "true"
    },
    {
        name: less,
        file: "cobra_less.snek",
        input: "false",
        expected: "true"
    },
    {
        name: less_equal,
        file: "cobra_less_equal.snek",
        input: "false",
        expected: "true"
    },
    {
        name: equal_num,
        file: "cobra_equal_num.snek",
        input: "false",
        expected: "true"
    },
    {
        name: equal_bool,
        file: "cobra_equal_bool.snek",
        input: "false",
        expected: "true"
    },
    {
        name: isnum_true,
        file: "cobra_isnum.snek",
        input: "3",
        expected: "true"
    },
    {
        name: isnum_false,
        file: "cobra_isnum.snek",
        input: "true",
        expected: "false"
    },
    {
        name: isbool_true,
        file: "cobra_isbool.snek",
        input: "true",
        expected: "true"
    },
    {
        name: isbool_false,
        file: "cobra_isbool.snek",
        input: "2",
        expected: "false"
    },
    {
        name: input_1,
        file: "cobra_input.snek",
        input: "true",
        expected: "true"
    },
    {
        name: input_2,
        file: "cobra_input.snek",
        input: "12",
        expected: "12"
    },
    {
        name: input_4,
        file: "cobra_input.snek",
        input: "-12",
        expected: "-12"
    },
    {
        name: set,
        file: "cobra_set.snek",
        expected: "6"
    },
    {
        name: loop1,
        file: "cobra_loop1.snek",
        expected: "-6"
    },
    {
        name: fac,
        file: "cobra_fac.snek",
        input: "3",
        expected: "6"
    },
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "cobra_invalid_argument.snek",
        expected: "not a number",
    },
    {
        name: input_compare_3,
        file: "cobra_input_compare.snek",
        input: "true",
        expected: "not a number",
    },
    {
        name: input_3,
        file: "cobra_input.snek",
        input: "4611686018427387904",
        expected: "overflow"
    },
    {
        name: compare_boolean,
        file: "cobra_compare_boolean.snek",
        expected: "not a number",
    },
    {
        name: index_oob,
        file: "egg_oob.snek",
        expected: "index out of bounds",
    },
}

static_error_tests! {
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "",
    },
    {
        name: invalid_argnum,
        file: "invalid_argnum.snek",
        expected: "",
    },
    {
        name: fun_imaginary,
        file: "fun_imaginary.snek",
        expected: "",
    },
    {
        name: input_param,
        file: "fun_input_param.snek",
        expected: "",
    },
    {
        name: fun_empty_body,
        file: "fun_empty_body.snek",
        input: "true",
        expected: "",
    },
    {
        name: fun_input_param,
        file: "fun_input_param.snek",
        input: "true",
        expected: "",
    },
    {
        name: fun_aftermain,
        file: "fun_aftermain.snek",
        input: "true",
        expected: "",
    },
    {
        name: unbound_id,
        file: "boa_unbound_id.snek",
        input: "true",
        expected: "Unbound variable identifier x",
    },
    {
        name: let_scoping_error,
        file: "boa_let_scoping_error.snek",
        input: "true",
        expected: "Unbound variable identifier x",
    },
    {
        name: let_nobinding,
        file: "boa_let_nobinding.snek",
        input: "true",
        expected: "Invalid",
    },
    {
        name: duplicate_binding,
        file: "boa_duplicate_binding.snek",
        input: "true",
        expected: "Duplicate binding",
    },
    {
        name: let_nobody,
        file: "boa_let_nobody.snek",
        input: "true",
        expected: "Invalid",
    },
    {
        name: empty_paren,
        file: "boa_empty_paren.snek",
        input: "true",
        expected: "Invalid",
    },
}
