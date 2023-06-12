use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use sexp::Atom::*;
use sexp::*;
use im::{HashMap, hashmap};
use Definition::*;

struct Program {
    defs: Vec<Definition>,
    main: Expr,
    info: HashMap<String, i32>,
}

enum Definition {
    Fun(String, Vec<String>, Expr),
}

#[derive(Debug, Clone)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
    RegOffsetIndex(Reg, Reg, i32, i32), //ASSUMES NEGATIVE OFFSET
    Label(String),
    FunName(String),
}


#[derive(Debug, Clone)]
enum Reg {
    RAX,
    RSP,
    RDI,
    RBX,
    RCX,
    RDX,
    R15,
    RSI,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IMovq(Val, Val),
    ICMove(Val, Val),
    ICMovg(Val, Val),
    ICMovl(Val, Val),
    ICMovge(Val, Val),
    ICMovle(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    ICall(Val),
    IJump(Val),
    IJumpEq(Val),
    IJumpNe(Val),
    IJumpNz(Val),
    IXor(Val, Val),
    ICmp(Val, Val),
    ITest(Val, Val),
    ILabel(Val),
    ISar(Val, Val),
    IJumpOf(Val),
    IAnd(Val, Val),
    IRet,
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
    IsNil, 
    Print,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    DoubleEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual, 
}

#[derive(Debug)]
enum Expr {
    Number(i64),
    Boolean(bool),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Tuple(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    SetIdx(Box<Expr>, Box<Expr>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Call(String, Vec<Expr>),
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if(*n < -4611686018427387904 || *n > 4611686018427387903) {
                panic!("Invalid");
            }
            Expr::Number(i64::try_from(*n).unwrap())
        }
        Sexp::Atom(S(s)) if s == "true" => Expr::Boolean(true),
        Sexp::Atom(S(s)) if s == "false" => Expr::Boolean(false),
        Sexp::Atom(S(s)) => Expr::Id(s.to_string()),
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnil" => Expr::UnOp(Op1::IsNil, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "print" => Expr::UnOp(Op1::Print, Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), id, e] if keyword == "set!" => Expr::Set(id.to_string(), Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(keyword)), e] if keyword == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    Expr::Block(exprs.into_iter().map(parse_expr).collect())
                }
                [Sexp::Atom(S(op)), exprs @ ..] if op == "tuple" => {
                    Expr::Tuple(exprs.into_iter().map(parse_expr).collect())
                }
                [Sexp::Atom(S(keyword)), cond, thn, els] if keyword == "if" => Expr::If(
                    Box::new(parse_expr(cond)),
                    Box::new(parse_expr(thn)),
                    Box::new(parse_expr(els)),
                ),
                [Sexp::Atom(S(keyword)), e1, e2] if keyword == "index" => Expr::Index(Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(keyword)), e1, e2, e3] if keyword == "setidx" => Expr::SetIdx(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "==" => Expr::BinOp(Op2::DoubleEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), bindings, body] if op == "let" => {
                    let mut vector: Vec<(String, Expr)> = Vec::new();
                    match bindings {
                        Sexp::List(vec) => {
                            if(vec.len() == 0) {
                                panic!("Invalid")
                            }
                            for sexp in vec {
                                match sexp {
                                    Sexp::List(vect) => {
                                        let slice = &vect[..];
                                        match slice {
                                            [Sexp::Atom(S(id)), e] => {
                                                let mut set:HashSet<String> = HashSet::new();
                                                set.insert("add1".to_string()); set.insert("sub1".to_string()); set.insert("+".to_string());
                                                set.insert("-".to_string()); set.insert("*".to_string()); set.insert("let".to_string());
                                                set.insert("<".to_string()); set.insert("<=".to_string()); set.insert(">".to_string());
                                                set.insert("=".to_string()); set.insert(">=".to_string()); set.insert("isnum".to_string());
                                                set.insert("isbool".to_string()); set.insert("block".to_string()); set.insert("loop".to_string());
                                                set.insert("break".to_string()); set.insert("set!".to_string()); set.insert("input".to_string());
                                                set.insert("if".to_string());
                                                if(set.contains(id)) {
                                                    panic!("Invalid - keyword usage")
                                                }
                                                vector.push((id.to_string(), parse_expr(e)))
                                            }
                                            _ => panic!("Invalid")
                                        }
                                    }
                                    _ => panic!("Invalid")
                                }
                            }
                            Expr::Let(vector, Box::new(parse_expr(body)))
                        }
                        _ => panic!("Invalid")
                    }
                }
                [Sexp::Atom(S(funname)), args @ ..] => {
                    let mut set:HashSet<String> = HashSet::new();
                    set.insert("add1".to_string()); set.insert("sub1".to_string()); set.insert("+".to_string());
                    set.insert("-".to_string()); set.insert("*".to_string()); set.insert("let".to_string());
                    set.insert("<".to_string()); set.insert("<=".to_string()); set.insert(">".to_string());
                    set.insert("=".to_string()); set.insert(">=".to_string()); set.insert("isnum".to_string());
                    set.insert("isbool".to_string()); set.insert("block".to_string()); set.insert("loop".to_string());
                    set.insert("break".to_string()); set.insert("set!".to_string()); set.insert("input".to_string());
                    set.insert("if".to_string()); set.insert("fun".to_string()); set.insert("print".to_string());
                        if(set.contains(funname)) {
                            panic!("Invalid (not a call)")
                        }
                    Expr::Call(funname.to_string(), args.into_iter().map(parse_expr).collect())
                }
                _ => panic!("Invalid"),
            }
        },
        _ => panic!("Invalid"),
    }
}

fn is_def(s: &Sexp) -> bool {
    match s {
        Sexp::List(def_vec) => match &def_vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(_), _] if keyword == "fun" => true,
            _ => false
        }
        _ => false,
    }
}

fn parse_definition(s: &Sexp) -> (Definition, (String, i32)) {
    let mut set:HashSet<String> = HashSet::new();
    set.insert("add1".to_string()); set.insert("sub1".to_string()); set.insert("+".to_string());
    set.insert("-".to_string()); set.insert("*".to_string()); set.insert("let".to_string());
    set.insert("<".to_string()); set.insert("<=".to_string()); set.insert(">".to_string());
    set.insert("=".to_string()); set.insert(">=".to_string()); set.insert("isnum".to_string());
    set.insert("isbool".to_string()); set.insert("block".to_string()); set.insert("loop".to_string());
    set.insert("break".to_string()); set.insert("set!".to_string()); set.insert("input".to_string());
    set.insert("if".to_string()); set.insert("fun".to_string()); set.insert("print".to_string());
    match s {
        Sexp::List(def_vec) => match &def_vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(name_vec), body] if keyword == "fun" => {
                if(name_vec.len() < 1) {
                    panic!("Invalid")
                }
                let mut funname: String = "".to_string();
                let mut args: Vec<String> = Vec::new();
                let mut dups: HashSet<String> = HashSet::new();
                for i in 0..name_vec.len() {
                    match &name_vec[i] {
                        Sexp::Atom(S(string)) => {
                            if(set.contains(string)) {
                                panic!("Invalid - keyword usage")
                            }
                            if(i == 0) {
                                funname = string.to_string();
                            }
                            else {
                                if(dups.contains(string)) {
                                    panic!("Invalid - duplicate params");
                                }
                                if(string == "input") {
                                    panic!("Invalid - usage of input in def");
                                }
                                args.push(string.to_string());
                                dups.insert(string.to_string());
                            }
                        }
                        _ => panic!("Invalid definition, found non-string")
                    }
                }
                (Fun(funname.to_string(), args.clone(), parse_expr(body)), (funname.to_string(), args.len().try_into().unwrap()))
            }
            _ => panic!("Invalid fun name")
        }
    _ => panic!("Invalid - Bad fundef"),
    }
}

fn parse_program(s: &Sexp) -> Program {
    match s {
        Sexp::List(vec) => {
            let mut defs: Vec<Definition> = vec![];
            let mut fun_info: HashMap<String, i32> = HashMap::new();
            let mut index = 0;
            for def_or_exp in vec {
                if is_def(def_or_exp) {
                    let result = parse_definition(def_or_exp);
                    if(fun_info.contains_key(&result.1.0)) {
                        panic!("Invalid - duplicate function name")
                    }
                    defs.push(result.0);
                    fun_info.insert(result.1.0, result.1.1);
                    index = index + 1;
                } else {
                    if(index != vec.len() - 1) {
                        panic!("Invalid - more after main?")
                    }
                    return Program {
                        defs: defs,
                        main: parse_expr(def_or_exp),
                        info: fun_info,
                    };
                }
            }
            panic!("Invalid - Only found definitions");
        }
        _ => panic!("Invalid - Program should be a list")
    }
}

fn compile_to_instrs(e: &Expr, si: i32, vec: &mut Vec<Instr>, env: &HashMap<String, i32>, brake: &String, l: &mut i32, funs: &HashMap<String, i32>, is_def: bool) {
    //println!("{:?}", e);
    match e {
        Expr::Number(n) => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm((*n) << 1))),
        Expr::Boolean(true) => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(7))), //true = 7
        Expr::Boolean(false) => vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3))),//false = 3
        Expr::UnOp(op, e1) => {
             match op {
                Op1::Add1 => {
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(2)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op1::Sub1 => {
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op1::IsNum => {
                    let end_label = new_label(l, "ifend");
                    let else_label = new_label(l, "ifelse");
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IJumpNz(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(7)));
                    vec.push(Instr::IJump(Val::Label(end_label.clone())));
                    vec.push(Instr::ILabel(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::ILabel(Val::Label(end_label.clone())));
                }
                Op1::IsBool => {
                    let end_label = new_label(l, "ifend");
                    let else_label = new_label(l, "ifelse");
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IJumpNz(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::IJump(Val::Label(end_label.clone())));
                    vec.push(Instr::ILabel(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(7)));
                    vec.push(Instr::ILabel(Val::Label(end_label.clone())));
                }
                Op1::IsNil => {
                    let end_label = new_label(l, "ifend");
                    let else_label = new_label(l, "ifelse");
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IJumpEq(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::IJump(Val::Label(end_label.clone())));
                    vec.push(Instr::ILabel(Val::Label(else_label.clone())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(7)));
                    vec.push(Instr::ILabel(Val::Label(end_label.clone())));
                }
                Op1::Print => {
                    let offset = 8;

                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset.into())));
                    
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, 0), Val::Reg(Reg::RDI)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
                    vec.push(Instr::ICall(Val::FunName("snek_print".to_string())));
                    vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, 0)));

                    vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset.into())));
                }
                
             }
        }
        Expr::Id(id) => {
            let mut set:HashSet<String> = HashSet::new();
            set.insert("add1".to_string()); set.insert("sub1".to_string()); set.insert("+".to_string());
            set.insert("-".to_string()); set.insert("*".to_string()); set.insert("let".to_string());
            set.insert("<".to_string()); set.insert("<=".to_string()); set.insert(">".to_string());
            set.insert("=".to_string()); set.insert(">=".to_string()); set.insert("isnum".to_string());
            set.insert("isbool".to_string()); set.insert("block".to_string()); set.insert("loop".to_string());
            set.insert("break".to_string()); set.insert("set!".to_string()); set.insert("input".to_string());
            set.insert("if".to_string()); set.insert("fun".to_string()); set.insert("print".to_string());
            if(is_def && id == "input") {
                panic!("Invalid - usage of input in body")
            }
            if(id == "input") {
                vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI)));
            }
            else if (id == "nil") {
                vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
            }
            else if(set.contains(id)) {
                panic!("keyword usage");
            }
            else if(!env.contains_key(id)) {
                panic!("Unbound variable identifier {}", id);
            }
            else {
                vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, *env.get(id).expect("Unbound variable identifier"))))
            } 
        }
        Expr::Let(bindings, body) => {
            let mut num_vars = 0;
            let mut nenv: HashMap<String, i32> = env.clone();
            let mut set: HashSet<String> = HashSet::new();
            for (var, e) in bindings {
                if(set.contains(var)) {
                    panic!("Duplicate binding")
                }
                set.insert(var.to_string());
                compile_to_instrs(e, si+num_vars, vec, &nenv, brake, l, funs, is_def);
                vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, ((si+num_vars)) * 8), Val::Reg(Reg::RAX)));
                nenv = nenv.update(var.to_string(), (si+num_vars)*8);
                num_vars += 1;
            }
            compile_to_instrs(body, si+num_vars, vec, &nenv, brake, l, funs, is_def)
        }
        Expr::BinOp(op, e1, e2) => {
            match op {
                Op2::Plus => {
                    //let stack_offset = si * 8;
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op2::Minus => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e2, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e1, si + 1, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op2::Times => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(9)));
                    vec.push(Instr::IJumpOf(Val::Label("throw_error".to_string())));
                }
                Op2::Equal => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    vec.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(7))); //7 here is error code
                    vec.push(Instr::IJumpNe(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::ICMove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::Greater => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::ICMovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::GreaterEqual => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::ICMovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::Less => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::ICMovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::LessEqual => {
                    let stack_offset = si * 8;
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
                    vec.push(Instr::IJumpNz(Val::Label("throw_error".to_string())));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    vec.push(Instr::ICMovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                }
                Op2::DoubleEqual => {
                    let stack_offset = si * 8;
                    
                    compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
                    compile_to_instrs(e2, si + 1, vec, env, brake, l, funs, is_def);
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    vec.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(3)));
                    vec.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(7)));
                    vec.push(Instr::IJumpNe(Val::Label("throw_error".to_string())));
                    
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    vec.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
                    vec.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(3)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(7))); //7 here is error code
                    vec.push(Instr::IJumpNe(Val::Label("throw_error".to_string())));

                    // call rust function
                    let offset = 8;
                    vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset.into())));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, 0), Val::Reg(Reg::RDI)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, stack_offset + offset)));
                    vec.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Reg(Reg::RAX)));
                    vec.push(Instr::ICall(Val::FunName("snek_eq".to_string())));

                    vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset.into())));
                    
                }
            }
        }
        Expr::If(cond, thn, els) => {
            let end_label = new_label(l, "ifend");
            let else_label = new_label(l, "ifelse");
            compile_to_instrs(cond, si, vec, env, brake, l, funs, is_def);
            vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(3)));
            vec.push(Instr::IJumpEq(Val::Label(else_label.clone())));
            compile_to_instrs(thn, si, vec, env, brake, l, funs, is_def);
            vec.push(Instr::IJump(Val::Label(end_label.clone())));
            vec.push(Instr::ILabel(Val::Label(else_label)));
            compile_to_instrs(els, si, vec, env, brake, l, funs, is_def);
            vec.push(Instr::ILabel(Val::Label(end_label)));
        }
        Expr::Loop(e1) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "endloop");
            vec.push(Instr::ILabel(Val::Label(startloop.clone())));
            compile_to_instrs(e1, si, vec, env, &endloop, l, funs, is_def);
            vec.push(Instr::IJump(Val::Label(startloop)));
            vec.push(Instr::ILabel(Val::Label(endloop)));
        }
        Expr::Break(e1) => {
            if(brake == "") {
                panic!("break not within loop");
            }
            compile_to_instrs(e1, si, vec, env, brake, l, funs, is_def);
            vec.push(Instr::IJump(Val::Label(brake.to_string())))
        }
        Expr::Set(id, e1) => {
            if(!env.contains_key(id)) {
                panic!("Unbound variable identifier {}", id);
            }
            compile_to_instrs(e1, si, vec, &env, brake, l, funs, is_def);
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, *env.get(id).unwrap()), Val::Reg(Reg::RAX)));
        }
        Expr::Block(es) => {
            if(es.len() == 0) {
                panic!("Invalid")
            }
            for expr in es {
                compile_to_instrs(expr, si, vec, env, brake, l, funs, is_def)
            }
        }
        Expr::Call(name, args) => {
            if(!funs.contains_key(name)) {
                panic!("Invalid - function name doesn't exist")
            }
            if *funs.get(name).unwrap() != args.len() as i32 {
                panic!("Invalid - calling function with invalid number of arguments")
            }
            let mut curr_word = si * 8;
            let mut offset: i32 = (((args.len() + 1))).try_into().unwrap();
            let modified_offset = if offset % 2 == 1 {8} else {0};
            offset = if offset % 2 == 0 {offset * 8} else {(offset + 1) * 8}; //offset will be even
            for i in 0..args.len() {
                if(i == args.len() - 1) {
                    break;
                }
                compile_to_instrs(&args[i], si + i as i32, vec, env, brake, l, funs, is_def);
                vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, curr_word), Val::Reg(Reg::RAX)));
                curr_word = curr_word + 8;
            }
            if(args.len() > 1) {
                curr_word = curr_word - 8; // account for increment
            }
            
            vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset.into())));

            let mut curr_word_after_sub = offset + curr_word; //points to 2nd to last argument, last arg in rax
            if(args.len() != 0) {
                for i in (0..(args.len() - 1)).rev() { // ???
                    vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, curr_word_after_sub)));
                    vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, (i * 8) as i32), (Val::Reg(Reg::RBX))));
                    curr_word_after_sub = curr_word_after_sub - 8;
                }
            }
            let mut nenv = env.clone();
            for (var, location) in env {
                    nenv = nenv.update(var.to_string(), location + offset); // lookups are different now due to rsp shift?
            }
            if(args.len() != 0) {
                compile_to_instrs(&args[args.len() - 1], si + args.len() as i32 - 1 as i32, vec, &nenv, brake, l, funs, is_def);
                vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset - 16 - modified_offset), Val::Reg(Reg::RAX))); // offset - 16 is where last arg is stored
            }
            vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset - 8 - modified_offset), Val::Reg(Reg::RDI))); // store rdi right above the original current word
            vec.push(Instr::ICall(Val::FunName((&name).to_string())));
            vec.push(Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, offset - 8 - modified_offset))); // restore rdi
            vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset.into()))); // restore rsp
        }
        Expr::Tuple(elements) => {
            let stack_offset = si * 8;
            let length: i64 = elements.len().try_into().unwrap();

            if(elements.len() == 0) {
                panic!("Cannot construct tuple of size 0")
            }

            for i in 0..(elements.len() - 1) {
                compile_to_instrs(&elements[i], si + i as i32 , vec, env, brake, l, funs, is_def);
                vec.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset + (i as i32 * 8)), Val::Reg(Reg::RAX)));
            }
            compile_to_instrs(&elements[elements.len() - 1], si + length as i32 - 1, vec, env, brake, l, funs, is_def);
            vec.push(Instr::IMov(Val::RegOffset(Reg::R15, ((length) * 8).try_into().unwrap()), Val::Reg(Reg::RAX)));

            for i in 0..(elements.len() - 1) {
                vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset + (i as i32 * 8))));
                vec.push(Instr::IMov(Val::RegOffset(Reg::R15, ((1 + i) * 8).try_into().unwrap()), Val::Reg(Reg::RAX)));
            }

            vec.push(Instr::IMovq(Val::RegOffset(Reg::R15, 0), Val::Imm(length)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::R15)));
            vec.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)));
            vec.push(Instr::IAdd(Val::Reg(Reg::R15), Val::Imm((length + 1) * 8)));
        }
        Expr::Index(tuple, index) => {
            // println!("{:?}", tuple);
            // println!("{:?}", index);
            compile_to_instrs(index, si, vec, env, brake, l, funs, is_def); 
            vec.push(Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::RAX))); //RDX = index
            vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(2))); // no indices less than 1 
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7))); // true
            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3))); // false
            vec.push(Instr::ICMovl(Val::Reg(Reg::RCX), Val::Reg(Reg::RBX)));

            vec.push(Instr::ICmp(Val::Reg(Reg::RCX), Val::Imm(7)));
            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(12))); //OOB
            vec.push(Instr::IJumpEq(Val::Label("throw_error".to_string())));
            
            compile_to_instrs(tuple, si + 1, vec, env, brake, l, funs, is_def); //RAX = addr
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDX))); //RBX = index
            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RAX, -1))); //RCX = size
            vec.push(Instr::IMul(Val::Reg(Reg::RCX), Val::Imm(2))); //multiply by 2
            vec.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Reg(Reg::RCX))); // no indices greater than size
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            vec.push(Instr::ICMovg(Val::Reg(Reg::RCX), Val::Reg(Reg::RBX)));

            vec.push(Instr::ICmp(Val::Reg(Reg::RCX), Val::Imm(7)));
            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(12))); //OOB
            vec.push(Instr::IJumpEq(Val::Label("throw_error".to_string())));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffsetIndex(Reg::RAX, Reg::RDX, 4, 1))) //multiply by 4 to account for doubling???
        }
        Expr::SetIdx(tuple, index, element) => {
            compile_to_instrs(index, si, vec, env, brake, l, funs, is_def);
            vec.push(Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::RAX))); //RDX = index
            vec.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(2))); // no indices less than 1 
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7))); // true
            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3))); // false
            vec.push(Instr::ICMovl(Val::Reg(Reg::RCX), Val::Reg(Reg::RBX)));

            compile_to_instrs(tuple, si + 1, vec, env, brake, l, funs, is_def); //RAX = addr
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RDX))); //RBX = index
            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::RegOffset(Reg::RAX, -1))); //RCX = size
            vec.push(Instr::IMul(Val::Reg(Reg::RCX), Val::Imm(2))); //multiply by 2
            vec.push(Instr::ICmp(Val::Reg(Reg::RBX), Val::Reg(Reg::RCX))); // no indices greater than size
            vec.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(7)));
            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(3)));
            vec.push(Instr::ICMovg(Val::Reg(Reg::RCX), Val::Reg(Reg::RBX)));

            vec.push(Instr::ICmp(Val::Reg(Reg::RCX), Val::Imm(7)));
            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Imm(12))); //OOB
            vec.push(Instr::IJumpEq(Val::Label("throw_error".to_string())));

            vec.push(Instr::IMov(Val::Reg(Reg::RCX), Val::Reg(Reg::RAX))); // RCX = addr
            compile_to_instrs(element, si + 2, vec, env, brake, l, funs, is_def); //RAX = element
            vec.push(Instr::IMovq(Val::RegOffsetIndex(Reg::RCX, Reg::RDX, 4, 1), Val::Reg(Reg::RAX)));
            vec.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RCX)));
        }
    }
}

fn depth(e: &Expr) -> i32 {
    match e {
        Expr::Number(_) => 0,
        Expr::Boolean(_) => 0,
        Expr::UnOp(_, expr1) => depth(expr1),
        Expr::Let(bindings, body) => {
            let mut max_depth = 0;
            let mut index = 0;
            for (id, val) in bindings {
                max_depth = (depth(val)+index).max(max_depth);
                index = index + 1;
            }
            max_depth = (depth(body)+index).max(max_depth);
            max_depth
        }
        Expr::Id(_) => 0,
        Expr::BinOp(Op2::Minus, expr1, expr2) => (depth(expr1) + 1).max(depth(expr2)), //minus eval order
        Expr::BinOp(_, expr1, expr2) => depth(expr1).max(depth(expr2) + 1),
        Expr::If(expr1, expr2, expr3) => depth(expr1).max(depth(expr2)).max(depth(expr3)),
        Expr::Loop(expr) => depth(expr),
        Expr::Block(exprs) => exprs.iter().map(|expr| depth(expr)).max().unwrap_or(0),
        Expr::Break(expr) => depth(expr),
        Expr::Set(_, expr) => depth(expr),
        Expr::Call(_, args) => {
            let mut max_depth = 0;
            let mut arg_num = 0;
            for arg in args {
                max_depth = (depth(arg) + arg_num).max(max_depth);
                arg_num = arg_num + 1;
            }
            max_depth
        }
        Expr::Tuple(exprs) => {
            let mut max_depth = 0;
            let mut index = 0;
            for expr in exprs {
                max_depth = (depth(expr)+index).max(max_depth);
                index = index + 1;
            }
            max_depth
        }
        Expr::Index(tuple, index) => (depth(tuple) + 1).max(depth(index)), //index eval order
        Expr::SetIdx(tuple, index, element) => (depth(element) + 2).max((depth(tuple) + 1).max(depth(index))) // ???
        }
    }

fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\nmov {dst_str}, {src_str}")
        }
        Instr::IMovq(dst,src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\nmov qword {dst_str}, {src_str}")
        }
        Instr::IAdd(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\nadd {a_str}, {b_str}")
        }
        Instr::ISub(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\nsub {a_str}, {b_str}")
        }
        Instr::IMul(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\nimul {a_str}, {b_str}")
        }
        Instr::ICall(n) => {
            let n_str = val_to_str(n);
            format!("\ncall {n_str}")
        }
        Instr::IJump(l) => {
            let label = val_to_str(l);
            format!("\njmp {label}")
        }
        Instr::IJumpEq(l) => {
            let label = val_to_str(l);
            format!("\nje {label}")
        }
        Instr::ICMove(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmove {dst_str}, {src_str}")
        }
        Instr::IJumpNe(l) => {
            let label = val_to_str(l);
            format!("\njne {label}")
        }
        Instr::IJumpNz(l) => {
            let label = val_to_str(l);
            format!("\njnz {label}")
        }
        Instr::IXor(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\nxor {a_str}, {b_str}")
        }
        Instr::ICmp(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\ncmp {a_str}, {b_str}")
        }
        Instr::ITest(a, b) => {
            let a_str = val_to_str(a);
            let b_str = val_to_str(b);
            format!("\ntest {a_str}, {b_str}")
        }
        Instr::ILabel(l) => {
            match l {
                Val::Label(s) => format!("\n{}:", s),
                Val::FunName(s) => format!("\n{}:", s),
                _ => panic!("Not a string (label)")
            }
            
        }
        Instr::ICMovg(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmovg {dst_str}, {src_str}")
        }
        Instr::ICMovge(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmovge {dst_str}, {src_str}")
        }
        Instr::ICMovl(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmovl {dst_str}, {src_str}")
        }
        Instr::ICMovle(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\ncmovle {dst_str}, {src_str}")
        }
        Instr::ISar(dst, cnt) => {
            let dst_str = val_to_str(dst);
            let cnt_str = val_to_str(cnt);
            format!("\nsar {dst_str}, {cnt_str}")
        }
        Instr::IJumpOf(l) => {
            let label = val_to_str(l);
            format!("\njo {label}")
        }
        Instr::IRet => {
            format!("\nret")
        }
        Instr::IAnd(dst, src) => {
            let dst_str = val_to_str(dst);
            let src_str = val_to_str(src);
            format!("\nand {dst_str}, {src_str}")
        }
    }
}

fn val_to_str(v: &Val) -> String {
    match v {
        Val::Imm(n) => format!("{}", *n),
        Val::Reg(reg) => {
            match reg {
                Reg::RAX => format!("rax"),
                Reg::RSP => format!("rsp"),
                Reg::RDI => format!("rdi"),
                Reg::RBX => format!("rbx"),
                Reg::RCX => format!("rcx"),
                Reg::R15 => format!("r15"),
                Reg::RDX => format!("rdx"),
                Reg::RSI => format!("rsi"),
            }
        }
        Val::RegOffset(reg, offset) => {
            if(*offset < 0) {
                match reg {
                    Reg::RAX => format!("[rax {}]", *offset),
                    Reg::RSP => format!("[rsp {}]", *offset),
                    Reg::RDI => format!("[rdi {}]", *offset),
                    Reg::RBX => format!("[rbx {}]", *offset),
                    Reg::RCX => format!("[rcx {}]", *offset),
                    Reg::R15 => format!("[r15 {}]", *offset),
                    Reg::RDX => format!("[rdx {}]", *offset),
                    Reg::RSI => format!("[rsi {}]", *offset),
                }
            }
            else {
                match reg {
                    Reg::RAX => format!("[rax + {}]", *offset),
                    Reg::RSP => format!("[rsp + {}]", *offset),
                    Reg::RDI => format!("[rdi + {}]", *offset),
                    Reg::RBX => format!("[rbx + {}]", *offset),
                    Reg::RCX => format!("[rcx + {}]", *offset),
                    Reg::R15 => format!("[r15 + {}]", *offset),
                    Reg::RDX => format!("[rdx + {}]", *offset),
                    Reg::RSI => format!("[rsi + {}]", *offset),
                }
            }
            
        }
        Val::Label(s) => format!("{}", s),
        Val::FunName(s) => format!("{}", s),
        Val::RegOffsetIndex(reg, reg_index, scale,  offset) => { 
            match reg { //TODO CLEANUP BY IMPLEMENTING DISPLAY
                Reg::RAX => {
                    match reg_index {
                        Reg::RAX => format!("[rax + {}*rax - {}]", *scale, *offset),
                        Reg::RSP => format!("[rax + {}*rsp - {}]", *scale, *offset),
                        Reg::RDI => format!("[rax + {}*rdi - {}]", *scale, *offset),
                        Reg::RBX => format!("[rax + {}*rbx - {}]", *scale, *offset),
                        Reg::RCX => format!("[rax + {}*rcx - {}]", *scale, *offset),
                        Reg::R15 => format!("[rax + {}*r15 - {}]", *scale, *offset),
                        Reg::RDX => format!("[rax + {}*rdx - {}]", *scale, *offset),
                        Reg::RSI => format!("[rax + {}*rsi - {}]", *scale, *offset),
                    }
                }
                Reg::RSP => {
                    match reg_index {
                        Reg::RAX => format!("[rsp + {}*rax - {}]", *scale, *offset),
                        Reg::RSP => format!("[rsp + {}*rsp - {}]", *scale, *offset),
                        Reg::RDI => format!("[rsp + {}*rdi - {}]", *scale, *offset),
                        Reg::RBX => format!("[rsp + {}*rbx - {}]", *scale, *offset),
                        Reg::RCX => format!("[rsp + {}*rcx - {}]", *scale, *offset),
                        Reg::R15 => format!("[rsp + {}*r15 - {}]", *scale, *offset),
                        Reg::RDX => format!("[rsp + {}*rdx - {}]", *scale, *offset),
                        Reg::RSI => format!("[rsp + {}*rsi - {}]", *scale, *offset),
                    }
                }
                Reg::RDI => {
                    match reg_index {
                        Reg::RAX => format!("[rdi + {}*rax - {}]", *scale, *offset),
                        Reg::RSP => format!("[rdi + {}*rsp - {}]", *scale, *offset),
                        Reg::RDI => format!("[rdi + {}*rdi - {}]", *scale, *offset),
                        Reg::RBX => format!("[rdi + {}*rbx - {}]", *scale, *offset),
                        Reg::RCX => format!("[rdi + {}*rcx - {}]", *scale, *offset),
                        Reg::R15 => format!("[rdi + {}*r15 - {}]", *scale, *offset),
                        Reg::RDX => format!("[rdi + {}*rdx - {}]", *scale, *offset),
                        Reg::RSI => format!("[rdi + {}*rsi - {}]", *scale, *offset),
                    }
                }
                Reg::RBX => {
                    match reg_index {
                        Reg::RAX => format!("[rbx + {}*rax - {}]", *scale, *offset),
                        Reg::RSP => format!("[rbx + {}*rsp - {}]", *scale, *offset),
                        Reg::RDI => format!("[rbx + {}*rdi - {}]", *scale, *offset),
                        Reg::RBX => format!("[rbx + {}*rbx - {}]", *scale, *offset),
                        Reg::RCX => format!("[rbx + {}*rcx - {}]", *scale, *offset),
                        Reg::R15 => format!("[rbx + {}*r15 - {}]", *scale, *offset),
                        Reg::RDX => format!("[rbx + {}*rdx - {}]", *scale, *offset),
                        Reg::RSI => format!("[rbx + {}*rsi - {}]", *scale, *offset),
                    }
                }
                Reg::RCX => {
                    match reg_index {
                        Reg::RAX => format!("[rcx + {}*rax - {}]", *scale, *offset),
                        Reg::RSP => format!("[rcx + {}*rsp - {}]", *scale, *offset),
                        Reg::RDI => format!("[rcx + {}*rdi - {}]", *scale, *offset),
                        Reg::RBX => format!("[rcx + {}*rbx - {}]", *scale, *offset),
                        Reg::RCX => format!("[rcx + {}*rcx - {}]", *scale, *offset),
                        Reg::R15 => format!("[rcx + {}*r15 - {}]", *scale, *offset),
                        Reg::RDX => format!("[rcx + {}*rdx - {}]", *scale, *offset),
                        Reg::RSI => format!("[rcx + {}*rsi - {}]", *scale, *offset),
                    }
                },
                Reg::R15 => {
                    match reg_index {
                        Reg::RAX => format!("[r15 + {}*rax - {}]", *scale, *offset),
                        Reg::RSP => format!("[r15 + {}*rsp - {}]", *scale, *offset),
                        Reg::RDI => format!("[r15 + {}*rdi - {}]", *scale, *offset),
                        Reg::RBX => format!("[r15 + {}*rbx - {}]", *scale, *offset),
                        Reg::RCX => format!("[r15 + {}*rcx - {}]", *scale, *offset),
                        Reg::R15 => format!("[r15 + {}*r15 - {}]", *scale, *offset),
                        Reg::RDX => format!("[r15 + {}*rdx - {}]", *scale, *offset),
                        Reg::RSI => format!("[r15 + {}*rsi - {}]", *scale, *offset),
                    }
                }
                Reg::RDX => {
                    match reg_index {
                        Reg::RAX => format!("[rdx + {}*rax - {}]", *scale, *offset),
                        Reg::RSP => format!("[rdx + {}*rsp - {}]", *scale, *offset),
                        Reg::RDI => format!("[rdx + {}*rdi - {}]", *scale, *offset),
                        Reg::RBX => format!("[rdx + {}*rbx - {}]", *scale, *offset),
                        Reg::RCX => format!("[rdx + {}*rcx - {}]", *scale, *offset),
                        Reg::R15 => format!("[rdx + {}*r15 - {}]", *scale, *offset),
                        Reg::RDX => format!("[rdx + {}*rdx - {}]", *scale, *offset),
                        Reg::RSI => format!("[rdx + {}*rsi - {}]", *scale, *offset),
                    }
                }
                Reg::RSI => {
                    match reg_index {
                        Reg::RAX => format!("[rsi + {}*rax - {}]", *scale, *offset),
                        Reg::RSP => format!("[rsi + {}*rsp - {}]", *scale, *offset),
                        Reg::RDI => format!("[rsi + {}*rdi - {}]", *scale, *offset),
                        Reg::RBX => format!("[rsi + {}*rbx - {}]", *scale, *offset),
                        Reg::RCX => format!("[rsi + {}*rcx - {}]", *scale, *offset),
                        Reg::R15 => format!("[rsi + {}*r15 - {}]", *scale, *offset),
                        Reg::RDX => format!("[rsi + {}*rdx - {}]", *scale, *offset),
                        Reg::RSI => format!("[rsi + {}*rsi - {}]", *scale, *offset),
                    }
                }
            }
        }
    }
}
fn compile_defs(def_vec: Vec<Definition>, vec: &mut Vec<Instr>, l: &mut i32, funs: &HashMap<String, i32>, is_def: bool) -> String {
    let mut assembly = String::new();
    for def in def_vec {
        match def {
            Fun(name, args, body) => {
                let depth = depth(&body);
                let offset = if depth % 2 == 1 {depth * 8}  else {(depth + 1) * 8}; // offset will always be odd
                let mut body_env: HashMap<String, i32> = HashMap::new();
                for i in 0..args.len() {
                    body_env = body_env.update(args[i].to_string(), ((offset/8) + (i as i32 + 1)) * 8);
                }
                vec.push(Instr::ILabel(Val::FunName(name.to_string())));
                vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset.into())));
                compile_to_instrs(&body, 0, vec, &body_env, &String::from(""), l, funs, is_def);
                vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset.into())));
                vec.push(Instr::IRet);
            }
        }
    }
    for instr in vec {
        assembly.push_str(&instr_to_str(instr));
    }
    return assembly;
}

fn compile_main(e: &Expr, si: i32, vec: &mut Vec<Instr>, env: &HashMap<String, i32>, brake: &String, l: &mut i32, funs: &HashMap<String, i32>) -> String {
    let mut assembly = String::new();
    let depth_val = depth(e);
    let offset = if depth_val % 2 == 0 {depth_val * 8} else {(depth_val + 1) * 8}; //main allocates even number, always
    //let offset = depth_val * 8;
    vec.push(Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset.into())));
    compile_to_instrs(e, si, vec, env, brake, l, funs, false);
    vec.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset.into())));
    for instr in vec {
        assembly.push_str(&instr_to_str(instr));
    }
    return assembly;
}

fn new_label(l: &mut i32, s: &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let mut vec_main: Vec<Instr> = Vec::new();
    let mut vec_defs: Vec<Instr> = Vec::new();
    let map: HashMap<String, i32> = HashMap::new();
    let mut labels = 0;

    let prog = "(".to_owned() + &in_contents + ")";

    let prog = parse_program(&parse(&prog).expect("Invalid"));
    let defs = compile_defs(prog.defs, &mut vec_defs, &mut labels, &prog.info, true);
    let main = compile_main(&prog.main, 0, &mut vec_main, &map, &String::from(""), &mut labels, &prog.info);

    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_error
extern snek_print
extern snek_eq
throw_error:
  push rsp
  mov rdi, rcx
  call snek_error
  ret
{}
our_code_starts_here:
  mov r15, rsi
  {}
  ret
",      defs,
        main
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
  }