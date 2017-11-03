exception Error

type token = 
  | WHILE
  | VAR
  | TRUE
  | SUB
  | SET
  | SEMI
  | PRINT
  | OR
  | NEQ
  | MULT
  | MT
  | ME
  | LT
  | LITINT of (int)
  | LE
  | INT
  | INC
  | IF
  | IDENT of (string)
  | FOR
  | FALSE
  | EQ
  | EOF
  | END
  | ELSE
  | DIV
  | DEC
  | COMMA
  | BOOLEAN
  | BEGIN
  | AND
  | ADD

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState114
  | MenhirState110
  | MenhirState109
  | MenhirState106
  | MenhirState96
  | MenhirState90
  | MenhirState86
  | MenhirState83
  | MenhirState79
  | MenhirState77
  | MenhirState70
  | MenhirState68
  | MenhirState65
  | MenhirState61
  | MenhirState59
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState29
  | MenhirState27
  | MenhirState24
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState13
  | MenhirState12
  | MenhirState7
  | MenhirState3
  | MenhirState0

  

  open SourceAst

let _eRR =
  Error

let rec _menhir_goto_arguments : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let a = _v in
        let (_menhir_stack, _menhir_s, e) = _menhir_stack in
        let _v : (SourceAst.expression list) =                                     ( e :: a ) in
        _menhir_goto_arguments _menhir_env _menhir_stack _menhir_s _v
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let a = _v in
        let _v : (SourceAst.expression list) =               ( a ) in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_goto_fun_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.Symb_Tbl.key * SourceAst.function_info) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | EOF ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | INT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_goto_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, id), _, a) = _menhir_stack in
        let _v : (SourceAst.call) =                               ( id, a ) in
        (match _menhir_s with
        | MenhirState20 | MenhirState65 | MenhirState77 | MenhirState79 | MenhirState83 | MenhirState70 | MenhirState61 | MenhirState24 | MenhirState52 | MenhirState50 | MenhirState34 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState36 | MenhirState38 | MenhirState27 | MenhirState32 | MenhirState29 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let c = _v in
            let _v : (SourceAst.expression) =          ( FunCall(c) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | MenhirState110 | MenhirState18 | MenhirState59 | MenhirState68 | MenhirState96 | MenhirState86 | MenhirState90 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let c = _v in
            let _v : (SourceAst.instruction list) =          ( [ProcCall(c)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | IDENT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | LITINT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | TRUE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : (SourceAst.expression list) =                ( [e] ) in
            _menhir_goto_arguments _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Sub )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
        let _v : (SourceAst.expression) = let b =
                 ( Mult )
        in
                                                  ( Binop(b,e1,e2) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
        let _v : (SourceAst.expression) = let b =
                 ( Div )
        in
                                                  ( Binop(b,e1,e2) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Or )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Neq )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Add )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Mt )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Me )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Lt )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Le )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Eq )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | END | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( And )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FOR ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | IDENT _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
                | IF ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | PRINT ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | WHILE ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | END ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (SourceAst.instruction list) =                                           ( [Print(e)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FOR ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | IDENT _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
                | IF ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | PRINT ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | WHILE ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | END ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, id), _, e) = _menhir_stack in
            let _v : (SourceAst.instruction list) =                               ( [Set(Identifier id,e)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | IDENT _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | LITINT _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | TRUE ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | SET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | FALSE ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                    | IDENT _v ->
                        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
                    | LITINT _v ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
                    | TRUE ->
                        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FOR ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | IDENT _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                | IF ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | PRINT ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | WHILE ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | END ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), id1), _, e1), _, e2), id2), _, e3), _, bl) = _menhir_stack in
            let _v : (SourceAst.instruction list) =     ( let block = bl @ [Set(Identifier id2, e3)] in [Set(Identifier id1, e1);While(e2, block)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, i), _, is) = _menhir_stack in
        let _v : (SourceAst.block) =                                           ( i @ is ) in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | FOR ->
                        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                    | IDENT _v ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
                    | IF ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                    | PRINT ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                    | WHILE ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                    | END ->
                        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, e), _, is) = _menhir_stack in
                let _v : (SourceAst.instruction list) =                                                             ( [If(e,is,[])] ) in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, e), _, is1), _, is2) = _menhir_stack in
            let _v : (SourceAst.instruction list) =                                                                                                   ( [If(e,is1,is2)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, e), _, is) = _menhir_stack in
            let _v : (SourceAst.instruction list) =                                                                ( [While(e,is)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, id), _, ps), _, vds), _, is) = _menhir_stack in
            let _v : (SourceAst.Symb_Tbl.key * SourceAst.function_info) =                                                                               (

      let merge_vars k v1 v2 =
          match v1, v2 with
          |Some v1,_ -> Some v1
          |None,Some v2 -> Some v2
          |None,None -> None
          in

      let index = ref 0 in
      let ftl = List.fold_left (fun acc (t,i) ->
        incr index; Symb_Tbl.add i {typ=t; kind=Formal(!index)} acc )
        Symb_Tbl.empty ps in
      let locals = Symb_Tbl.merge merge_vars vds ftl in

      let formals = List.fold_left (fun acc (t,_) -> t::acc) [] ps in
      id, {
        return=None;
        formals=formals;
        locals=locals;
        code=is
      }
    ) in
            _menhir_goto_fun_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, t), id), _, ps), _, vds), _, is) = _menhir_stack in
            let _v : (SourceAst.Symb_Tbl.key * SourceAst.function_info) =                                                                                     (

  let merge_vars k v1 v2 =
      match v1, v2 with
      |Some v1,_ -> Some v1
      |None,Some v2 -> Some v2
      |None,None -> None
      in

    let res  = Symb_Tbl.singleton "result" { typ=t; kind=Return } in

    let local = Symb_Tbl.merge merge_vars res vds in
    let index = ref 0 in
    let ftl = List.fold_left (fun acc (t,i) ->
      incr index; Symb_Tbl.add i {typ=t; kind=Formal(!index)} acc )
      Symb_Tbl.empty ps in

    let locals = Symb_Tbl.merge merge_vars local ftl in
    let formals = List.fold_left (fun acc (t,_) -> t::acc) [] ps in

    id, {
      return=Some t;
      formals=formals;
      locals=locals;
      code=is
    }
  ) in
            _menhir_goto_fun_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.instruction list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FOR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | IDENT _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | IF ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PRINT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | WHILE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | END ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | IDENT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LITINT _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | TRUE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState24 in
        let _v : (SourceAst.expression list) =               ( [] ) in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (SourceAst.expression) =        ( Literal(Bool true) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (SourceAst.expression) =            ( Literal(Int i) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
    | ADD | AND | COMMA | DIV | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : (SourceAst.expression) =            ( Location( Identifier id ) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (SourceAst.expression) =         ( Literal(Bool false) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.block) =                                           ( [] ) in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | IDENT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | LITINT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | TRUE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | IDENT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | LITINT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | TRUE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | IDENT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LITINT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | TRUE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
    | DEC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : (SourceAst.instruction list) =                 ( [Set(Identifier id,Binop(Sub,Location( Identifier id ),Literal(Int 1)) )] ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | INC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : (SourceAst.instruction list) =                 ( [Set(Identifier id,Binop(Add,Location( Identifier id ),Literal(Int 1)) )] ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | SET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | IDENT _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | LITINT _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | TRUE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | SET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FALSE ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | IDENT _v ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                | LITINT _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                | TRUE ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_var_decls : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.identifier_info SourceAst.Symb_Tbl.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, t), id), _, tbl) = _menhir_stack in
        let _v : (SourceAst.identifier_info SourceAst.Symb_Tbl.t) =                                              ( let info = {typ=t; kind=Local} in Symb_Tbl.add id info tbl ) in
        _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | IDENT _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | IF ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | PRINT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | WHILE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | END ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | IDENT _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | IF ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PRINT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | WHILE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | END ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | _ ->
        _menhir_fail ()

and _menhir_goto_para : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | VAR ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                | END | FOR | IDENT _ | IF | PRINT | WHILE ->
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState12
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | VAR ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState109
                | END | FOR | IDENT _ | IF | PRINT | WHILE ->
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState109
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.identifier_info SourceAst.Symb_Tbl.t) =               ( Symb_Tbl.empty ) in
    _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BOOLEAN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_goto_parameters : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let ((_menhir_stack, _menhir_s, t), id) = _menhir_stack in
        let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                                         ( (t,id) :: p ) in
        _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
    | MenhirState106 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                ( p ) in
        _menhir_goto_para _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =               ( [] ) in
    _menhir_goto_para _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.prog) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Obj.magic _1
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let (_menhir_stack, _menhir_s, fs) = _menhir_stack in
        let _v : (SourceAst.prog) =     (
      let (id, infos) = fs in
      Symb_Tbl.add id infos p
    ) in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_typs : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState106 | MenhirState7 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BOOLEAN ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                | INT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, t), id) = _menhir_stack in
                let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                    ( [(t,id)] ) in
                _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | VAR ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | END | FOR | IDENT _ | IF | PRINT | WHILE ->
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BOOLEAN ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | INT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | END ->
                    _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (SourceAst.typ) =       ( TypInteger ) in
    _menhir_goto_typs _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BOOLEAN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | INT ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | END ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (SourceAst.prog) =                   ( Symb_Tbl.empty ) in
    _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (SourceAst.typ) =           ( TypBoolean ) in
    _menhir_goto_typs _menhir_env _menhir_stack _menhir_s _v

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SourceAst.prog) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



