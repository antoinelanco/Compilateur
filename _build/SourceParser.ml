exception Error

type token = 
  | WHILE
  | VAR
  | TRY
  | TRUE
  | TO
  | THROW
  | SUB
  | STRUCT
  | SET
  | SEMI
  | PRINT
  | OR
  | NEW
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
  | EB
  | EA
  | DOT
  | DIV
  | DEC
  | COMMA
  | CATCH
  | BOOLEAN
  | BEGIN
  | BB
  | BA
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
  | MenhirState180
  | MenhirState178
  | MenhirState173
  | MenhirState172
  | MenhirState169
  | MenhirState160
  | MenhirState154
  | MenhirState148
  | MenhirState143
  | MenhirState140
  | MenhirState138
  | MenhirState136
  | MenhirState131
  | MenhirState126
  | MenhirState123
  | MenhirState120
  | MenhirState118
  | MenhirState111
  | MenhirState108
  | MenhirState104
  | MenhirState101
  | MenhirState99
  | MenhirState92
  | MenhirState89
  | MenhirState86
  | MenhirState84
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState68
  | MenhirState66
  | MenhirState64
  | MenhirState62
  | MenhirState58
  | MenhirState56
  | MenhirState49
  | MenhirState48
  | MenhirState46
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState31
  | MenhirState30
  | MenhirState25
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState10
  | MenhirState3
  | MenhirState0

  

  open SourceAst

let _eRR =
  Error

let rec _menhir_goto_fun_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.Symb_Tbl.key * SourceAst.function_info) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
    | BEGIN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | BOOLEAN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | IDENT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v _menhir_env._menhir_startp
    | INT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | EOF ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178

and _menhir_reduce6 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.call) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, c, _startpos_c_) = _menhir_stack in
    let _startpos = _startpos_c_ in
    let _v : (SourceAst.expression) =          ( FunCall(c) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.literal) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, i, _startpos_i_) = _menhir_stack in
    let _startpos = _startpos_i_ in
    let _v : (SourceAst.expression) =             ( Literal(i) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_goto_set_direct : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Lexing.position * SourceAst.instruction) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState173 | MenhirState36 | MenhirState99 | MenhirState160 | MenhirState101 | MenhirState154 | MenhirState111 | MenhirState148 | MenhirState143 | MenhirState131 | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : ((Lexing.position * SourceAst.instruction) list) =   ( s ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState138 ->
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
                | BA ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | BB ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | FALSE ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | FOR ->
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | IDENT _v ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v _menhir_env._menhir_startp
                | IF ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | LITINT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | PRINT ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | THROW ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | TRY ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
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

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), id, _startpos_id_), _, i1, _startpos_i1_), _, i2, _startpos_i2_), _, bl) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : ((Lexing.position * SourceAst.instruction) list) =   ( let block = bl @ [(_startpos,Set(Identifier id, Binop(Add,Location( Identifier id ),Literal(Int 1))))] in
     [(_startpos,Set(Identifier id, Literal i1));
     (_startpos,While(Binop(Le,Location(Identifier id),Literal i2), block))] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, i), _, is) = _menhir_stack in
        let _v : (SourceAst.block) =                                           ( i @ is ) in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s, _startpos__1_), l1, _startpos_l1_), _, e1, _startpos_e1_), _, e2, _startpos_e2_), _, l3, _startpos_l3_), _, e3, _startpos_e3_), _, bl) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : ((Lexing.position * SourceAst.instruction) list) =   ( let block = bl @ [(_startpos,Set(Identifier(l3),e3))] in
     [(_startpos,Set(Identifier(l1),e1));(_startpos,While(e2, block))] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s, _startpos__1_), l1, _startpos_l1_), _, e1, _startpos_e1_), _, e2, _startpos_e2_), _, s), _, bl) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : ((Lexing.position * SourceAst.instruction) list) =   ( let block = bl @ s in
    [(_startpos,Set(Identifier(l1),e1));(_startpos,While(e2, block))] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState111 ->
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
                    | BA ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | BB ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | FALSE ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | FOR ->
                        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | IDENT _v ->
                        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp
                    | IF ->
                        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | LITINT _v ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp
                    | NEW ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | PRINT ->
                        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | THROW ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | TRUE ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | TRY ->
                        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | WHILE ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
                    | END ->
                        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState154
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_), _, is) = _menhir_stack in
                let _startpos = _startpos__1_ in
                let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,If(e,is,[]))] ) in
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
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_), _, is1), _, is2) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,If(e,is1,is2))] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | CATCH ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | BA ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | BB ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | FALSE ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | FOR ->
                        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | IDENT _v ->
                        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_startp
                    | IF ->
                        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | LITINT _v ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_startp
                    | NEW ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | PRINT ->
                        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | THROW ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | TRUE ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | TRY ->
                        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | WHILE ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | END ->
                        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
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
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, is1), _, is2) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,Try(is1,is2))] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_), _, is) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,While(e,is))] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
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
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, id, _startpos_id_), _, ps), _, vds), _, is) = _menhir_stack in
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
      let formals = List.fold_left (fun acc (t,id) -> (t,id)::acc) [] ps in
      let new_id =
        List.fold_left (fun acc (t,_) -> acc^"_"^(SourceAst.print_typ t))
          id ps
      in
      new_id, {
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
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, t), id, _startpos_id_), _, ps), _, vds), _, is) = _menhir_stack in
            let _v : (SourceAst.Symb_Tbl.key * SourceAst.function_info) =                                                                                     (

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
    let locals = Symb_Tbl.add "result" { typ=t; kind=Return } locals in
    let formals = List.fold_left (fun acc (t,id) -> (t,id)::acc) [] ps in
    let new_id =
      List.fold_left (fun acc (t,_) -> acc^"_"^(SourceAst.print_typ t))
        id ps
    in

    new_id, {
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
        let ((_menhir_stack, _menhir_s, id, _startpos_id_), _, a) = _menhir_stack in
        let _startpos = _startpos_id_ in
        let _v : (SourceAst.call) =                               ( id, a ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        (match _menhir_s with
        | MenhirState38 | MenhirState108 | MenhirState118 | MenhirState136 | MenhirState140 | MenhirState126 | MenhirState104 | MenhirState46 | MenhirState92 | MenhirState48 | MenhirState49 | MenhirState86 | MenhirState56 | MenhirState58 | MenhirState62 | MenhirState84 | MenhirState64 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState66 | MenhirState71 | MenhirState68 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
        | MenhirState173 | MenhirState36 | MenhirState99 | MenhirState101 | MenhirState160 | MenhirState111 | MenhirState154 | MenhirState148 | MenhirState143 | MenhirState123 | MenhirState131 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, c, _startpos_c_) = _menhir_stack in
                let _startpos = _startpos_c_ in
                let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,ProcCall(c))] ) in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | ADD | AND | BB | DIV | DOT | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SUB ->
                _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce10 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.location) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, loc, _startpos_loc_) = _menhir_stack in
    let _startpos = _startpos_loc_ in
    let _v : (SourceAst.expression) =                ( Location(loc) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce45 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, id, _startpos_id_) = _menhir_stack in
    let _startpos = _startpos_id_ in
    let _v : (SourceAst.location) =            ( Identifier(id) ) in
    _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState46 in
        let _v : (SourceAst.expression list) =               ( [] ) in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.literal) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState173 | MenhirState36 | MenhirState99 | MenhirState160 | MenhirState101 | MenhirState154 | MenhirState111 | MenhirState148 | MenhirState143 | MenhirState140 | MenhirState136 | MenhirState131 | MenhirState123 | MenhirState126 | MenhirState108 | MenhirState104 | MenhirState38 | MenhirState92 | MenhirState46 | MenhirState48 | MenhirState86 | MenhirState84 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState58 | MenhirState56 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp
            | LITINT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
        | ADD | AND | BB | DIV | DOT | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState120 ->
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
                | BA ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | BB ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | FALSE ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | FOR ->
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | IDENT _v ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v _menhir_env._menhir_startp
                | IF ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | LITINT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | PRINT ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | THROW ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | TRY ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState123
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMI_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, xs0) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (SourceAst.expression) = let es =
          let xs = xs0 in
              ( xs )
        in
                                                      ( NewArrayAcol(es) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run113 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, id, _startpos_id_) = _menhir_stack in
    let _startpos = _startpos_id_ in
    let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,Set(Identifier id,Binop(Add,Location( Identifier id ),Literal(Int 1)) ))] ) in
    _menhir_goto_set_direct _menhir_env _menhir_stack _menhir_s _v

and _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, id, _startpos_id_) = _menhir_stack in
    let _startpos = _startpos_id_ in
    let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,Set(Identifier id,Binop(Sub,Location( Identifier id ),Literal(Int 1))) )] ) in
    _menhir_goto_set_direct _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Lexing.position * SourceAst.instruction) list) -> 'ttv_return =
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
        | BA ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | BB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | FOR ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v _menhir_env._menhir_startp
        | IF ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | LITINT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v _menhir_env._menhir_startp
        | NEW ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | THROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | TRY ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | WHILE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
        | END ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.block) =                                           ( [] ) in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BA ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
        | BB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v _menhir_env._menhir_startp
        | LITINT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v _menhir_env._menhir_startp
        | NEW ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BA ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | BB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | FOR ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_startp
        | IF ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | LITINT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_startp
        | NEW ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | THROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | TRY ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | WHILE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
        | END ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,Throw)] ) in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BA ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | BB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp
        | LITINT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp
        | NEW ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BA ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | BB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp
        | LITINT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp
        | NEW ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
    | DEC ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
    | INC ->
        _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
    | ADD | AND | BB | DIV | DOT | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SET | SUB ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run115 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | SET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BA ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | BB ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | FALSE ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | IDENT _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v _menhir_env._menhir_startp
                | LITINT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_arguments : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let a = _v in
        let (_menhir_stack, _menhir_s, e, _startpos_e_) = _menhir_stack in
        let _v : (SourceAst.expression list) =                                     ( a @ [e] ) in
        _menhir_goto_arguments _menhir_env _menhir_stack _menhir_s _v
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let a = _v in
        let _v : (SourceAst.expression list) =               ( a ) in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_location : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.location) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState140 | MenhirState136 | MenhirState126 | MenhirState118 | MenhirState108 | MenhirState104 | MenhirState38 | MenhirState92 | MenhirState46 | MenhirState48 | MenhirState86 | MenhirState84 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState58 | MenhirState56 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState173 | MenhirState36 | MenhirState99 | MenhirState160 | MenhirState101 | MenhirState154 | MenhirState111 | MenhirState148 | MenhirState143 | MenhirState131 | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BA ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
            | BB ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
            | FALSE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v _menhir_env._menhir_startp
            | LITINT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v _menhir_env._menhir_startp
            | NEW ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | ADD | AND | BB | DIV | DOT | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SUB ->
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMI_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (SourceAst.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMI_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
        let _v : (SourceAst.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMI_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (SourceAst.literal) =        ( Bool true ) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _startpos__1_), id, _startpos_id_) = _menhir_stack in
                let _startpos = _startpos__1_ in
                let _v : (SourceAst.expression) =                             ( NewRecord(id) ) in
                _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _startpos_i_ = _startpos in
    let _startpos = _startpos_i_ in
    let _v : (SourceAst.literal) =            ( Int i ) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
    | ADD | AND | BB | COMMA | DIV | DOT | EA | EB | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (SourceAst.literal) =         ( Bool false ) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | EA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState49 in
        let _v : (SourceAst.expression list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_SEMI_expression__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let id = _v in
        let _startpos_id_ = _startpos in
        let (_menhir_stack, _menhir_s, e, _startpos_e_) = _menhir_stack in
        let _startpos = _startpos_e_ in
        let _v : (SourceAst.location) =                               ( FieldAccess(e,id) ) in
        _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | BB ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_startp
    | LITINT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_startp
    | NEW ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_goto_var_decls : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.identifier_info SourceAst.Symb_Tbl.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, t), id, _startpos_id_), _, tbl) = _menhir_stack in
        let _v : (SourceAst.identifier_info SourceAst.Symb_Tbl.t) =   ( let info = {typ=t; kind=Local} in Symb_Tbl.add id info tbl ) in
        _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BA ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | BB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | FOR ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_startp
        | IF ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | LITINT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_startp
        | NEW ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | THROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | TRY ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | WHILE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | END ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BA ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | BB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | FOR ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v _menhir_env._menhir_startp
        | IF ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | LITINT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v _menhir_env._menhir_startp
        | NEW ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | THROW ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | TRY ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | WHILE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
        | END ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173)
    | _ ->
        _menhir_fail ()

and _menhir_goto_para : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
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
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | BA | BB | END | FALSE | FOR | IDENT _ | IF | LITINT _ | NEW | PRINT | THROW | TRUE | TRY | WHILE ->
                    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
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
    | MenhirState169 ->
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
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState172
                | BA | BB | END | FALSE | FOR | IDENT _ | IF | LITINT _ | NEW | PRINT | THROW | TRUE | TRY | WHILE ->
                    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState172
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
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

and _menhir_goto_progs : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.function_info SourceAst.Symb_Tbl.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, s), _, p) = _menhir_stack in
            let _v : (SourceAst.prog) =   ( {functions = p; structs = s} ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, fs), _, p) = _menhir_stack in
        let _v : (SourceAst.function_info SourceAst.Symb_Tbl.t) =     (
      let (id, infos) = fs in
      Symb_Tbl.add id infos p
    ) in
        _menhir_goto_progs _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                   ( [] ) in
    _menhir_goto_para _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState86 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BA ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
            | BB ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
            | FALSE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_startp
            | LITINT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_startp
            | NEW ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | EA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
            let _v : (SourceAst.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_SEMI_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Sub )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | DIV | EA | EB | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Mult )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, e1, _startpos_e1_), _startpos__2_), _, e, _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.location) =                                       ( ArrayAccess(e1,e) ) in
            _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v _startpos
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Or )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Neq )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | DIV | EA | EB | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Div )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Add )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Mt )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Me )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Lt )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Le )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( Eq )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | OR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1, _startpos_e1_), _, e2, _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _v : (SourceAst.expression) = let b =
                     ( And )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
            | BEGIN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | BOOLEAN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | INT ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BA ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
            | BB ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
            | FALSE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp
            | LITINT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp
            | NEW ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, e, _startpos_e_) = _menhir_stack in
            let _v : (SourceAst.expression list) =                ( [e] ) in
            _menhir_goto_arguments _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BA ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | BB ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | FALSE ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | FOR ->
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | IDENT _v ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp
                | IF ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | LITINT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | PRINT ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | THROW ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | TRY ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,Print(e))] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BA ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | BB ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | FALSE ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | FOR ->
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | IDENT _v ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v _menhir_env._menhir_startp
                | IF ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | LITINT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | PRINT ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | THROW ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | TRY ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, l, _startpos_l_), _, e, _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_l_ in
            let _v : ((Lexing.position * SourceAst.instruction) list) =   ( [(_startpos,Set(l,e))] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState173 | MenhirState36 | MenhirState99 | MenhirState101 | MenhirState160 | MenhirState111 | MenhirState154 | MenhirState148 | MenhirState143 | MenhirState123 | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BA ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
            | BB ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
            | FALSE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_startp
            | LITINT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_startp
            | NEW ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState138 in
                let _startpos = _menhir_env._menhir_startp in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | DEC ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
                | INC ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack)
                | SET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | BA ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
                    | BB ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
                    | FALSE ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
                    | IDENT _v ->
                        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_startp
                    | LITINT _v ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_startp
                    | NEW ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
                    | TRUE ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | DIV ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BA ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | BB ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | FALSE ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | FOR ->
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | IDENT _v ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp
                | IF ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | LITINT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | PRINT ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | THROW ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | TRY ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.identifier_info SourceAst.Symb_Tbl.t) =               ( Symb_Tbl.empty ) in
    _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
    | BEGIN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | BOOLEAN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_goto_parameters : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let ((_menhir_stack, _menhir_s, t), id, _startpos_id_) = _menhir_stack in
        let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                                             ( (t,id) :: p ) in
        _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
    | MenhirState169 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                    ( p ) in
        _menhir_goto_para _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMI_field_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.struct_info) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (SourceAst.struct_info) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMI_field_decl__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (SourceAst.struct_info) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMI_field_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.function_info SourceAst.Symb_Tbl.t) =               ( Symb_Tbl.empty ) in
    _menhir_goto_progs _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_startp
        | BEGIN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | BOOLEAN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | INT ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | END ->
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_typs : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, t) = _menhir_stack in
        let _v : (SourceAst.typ) =                  ( TypArray(t) ) in
        _menhir_goto_typs _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let id = _v in
            let _startpos_id_ = _startpos in
            let (_menhir_stack, _menhir_s, t) = _menhir_stack in
            let _v : (string * SourceAst.typ) =                   ((id,t)) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BB ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp
                | BEGIN ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
                | BOOLEAN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
                | INT ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : (SourceAst.struct_info) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_SEMI_field_decl_ _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState169 | MenhirState25 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BB ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_startp
                | BEGIN ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | BOOLEAN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | INT ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, t), id, _startpos_id_) = _menhir_stack in
                let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                        ( [(t,id)] ) in
                _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | VAR ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | BA | BB | END | FALSE | FOR | IDENT _ | IF | LITINT _ | NEW | PRINT | THROW | TRUE | TRY | WHILE ->
                    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_), _, t) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (SourceAst.expression) =                                ( NewArray(e,TypArray t) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos
    | MenhirState178 | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _v, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BB ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp
                | BEGIN ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                | BOOLEAN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                | INT ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                | END ->
                    _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_structs : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.struct_info SourceAst.Symb_Tbl.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
        | BEGIN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | BOOLEAN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | IDENT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_startp
        | INT ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | EOF ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, st), _, sts) = _menhir_stack in
        let _v : (SourceAst.struct_info SourceAst.Symb_Tbl.t) =   (
    let (id,infos) = st in
    Symb_Tbl.add id infos sts
  ) in
        _menhir_goto_structs _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMI_field_decl__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.struct_info) -> 'ttv_return =
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
        let (((_menhir_stack, _menhir_s), id, _startpos_id_), _, xs0) = _menhir_stack in
        let _v : (SourceAst.Symb_Tbl.key * SourceAst.struct_info) = let fds =
          let xs = xs0 in
              ( xs )
        in
          (id,fds) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STRUCT ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | BB | BEGIN | BOOLEAN | EOF | IDENT _ | INT ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (SourceAst.typ) =       ( TypInteger ) in
    _menhir_goto_typs _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (SourceAst.typ) =           ( TypBoolean ) in
    _menhir_goto_typs _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), id, _startpos_id_) = _menhir_stack in
            let _v : (SourceAst.typ) =                        ( TypStruct(id) ) in
            _menhir_goto_typs _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | EB ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BB ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
        | BEGIN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | BOOLEAN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | INT ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState169 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
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
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.struct_info SourceAst.Symb_Tbl.t) =               ( Symb_Tbl.empty ) in
    _menhir_goto_structs _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BB ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _menhir_env._menhir_startp
            | BEGIN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | BOOLEAN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | INT ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState3 in
                let _v : (SourceAst.struct_info) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_SEMI_field_decl__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BB | BEGIN | BOOLEAN | EOF | IDENT _ | INT ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



