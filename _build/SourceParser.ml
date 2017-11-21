exception Error

type token = 
  | WHILE
  | VAR
  | TRUE
  | TO
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
  | EB
  | EA
  | DIV
  | DEC
  | COMMA
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
  | MenhirState135
  | MenhirState131
  | MenhirState130
  | MenhirState127
  | MenhirState117
  | MenhirState111
  | MenhirState108
  | MenhirState104
  | MenhirState99
  | MenhirState94
  | MenhirState92
  | MenhirState89
  | MenhirState87
  | MenhirState80
  | MenhirState77
  | MenhirState73
  | MenhirState71
  | MenhirState64
  | MenhirState61
  | MenhirState59
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState29
  | MenhirState27
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState16
  | MenhirState15
  | MenhirState10
  | MenhirState6
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
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | BOOLEAN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | EOF ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | INT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135

and _menhir_reduce6 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.call) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, c) = _menhir_stack in
    let _v : (SourceAst.expression) =          ( FunCall(c) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce9 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.literal) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, i) = _menhir_stack in
    let _v : (SourceAst.expression) =             ( Literal(i) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instructions : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), id), _, i1), _, i2), _, bl) = _menhir_stack in
            let _v : (SourceAst.instruction list) =   ( let block = bl @ [Set(Identifier id, Binop(Add,Location( Identifier id ),Literal(Int 1)))] in
     [Set(Identifier id, Literal i1);While(Binop(Le,Location(Identifier id),Literal i2), block)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, i), _, is) = _menhir_stack in
        let _v : (SourceAst.block) =                                           ( i @ is ) in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), id1), _, e1), _, e2), id2), _, e3), _, bl) = _menhir_stack in
            let _v : (SourceAst.instruction list) =   ( let block = bl @ [Set(Identifier id2, e3)] in
     [Set(Identifier id1, e1);While(e2, block)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
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
                    | BB ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                    | FALSE ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                    | FOR ->
                        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                    | IDENT _v ->
                        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                    | IF ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                    | LITINT _v ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
                    | PRINT ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                    | TRUE ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                    | WHILE ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                    | END ->
                        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
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
    | MenhirState117 ->
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
    | MenhirState71 ->
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
    | MenhirState21 ->
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

      let formals = List.fold_left (fun acc (t,id) -> (t,id)::acc) [] ps in
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
    | MenhirState131 ->
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


    let index = ref 0 in
    let ftl = List.fold_left (fun acc (t,i) ->
      incr index; Symb_Tbl.add i {typ=t; kind=Formal(!index)} acc )
      Symb_Tbl.empty ps in

    let locals = Symb_Tbl.merge merge_vars vds ftl in
    let locals = Symb_Tbl.add "result" { typ=t; kind=Return } locals in
    let formals = List.fold_left (fun acc (t,id) -> (t,id)::acc) [] ps in

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
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState23 | MenhirState77 | MenhirState87 | MenhirState104 | MenhirState108 | MenhirState94 | MenhirState73 | MenhirState27 | MenhirState64 | MenhirState29 | MenhirState33 | MenhirState35 | MenhirState37 | MenhirState59 | MenhirState39 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState41 | MenhirState46 | MenhirState43 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
        | MenhirState131 | MenhirState21 | MenhirState71 | MenhirState80 | MenhirState117 | MenhirState111 | MenhirState92 | MenhirState99 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, c) = _menhir_stack in
                let _v : (SourceAst.instruction list) =          ( [ProcCall(c)] ) in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | ADD | AND | BB | DIV | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SUB ->
                _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce39 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, id) = _menhir_stack in
    let _v : (SourceAst.location) =            ( Identifier(id) ) in
    _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | END ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState27 in
        let _v : (SourceAst.expression list) =               ( [] ) in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_goto_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.literal) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState131 | MenhirState21 | MenhirState71 | MenhirState117 | MenhirState80 | MenhirState111 | MenhirState108 | MenhirState104 | MenhirState99 | MenhirState92 | MenhirState94 | MenhirState77 | MenhirState73 | MenhirState23 | MenhirState64 | MenhirState27 | MenhirState59 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState46 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState33 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | LITINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | ADD | AND | BB | DIV | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
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
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | FOR ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | IDENT _v ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                | IF ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                | PRINT ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | WHILE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | END ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
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

and _menhir_reduce8 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, loc) = _menhir_stack in
    let _v : (SourceAst.expression) =                ( Location(loc) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

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
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | FOR ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | IDENT _v ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | IF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | PRINT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | WHILE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | END ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.block) =                                           ( [] ) in
    _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | IDENT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | IDENT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | IDENT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
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
    | ADD | AND | BB | DIV | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SET | SUB ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | IDENT _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
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

and _menhir_goto_arguments : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let a = _v in
        let (_menhir_stack, _menhir_s, e) = _menhir_stack in
        let _v : (SourceAst.expression list) =                                     ( a @ [e] ) in
        _menhir_goto_arguments _menhir_env _menhir_stack _menhir_s _v
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let a = _v in
        let _v : (SourceAst.expression list) =               ( a ) in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (SourceAst.literal) =        ( Bool true ) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let _v : (SourceAst.literal) =            ( Int i ) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
    | ADD | AND | BB | COMMA | DIV | EB | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (SourceAst.literal) =         ( Bool false ) in
    _menhir_goto_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_goto_location : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState108 | MenhirState104 | MenhirState94 | MenhirState87 | MenhirState77 | MenhirState73 | MenhirState23 | MenhirState64 | MenhirState27 | MenhirState59 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState46 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState33 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState131 | MenhirState21 | MenhirState71 | MenhirState117 | MenhirState80 | MenhirState111 | MenhirState99 | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BB ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | FALSE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | IDENT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | LITINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | ADD | AND | BB | DIV | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SUB ->
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_var_decls : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.identifier_info SourceAst.Symb_Tbl.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, t), id), _, tbl) = _menhir_stack in
        let _v : (SourceAst.identifier_info SourceAst.Symb_Tbl.t) =   ( let info = {typ=t; kind=Local} in Symb_Tbl.add id info tbl ) in
        _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | FOR ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | IDENT _v ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | IF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | PRINT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | WHILE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | END ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | FOR ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | IDENT _v ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
        | IF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
        | PRINT ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | WHILE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | END ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
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
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
                | BB | END | FALSE | FOR | IDENT _ | IF | LITINT _ | PRINT | TRUE | WHILE ->
                    _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState15
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
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
    | MenhirState127 ->
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
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | BB | END | FALSE | FOR | IDENT _ | IF | LITINT _ | PRINT | TRUE | WHILE ->
                    _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
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

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | EB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BB ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | BOOLEAN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | INT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI | SUB ->
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
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | DIV | EB | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Mult )
            in
                                                      ( Binop(b,e1,e2) ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | EB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e) = _menhir_stack in
            let _v : (SourceAst.location) =                                       ( ArrayAccess(e1,e) ) in
            _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EB | END | OR | SEMI ->
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
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | DIV | EB | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e2) = _menhir_stack in
            let _v : (SourceAst.expression) = let b =
                     ( Div )
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
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI | SUB ->
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
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EB | END | OR | SEMI ->
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
    | MenhirState64 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BB ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | FALSE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | IDENT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | LITINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | FOR ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | IDENT _v ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
                | IF ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
                | PRINT ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | WHILE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | END ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (SourceAst.instruction list) =                                   ( [Print(e)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | FOR ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | IDENT _v ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
                | IF ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
                | PRINT ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | WHILE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | END ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, l), _, e) = _menhir_stack in
            let _v : (SourceAst.instruction list) =                                 ( [Set(l,e)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState131 | MenhirState21 | MenhirState71 | MenhirState80 | MenhirState117 | MenhirState111 | MenhirState92 | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BB ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | FALSE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | IDENT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | LITINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
                    | BB ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                    | FALSE ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                    | IDENT _v ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
                    | LITINT _v ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
                    | TRUE ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState108
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
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
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | FOR ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | IDENT _v ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
                | IF ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
                | PRINT ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | WHILE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | END ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (SourceAst.identifier_info SourceAst.Symb_Tbl.t) =               ( Symb_Tbl.empty ) in
    _menhir_goto_var_decls _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BB ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | BOOLEAN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_goto_parameters : _menhir_env -> 'ttv_tail -> _menhir_state -> ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let ((_menhir_stack, _menhir_s, t), id) = _menhir_stack in
        let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                                         ( (t,id) :: p ) in
        _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
    | MenhirState127 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                ( p ) in
        _menhir_goto_para _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
    | MenhirState135 ->
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
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
        let _v : (SourceAst.typ) =                  ( TypArray(t) ) in
        _menhir_goto_typs _menhir_env _menhir_stack _menhir_s _v
    | MenhirState127 | MenhirState10 | MenhirState3 ->
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
                | BB ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | BOOLEAN ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | INT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
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
    | MenhirState16 ->
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
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | BB | END | FALSE | FOR | IDENT _ | IF | LITINT _ | PRINT | TRUE | WHILE ->
                    _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState19
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
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
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, e), _, t) = _menhir_stack in
        let _v : (SourceAst.expression) =                                ( NewArray(e,TypArray t) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState135 | MenhirState0 ->
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
                | BB ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | BOOLEAN ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | INT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | END ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
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
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
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
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        | BB ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | BOOLEAN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | INT ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | END ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState3
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

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | EB ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BB ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | BOOLEAN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | INT ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
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
    | BB ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BOOLEAN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



