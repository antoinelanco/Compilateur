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
  | MenhirState148
  | MenhirState144
  | MenhirState143
  | MenhirState140
  | MenhirState130
  | MenhirState124
  | MenhirState119
  | MenhirState116
  | MenhirState114
  | MenhirState112
  | MenhirState107
  | MenhirState102
  | MenhirState99
  | MenhirState96
  | MenhirState94
  | MenhirState87
  | MenhirState84
  | MenhirState80
  | MenhirState78
  | MenhirState71
  | MenhirState68
  | MenhirState65
  | MenhirState63
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState30
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
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | BOOLEAN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | EOF ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | INT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState148
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148

and _menhir_reduce6 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.call) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, c) = _menhir_stack in
    let _v : (SourceAst.expression) =          ( FunCall(c) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce10 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.literal) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, i) = _menhir_stack in
    let _v : (SourceAst.expression) =             ( Literal(i) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_set_direct : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.instruction list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState144 | MenhirState21 | MenhirState78 | MenhirState130 | MenhirState87 | MenhirState124 | MenhirState119 | MenhirState107 | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : (SourceAst.instruction list) =                ( s ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState114 ->
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
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | FOR ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | IDENT _v ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | IF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                | PRINT ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | WHILE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | END ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
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
    | MenhirState99 ->
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
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, i), _, is) = _menhir_stack in
        let _v : (SourceAst.block) =                                           ( i @ is ) in
        _menhir_goto_instructions _menhir_env _menhir_stack _menhir_s _v
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s), l1), _, e1), _, e2), _, l3), _, e3), _, bl) = _menhir_stack in
            let _v : (SourceAst.instruction list) =   ( let block = bl @ [Set(Identifier(l3),e3)] in [Set(Identifier(l1),e1);While(e2, block)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), l1), _, e1), _, e2), _, s), _, bl) = _menhir_stack in
            let _v : (SourceAst.instruction list) =   ( let block = bl @ s in [Set(Identifier(l1),e1);While(e2, block)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
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
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | BB ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | FALSE ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | FOR ->
                        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | IDENT _v ->
                        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
                    | IF ->
                        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | LITINT _v ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
                    | PRINT ->
                        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | TRUE ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | WHILE ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | END ->
                        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState130
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
    | MenhirState130 ->
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
    | MenhirState78 ->
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
    | MenhirState144 ->
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
        let ((_menhir_stack, _menhir_s, id), _, a) = _menhir_stack in
        let _v : (SourceAst.call) =                               ( id, a ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState23 | MenhirState84 | MenhirState94 | MenhirState112 | MenhirState116 | MenhirState102 | MenhirState80 | MenhirState27 | MenhirState71 | MenhirState29 | MenhirState30 | MenhirState65 | MenhirState37 | MenhirState39 | MenhirState41 | MenhirState63 | MenhirState43 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState45 | MenhirState50 | MenhirState47 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
        | MenhirState144 | MenhirState21 | MenhirState78 | MenhirState87 | MenhirState130 | MenhirState124 | MenhirState119 | MenhirState99 | MenhirState107 ->
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

and _menhir_reduce9 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, loc) = _menhir_stack in
    let _v : (SourceAst.expression) =                ( Location(loc) ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce40 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, id) = _menhir_stack in
    let _v : (SourceAst.location) =            ( Identifier(id) ) in
    _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27
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
    | MenhirState144 | MenhirState21 | MenhirState78 | MenhirState130 | MenhirState87 | MenhirState124 | MenhirState119 | MenhirState116 | MenhirState112 | MenhirState107 | MenhirState99 | MenhirState102 | MenhirState84 | MenhirState80 | MenhirState23 | MenhirState71 | MenhirState27 | MenhirState29 | MenhirState65 | MenhirState63 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState47 | MenhirState45 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LITINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | ADD | AND | BB | DIV | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
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
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BA ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | FOR ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | IDENT _v ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | IF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | PRINT ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | WHILE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | END ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
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
        let ((_menhir_stack, _menhir_s), _, xs0) = _menhir_stack in
        let _v : (SourceAst.expression) = let es =
          let xs = xs0 in
              ( xs )
        in
                                                      ( NewArrayAcol(es) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, id) = _menhir_stack in
    let _v : (SourceAst.instruction list) =                 ( [Set(Identifier id,Binop(Add,Location( Identifier id ),Literal(Int 1)) )] ) in
    _menhir_goto_set_direct _menhir_env _menhir_stack _menhir_s _v

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, id) = _menhir_stack in
    let _v : (SourceAst.instruction list) =                 ( [Set(Identifier id,Binop(Sub,Location( Identifier id ),Literal(Int 1)) )] ) in
    _menhir_goto_set_direct _menhir_env _menhir_stack _menhir_s _v

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
        | BA ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | FOR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | IDENT _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | IF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | PRINT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | WHILE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | END ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BA ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23
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

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BA ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | IDENT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | BA ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | IDENT _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BEGIN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
    | DEC ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
    | INC ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
    | ADD | AND | BB | DIV | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SET | SUB ->
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                | BA ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94
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
    | MenhirState71 ->
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

and _menhir_goto_location : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState116 | MenhirState112 | MenhirState102 | MenhirState94 | MenhirState84 | MenhirState80 | MenhirState23 | MenhirState71 | MenhirState27 | MenhirState29 | MenhirState65 | MenhirState63 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState47 | MenhirState45 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState144 | MenhirState21 | MenhirState78 | MenhirState130 | MenhirState87 | MenhirState124 | MenhirState119 | MenhirState107 | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BA ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | BB ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | FALSE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | IDENT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | LITINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
        | ADD | AND | BB | DIV | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SUB ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMI_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (SourceAst.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMI_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (SourceAst.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMI_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37
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
    | ADD | AND | BB | COMMA | DIV | EA | EB | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack)
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
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
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

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | EA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState30 in
        let _v : (SourceAst.expression list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_SEMI_expression__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43
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

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39
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

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52
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
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
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
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
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

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
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

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | BB ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FALSE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LITINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (SourceAst.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | BA ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
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
        | BA ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | FOR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | IDENT _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | IF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | PRINT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | WHILE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | END ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BA ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | BB ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | FALSE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | FOR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | IDENT _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | IF ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | LITINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | PRINT ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | WHILE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | END ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
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
                | BA | BB | END | FALSE | FOR | IDENT _ | IF | LITINT _ | PRINT | TRUE | WHILE ->
                    _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState15
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
    | MenhirState140 ->
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
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | BA | BB | END | FALSE | FOR | IDENT _ | IF | LITINT _ | PRINT | TRUE | WHILE ->
                    _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
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
    | MenhirState65 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BA ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | BB ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | FALSE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | IDENT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | LITINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (SourceAst.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_SEMI_expression_ _menhir_env _menhir_stack _menhir_s _v
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
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI | SUB ->
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
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | DIV | EA | EB | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
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
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, e1), _, e) = _menhir_stack in
            let _v : (SourceAst.location) =                                       ( ArrayAccess(e1,e) ) in
            _menhir_goto_location _menhir_env _menhir_stack _menhir_s _v
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | OR | SEMI ->
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
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | DIV | EA | EB | END | EQ | LE | LT | ME | MT | MULT | NEQ | OR | SEMI | SUB ->
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
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI | SUB ->
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
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | EQ | LE | LT | ME | MT | NEQ | OR | SEMI ->
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
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EA | EB | END | OR | SEMI ->
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
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BB ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | BOOLEAN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | INT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BA ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | BB ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | FALSE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | IDENT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | LITINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BA ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | FOR ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | IDENT _v ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
                | IF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
                | PRINT ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | WHILE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | END ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState78
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : (SourceAst.instruction list) =                                   ( [Print(e)] ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BA ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | FOR ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | IDENT _v ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                | IF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                | PRINT ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | WHILE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | END ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState144 | MenhirState21 | MenhirState78 | MenhirState87 | MenhirState130 | MenhirState124 | MenhirState119 | MenhirState99 | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BA ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | BB ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | FALSE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | IDENT _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | LITINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState114 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | DEC ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
                | INC ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
                | SET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | BA ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                    | BB ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                    | FALSE ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                    | IDENT _v ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                    | LITINT _v ->
                        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                    | TRUE ->
                        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | BB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | BA ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | BB ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | FALSE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | FOR ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | IDENT _v ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | IF ->
                    _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | LITINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | PRINT ->
                    _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | WHILE ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | END ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ME ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                                             ( (t,id) :: p ) in
        _menhir_goto_parameters _menhir_env _menhir_stack _menhir_s _v
    | MenhirState140 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let p = _v in
        let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                    ( p ) in
        _menhir_goto_para _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                   ( [] ) in
    _menhir_goto_para _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (SourceAst.prog) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Obj.magic _1
    | MenhirState148 ->
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
    | MenhirState140 | MenhirState10 | MenhirState3 ->
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
                let _v : ((SourceAst.typ * SourceAst.Symb_Tbl.key) list) =                        ( [(t,id)] ) in
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
                | BA | BB | END | FALSE | FOR | IDENT _ | IF | LITINT _ | PRINT | TRUE | WHILE ->
                    _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState19
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
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, e), _, t) = _menhir_stack in
        let _v : (SourceAst.expression) =                                ( NewArray(e,TypArray t) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState148 | MenhirState0 ->
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
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                | BOOLEAN ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                | INT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                | END ->
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState140
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
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
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
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
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
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
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
            _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState3
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

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



