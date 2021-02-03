
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | SYMBOLE of (
# 7 "schemeyacc.mly"
       (string)
# 11 "schemeyacc.ml"
  )
    | QUOTE
    | PARRIGHT
    | PARLEFT
    | OR
    | NIL
    | MOT of (
# 7 "schemeyacc.mly"
       (string)
# 21 "schemeyacc.ml"
  )
    | LET
    | LAMBDA
    | IF
    | ENTIER of (
# 9 "schemeyacc.mly"
       (int)
# 29 "schemeyacc.ml"
  )
    | DEFINE
    | COND
    | BOOLEEN of (
# 8 "schemeyacc.mly"
       (bool)
# 36 "schemeyacc.ml"
  )
    | BEGIN
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState117
  | MenhirState115
  | MenhirState111
  | MenhirState110
  | MenhirState108
  | MenhirState107
  | MenhirState105
  | MenhirState104
  | MenhirState101
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState92
  | MenhirState89
  | MenhirState87
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState77
  | MenhirState76
  | MenhirState72
  | MenhirState70
  | MenhirState66
  | MenhirState65
  | MenhirState61
  | MenhirState60
  | MenhirState56
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState47
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState38
  | MenhirState35
  | MenhirState31
  | MenhirState26
  | MenhirState25
  | MenhirState20
  | MenhirState18
  | MenhirState14
  | MenhirState13
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState2
  | MenhirState0

# 2 "schemeyacc.mly"
  
open List	
open Scheme_exp

# 110 "schemeyacc.ml"

let rec _menhir_reduce14 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'tv_liste -> 'ttv_return =
  fun _menhir_env _menhir_stack _ (_2 : 'tv_liste) ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _1 = () in
    let _v : 'tv_exp = 
# 31 "schemeyacc.mly"
                        ( _2 )
# 119 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce26 : _menhir_env -> ('ttv_tail * _menhir_state * 'tv_exp) * _menhir_state * 'tv_largs -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s, (_1 : 'tv_exp)), _, (_2 : 'tv_largs)) = _menhir_stack in
    let _3 = () in
    let _v : 'tv_liste = 
# 58 "schemeyacc.mly"
                                                ( Call(_1, _2 ) )
# 130 "schemeyacc.ml"
     in
    _menhir_goto_liste _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce28 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_largs -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_largs)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : 'tv_liste = 
# 60 "schemeyacc.mly"
                                          ( Begin(_2))
# 142 "schemeyacc.ml"
     in
    _menhir_goto_liste _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_liste : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_liste -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv407 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_liste) = _v in
        (_menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv408)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_liste) = _v in
        (_menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv410)
    | _ ->
        _menhir_fail ()

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_symbliste -> (
# 7 "schemeyacc.mly"
       (string)
# 167 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv405 * _menhir_state * 'tv_symbliste) = Obj.magic _menhir_stack in
    let ((_2 : (
# 7 "schemeyacc.mly"
       (string)
# 176 "schemeyacc.ml"
    )) : (
# 7 "schemeyacc.mly"
       (string)
# 180 "schemeyacc.ml"
    )) = _v in
    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_symbliste)) = _menhir_stack in
    let _v : 'tv_symbliste = 
# 65 "schemeyacc.mly"
                       (List.append _1 (_2 :: []) )
# 186 "schemeyacc.ml"
     in
    _menhir_goto_symbliste _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)

and _menhir_reduce12 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp_cond -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (((_menhir_stack, _menhir_s), _), _, (_3 : 'tv_exp_cond)) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : 'tv_exp = 
# 29 "schemeyacc.mly"
                                                 ( Cond(_3))
# 199 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce27 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let (_menhir_stack, _menhir_s, (_1 : 'tv_exp)) = _menhir_stack in
    let _2 = () in
    let _v : 'tv_liste = 
# 59 "schemeyacc.mly"
                                              ( Call0(_1) )
# 210 "schemeyacc.ml"
     in
    _menhir_goto_liste _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce21 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_exp)), _, (_3 : 'tv_exp)) = _menhir_stack in
    let _4 = () in
    let _1 = () in
    let _v : 'tv_liste = 
# 53 "schemeyacc.mly"
                                                   ( And(_2, _3))
# 222 "schemeyacc.ml"
     in
    _menhir_goto_liste _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_largs : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_largs -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv391 * _menhir_state) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv389 * _menhir_state) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState61 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv392)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv395 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv393 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState72 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv396)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv399 * _menhir_state) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv397 * _menhir_state) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState108 in
            (_menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv398)
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv400)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv403 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv401 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState117 in
            (_menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv402)
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv404)
    | _ ->
        _menhir_fail ()

and _menhir_reduce25 : _menhir_env -> (('ttv_tail * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 346 "schemeyacc.ml"
)) * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), (_2 : (
# 7 "schemeyacc.mly"
       (string)
# 352 "schemeyacc.ml"
    ))), _, (_3 : 'tv_exp)) = _menhir_stack in
    let _4 = () in
    let _1 = () in
    let _v : 'tv_liste = 
# 57 "schemeyacc.mly"
                                              ( Define(_2,_3))
# 359 "schemeyacc.ml"
     in
    _menhir_goto_liste _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce20 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_exp)), _, (_3 : 'tv_exp)), _, (_4 : 'tv_exp)) = _menhir_stack in
    let _5 = () in
    let _1 = () in
    let _v : 'tv_liste = 
# 52 "schemeyacc.mly"
                                                   ( If(_2, _3, _4))
# 371 "schemeyacc.ml"
     in
    _menhir_goto_liste _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce23 : _menhir_env -> (('ttv_tail * _menhir_state)) * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (_3 : 'tv_exp)) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : 'tv_liste = 
# 55 "schemeyacc.mly"
                                                       ( Lambda([], _3) )
# 384 "schemeyacc.ml"
     in
    _menhir_goto_liste _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce24 : _menhir_env -> (((('ttv_tail * _menhir_state)) * _menhir_state * 'tv_symbliste)) * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (_3 : 'tv_symbliste)), _, (_5 : 'tv_exp)) = _menhir_stack in
    let _6 = () in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : 'tv_liste = 
# 56 "schemeyacc.mly"
                                                       ( Lambda(_3, _5) )
# 398 "schemeyacc.ml"
     in
    _menhir_goto_liste _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce11 : _menhir_env -> ((((('ttv_tail * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) * _menhir_state) * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((((_menhir_stack, _menhir_s), _), _, (_4 : 'tv_bindliste)), _), _, (_6 : 'tv_exp)) = _menhir_stack in
    let _7 = () in
    let _5 = () in
    let _3 = () in
    let _2 = () in
    let _1 = () in
    let _v : 'tv_exp = 
# 28 "schemeyacc.mly"
                                                          ( Let(_4,_6) )
# 413 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce22 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_exp)), _, (_3 : 'tv_exp)) = _menhir_stack in
    let _4 = () in
    let _1 = () in
    let _v : 'tv_liste = 
# 54 "schemeyacc.mly"
                                               ( Or(_2, _3))
# 430 "schemeyacc.ml"
     in
    _menhir_goto_liste _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce13 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _), _, (_3 : 'tv_exp)) = _menhir_stack in
    let _4 = () in
    let _2 = () in
    let _1 = () in
    let _v : 'tv_exp = 
# 30 "schemeyacc.mly"
                                 ( Literal (Quote _3) )
# 443 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_bindliste : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_bindliste -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv383 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARLEFT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv381 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState25 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv382)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv384)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv387 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARLEFT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv385 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState82 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv386)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82) : 'freshtv388)
    | _ ->
        _menhir_fail ()

and _menhir_goto_symbliste : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_symbliste -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv373 * _menhir_state)) * _menhir_state * 'tv_symbliste) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * _menhir_state)) * _menhir_state * 'tv_symbliste) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35) : 'freshtv370)
        | SYMBOLE _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv371 * _menhir_state)) * _menhir_state * 'tv_symbliste) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)) : 'freshtv374)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state)) * _menhir_state * 'tv_symbliste) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state)) * _menhir_state * 'tv_symbliste) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv376)
        | SYMBOLE _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv377 * _menhir_state)) * _menhir_state * 'tv_symbliste) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv378)) : 'freshtv380)
    | _ ->
        _menhir_fail ()

and _menhir_goto_exp_cond : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exp_cond -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv363 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp_cond) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARLEFT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv361 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp_cond) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState56 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv362)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv364)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv367 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp_cond) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARLEFT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv365 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp_cond) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState105 in
            (_menhir_reduce12 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv366)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv368)
    | _ ->
        _menhir_fail ()

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv189 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv185 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv186)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv187 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)) : 'freshtv190)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv192)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv197 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv193 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv194)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv195 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)) : 'freshtv198)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv215 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 716 "schemeyacc.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv211 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 726 "schemeyacc.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv209 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 733 "schemeyacc.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (_2 : (
# 7 "schemeyacc.mly"
       (string)
# 738 "schemeyacc.ml"
            ))), _, (_3 : 'tv_exp)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_bindpair = 
# 40 "schemeyacc.mly"
                                       (  (_2,_3) )
# 745 "schemeyacc.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv207) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_bindpair) = _v in
            ((match _menhir_s with
            | MenhirState81 | MenhirState18 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bindpair) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_bindpair) : 'tv_bindpair) = _v in
                ((let _v : 'tv_bindliste = 
# 36 "schemeyacc.mly"
                ( _1 :: [] )
# 764 "schemeyacc.ml"
                 in
                _menhir_goto_bindliste _menhir_env _menhir_stack _menhir_s _v) : 'freshtv200)) : 'freshtv202)
            | MenhirState82 | MenhirState25 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv205 * _menhir_state * 'tv_bindliste) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bindpair) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv203 * _menhir_state * 'tv_bindliste) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_2 : 'tv_bindpair) : 'tv_bindpair) = _v in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_bindliste)) = _menhir_stack in
                let _v : 'tv_bindliste = 
# 37 "schemeyacc.mly"
                                 ( List.append _1 (_2 :: []))
# 780 "schemeyacc.ml"
                 in
                _menhir_goto_bindliste _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)
            | _ ->
                _menhir_fail ()) : 'freshtv208)) : 'freshtv210)) : 'freshtv212)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv213 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 792 "schemeyacc.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv221 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv217 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv218)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv219 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv227 * _menhir_state)) * _menhir_state * 'tv_symbliste)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv223 * _menhir_state)) * _menhir_state * 'tv_symbliste)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv224)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv225 * _menhir_state)) * _menhir_state * 'tv_symbliste)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv229 * _menhir_state)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv230)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv231 * _menhir_state)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)) : 'freshtv234)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42) : 'freshtv236)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv237 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv238)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv243 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv239 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv240)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv241 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)) : 'freshtv244)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv249 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 917 "schemeyacc.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv245 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 927 "schemeyacc.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv247 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 938 "schemeyacc.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv252)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv269 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv265 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv263 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_exp)), _, (_3 : 'tv_exp)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_cond_pair = 
# 49 "schemeyacc.mly"
                                    (  (_2,_3) )
# 982 "schemeyacc.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv261) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_cond_pair) = _v in
            ((match _menhir_s with
            | MenhirState105 | MenhirState56 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_exp_cond) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_cond_pair) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_exp_cond) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_2 : 'tv_cond_pair) : 'tv_cond_pair) = _v in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_exp_cond)) = _menhir_stack in
                let _v : 'tv_exp_cond = 
# 46 "schemeyacc.mly"
                           ( List.append _1 (_2 :: []))
# 1002 "schemeyacc.ml"
                 in
                _menhir_goto_exp_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv254)) : 'freshtv256)
            | MenhirState104 | MenhirState50 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv259) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_cond_pair) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv257) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_cond_pair) : 'tv_cond_pair) = _v in
                ((let _v : 'tv_exp_cond = 
# 45 "schemeyacc.mly"
                 ( _1 :: [])
# 1017 "schemeyacc.ml"
                 in
                _menhir_goto_exp_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv258)) : 'freshtv260)
            | _ ->
                _menhir_fail ()) : 'freshtv262)) : 'freshtv264)) : 'freshtv266)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv267 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)) : 'freshtv270)
    | MenhirState117 | MenhirState108 | MenhirState72 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv273 * _menhir_state * 'tv_largs) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv271 * _menhir_state * 'tv_largs) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_largs)), _, (_2 : 'tv_exp)) = _menhir_stack in
        let _v : 'tv_largs = 
# 69 "schemeyacc.mly"
               (List.append _1 (_2 :: []) )
# 1038 "schemeyacc.ml"
         in
        _menhir_goto_largs _menhir_env _menhir_stack _menhir_s _v) : 'freshtv272)) : 'freshtv274)
    | MenhirState115 | MenhirState107 | MenhirState70 | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv275 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_exp)) = _menhir_stack in
        let _v : 'tv_largs = 
# 68 "schemeyacc.mly"
              (_1 :: [])
# 1050 "schemeyacc.ml"
         in
        _menhir_goto_largs _menhir_env _menhir_stack _menhir_s _v) : 'freshtv276)) : 'freshtv278)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv279 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv280)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv285 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv281 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv282)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv283 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)) : 'freshtv286)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv287 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState70 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv290)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv295 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv291 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            (_menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) : 'freshtv292)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv293 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv294)) : 'freshtv296)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv297 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77) : 'freshtv298)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv303 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv299 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            (_menhir_reduce22 _menhir_env (Obj.magic _menhir_stack) : 'freshtv300)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv301 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv309 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv305 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            (_menhir_reduce11 _menhir_env (Obj.magic _menhir_stack) : 'freshtv306)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv307 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)) : 'freshtv310)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv315 * _menhir_state)) * _menhir_state * 'tv_symbliste)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv311 * _menhir_state)) * _menhir_state * 'tv_symbliste)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            (_menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) : 'freshtv312)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv313 * _menhir_state)) * _menhir_state * 'tv_symbliste)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)) : 'freshtv316)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * _menhir_state)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv317 * _menhir_state)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            (_menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv319 * _menhir_state)) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv323 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv324)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv325 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv326)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv331 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv327 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            (_menhir_reduce20 _menhir_env (Obj.magic _menhir_stack) : 'freshtv328)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv329 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)) : 'freshtv332)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv337 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 1294 "schemeyacc.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv333 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 1304 "schemeyacc.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            (_menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) : 'freshtv334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv335 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 1314 "schemeyacc.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv339 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv340)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv345 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv341 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            (_menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) : 'freshtv342)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv343 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)) : 'freshtv346)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv347 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState115 in
            (_menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv348)
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv350)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv359 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv357 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_exp)) = _menhir_stack in
        let _v : (
# 16 "schemeyacc.mly"
      (Scheme_exp.exp)
# 1393 "schemeyacc.ml"
        ) = 
# 20 "schemeyacc.mly"
             ( _1 )
# 1397 "schemeyacc.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv355) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 16 "schemeyacc.mly"
      (Scheme_exp.exp)
# 1405 "schemeyacc.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 16 "schemeyacc.mly"
      (Scheme_exp.exp)
# 1413 "schemeyacc.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 16 "schemeyacc.mly"
      (Scheme_exp.exp)
# 1421 "schemeyacc.ml"
        )) : (
# 16 "schemeyacc.mly"
      (Scheme_exp.exp)
# 1425 "schemeyacc.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv352)) : 'freshtv354)) : 'freshtv356)) : 'freshtv358)) : 'freshtv360)
    | _ ->
        _menhir_fail ()

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 1434 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 7 "schemeyacc.mly"
       (string)
# 1439 "schemeyacc.ml"
  )) ->
    let _v : 'tv_exp = 
# 23 "schemeyacc.mly"
                      ( Var(_1) )
# 1444 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SYMBOLE _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 7 "schemeyacc.mly"
       (string)
# 1460 "schemeyacc.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20) : 'freshtv182)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv179) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_bindliste = 
# 35 "schemeyacc.mly"
              ( [] )
# 1500 "schemeyacc.ml"
     in
    _menhir_goto_bindliste _menhir_env _menhir_stack _menhir_s _v) : 'freshtv180)

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 1507 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv177) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 7 "schemeyacc.mly"
       (string)
# 1517 "schemeyacc.ml"
    )) : (
# 7 "schemeyacc.mly"
       (string)
# 1521 "schemeyacc.ml"
    )) = _v in
    ((let _v : 'tv_symbliste = 
# 64 "schemeyacc.mly"
              (_1 :: [])
# 1526 "schemeyacc.ml"
     in
    _menhir_goto_symbliste _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | ENTIER _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | MOT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | NIL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PARLEFT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SYMBOLE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv175) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_exp_cond = 
# 44 "schemeyacc.mly"
             ( [] )
# 1563 "schemeyacc.ml"
     in
    _menhir_goto_exp_cond _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv91 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp_cond) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 1615 "schemeyacc.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv97 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv103 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv105 * _menhir_state)) * _menhir_state * 'tv_symbliste)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv109 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState82 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv111 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv113 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv115 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv127 * _menhir_state) * _menhir_state * 'tv_largs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv131 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_exp_cond) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv137 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 1729 "schemeyacc.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv141 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState42 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv149 * _menhir_state)) * _menhir_state * 'tv_symbliste)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv153 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv155 * _menhir_state) * _menhir_state)) * _menhir_state * 'tv_bindliste) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv157 * _menhir_state) * (
# 7 "schemeyacc.mly"
       (string)
# 1778 "schemeyacc.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv159 * _menhir_state) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv161 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv169 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv174)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 1825 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AND ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv48)
    | BEGIN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv50)
    | BOOLEEN _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | COND ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | NIL ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PARLEFT ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv52)
    | DEFINE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SYMBOLE _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv53 * _menhir_state) = Obj.magic _menhir_stack in
            let (_v : (
# 7 "schemeyacc.mly"
       (string)
# 1917 "schemeyacc.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv54)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)) : 'freshtv58)
    | ENTIER _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | IF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41) : 'freshtv60)
    | LAMBDA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | NIL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61 * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv62)
        | PARLEFT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63 * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SYMBOLE _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv64)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv65 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)) : 'freshtv68)
    | LET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARLEFT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv69 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | NIL ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | PARLEFT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18) : 'freshtv70)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv71 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)) : 'freshtv74)
    | MOT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | NIL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | OR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv76)
    | PARLEFT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | QUOTE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv78)
    | SYMBOLE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 2119 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "schemeyacc.mly"
       (int)
# 2128 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "schemeyacc.mly"
       (bool)
# 2137 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_reduce8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _1 = () in
    let _v : 'tv_exp = 
# 25 "schemeyacc.mly"
               ( Literal(Nil) )
# 2149 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 2156 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 7 "schemeyacc.mly"
       (string)
# 2161 "schemeyacc.ml"
  )) ->
    let _v : 'tv_exp = 
# 27 "schemeyacc.mly"
                    ( Literal(Mot(_1)) )
# 2166 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "schemeyacc.mly"
       (int)
# 2173 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 9 "schemeyacc.mly"
       (int)
# 2178 "schemeyacc.ml"
  )) ->
    let _v : 'tv_exp = 
# 26 "schemeyacc.mly"
                 ( Literal(Entier(_1)) )
# 2183 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "schemeyacc.mly"
       (bool)
# 2190 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 8 "schemeyacc.mly"
       (bool)
# 2195 "schemeyacc.ml"
  )) ->
    let _v : 'tv_exp = 
# 24 "schemeyacc.mly"
                   ( Literal(Booleen(_1)) )
# 2200 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 16 "schemeyacc.mly"
      (Scheme_exp.exp)
# 2219 "schemeyacc.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv45) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_v : (
# 8 "schemeyacc.mly"
       (bool)
# 2245 "schemeyacc.ml"
        )) = _v in
        (_menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv2)
    | ENTIER _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_v : (
# 9 "schemeyacc.mly"
       (int)
# 2255 "schemeyacc.ml"
        )) = _v in
        (_menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv4)
    | MOT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_v : (
# 7 "schemeyacc.mly"
       (string)
# 2265 "schemeyacc.ml"
        )) = _v in
        (_menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv6)
    | NIL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        (_menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) _menhir_s : 'freshtv8)
    | PARLEFT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv10)
        | BEGIN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv12)
        | BOOLEEN _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | COND ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv13 * _menhir_state) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | NIL ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | PARLEFT ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv14)
        | DEFINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SYMBOLE _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
                let (_v : (
# 7 "schemeyacc.mly"
       (string)
# 2361 "schemeyacc.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BOOLEEN _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | ENTIER _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | MOT _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | NIL ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | PARLEFT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | SYMBOLE _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv16)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)) : 'freshtv20)
        | ENTIER _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | IF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv22)
        | LAMBDA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | NIL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BOOLEEN _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                | ENTIER _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                | MOT _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                | NIL ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | PARLEFT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | SYMBOLE _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv24)
            | PARLEFT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv25 * _menhir_state) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SYMBOLE _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv26)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)) : 'freshtv30)
        | LET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv35 * _menhir_state) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | PARLEFT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv31 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | NIL ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | PARLEFT ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv32)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv33 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)) : 'freshtv36)
        | MOT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | NIL ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | OR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv38)
        | PARLEFT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | QUOTE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv39 * _menhir_state) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState2 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BOOLEEN _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | ENTIER _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | MOT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | NIL ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | PARLEFT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | SYMBOLE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv40)
        | SYMBOLE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2) : 'freshtv42)
    | SYMBOLE _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        let (_v : (
# 7 "schemeyacc.mly"
       (string)
# 2561 "schemeyacc.ml"
        )) = _v in
        (_menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v : 'freshtv44)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv46))

# 71 "schemeyacc.mly"
  

# 2572 "schemeyacc.ml"

# 269 "<standard.mly>"
  

# 2577 "schemeyacc.ml"
