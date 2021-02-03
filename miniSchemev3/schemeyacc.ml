
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
    | NIL
    | MOT of (
# 7 "schemeyacc.mly"
       (string)
# 20 "schemeyacc.ml"
  )
    | ENTIER of (
# 9 "schemeyacc.mly"
       (int)
# 25 "schemeyacc.ml"
  )
    | BOOLEEN of (
# 8 "schemeyacc.mly"
       (bool)
# 30 "schemeyacc.ml"
  )
  
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
  | MenhirState14
  | MenhirState7
  | MenhirState5
  | MenhirState3
  | MenhirState2
  | MenhirState0

# 2 "schemeyacc.mly"
  
open List	
open Scheme_exp

# 60 "schemeyacc.ml"

let rec _menhir_reduce7 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_paire -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_paire)) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : 'tv_exp = 
# 26 "schemeyacc.mly"
                                  ( _2 )
# 70 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce6 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_exp)) = _menhir_stack in
    let _1 = () in
    let _v : 'tv_exp = 
# 25 "schemeyacc.mly"
                    (  Paire(ref (Symbole("quote")), ref(_2)))
# 81 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_paire : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_paire -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * _menhir_state) * _menhir_state * 'tv_paire) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv39 * _menhir_state) * _menhir_state * 'tv_paire) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)) : 'freshtv40)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv41 * _menhir_state) * _menhir_state * 'tv_paire) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)) : 'freshtv44)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_paire) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_paire) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_exp)), _, (_2 : 'tv_paire)) = _menhir_stack in
        let _v : 'tv_paire = 
# 34 "schemeyacc.mly"
                      (Paire (ref(_1), ref(_2)))
# 116 "schemeyacc.ml"
         in
        _menhir_goto_paire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv46)) : 'freshtv48)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_paire) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | PARRIGHT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv49 * _menhir_state) * _menhir_state * 'tv_paire) = Obj.magic _menhir_stack in
            (_menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) : 'freshtv50)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv51 * _menhir_state) * _menhir_state * 'tv_paire) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)) : 'freshtv54)
    | _ ->
        let (() : unit) = () in
        ((Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
        assert false) : 'freshtv55)

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 | MenhirState14 | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BOOLEEN _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | ENTIER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | MOT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | NIL ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | PARLEFT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | QUOTE ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | SYMBOLE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | PARRIGHT ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv24)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        (_menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) : 'freshtv26)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        (_menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) : 'freshtv28)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_exp)) = _menhir_stack in
        let _v : (
# 13 "schemeyacc.mly"
      (Scheme_exp.exp)
# 184 "schemeyacc.ml"
        ) = 
# 17 "schemeyacc.mly"
             ( _1 )
# 188 "schemeyacc.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 13 "schemeyacc.mly"
      (Scheme_exp.exp)
# 196 "schemeyacc.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : (
# 13 "schemeyacc.mly"
      (Scheme_exp.exp)
# 204 "schemeyacc.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : (
# 13 "schemeyacc.mly"
      (Scheme_exp.exp)
# 212 "schemeyacc.ml"
        )) : (
# 13 "schemeyacc.mly"
      (Scheme_exp.exp)
# 216 "schemeyacc.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv30)) : 'freshtv32)) : 'freshtv34)) : 'freshtv36)) : 'freshtv38)

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 223 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 7 "schemeyacc.mly"
       (string)
# 228 "schemeyacc.ml"
  )) ->
    let _v : 'tv_exp = 
# 20 "schemeyacc.mly"
                        (  Symbole(_1) )
# 233 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_paire = 
# 29 "schemeyacc.mly"
                          (Nil )
# 242 "schemeyacc.ml"
     in
    _menhir_goto_paire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 249 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        let (_v : (
# 8 "schemeyacc.mly"
       (bool)
# 268 "schemeyacc.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv16)
    | ENTIER _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        let (_v : (
# 9 "schemeyacc.mly"
       (int)
# 279 "schemeyacc.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv18)
    | MOT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        let (_v : (
# 7 "schemeyacc.mly"
       (string)
# 290 "schemeyacc.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv20)
    | NIL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | PARLEFT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | QUOTE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SYMBOLE _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        let (_v : (
# 7 "schemeyacc.mly"
       (string)
# 307 "schemeyacc.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v) : 'freshtv22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | ENTIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | MOT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | NIL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PARLEFT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | QUOTE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | SYMBOLE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | PARRIGHT ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 347 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "schemeyacc.mly"
       (int)
# 356 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "schemeyacc.mly"
       (bool)
# 365 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_reduce3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _1 = () in
    let _v : 'tv_exp = 
# 22 "schemeyacc.mly"
               (  Nil )
# 377 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 384 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 7 "schemeyacc.mly"
       (string)
# 389 "schemeyacc.ml"
  )) ->
    let _v : 'tv_exp = 
# 24 "schemeyacc.mly"
                    (  Mot(_1) )
# 394 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "schemeyacc.mly"
       (int)
# 401 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 9 "schemeyacc.mly"
       (int)
# 406 "schemeyacc.ml"
  )) ->
    let _v : 'tv_exp = 
# 23 "schemeyacc.mly"
                 (  Entier(_1) )
# 411 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "schemeyacc.mly"
       (bool)
# 418 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s (_1 : (
# 8 "schemeyacc.mly"
       (bool)
# 423 "schemeyacc.ml"
  )) ->
    let _v : 'tv_exp = 
# 21 "schemeyacc.mly"
                   (  Booleen(_1) )
# 428 "schemeyacc.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv4)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState2 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv14)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 468 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | ENTIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | MOT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | NIL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | PARLEFT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | QUOTE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | SYMBOLE _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | ENTIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | MOT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | NIL ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | PARLEFT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | QUOTE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | SYMBOLE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | PARRIGHT ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "schemeyacc.mly"
       (string)
# 528 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "schemeyacc.mly"
       (int)
# 536 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "schemeyacc.mly"
       (bool)
# 544 "schemeyacc.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

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
# 13 "schemeyacc.mly"
      (Scheme_exp.exp)
# 564 "schemeyacc.ml"
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
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEEN _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | ENTIER _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | MOT _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | NIL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PARLEFT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | QUOTE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SYMBOLE _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 36 "schemeyacc.mly"
  

# 605 "schemeyacc.ml"

# 269 "<standard.mly>"
  

# 610 "schemeyacc.ml"
