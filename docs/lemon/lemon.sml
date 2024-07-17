(* LEMON -- A simple interpreter for lambda-mu-nu.
 * Version 1.2, last modified August 30, 1995, by Brian Howard *)

datatype Type = VarT of string
              | ZeroT
              | OneT
              | SumT of Type * Type
              | ProdT of Type * Type
              | ExptT of Type * Type
              | MuT of TypeFunc
              | NuT of TypeFunc
              | Dummy of Type

withtype TypeFunc = string * Type

local
  fun ShowType (ExptT (T,ZeroT)) = implode ["~", ShowAtomT T]
    | ShowType (ExptT (T,U))     = implode [LShowSumT T, " -> ", ShowType U]
    | ShowType T                 = ShowSumT T

  and ShowSumT (SumT (OneT, OneT)) = "bool"
    | ShowSumT (SumT (T,U)) = implode [LShowSumT T, " + ", ShowProdT U]
    | ShowSumT T            = ShowProdT T

  and LShowSumT (SumT (OneT, OneT)) = "bool"
    | LShowSumT (SumT (T,U)) = implode [LShowSumT T, " + ", LShowProdT U]
    | LShowSumT T            = LShowProdT T

  and ShowProdT (ProdT (T,U)) = implode [LShowProdT T, " * ", ShowAtomT U]
    | ShowProdT T             = ShowAtomT T

  and LShowProdT (ProdT (T,U)) = implode [LShowProdT T, " * ", LShowAtomT U]
    | LShowProdT T             = LShowAtomT T

  and ShowAtomT (MuT (F as (v,SumT (OneT,VarT w)))) =
        if v=w
        then "nat"
        else  implode ["mu ", ShowTypeFunc F]
    | ShowAtomT (MuT F)            = implode ["mu ", ShowTypeFunc F]
    | ShowAtomT (NuT F)            = implode ["nu ", ShowTypeFunc F]
    | ShowAtomT (ExptT (T,ZeroT))  = implode ["~", ShowAtomT T]
    | ShowAtomT T                  = LShowAtomT T

  and LShowAtomT (MuT (F as (v,SumT (OneT,VarT w)))) =
        if v=w
        then "nat"
        else  implode ["(mu ", ShowTypeFunc F, ")"]
    | LShowAtomT (VarT v)           = v
    | LShowAtomT ZeroT              = "0"
    | LShowAtomT OneT               = "1"
    | LShowAtomT (SumT (OneT,OneT)) = "bool"
    | LShowAtomT (ExptT (T,ZeroT))  = implode ["~", LShowAtomT T]
    | LShowAtomT (Dummy _)          = "..."
    | LShowAtomT T                  = implode ["(", ShowType T, ")"]

  and ShowTypeFunc (v,T) = implode [v, ". ", ShowType T]
in
  val ShowType = ShowType
  val ShowTypeFunc = ShowTypeFunc
end

val NatT = MuT ("X", SumT (OneT, VarT "X"))

fun PrType T = (print (ShowType T); print "\n")

fun FreeVarT w (VarT v)      = v=w
  | FreeVarT w (SumT (T,U))  = FreeVarT w T orelse FreeVarT w U
  | FreeVarT w (ProdT (T,U)) = FreeVarT w T orelse FreeVarT w U
  | FreeVarT w (ExptT (T,U)) = FreeVarT w T orelse FreeVarT w U
  | FreeVarT w (MuT F)       = FreeVarF w F
  | FreeVarT w (NuT F)       = FreeVarF w F
  | FreeVarT w _             = false

and FreeVarF w (v,T) = not (v=w) andalso FreeVarT w T

local
  fun AuxNewVarT Ts n = let val w = "X"^makestring n
                        in
                          if exists (FreeVarT w) Ts
                          then AuxNewVarT Ts (n+1)
                          else w
                        end
in
  fun NewVarT Ts = AuxNewVarT Ts 0
end

fun delete x [] = []
  | delete x (y::ys) = if x=y
                       then ys
                       else y::(delete x ys)

fun merge l [] = l
  | merge l (x::xs) = x::(merge (delete x l) xs)

fun ListFreeVarT (VarT v)      = [v]
  | ListFreeVarT (SumT (T,U))  = merge (ListFreeVarT T) (ListFreeVarT U)
  | ListFreeVarT (ProdT (T,U)) = merge (ListFreeVarT T) (ListFreeVarT U)
  | ListFreeVarT (ExptT (T,U)) = merge (ListFreeVarT T) (ListFreeVarT U)
  | ListFreeVarT (MuT (v,T))   = delete v (ListFreeVarT T)
  | ListFreeVarT (NuT (v,T))   = delete v (ListFreeVarT T)
  | ListFreeVarT _             = []

fun SubstT S w (VarT v)      = if v=w then S else VarT v
  | SubstT S w (SumT (T,U))  = SumT (SubstT S w T, SubstT S w U)
  | SubstT S w (ProdT (T,U)) = ProdT (SubstT S w T, SubstT S w U)
  | SubstT S w (ExptT (T,U)) = ExptT (SubstT S w T, SubstT S w U)
  | SubstT S w (MuT F)       = MuT (SubstF S w F)
  | SubstT S w (NuT F)       = NuT (SubstF S w F)
  | SubstT S w T             = T

and SubstF S w (v,T) = if v=w
                       then (v,T)
                       else if FreeVarT v S
                            then let val u = NewVarT [S, T, VarT w]
                                     val U = SubstT (VarT u) v T
                                 in (u, SubstT S w U)
                                 end
                            else (v, SubstT S w T)

fun FuncT ((v,U),T) = SubstT T v U;

exception Error of string

datatype Term = Var of string
              | Box
              | Diamond
              | In1
              | In2
              | Case of Term * Term
              | Pi1
              | Pi2
              | Pair of Term * Term
              | Abs of (string * Type) * Term
              | App of Term * Term * Type
              | Fold of Type
              | Iter of Type * Term * Term
              | Unfold of Type
              | Gen of Type * Term * Term

local
  fun ShowTerm (App (M,N,T)) = implode [LShowTerm M, " ", ShowAtom N]
    | ShowTerm M             = ShowAtom M

  and LShowTerm (App (M,N,T)) = implode [LShowTerm M, " ", LShowAtom N]
    | LShowTerm M             = LShowAtom M

  and ShowAtom (Abs ((v,T),M)) = implode ["\\", v, ": ", ShowType T,
                                          ". ", ShowTerm M]
    | ShowAtom M               = LShowAtom M

  and LShowAtom (Var v)        = v
    | LShowAtom Box            = "[]"
    | LShowAtom Diamond        = "<>"
    | LShowAtom In1            = "in1"
    | LShowAtom In2            = "in2"
    | LShowAtom (Case (M,N))   = implode ["[", ShowTerm M, ", ",
                                               ShowTerm N, "]"]
    | LShowAtom Pi1            = "pi1"
    | LShowAtom Pi2            = "pi2"
    | LShowAtom (Pair (M,N))   = implode ["<", ShowTerm M, ", ",
                                               ShowTerm N, ">"]
    | LShowAtom (Fold T)       = implode ["fold ", ShowType T]
    | LShowAtom (Iter (T,F,M)) = implode ["[|", ShowType T, ", ",
                                                ShowTerm M, "|]"]
    | LShowAtom (Unfold T)     = implode ["unfold ", ShowType T]
    | LShowAtom (Gen (T,F,M))  = implode ["<|", ShowType T, ", ",
                                                ShowTerm M, "|>"]
    | LShowAtom M              = implode ["(", ShowTerm M, ")"]
in
  val ShowTerm = ShowTerm
end

val Zero = App (Fold NatT, App (In1, Diamond, OneT), SumT(OneT, NatT))

fun Succ M = App (Fold NatT, App (In2, M, NatT), SumT (OneT, NatT))

fun PrTerm M = (print (ShowTerm M); print "\n")

fun FreeVar w (Var v)         = v=w
  | FreeVar w (Case (M,N))    = (FreeVar w M) orelse (FreeVar w N)
  | FreeVar w (Pair (M,N))    = (FreeVar w M) orelse (FreeVar w N)
  | FreeVar w (Abs ((v,_),M)) = not (v=w) andalso (FreeVar w M)
  | FreeVar w (App (M,N,_))   = (FreeVar w M) orelse (FreeVar w N)
  | FreeVar w (Iter (_,_,M))  = FreeVar w M
  | FreeVar w (Gen (_,_,M))   = FreeVar w M
  | FreeVar w _               = false

local
  fun AuxNewVar Ms n = let val w = "x"^makestring n
                       in
                         if exists (FreeVar w) Ms
                         then AuxNewVar Ms (n+1)
                         else w
                       end
in
  fun NewVar Ms = AuxNewVar Ms 0
end

fun ListFreeVarTM (Case (M,N))    = merge (ListFreeVarTM M)
                                          (ListFreeVarTM N)
  | ListFreeVarTM (Pair (M,N))    = merge (ListFreeVarTM M)
                                          (ListFreeVarTM N)
  | ListFreeVarTM (Abs ((_,T),M)) = merge (ListFreeVarT T)
                                          (ListFreeVarTM M)
  | ListFreeVarTM (App (M,N,T))   = merge (merge (ListFreeVarTM M)
                                                 (ListFreeVarTM N))
                                          (ListFreeVarT T)
  | ListFreeVarTM (Fold T)        = ListFreeVarT T
  | ListFreeVarTM (Iter (T,M,N))  = merge (merge (ListFreeVarTM M)
                                                 (ListFreeVarTM N))
                                          (ListFreeVarT T)
  | ListFreeVarTM (Unfold T)      = ListFreeVarT T
  | ListFreeVarTM (Gen (T,M,N))   = merge (merge (ListFreeVarTM M)
                                                 (ListFreeVarTM N))
                                          (ListFreeVarT T)
  | ListFreeVarTM _               = []

fun ListFreeVarTMT (M,T) = merge (ListFreeVarTM M) (ListFreeVarT T)

fun Subst P w (Var v)         = if v=w then P else Var v
  | Subst P w (Case (M,N))    = Case (Subst P w M, Subst P w N)
  | Subst P w (Pair (M,N))    = Pair (Subst P w M, Subst P w N)
  | Subst P w (Abs ((v,U),M)) =
      if v=w
      then Abs ((v, U), M)
      else if FreeVar v P
           then let val u = NewVar [P, M, Var w]
                    val N = Subst (Var u) v M
                in Abs ((u, U), Subst P w N)
                end
           else Abs ((v, U), Subst P w M)
  | Subst P w (App (M,N,T))   = (App (Subst P w M, Subst P w N, T))
  | Subst P w (Iter (T,F,M))  = Iter (T, F, Subst P w M)
  | Subst P w (Gen (T,F,M))   = Gen (T, F, Subst P w M)
  | Subst P w M               = M

fun Id T = Abs (("x", T), Var "x")

(* if M: U->T and N: V->U, then (Comp M N V U): V->T *)
fun Comp M N V U = let val v = NewVar [M, N]
                   in Abs ((v, V), App (M, App (N, Var v, V), U))
                   end;

fun red (Case (M,N),ExptT (_,V))                 ((P,U)::Ps) =
      (case red (P, U) [] of
         App (In1,Q,T) => red (M, ExptT (T, V)) ((Q, T)::Ps)
       | App (In2,Q,T) => red (N, ExptT (T, V)) ((Q, T)::Ps)
       | _             => raise Error "Bad value of sum type")
  | red (Pi1,ExptT (_,V))                        ((P,U)::Ps) =
      (case red (P, U) [] of
         Pair (M,N) => red (M, V) Ps
       | _          => raise Error "Bad value of product type")
  | red (Pi2,ExptT (_,V))                        ((P,U)::Ps) =
      (case red (P, U) [] of
         Pair (M,N) => red (N, V) Ps
       | _          => raise Error "Bad value of product type")
(* The commented-out code is less eager -- performs much better on
 * "worsepred" but worse on "evenp" (see examples.lmn)
 *)
(*  | red (Abs ((v,_),M),ExptT (_,V))              ((P,_)::Ps) =
 *      red (Subst P v M, V) Ps
 *)
  | red (Abs ((v,_),M),ExptT (_,V))              ((P,U)::Ps) =
      red (Subst (red (P, U) []) v M, V) Ps
  | red (App (M,N,T),V)                          Ps =
      red (M, ExptT (T, V)) ((N, T)::Ps)
  | red (M as Iter (MuT G,F,N),T as ExptT (_,V)) ((P,U)::Ps) =
      let val GV = FuncT (G, V)
      in
        case red (P, U) [] of
          App (Fold _,Q,W) => red (N, ExptT (GV, V))
                                  ((App (App (F, M, T), Q, W), GV)::Ps)
        | _                => raise Error "Bad value of inductive type"
      end
  | red (Unfold _,ExptT (_,T))                   ((P,U)::Ps) =
      (case red (P, U) [] of
         App (Fold _,M,V)               => red (M, V) Ps
       | App (M as Gen (NuT G,F,N),Q,V) =>
           let val GV = FuncT (G, V)
               val VU = ExptT (V, U)
           in red (F, ExptT (VU, ExptT (GV, T)))
                  ((M, VU)::(App (N, Q, V), GV)::Ps)
           end
       | _                              => raise Error
                                             "Bad value of coinductive type")
  | red (M,_)             [(P,U)]     = App (M, P, U)
  | red (M,_)             []          = M
  | red _                 _           = raise Error "Bad term in red"

fun norm (M,T) = (red (M, T) [], T)

fun cnorm (M,T) =
      case norm (M, T) of
        (App (In1,N,U),_) => let val (N,_) = cnorm (N, U)
                             in (App (In1, N, U), T)
                             end
      | (App (In2,N,U),_) => let val (N,_) = cnorm (N, U)
                             in (App (In2, N, U), T)
                             end
      | (App (Fold _,N,U),_) => let val (N,_) = cnorm (N, U)
                                in (App (Fold T, N, U), T)
                                end
      | (App (Gen (_,F,M),N,U),_) => let val (N,_) = cnorm (N, U)
                                     in (App (Gen (T,F,M), N, U), T)
                                     end
      | (Pair (M,N),ProdT (U,V)) => let val (M,_) = cnorm (M, U)
                                        val (N,_) = cnorm (N, V)
                                    in (Pair (M, N), T)
                                    end
      | (M,T) => (M, T);

fun AuxShowNat (App (In1,_,_)) = 0
  | AuxShowNat (App (In2,App (Fold _,M,_),_)) = 1 + AuxShowNat M
  | AuxShowNat _ = raise Error "Bad term of type nat"

fun ShowNat M = makestring (AuxShowNat M)

fun ShowTermT (App (In1,M,T),SumT (U,V)) =
      if U=OneT andalso V=OneT
      then "true"
      else implode ["in1 (", ShowTermT (M, T), ")"]
  | ShowTermT (App (In2,M,T),SumT (U,V)) =
      if U=OneT andalso V=OneT
      then "false"
      else implode ["in2 (", ShowTermT (M, T), ")"]
  | ShowTermT (Pair (M,N),ProdT (T,U)) =
      implode ["<", ShowTermT (M, T), ", ", ShowTermT (N, U), ">"]
  | ShowTermT (App (Fold _,M,T),U as MuT (v,V)) =
      if V=SumT (OneT, VarT v)
      then ShowNat M
      else implode ["fold ", ShowType U, " (", ShowTermT (M, T), ")"]
  | ShowTermT (M,_) = ShowTerm M;

local
  fun SubstMTP p (M,V) w T = if FreeVarT w T
                             then SubstMTP1 p (M, V) w T
                             else Id T

(* Know that w occurs free in T when SubstMTP1 p M w T is called *)
  and SubstMTP1 p (M,V) w (VarT v) = if p
                                    then M
                                    else raise Error "Contravariance!"
    | SubstMTP1 p (M,V as ExptT (W,X)) w (SumT (T,U)) =
        let val WT = SubstT W w T
            val WU = SubstT W w U
            val XT = SubstT X w T
            val XU = SubstT X w U
        in Case (Comp In1 (SubstMTP p (M, V) w T) WT XT,
                 Comp In2 (SubstMTP p (M, V) w U) WU XU)
        end
    | SubstMTP1 p (M,V as ExptT (W,X)) w (ProdT (T,U)) =
        let val v = NewVar [M]
            val WT = SubstT W w T
            val WU = SubstT W w U
            val Y = ProdT (WT, WU)
        in Abs ((v, Y), Pair (App (SubstMTP p (M, V) w T,
                                   App (Pi1, Var v, Y), WT),
                              App (SubstMTP p (M, V) w U,
                                   App (Pi2, Var v, Y), WU)))
        end
    | SubstMTP1 p (M,V as ExptT (W,X)) w (ExptT (T,U)) =
        let val v = NewVar [M]
            val WT = SubstT W w T
            val WU = SubstT W w U
            val XT = SubstT X w T
            val XU = SubstT X w U
            val Y = ExptT (WT, WU)
            val Vop = ExptT (X, W)
        in Abs ((v, Y), Comp (SubstMTP p (M, V) w U)
                             (Comp (Var v)
                                   (SubstMTP (not p) (M, Vop) w T)
                                   XT WT)
                             XT WU)
        end
    | SubstMTP1 p (M,V as ExptT (W,X)) w (MuT (v,T)) =
        let val WT = SubstT W w T
            val XT = SubstT X w T
            val MT = SubstT (MuT (v, XT)) v T
            val WMT = SubstT W w MT
            val XMT = SubstT X w MT
        in IterT (MuT (v, WT))
                 (Comp (Fold (MuT (v, XT)))
                       (SubstMTP1 p (M, V) w MT)
                       WMT XMT)
                 (MuT (v, XT))
        end
    | SubstMTP1 p (M,V as ExptT (W,X)) w (NuT (v,T)) =
        let val WT = SubstT W w T
            val XT = SubstT X w T
            val NT = SubstT (NuT (v, WT)) v T
            val WNT = SubstT W w NT
        in GenT (NuT (v, XT))
                (Comp (SubstMTP1 p (M, V) w NT)
                      (Unfold (NuT (v, WT)))
                      (NuT (v, WT)) WNT)
                (NuT (v, WT))
        end
    | SubstMTP1 p (M,V) w T = raise Error "Internal error in SubstMTP"

  and IterT (MuT F) M V =
        let val T = ExptT (MuT F, V)
        in Iter (MuT F, Abs (("f", T), AppF F (Var "f", T)), M)
        end
    | IterT _       _ _ = raise Error "Illegal type in iteration"

  and GenT (NuT F) M V =
        let val T = ExptT (V, NuT F)
        in Gen (NuT F, Abs (("f", T), AppF F (Var "f", T)), M)
        end
    | GenT _       _ _ = raise Error "Illegal type in coiteration"

  and AppF (v,T) (M,V as ExptT (W,X)) = SubstMTP true (M, V) v T
    | AppF _     _ = raise Error "Internal error in AppF"
in
  val SubstMT = SubstMTP true

  val IterT = IterT

  val GenT = GenT

  val AppF = AppF
end;

fun NotT T = ExptT (T, ZeroT)

(* The following character-type functions assume an ASCII-like encoding:
 *)
fun isAlpha c = let val ordc = ord c
                in
                  (ord "A" <= ordc andalso ordc <= ord "Z") orelse
                  (ord "a" <= ordc andalso ordc <= ord "z")
                end
                handle Ord => false

fun isDigit c = let val ordc = ord c
                in
                  ord "0" <= ordc andalso ordc <= ord "9"
                end
                handle Ord => false

fun isAlnum c = isAlpha c orelse isDigit c

fun member x = exists (fn y => x=y)

datatype Lexeme = ID of string | NUM of int | STR of string
                | LBR | RBR | LPA | RPA
                | ZER | ONE | SUM | PRD | ARO | MU | NU | NOT
                | BOX | IN1 | IN2 | LSB | RSB
                | DIA | PI1 | PI2 | LAB | RAB
                | FLD | LIB | RIB | UFD | LGB | RGB
                | LAM | DOT | COM | SEM | COL | EQU | EOS

fun ShowLex (ID v) = v
  | ShowLex (NUM n) = makestring n
  | ShowLex (STR s) = implode ["\"", s, "\""]
  | ShowLex LBR = "{"
  | ShowLex RBR = "}"
  | ShowLex LPA = "("
  | ShowLex RPA = ")"
  | ShowLex ZER = "0"
  | ShowLex ONE = "1"
  | ShowLex SUM = "+"
  | ShowLex PRD = "*"
  | ShowLex ARO = "->"
  | ShowLex MU  = "mu"
  | ShowLex NU  = "nu"
  | ShowLex NOT = "~"
  | ShowLex BOX = "[]"
  | ShowLex IN1 = "in1"
  | ShowLex IN2 = "in2"
  | ShowLex LSB = "["
  | ShowLex RSB = "]"
  | ShowLex DIA = "<>"
  | ShowLex PI1 = "pi1"
  | ShowLex PI2 = "pi2"
  | ShowLex LAB = "<"
  | ShowLex RAB = ">"
  | ShowLex FLD = "fold"
  | ShowLex LIB = "[|"
  | ShowLex RIB = "|]"
  | ShowLex UFD = "unfold"
  | ShowLex LGB = "<|"
  | ShowLex RGB = "|>"
  | ShowLex LAM = "\\"
  | ShowLex DOT = "."
  | ShowLex COM = ","
  | ShowLex SEM = ";"
  | ShowLex COL = ":"
  | ShowLex EQU = "="
  | ShowLex EOS = "end of input"

abstype Lexstream = LS of Lexeme list * Lexstreamref
                  | IS of instream * Lexstreamref
                  | NS
withtype Lexstreamref = Lexstream ref
with
  val nulls = ref NS

  fun uses is ls = ref (IS (is, ls))

  local
    fun idorkey "mu"     = MU
      | idorkey "nu"     = NU
      | idorkey "in1"    = IN1
      | idorkey "in2"    = IN2
      | idorkey "pi1"    = PI1
      | idorkey "pi2"    = PI2
      | idorkey "fold"   = FLD
      | idorkey "unfold" = UFD
      | idorkey s        = ID s

    fun zon 0 = ZER
      | zon 1 = ONE
      | zon n = NUM n

    fun dignum c = ord c - ord "0"

    fun lexlineaux [] = []
      | lexlineaux ("{"::l) = LBR::(lexlineaux l)
      | lexlineaux ("}"::l) = RBR::(lexlineaux l)
      | lexlineaux ("("::l) = LPA::(lexlineaux l)
      | lexlineaux (")"::l) = RPA::(lexlineaux l)
      | lexlineaux ("+"::l) = SUM::(lexlineaux l)
      | lexlineaux ("*"::l) = PRD::(lexlineaux l)
      | lexlineaux ("~"::l) = NOT::(lexlineaux l)
      | lexlineaux ("]"::l) = RSB::(lexlineaux l)
      | lexlineaux (">"::l) = RAB::(lexlineaux l)
      | lexlineaux ("."::l) = DOT::(lexlineaux l)
      | lexlineaux (","::l) = COM::(lexlineaux l)
      | lexlineaux (";"::l) = SEM::(lexlineaux l)
      | lexlineaux (":"::l) = COL::(lexlineaux l)
      | lexlineaux ("="::l) = EQU::(lexlineaux l)
      | lexlineaux ("\\"::l) = LAM::(lexlineaux l)
      | lexlineaux ("-"::">"::l) = ARO::(lexlineaux l)
      | lexlineaux ("["::"]"::l) = BOX::(lexlineaux l)
      | lexlineaux ("["::"|"::l) = LIB::(lexlineaux l)
      | lexlineaux ("["::l) = LSB::(lexlineaux l)
      | lexlineaux ("<"::">"::l) = DIA::(lexlineaux l)
      | lexlineaux ("<"::"|"::l) = LGB::(lexlineaux l)
      | lexlineaux ("<"::l) = LAB::(lexlineaux l)
      | lexlineaux ("|"::"]"::l) = RIB::(lexlineaux l)
      | lexlineaux ("|"::">"::l) = RGB::(lexlineaux l)
      | lexlineaux (" "::l) = lexlineaux l
      | lexlineaux ("\n"::l) = lexlineaux l
      | lexlineaux ("\t"::l) = lexlineaux l
      | lexlineaux ("-"::"-"::l) = [] (* comment to end of line *)
      | lexlineaux ("\""::l) = getSTR "" l
      | lexlineaux (c::l) = if isAlpha c
                            then getID c l
                            else if isDigit c
                            then getNUM (dignum c) l
                            else raise Error ("Illegal character: "^c)

    and getID s [] = [idorkey s]
      | getID s (c::l) = if isAlnum c
                         then getID (s^c) l
                         else (idorkey s)::(lexlineaux (c::l))

    and getNUM n [] = [zon n]
      | getNUM n (c::l) = if isDigit c
                          then getNUM (10*n+dignum c) l
                          else (zon n)::(lexlineaux (c::l))

    and getSTR s [] = raise Error "Unclosed string at end of line"
      | getSTR s ("\""::l) = (STR s)::(lexlineaux l)
      | getSTR s (c::l) = getSTR (s^c) l

    fun lexline is = lexlineaux (explode (input_line is))
  in
    fun getlex ls =
          (case !ls of
             LS ([],ls')    => getlex ls'
           | LS (x::xs,ls') => (x, ref (LS (xs, ls')))
           | IS (is,ls')    => if end_of_stream is
                               then getlex ls'
                               else (ls := LS (lexline is, ref (!ls));
                                     getlex ls)
           | NS             => (EOS, ls))
  end

  fun putlex (x,ls) = (case !ls of
                         LS (xs,ls') => LS (x::xs, ls')
                       | _           => LS ([x], ls))
end;

fun Bind A v x = (v,x)::A

fun BindGT ((GT,GM),L) v T = ((Bind GT v T, GM), L)

fun BindLT (G,(LT,LM)) v T = (G, (Bind LT v T, LM))

fun BindGM ((GT,GM),L) v M = ((GT, Bind GM v M), L)

fun BindLM (G,(LT,LM)) v M = (G, (LT, Bind LM v M))

fun Lookup [] v         = raise Error ("Unbound variable "^v)
  | Lookup ((w,x)::A) v = if v=w then x else Lookup A v

fun LookupT ((GT,_),(LT,_)) v = Lookup LT v
                                handle Error _ =>
                                  Lookup GT v
                                  handle Error _ => VarT v

local
  fun AuxNewVarE GT LT n = let val w = "#"^makestring n
                           in
                             if exists (fn (v,_) => v=w) GT orelse
                                exists (fn (v,_) => v=w) LT
                             then AuxNewVarE GT LT (n+1)
                             else w
                           end
in
  fun NewVarE (E as ((GT,_),(LT,_))) =
        let val w = AuxNewVarE GT LT 0
        in (BindGT E w (VarT w), VarT w)
        end
end

fun UpdateE ((GT,GM),(LT,LM)) v T =
      ((map (fn (w,U) => (w, SubstT T v U)) GT,
        map (fn (w,(M,U)) => (w, (M, SubstT T v U))) GM),
       (map (fn (w,U) => (w, SubstT T v U)) LT,
        map (fn (w,(M,U)) => (w, (M, SubstT T v U))) LM))

fun AppE E (VarT v)      = let val T = LookupT E v
                           in
                             if T = VarT v
                             then T
                             else AppE E T
                           end
  | AppE E (SumT (T,U))  = SumT (AppE E T, AppE E U)
  | AppE E (ProdT (T,U)) = ProdT (AppE E T, AppE E U)
  | AppE E (ExptT (T,U)) = ExptT (AppE E T, AppE E U)
  | AppE E (MuT F)       = MuT (AppEF E F)
  | AppE E (NuT F)       = NuT (AppEF E F)
  | AppE E T             = T

and AppEF E (v,T) = (v, AppE (BindLT E v (VarT v)) T)

fun AppEMT E (Case (M,N),ExptT (SumT (T,U),V)) =
      Case (AppEMT E (M, ExptT (T, V)), AppEMT E (N, ExptT (U, V)))
  | AppEMT E (Pair (M,N),ProdT (T,U)) =
      Pair (AppEMT E (M, T), AppEMT E (N, U))
  | AppEMT E (Abs ((v,T),M),ExptT (_,U)) =
      Abs ((v, AppE E T), AppEMT E (M, U))
  | AppEMT E (App (M,N,T),U) =
      let val T = AppE E T
      in App (AppEMT E (M, ExptT (T, U)), AppEMT E (N, T), T)
      end
  | AppEMT E (Iter (T as (MuT F),_,M),ExptT (_,U)) =
      IterT (AppE E T) (AppEMT E (M, ExptT (FuncT (AppEF E F, U), U))) U
  | AppEMT E (Gen (T as (NuT F),_,M),ExptT (U,_)) =
      GenT (AppE E T) (AppEMT E (M, ExptT (U, FuncT (AppEF E F, U)))) U
  | AppEMT E (Fold T,_)   = Fold (AppE E T)
  | AppEMT E (Unfold T,_) = Unfold (AppE E T)
  | AppEMT E (M,_)        = M

fun CombineE ((GT,GM),(LT,LM)) ((GT',GM'),(LT',LM')) =
      ((GT'@GT, GM'@GM), (LT'@LT, LM'@LM))

fun InstAux (v,E) = let val (E,T) = NewVarE E
                    in BindLT E v T
                    end

fun Inst E (M,T) = let val FVL = ListFreeVarTMT (M, T)
                       val (_,L) = E
                       val E = fold InstAux FVL E
                       val (G,_) = E
                       val T = AppE E T
                       val M = AppEMT E (M, T)
                   in ((G, L), (M, T))
                   end

fun InstT E T = let val FVL = ListFreeVarT T
                    val (_,L) = E
                    val E = fold InstAux FVL E
                    val (G,_) = E
                    val T = AppE E T
                in ((G, L), T)
                end

fun InstF E (v,T) = let val FVL = delete v (ListFreeVarT T)
                        val (_,L) = E
                        val E = BindLT (fold InstAux FVL E) v (VarT v)
                        val (G,_) = E
                        val T = AppE E T
                    in ((G, L), (v, T))
                    end

fun LookupM (E as ((_,GM),(_,LM))) v = (E, Lookup LM v)
                                       handle Error _ => Inst E (Lookup GM v)

val EmptyEnv = (([], []),([],[]));

fun RenameTAux []      _ T = T
  | RenameTAux (v::vs) n T = let val w = "$"^makestring n
                                 val T = SubstT (VarT w) v T
                             in RenameTAux vs (n+1) T
                             end

fun RenameT T = let val FVL = ListFreeVarT T
                in RenameTAux FVL 0 T
                end

fun SubstTM S w (Case (M,N))    = Case (SubstTM S w M, SubstTM S w N)
  | SubstTM S w (Pair (M,N))    = Pair (SubstTM S w M, SubstTM S w N)
  | SubstTM S w (Abs ((v,T),M)) = Abs ((v, SubstT S w T), SubstTM S w M)
  | SubstTM S w (App (M,N,T))   = App (SubstTM S w M, SubstTM S w N,
                                       SubstT S w T)
  | SubstTM S w (Fold T)        = Fold (SubstT S w T)
  | SubstTM S w (Iter (T,M,N))  = Iter (SubstT S w T, SubstTM S w M,
                                        SubstTM S w N)
  | SubstTM S w (Unfold T)      = Unfold (SubstT S w T)
  | SubstTM S w (Gen (T,M,N))   = Gen (SubstT S w T, SubstTM S w M,
                                       SubstTM S w N)
  | SubstTM S w M               = M

fun RenameAux []      _ (M,T) = (M, T)
  | RenameAux (v::vs) n (M,T) = let val w = "$"^makestring n
                                    val M = SubstTM (VarT w) v M
                                    val T = SubstT (VarT w) v T
                                in RenameAux vs (n+1) (M, T)
                                end

fun Rename (M,T) = let val FVL = ListFreeVarTMT (M, T)
                   in RenameAux FVL 0 (M, T)
                   end;

fun Unify E T U = let val ET = AppE E T
                      val EU = AppE E U
                  in
                    case (ET, EU) of
                      (Dummy V,Dummy W) =>
                        if V=W
                        then E
                        else raise Error "Mismatched recursive types"
                    | (Dummy _,_) =>
                        raise Error "Mismatched recursive types"
                    | (_,Dummy _) =>
                        raise Error "Mismatched recursive types"
                    | (VarT v,VarT w) => UpdateE E v EU
                    | (VarT v,_) =>
                        if FreeVarT v EU
                        then raise Error (implode ["Type mismatch:\n ",
                                                   ShowType ET, "\n ",
                                                   ShowType EU])
                        else UpdateE E v EU
                    | (_,VarT v) =>
                        if FreeVarT v ET
                        then raise Error (implode ["Type mismatch:\n ",
                                                   ShowType ET, "\n ",
                                                   ShowType EU])
                        else UpdateE E v ET
                    | (ZeroT,ZeroT) => E
                    | (OneT,OneT) => E
                    | (SumT (V1,V2),SumT (W1,W2)) =>
                        Unify (Unify E V1 W1) V2 W2
                    | (ProdT (V1,V2),ProdT (W1,W2)) =>
                        Unify (Unify E V1 W1) V2 W2
                    | (ExptT (V1,V2),ExptT (W1,W2)) =>
                        Unify (Unify E V1 W1) V2 W2
                    | (MuT F,MuT G) => UnifyF E F G
                    | (NuT F,NuT G) => UnifyF E F G
                    | _ => raise Error (implode ["Type mismatch:\n ",
                                                 ShowType ET, "\n ",
                                                 ShowType EU])
                  end

and UnifyF (G,L) (v,T) (w,U) =
      let val E = BindLT (G, L) v (Dummy T)
          val E = BindLT E w (Dummy T)
          val (G,_) = Unify E T U
      in (G,L)
      end;

fun GTT (SumT (T,U))  = NotT (NotT (SumT (GTT T, GTT U)))
  | GTT (ProdT (T,U)) = ProdT (GTT T, GTT U)
  | GTT (ExptT (T,U)) = ExptT (GTT T, GTT U)
  | GTT (MuT (v,T))   = MuT (v, GTT T)
  | GTT (NuT (v,T))   = NotT (MuT (v, NotT (SubstT (NotT (VarT v))
                                                   v (GTT T))))
  | GTT T             = T

fun isTrivial (v,VarT w)      = v=w
  | isTrivial (v,ProdT (T,U)) = isTrivial (v,T) orelse isTrivial (v,U)
  | isTrivial (v,ExptT (_,T)) = isTrivial (v,T)
  | isTrivial (v,MuT (w,T))   = isTrivial (v,T) orelse isTrivial (w,T)
  | isTrivial _               = false

fun Eps (VarT v) = raise Error "Type not closed"
  | Eps ZeroT = Abs (("y", NotT (NotT ZeroT)), App (Var "y",
                                                    Id ZeroT,
                                                    NotT ZeroT))
  | Eps OneT = Abs (("y", NotT (NotT OneT)), Diamond)
  | Eps (SumT (T,U)) =
      let val TG = GTT T
          val UG = GTT U
          val SG = GTT (SumT (T, U))
      in Abs (("y", NotT (NotT SG)),
              Abs (("z", NotT (SumT (TG, UG))),
                   App (Var "y",
                        Abs (("k", SG), App (Var "k",
                                             Var "z",
                                             NotT (SumT (TG, UG)))),
                        NotT SG)))
      end
  | Eps (ProdT (T,U)) =
      let val TG = GTT T
          val UG = GTT U
          val PG = ProdT (TG, UG)
      in Abs (("y", NotT (NotT PG)),
              Pair (App (Eps T,
                         Abs (("z", NotT TG),
                              App (Var "y",
                                   Abs (("p", PG),
                                        App (Var "z",
                                             App (Pi1, Var "p", PG),
                                             TG)),
                                   NotT PG)),
                         NotT (NotT TG)),
                    App (Eps U,
                         Abs (("z", NotT UG),
                              App (Var "y",
                                   Abs (("p", PG),
                                        App (Var "z",
                                             App (Pi2, Var "p", PG),
                                             UG)),
                                   NotT PG)),
                         NotT (NotT UG))))
      end
  | Eps (ExptT (T,U)) =
      let val TG = GTT T
          val UG = GTT U
          val EG = ExptT (TG, UG)
      in Abs (("y", NotT (NotT EG)),
              Abs (("a", TG),
                   App (Eps U,
                        Abs (("z", NotT UG),
                             App (Var "y",
                                  Abs (("f", EG),
                                       App (Var "z",
                                            App (Var "f", Var "a", TG),
                                            UG)),
                                  NotT EG)),
                        NotT (NotT UG))))
      end
  | Eps (MuT (v,T)) =
      let val TG = GTT T
          val MG = MuT (v, TG)
          val A = if isTrivial (v, T)
                  then raise Error "Trivial inductive type"
                  else SubstT (MuT (v, T)) v T
          val AG = GTT A
      in Abs (("y", NotT (NotT MG)),
              App (Fold MG,
                   App (Eps A,
                        Abs (("z", NotT AG),
                             App (Var "y",
                                  Abs (("m", MG),
                                       App (Var "z",
                                            App (Unfold MG,
                                                 Var "m",
                                                 MG),
                                            AG)),
                                  NotT MG)),
                        NotT (NotT AG)),
                   AG))
      end
  | Eps (NuT (v,T)) =
      let val MG = MuT (v, NotT (SubstT (NotT (VarT v)) v (GTT T)))
          val NG = NotT MG
      in Abs (("y", NotT (NotT NG)),
              Abs (("z", MG),
                   App (Var "y",
                        Abs (("k", NG),
                             App (Var "k", Var "z", MG)),
                        NotT NG)))
      end
  | Eps (Dummy _) = raise Error "Internal error"

fun GTM (Box, ExptT (_,V)) =
      let val VG = GTT V
      in Abs (("p", ZeroT), App (Eps V,
                                 Abs (("z", NotT VG), Var "p"),
                                 NotT (NotT VG)))
      end
  | GTM (In1, ExptT (T, SumT (_,U))) =
      let val TG = GTT T
          val UG = GTT U
      in Abs (("m", TG), Abs (("k", NotT (SumT (TG, UG))),
                              App (Var "k",
                                   App (In1, Var "m", TG),
                                   SumT (TG, UG))))
      end
  | GTM (In2, ExptT (U, SumT (T,_))) =
      let val TG = GTT T
          val UG = GTT U
      in Abs (("m", UG), Abs (("k", NotT (SumT (TG, UG))),
                              App (Var "k",
                                   App (In2, Var "m", UG),
                                   SumT (TG, UG))))
      end
  | GTM (Case (M,N), ExptT (SumT (T,U),V)) =
      let val TG = GTT T
          val UG = GTT U
          val VG = GTT V
          val SG = GTT (SumT (T, U))
      in Abs (("p", SG),
              App (Eps V,
                   Abs (("z", NotT VG),
                        App (Var "p",
                             Abs (("q", SumT (TG, UG)),
                                  App (Var "z",
                                       App (Case (GTM (M, ExptT (T, V)),
                                                  GTM (N, ExptT (U, V))),
                                            Var "q",
                                            SumT (TG, UG)),
                                       VG)),
                             NotT (SumT (TG, UG)))),
                   NotT (NotT VG)))
      end
  | GTM (Pair (M,N),ProdT (T,U)) = Pair (GTM (M, T), GTM (N, U))
  | GTM (Abs ((v,T),M),ExptT(_,U)) = Abs ((v, GTT T), GTM (M, U))
  | GTM (App (M,N,T),U) = App (GTM (M, ExptT (T, U)), GTM (N, T), GTT T)
  | GTM (Fold (T as MuT F),_) = Fold (GTT T)
  | GTM (Iter (T as MuT F,_,M),ExptT (_,U)) =
      IterT (GTT T) (GTM (M, ExptT (FuncT (F, U), U))) (GTT U)
  | GTM (Unfold (T as MuT F),_) = Unfold (GTT T)
  | GTM (Unfold (T as NuT (v,U)),_) =
      let val MG = MuT (v, NotT (SubstT (NotT (VarT v)) v (GTT U)))
          val A = SubstT T v U
          val AG = GTT A
      in Abs (("m", NotT MG),
              App (Eps A,
                   Abs (("z", NotT AG),
                        App (Var "m",
                             App (Fold MG, Var "z", NotT AG),
                             MG)),
                   NotT (NotT AG)))
      end
  | GTM (Gen (NuT (v,U),_,M),ExptT (T,_)) =
      let val AG = GTT U
          val MG = MuT (v, NotT (SubstT (NotT (VarT v)) v AG))
          val TG = GTT T
          val ADTG = SubstT (NotT (NotT TG)) v AG
          val ATG = SubstT TG v AG
          val GM = GTM (M, ExptT (T, FuncT ((v, U), T)))
          val Aeta = SubstMT (Abs (("b", TG),
                                   Abs (("k", NotT TG),
                                        App (Var "k", Var "b", TG))),
                              ExptT (TG, NotT (NotT TG))) v AG
      in Abs (("n", TG),
              Abs (("m", MG),
                   App (App (IterT MG
                                   (Abs (("w", NotT ADTG),
                                         Abs (("x", TG),
                                              App (Var "w",
                                                   App (Aeta,
                                                        App (GM,
                                                             Var "x",
                                                             TG),
                                                        ATG),
                                                   ADTG))))
                                   (NotT TG),
                             Var "m",
                             MG),
                        Var "n",
                        TG)))
      end
  | GTM (Fold (NuT (v,U)),_) =
      let val AG = GTT U
          val MG = MuT (v, NotT (SubstT (NotT (VarT v)) v AG))
          val ATG = SubstT (NotT MG) v AG
      in Abs (("m", ATG),
              Abs (("x", MG),
                   App (App (Unfold MG, Var "x", MG), Var "m", ATG)))
      end
  | GTM (M, T) = M;

fun ATT ZeroT         = NatT
  | ATT (SumT (T,U))  = SumT (ATT T, ATT U)
  | ATT (ProdT (T,U)) = ProdT (ATT T, ATT U)
  | ATT (ExptT (T,U)) = ExptT (ATT T, ATT U)
  | ATT (MuT (v,T))   = MuT (v, ATT T)
  | ATT (NuT (v,T))   = NuT (v, ATT T)
  | ATT T             = T

fun ATM (Case (M,N))    = Case (ATM M, ATM N)
  | ATM (Pair (M,N))    = Pair (ATM M, ATM N)
  | ATM (Abs ((v,T),M)) = Abs ((v, ATT T), ATM M)
  | ATM (App (M,N,T))   = App (ATM M, ATM N, ATT T)
  | ATM (Fold T)        = Fold (ATT T)
  | ATM (Iter (T,M,N))  = Iter (ATT T, ATM M, ATM N)
  | ATM (Unfold T)      = Unfold (ATT T)
  | ATM (Gen (T,M,N))   = Gen (ATT T, ATM M, ATM N)
  | ATM Box             = raise Error "Can't A-translate []"
  | ATM M               = M;

fun TCheck G (Var v, T) =
      let val U = Lookup G v
      in
        if T=U
        then ()
        else raise Error ("Var "^v^":\n "^(ShowType T)^"\n "^(ShowType U))
      end
  | TCheck G (Box,ExptT (ZeroT,_)) = ()
  | TCheck G (Diamond,OneT) = ()
  | TCheck G (In1,T as ExptT (U,SumT (V,_))) =
      if U=V
      then ()
      else raise Error ("In1:\n "^(ShowType T))
  | TCheck G (In2,T as ExptT (U,SumT (_,V))) =
      if U=V
      then ()
      else raise Error ("In2:\n "^(ShowType T))
  | TCheck G (Case (M,N),ExptT(SumT(T,U),V)) =
      (TCheck G (M, ExptT (T, V));
       TCheck G (N, ExptT (U, V)))
  | TCheck G (Pi1,T as ExptT (ProdT (U,_),V)) =
      if U=V
      then ()
      else raise Error ("Pi1:\n "^(ShowType T))
  | TCheck G (Pi2,T as ExptT (ProdT (_,U),V)) =
      if U=V
      then ()
      else raise Error ("Pi2:\n "^(ShowType T))
  | TCheck G (Pair (M,N),ProdT (T,U)) =
      (TCheck G (M, T);
       TCheck G (N, U))
  | TCheck G (Abs ((v,T),M),ExptT (U,V)) =
      if T=U
      then TCheck (Bind G v T) (M, V)
      else raise Error ("Abs "^v^":\n "^(ShowType T)^"\n "^(ShowType U))
  | TCheck G (App (M,N,T),U) =
      (TCheck G (M, ExptT (T, U));
       TCheck G (N, T))
  | TCheck G (Fold (T as MuT F),ExptT (U,V)) =
      if T=V andalso U=FuncT (F,T)
      then ()
      else raise Error ("Fold "^(ShowType T)^":\n "^
                                (ShowType (ExptT (U, V)))^"\n "^
                                (ShowType (FuncT (F, T))))
  | TCheck G (Iter (MuT F,M,N),T as ExptT (U,V)) =
      if U=MuT F
      then (TCheck G (M, ExptT (T, ExptT (FuncT (F, U), FuncT (F, V))));
            TCheck G (N, ExptT (FuncT (F, V), V)))
      else raise Error ("Iter "^(ShowType (MuT F))^":\n "^(ShowType T))
  | TCheck G (Unfold (T as MuT F),ExptT (U,V)) =
      if T=U andalso V=FuncT (F,T)
      then ()
      else raise Error ("Unfold "^(ShowType T)^":\n "^
                                  (ShowType (ExptT (U, V)))^"\n "^
                                  (ShowType (FuncT (F, T))))
  | TCheck G (Fold (T as NuT F),ExptT (U,V)) =
      if T=V andalso U=FuncT (F,T)
      then ()
      else raise Error ("Fold "^(ShowType T)^":\n "^
                                (ShowType (ExptT (U, V)))^"\n "^
                                (ShowType (FuncT (F, T))))
  | TCheck G (Gen (NuT F,M,N),T as ExptT (U,V)) =
      if V=NuT F
      then (TCheck G (M, ExptT (T, ExptT (FuncT (F, U), FuncT (F, V))));
            TCheck G (N, ExptT (U, FuncT (F, U))))
      else raise Error ("Gen "^(ShowType (NuT F))^":\n "^(ShowType T))
  | TCheck G (Unfold (T as NuT F),ExptT (U,V)) =
      if T=U andalso V=FuncT (F,T)
      then ()
      else raise Error ("Unfold "^(ShowType T)^":\n "^
                                  (ShowType (ExptT (U, V)))^"\n "^
                                  (ShowType (FuncT (F, T))))
  | TCheck G (M,T) = raise Error ("Can't match "^(ShowTerm M)^
                                  " with "^(ShowType T));

(* Combinator parsing functions, adapted from Paulson (and influenced by
 * Hutton, Wadler, and others).  The complications of the Lexstream type
 * should allow arbitrary backtracking, even though the input_line
 * function is impure.
 *)

exception PError of string

infix 3 &&
infix 2 >>
infix 2 >:
infix 1 ||

fun (p1 && p2) ST = let val (r1,ST) = p1 ST
                        val (r2,ST) = p2 ST
                    in ((r1,r2),ST)
                    end

fun (p1 || p2) ST = p1 ST
                    handle PError _ => p2 ST

fun (p >> f) ST = let val (r,ST) = p ST
                  in (f r, ST)
                  end

fun (p >: f) ST = f (p ST)

fun epsilon ST = ([], ST)

fun ** p ST = (p && ** p >> op :: || epsilon) ST

fun `y (E,(x,ls)) = if x=y
                    then if x=EOS
                         then (x, (E, (x, ls)))
                         else (x, (E, getlex ls))
                    else raise PError (implode ["Expected ", ShowLex y,
                                                ", found ", ShowLex x])

(* n -- name of non-terminal, for error message
 * p -- a phrase parsing function
 * G -- global environment (types and terms)
 * L -- local environment, restored at end
 * S -- state of lexeme stream:  (current lexeme, lex stream handle)
 *)
fun NonTerm n ((G,L),S) p = let val (r,((G,_),S)) = p ((G,L),S)
                            in (r,((G,L),S))
                            end
                            handle PError _ =>
                              raise PError ("Unable to parse "^n)

fun foldright f (x,[])    = x
  | foldright f (x,y::ys) = f (x, foldright f (y, ys))

fun foldleft f (x,[])    = x
  | foldleft f (x,y::ys) = foldleft f (f (x, y), ys)

fun fst (x,_) = x

fun snd (_,x) = x

fun K x _ = x

fun idLookupT (E,(ID v,ls)) = let val T = LookupT E v
                              in (T, (E, getlex ls))
                              end
  | idLookupT (_,(x,_)) =
      raise PError (implode ["Expected identifier, found ", ShowLex x])

fun idBindLT (E,(ID v,ls)) = (v, (BindLT E v (VarT v), getlex ls))
  | idBindLT (_,(x,_)) =
      raise PError (implode ["Expected identifier, found ", ShowLex x])

fun id s (E,(ID v,ls)) =
      if s=v
      then ((), (E, getlex ls))
      else raise PError (implode ["Expected ", s, ", found ", v])
  | id s (_,(x,_)) =
      raise PError (implode ["Expected ", s, ", found ", ShowLex x])

fun PType ST = NonTerm "TYPE" ST (
       PSum && **(`ARO && PSum >> snd) >> foldright ExptT)

and PSum ST = NonTerm "SUM" ST (
       PProd && **(`SUM && PProd >> snd) >> foldleft SumT)

and PProd ST = NonTerm "PROD" ST (
       PAtomT && **(`PRD && PAtomT >> snd) >> foldleft ProdT)

and PAtomT ST = NonTerm "ATOMTYPE" ST (
       idLookupT
    || `MU && PTypeFunc               >> MuT o snd
    || `NU && PTypeFunc               >> NuT o snd
    || (`LBR && PTypeFunc >> snd) &&
       (`COM && PType >> snd) && `RBR >> FuncT o fst
    || `ZER                           >> K ZeroT
    || `ONE                           >> K OneT
    || `NOT && PAtomT                 >> NotT o snd
    || (`LPA && PType >> snd) && `RPA >> fst)

and PTypeFunc ST = NonTerm "TYPEFUNC" ST (
       (idBindLT && `DOT >> fst) && PType)

fun PCType ST = let val (T,(E,S)) = PType ST
                    val (E,T) = InstT E T
                in (T, (E, S))
                end

fun PCTypeFunc ST = let val (F,(E,S)) = PTypeFunc ST
                        val (E,F) = InstF E F
                    in (F, (E, S))
                    end;

fun NewVarST (E,S) = let val (E,T) = NewVarE E in (T, (E, S)) end

fun idLookupM (E,(ID v,ls)) =
      let val (E,(M,T)) = LookupM E v
      in ((M, T), (E, getlex ls))
      end
  | idLookupM (_,(x,_)) =
      raise PError (implode ["Expected identifier, found ", ShowLex x])

fun idBindLM (E,(ID v,ls)) =
      let val (T,(E,S)) = (`COL && PCType >> snd || NewVarST) (E, getlex ls)
      in ((v, T), (BindLM E v (Var v, T), S))
      end
  | idBindLM (_,(x,_)) =
      raise PError (implode ["Expected identifier, found ", ShowLex x])

fun idLet (E,(ID v,ls)) =
      let val (T,(E,S)) = (`COL && PCType >> snd || NewVarST) (E, getlex ls)
      in ((v, T), (E, S))
      end
  | idLet (_,(x,_)) =
      raise PError (implode ["Expected identifier, found ", ShowLex x])

fun idLetT (E,(ID v,ls)) = (v, (E, getlex ls))
  | idLetT (_,(x,_)) =
      raise PError (implode ["Expected identifier, found ", ShowLex x])

fun num (E,(NUM n,ls)) = (NUM n, (E, getlex ls))
  | num (_,(x,_)) =
      raise PError (implode ["Expected number, found ", ShowLex x])

fun str (E,(STR s,ls)) = (s, (E, getlex ls))
  | str (_,(x,_)) =
      raise PError (implode ["Expected string, found ", ShowLex x])

fun CheckAbs ((v,T),(M,U)) = (Abs ((v, T), M), ExptT (T, U))

fun CheckApp ((x,l),(E,S)) =
      foldleft (fn (((M,T),(E,S)),(N,U)) =>
                  let val (E,V) = NewVarE E
                      val E = Unify E T (ExptT(U, V))
                  in ((App (M, N, U), V), (E, S))
                  end)
               ((x, (E, S)), l)

fun CheckFunc ((F,(M,T)),(E,S)) =
      let val (E,U) = NewVarE E
          val (E,V) = NewVarE E
          val E = Unify E T (ExptT (U, V))
      in ((AppF F (M, ExptT (U, V)), ExptT (FuncT (F, U), FuncT (F, V))),
          (E, S))
      end

fun CheckBox (_,(E,S)) = let val (E,T) = NewVarE E
                         in ((Box, ExptT (ZeroT, T)), (E, S))
                         end

fun CheckIn1 (_,(E,S)) = let val (E,T) = NewVarE E
                             val (E,U) = NewVarE E
                         in ((In1, ExptT(T, SumT (T, U))), (E, S))
                         end

fun CheckIn2 (_,(E,S)) = let val (E,T) = NewVarE E
                             val (E,U) = NewVarE E
                         in ((In2, ExptT(U, SumT (T, U))), (E, S))
                         end

fun CheckCase (((M,T),(N,U)),(E,S)) =
      let val (E,V) = NewVarE E
          val (E,W) = NewVarE E
          val (E,X) = NewVarE E
          val E = Unify E T (ExptT (V, X))
          val E = Unify E U (ExptT (W, X))
      in ((Case (M, N), ExptT (SumT (V, W), X)), (E, S))
      end

fun CheckPi1 (_,(E,S)) = let val (E,T) = NewVarE E
                             val (E,U) = NewVarE E
                         in ((Pi1, ExptT(ProdT (T, U), T)), (E, S))
                         end

fun CheckPi2 (_,(E,S)) = let val (E,T) = NewVarE E
                             val (E,U) = NewVarE E
                         in ((Pi2, ExptT(ProdT (T, U), U)), (E, S))
                         end

fun CheckPair ((M,T),(N,U)) = (Pair (M, N), ProdT (T, U))

fun CheckFold T =
      (Fold T, case T of
                 MuT F => ExptT (FuncT (F, T), T)
               | NuT F => ExptT (FuncT (F, T), T)
               | _     => raise Error "Illegal type for 'fold'")

fun CheckIter ((T,(M,U)),(E,S)) =
      (case T of
         MuT F => let val (E,V) = NewVarE E
                      val E = Unify E U (ExptT (FuncT (F, V), V))
                  in ((IterT T M V, ExptT (T, V)), (E, S))
                  end
       | _     => raise Error "Illegal type for iteration")

fun CheckUnfold T =
      (Unfold T, case T of
                   MuT F => ExptT (T, FuncT (F, T))
                 | NuT F => ExptT (T, FuncT (F, T))
                 | _     => raise Error "Illegal type for 'unfold'")

fun CheckGen ((T,(M,U)),(E,S)) =
      (case T of
         NuT F => let val (E,V) = NewVarE E
                      val E = Unify E U (ExptT (V, FuncT (F, V)))
                  in ((GenT T M V, ExptT (V, T)), (E, S))
                  end
       | _     => raise Error "Illegal type for coiteration");

fun MakeNat 0 = Zero
  | MakeNat n = Succ (MakeNat (n-1))

fun CheckNat ZER = (Zero, NatT)
  | CheckNat ONE = (Succ Zero, NatT)
  | CheckNat (NUM n) = (MakeNat n, NatT)
  | CheckNat _ = raise Error "Internal error in CheckNat";

fun PTerm ST = NonTerm "TERM" ST (
       PAtom && ** PAtom                     >: CheckApp)

and PAtom ST = NonTerm "ATOMTERM" ST (
       idLookupM
    || (`LBR && PCTypeFunc >> snd) &&
       (`COM && PTerm >> snd) && `RBR >> fst >: CheckFunc
    || `BOX                                  >: CheckBox
    || `DIA                                  >> K (Diamond, OneT)
    || `IN1                                  >: CheckIn1
    || `IN2                                  >: CheckIn2
    || (`LSB && PTerm >> snd) &&
       (`COM && PTerm >> snd) && `RSB >> fst >: CheckCase
    || `PI1                                  >: CheckPi1
    || `PI2                                  >: CheckPi2
    || (`LAB && PTerm >> snd) &&
       (`COM && PTerm >> snd) && `RAB >> fst >> CheckPair
    || (`LAM && idBindLM >> snd) &&
       (`DOT && PTerm >> snd)                >> CheckAbs
    || `FLD && PCType                 >> snd >> CheckFold
    || (`LIB && PCType >> snd) &&
       (`COM && PTerm >> snd) && `RIB >> fst >: CheckIter
    || `UFD && PCType                 >> snd >> CheckUnfold
    || (`LGB && PCType >> snd) &&
       (`COM && PTerm >> snd) && `RGB >> fst >: CheckGen
    || `ZER                                  >> CheckNat
    || `ONE                                  >> CheckNat
    || num                                   >> CheckNat
    || (`LPA && PTerm >> snd) && `RPA >> fst);

fun prompt () = (print "> "; flush_out std_out)

fun DoLetT ((v,T),(E,S)) = let val T = RenameT T
                           in
                             print (v: string);
                             print " = ";
                             print (ShowType T);
                             print "\n";
                             prompt ();
                             (BindGT EmptyEnv v T, (E, S))
                           end

fun DoHelp _ = (print "val <id> = <term> ;\n";
                print "type <id> = <type> ;\n";
                print "<term> ;\n";
                print "gtrans <term> ;\n";
                print "atrans <term> ;\n";
                print "tcheck <term> ;\n";
                print "use \"<file>\" ;\n";
                print "help;\nquit;\n";
                prompt ();
                EmptyEnv)

fun DoUse (f,(E,(x,ls))) =
      (EmptyEnv, (E, (x, uses (open_in f) ls)))

exception Exit

fun DoQuit _ = raise Exit

fun DoLetAux ((v,(M,T)),ST) = let val (M,T) = Rename (M, T)
                              in
                                print (v: string);
                                print " = ";
                                print (ShowTermT (M, T));
                                print " : ";
                                print (ShowType T);
                                print "\n";
                                prompt ();
                                (BindGM EmptyEnv v (M, T), ST)
                              end

fun DoLet (((v,V),(M,T)),(E,S)) =
      let val E = Unify E V T
          val T = AppE E T
          val M = AppEMT E (M, T)
      in DoLetAux ((v, cnorm (M, T)), (E, S))
      end

fun DoNorm ((M,T),(E,S)) =
      let val T = AppE E T
          val M = AppEMT E (M, T)
      in DoLetAux (("it", cnorm (M, T)), (E, S))
      end

fun DoGTrans ((M,T),(E,S)) =
      let val T = AppE E T
          val M = GTM (AppEMT E (M, T), T)
          val T = GTT T
      in DoLetAux (("it", (M, T)), (E, S))
      end

fun DoATrans ((M,T),(E,S)) =
      let val T = AppE E T
          val M = ATM (AppEMT E (M, T))
          val T = ATT T
      in DoLetAux (("it", (M, T)), (E, S))
      end

fun DoTCheck ((M,T),(E,S)) = let val T = AppE E T
                                 val M = AppEMT E (M, T)
                             in
                               TCheck [] (M, T);
                               print (ShowTermT (M, T));
                               print " : ";
                               print (ShowType T);
                               print "\n";
                               prompt ();
                               (EmptyEnv, (E, S))
                             end

fun Command ST = NonTerm "COMMAND" ST (
       (id "val" && idLet >> snd) &&
       (`EQU && PTerm >> snd)                      >: DoLet
    || (id "type" && idLetT >> snd) &&
       (`EQU && PType >> snd)                      >: DoLetT
    || (id "use" && str >> snd)                    >: DoUse
    || id "help"                                   >> DoHelp
    || id "quit"                                   >> DoQuit
    || `EOS                                        >> DoQuit
    || `SEM                                        >> K EmptyEnv
    || (id "gtrans" && PTerm >> snd)               >: DoGTrans
    || (id "atrans" && PTerm >> snd)               >: DoATrans
    || (id "tcheck" && PTerm >> snd)               >: DoTCheck
    || PTerm                                       >: DoNorm);

fun TopLevel E (x,ls) =
      let val (E',(_,S)) = Command (E,(x,ls))
          val E = CombineE E E'
      in TopLevel E S
      end
      handle Error s  => (outputc std_err (s^"\n");
                          prompt ();
                          inputc std_in (can_input std_in);
                          TopLevel E (getlex (uses std_in nulls)))
           | PError s => (outputc std_err (s^"\n");
                          prompt ();
                          inputc std_in (can_input std_in);
                          TopLevel E (getlex (uses std_in nulls)))
           | Io s     => (outputc std_err (s^"\n");
                          prompt ();
                          inputc std_in (can_input std_in);
                          TopLevel E (getlex (uses std_in nulls)))
           | Exit     => ()

fun main (args,env) =
      (prompt ();
       TopLevel EmptyEnv (getlex (uses std_in nulls)));

exportFn ("lemon", main);
