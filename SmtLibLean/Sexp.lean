/- Copied from Logic and Mechanized Reasoning:
https://github.com/avigad/lamr/
-/

import Init
import Lean.Parser
import Lean.PrettyPrinter
import Std.Data.AssocList

namespace SmtLibLean

namespace Sexp

open Lean Parser PrettyPrinter

/-- Like `ident` but with no splitting on dots and accepts anything that's not whitespace
or parentheses. So e.g. `<=` works. -/
def generalIdent : Parser :=
  withAntiquot (mkAntiquot "generalIdent" `generalIdent) {
    fn := fun c s =>
      let startPos := s.pos
      let s := takeWhile1Fn (fun c => !("(){}[].".contains c) ∧ !c.isWhitespace) "expected generalized identifier" c s
      mkNodeToken `generalIdent startPos c s }

def getGeneralId (s : TSyntax `generalIdent) : String :=
  match s with
  | ⟨.node _ `generalIdent args⟩ => args[0]!.getAtomVal
  | s => panic! s!"unexpected syntax '{s}'"

@[combinator_formatter generalIdent] def generalIdent.formatter : Formatter := pure ()
@[combinator_parenthesizer generalIdent] def generalIdent.parenthesizer : Parenthesizer := pure ()

end Sexp

inductive Sexp where
  | atom : String → Sexp
  | expr : List Sexp → Sexp
  deriving Repr, BEq, Inhabited

namespace Sexp

instance : Coe String Sexp :=
  ⟨Sexp.atom⟩

declare_syntax_cat sexp
declare_syntax_cat sexps

syntax generalIdent : sexp
syntax "{" term "}" : sexp
syntax "(" sexps* ")" : sexp

syntax sexp : sexps
syntax "{" term "}*" : sexps

syntax "sexp!{" sexp "}" : term
syntax "sexps!{" sexps* "}" : term

macro_rules
  | `(sexp!{ $a:generalIdent }) => `(Sexp.atom $(Lean.quote (getGeneralId a)))
  | `(sexp!{ { $t:term } })     => `(($t : Sexp))
  | `(sexp!{ ( $ss:sexps* ) })  => `(Sexp.expr sexps!{ $ss* })

macro_rules
  | `(sexps!{ })                        => `([])
  | `(sexps!{ $s:sexp $ss:sexps* })     => `(sexp!{ $s } :: sexps!{ $ss* })
  -- special case so you can pattern match on "the rest of the sexp" with {}* syntax
  | `(sexps!{ { $t:term }* })           => `(($t : List Sexp))
  | `(sexps!{ { $t:term }* $ss:sexps* })=> `(($t : List Sexp) ++ sexps!{ $ss* })

partial def serialize : Sexp → String
  | atom s  =>
    if s.any (fun c => "[]{}()".contains c || c.isWhitespace) then
      s.quote
    else s
  | expr ss => s!"({" ".intercalate <| ss.map serialize})"

partial def serializeMany (ss : List Sexp) : String :=
  ss.map serialize |> "\n".intercalate

instance : ToString Sexp :=
  ⟨serialize⟩

instance : Repr Sexp where
  reprPrec s _ := s!"sexp!\{ {toString s} }"

partial def parse (s : String) : Except String (List Sexp) :=
  let tks := tokenize #[] s.toSubstring
  parseMany #[] tks.toList |>.map Prod.fst |>.map Array.toList
where
  tokenize (stk : Array Substring) (s : Substring) : Array Substring :=
    if s.isEmpty then stk
    else
      let c := s.front
      if c == ')' || c == '(' then
        tokenize (stk.push <| s.take 1) (s.drop 1)
      else if c.isWhitespace then tokenize stk (s.drop 1)
      else
        let tk := s.takeWhile fun c => !c.isWhitespace && c != '(' && c != ')'
        if tk.bsize > 0 then tokenize (stk.push tk) (s.extract ⟨tk.bsize⟩ ⟨s.bsize⟩)
        else unreachable!

  parseOne : List Substring → Except String (Sexp × List Substring)
    | tk :: tks => do
      if tk.front == ')' then
        throw "mismatched parentheses"
      if tk.front == '(' then
        let (ss, tks) ← parseMany #[] tks
        return (expr ss.toList, tks)
      else
        return  (atom tk.toString, tks)
    | [] => throw "expected input, got none"

  parseMany (stk : Array Sexp) : List Substring → Except String (Array Sexp × List Substring)
    | tk :: tks => do
      if tk.front == ')' then .ok (stk, tks)
      else
        let (e, tks) ← parseOne (tk :: tks)
        parseMany (stk.push e) tks
    | [] => .ok (stk, [])


class ToSexp (A : Type) where
  toSexp : A → Sexp

class FromSexp (A : Type) where
  fromSexp : Sexp → Except String A

class Sexpable (A : Type) extends ToSexp A, FromSexp A
