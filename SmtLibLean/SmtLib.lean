import SmtLibLean.Sexp

namespace SmtLibLean.SmtLib

/-- SMT-LIB <term> -/
def Term := Sexp
/-- SMT-LIB symbol -/
def Symbol := Sexp
/-- SMT-LIB attribute -/
def Attribute := Sexp
/-- SMT-LIB sorted var -/
structure SortedVar where
  name : Symbol
  sort : Sexp


private def hexdigits : Array Char :=
  #[ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' ]

private def enhexByte (x : UInt8) : String :=
  ⟨[hexdigits.get! $ UInt8.toNat $ (x.land 0xf0).shiftRight 4,
    hexdigits.get! $ UInt8.toNat $ x.land 0xf ]⟩

/-- Convert a little-endian (LSB first) list of bytes to hexadecimal. -/
private def enhexLE : List UInt8 → String
  | [] => ""
  | b::bs => enhexLE bs ++ enhexByte b

/-- Converts a number `n` to its hexadecimal SMT-LIB representation as a `nBits`-bit vector.
For example `toBVConst 32 0xf == "#x0000000f"`. -/
def toBVConst (nBits : Nat) (n : Nat) : String :=
  assert! nBits % 8 == 0
  let nBytes := nBits/8
  let bytes := List.range nBytes |>.map fun i => UInt8.ofNat ((n >>> (i*8)) &&& 0xff)
  "#x" ++ enhexLE bytes

open Std (AssocList)

/-- Extracts constants assigned in a model returned from an SMT solver.
The model is expected to be a single s-expression representing a list,
with constant expressions represented by `(define-fun <name> () <type> <body>)`. -/
def decodeModelConsts : Sexp → AssocList String Sexp
  | Sexp.expr ss =>
    ss.foldl (init := AssocList.nil) fun
      | acc, sexp!{(define-fun {Sexp.atom x} () {_} {body})} =>
        acc.cons x body
      | acc, _ => acc
  | _ => AssocList.nil

/-- Evaluates an SMT-LIB constant numeral such as `0` or `#b01` or `#x02`. -/
def evalNumConst : Sexp → Option Nat
  | Sexp.atom s =>
    let s' :=
      if s.startsWith "#b" then "0" ++ s.drop 1
      else if s.startsWith "#x" then "0" ++ s.drop 1
      else s
    Lean.Syntax.decodeNatLitVal? s'
  | Sexp.expr _ => none
