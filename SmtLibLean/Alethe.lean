import SmtLibLean.SmtLib

namespace SmtLibLean

namespace Alethe
open SmtLib

def Clause := List Term
inductive CtxAssignment
| var (v : SortedVar)
| assign (s : Symbol) (t : Term)
inductive StepArg
| arg (s : Symbol)
| app (s : Symbol) (t : Term)

inductive ProofCommand
| assume (name : Symbol) (term : Term)
| step (name : Symbol) (clause : Clause)
    (rule : Symbol)
    (premises : Option (List Symbol))
    (context : Option (List CtxAssignment))
    (attributes : List Attribute)
| anchor (step : Symbol)
    (args : Option (List StepArg))

structure Proof where
  step : Array ProofCommand
deriving Inhabited

def parse (s : String) : Except String Proof := do
  let sexps!{ unsat {rest}* } ← Sexp.parse s
    | throw "first line of proof is not `unsat`"

  let steps ← rest.foldlM (init := #[]) (fun acc s => do
      match s with
      | sexp!{ (assume {name} {clause}) } =>
        return acc.push (.assume name clause)
      | sexp!{ (step {name} (cl {clauses}*) :rule {rule} {rest}*) } =>
        let (premises, rest) ←
          match rest with
          | sexps!{ :premises ( {prems}* ) {rest}* } =>
            pure (some prems, rest)
          | sexps!{ :premises {_}* } =>
            throw "premises should be followed by a sexp-list of premises"
          | _ => pure (none, rest)
        let (ctx, rest) ←
          match rest with
          | sexps!{ :args ( {assn}* ) {rest}* } =>
            pure (some assn, rest)
          | sexps!{ :args {_}* } =>
            throw "args should be followed by sexp-list of arguments"
          | _ => pure (none, rest)
        return acc.push (.step name clauses rule premises none rest)
      | sexp!{ (anchor :step {step} {rest}*) } =>
        return acc.push (.anchor step none)
      | _ => throw s!"failed to parse step:\n{s}"
    )

  return ⟨steps⟩

#eval do
  let file ← IO.FS.readFile "smt-skeleton-examples/linear_arith.alethe"
  let res ← IO.ofExcept <| parse file
