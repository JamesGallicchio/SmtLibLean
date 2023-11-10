import SmtLibLean.Smt

namespace SmtLibLean

private def argsCvc4 : IO.Process.SpawnArgs := {
  cmd := "cvc4"
  args := #["--lang", "smt"] }

private def argsCvc5 : IO.Process.SpawnArgs := {
  cmd := "cvc5"
  args := #["--lang", "smt"] }

private def argsZ3 : IO.Process.SpawnArgs := {
  cmd := "z3"
  args := #["-smt2"] }

private def argsBoolector : IO.Process.SpawnArgs := {
  cmd := "boolector"
  args := #["--smt2"] }

-- Same as IO.Process.run, but does not require exitcode = 0
private def run' (args : IO.Process.SpawnArgs) : IO String := do
  let out ← IO.Process.output args
  pure out.stdout

/-- Executes the solver with the provided list of commands in SMT-LIB s-expression format.
Returns the solver output as s-expressions. -/
private def callSolver (args : IO.Process.SpawnArgs) (commands : List Sexp) (verbose : Bool := false)
    : IO (List Sexp) := do
  let cmdStr := Sexp.serializeMany commands
  if verbose then
    IO.println "Sending SMT-LIB problem:"
    IO.println cmdStr
  IO.FS.writeFile "LAMR/bin/temp.smt" cmdStr
  let out ← run' args
  if verbose then
    IO.println "\nSolver replied:"
    IO.println out
  let out ← IO.ofExcept (Sexp.parse out)
  return out

def callCvc4 := @callSolver argsCvc4
def callCvc5 := @callSolver argsCvc5
def callZ3 := @callSolver argsZ3
def callBoolector := @callSolver argsBoolector
