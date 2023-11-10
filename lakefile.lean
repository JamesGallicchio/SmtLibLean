import Lake
open Lake DSL

package «smt-lib-lean» {
  -- add package configuration options here
}

lean_lib «SmtLibLean» {
  -- add library configuration options here
}

@[default_target]
lean_exe «smt-lib-lean» {
  root := `Main
}

require std from git "https://github.com/leanprover/std4" @ "main"
