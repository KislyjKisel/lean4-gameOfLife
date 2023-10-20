import Lake
open Lake DSL

require raylib from git
  "https://github.com/KislyjKisel/Raylib.lean" @ "main"

package GameOfLife

@[default_target]
lean_exe gameoflife where
  root := `Main
  moreLinkArgs := #["-Llake-packages/raylib/raylib/build/raylib", "-lraylib"]
  supportInterpreter := false
