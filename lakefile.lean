import Lake
open Lake DSL

package "LeanStudy" where
  -- add package configuration options here

lean_lib «LeanStudy» where
  -- add library configuration options here

@[default_target]
lean_exe "leanstudy" where
  root := `Main
