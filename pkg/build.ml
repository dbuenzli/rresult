#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "rresult" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/rresult";
    Pkg.lib ~exts:Exts.library "src/rresult_top";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
