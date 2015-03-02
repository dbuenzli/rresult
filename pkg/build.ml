#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "result" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/result";
    Pkg.lib ~exts:Exts.interface_opt "src/result_infix";
    Pkg.lib ~exts:Exts.library "src/result_top";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
