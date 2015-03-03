#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  Pkg.describe "resultv" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/resultv";
    Pkg.lib ~exts:Exts.interface_opt "src/resultv_infix";
    Pkg.lib ~exts:Exts.library "src/resultv_top";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
