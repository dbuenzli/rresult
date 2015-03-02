#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let () =
  let builder = `Other ("ocamlbuild -classic-display", "_build") in
  Pkg.describe "result" ~builder [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/result";
    Pkg.lib ~exts:Exts.interface_opt "src/result_infix";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
