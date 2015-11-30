(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult

let log f = Format.printf (f ^^ "@.")

let raises_invalid_arg f x =
  try f x; raise Exit with
  | Invalid_argument _ -> ()
  | e -> assert false

let test_constructors () =
  log "Test R.{ok,error}";
  assert (R.ok 3 = Ok 3);
  assert (R.error `An_error = Error `An_error);
  ()

let test_reword_error () =
  log "Test R.reword_error";
  let reword `An_error = `Another_one in
  assert (R.reword_error reword (Ok 3) = Ok 3);
  assert (R.reword_error reword (Error `An_error) = (Error `Another_one));
  ()

let test_gets () =
  log "Test R.get_{ok,error}";
  assert (R.get_ok (Ok 2) = 2);
  assert (R.get_error (Error 3) = 3);
  raises_invalid_arg R.get_ok (Error 3);
  raises_invalid_arg R.get_error (Ok 2);
  ()

let test_bind () =
  log "Test R.bind";
  assert (R.bind (Ok 3) (fun v -> Error (string_of_int v)) = Error "3");
  assert (R.bind (Ok 3) (fun v -> Ok (string_of_int v)) = Ok "3");
  assert (R.bind (Error 1) (fun v -> Ok (string_of_int v)) = Error 1);
  ()

let test_map () =
  log "Test R.map";
  assert (R.map (fun v -> string_of_int v) (Ok 2) = Ok "2");
  assert (R.map (fun v -> string_of_int v) (Error 2) = Error 2);
  ()

let test_join () =
  log "Test R.join";
  assert (R.join (Ok (Ok 3)) = Ok 3);
  assert (R.join (Ok (Error 2)) = Error 2);
  assert (R.join (Error 3) = Error 3);
  assert (R.join (Error 4) = Error 4);
  ()

let test_msgs () =
  log "Test error messages.";
  assert (R.msg "bla" = `Msg "bla");
  assert (R.msgf "bla%d" 3 = `Msg "bla3");
  assert (R.error_msg "bla" = Error (`Msg "bla"));
  assert (R.error_msgf "bla%d" 3 = Error (`Msg "bla3"));
  let reword s = `Msg (s ^ "++")  in
  assert (R.reword_error_msg ~replace:true reword (Ok 2) = Ok 2);
  assert (R.reword_error_msg ~replace:false reword (Ok 2) = Ok 2);
  assert (R.reword_error_msg ~replace:true reword
            (Error (`Msg "ha")) = (Error (`Msg "ha++")));
  assert (R.reword_error_msg ~replace:false reword
            (Error (`Msg "ha")) = (Error (`Msg "ha\nha++")));
  let pp_error ppf = function `E -> Format.fprintf ppf "E" in
  assert (R.error_to_msg ~pp_error (Ok 2) = (Ok 2));
  assert (R.error_to_msg ~pp_error (Error `E) = (Error (`Msg "E")));
  assert (R.error_msg_to_invalid_arg (Ok 2) = 2);
  raises_invalid_arg R.error_msg_to_invalid_arg (Error (`Msg "E"));
  ()

let test_exn_trap () =
  log "Test trapping unexpected exceptions.";
  let no_raise x = string_of_int x in
  let do_raise x = raise Exit in
  assert (R.trap_exn no_raise 3 = Ok "3");
  begin match R.trap_exn do_raise 3 with
  | Ok _ -> assert false
  | Error (`Exn_trap (Exit, _)) -> ()
  | Error _ -> assert false
  end;
  ()

let test_is () =
  log "Test R.is_{ok,error}";
  assert (R.is_ok (Ok 2));
  assert (not @@ R.is_ok (Error 2));
  assert (R.is_error (Error 2));
  assert (not @@ R.is_error (Ok 2));
  ()

let test_converting () =
  log "Test R.{to,of}_{option,presult}";
  assert (R.to_option (Ok 3) = Some 3);
  assert (R.to_option (Error 3) = None);
  assert (R.of_option ~none:(fun () -> Error "none") (Some 3) = Ok 3);
  assert (R.of_option ~none:(fun () -> Error "none") (None) = Error "none");
  assert (R.to_presult (Ok 3) = (`Ok 3));
  assert (R.to_presult (Error 3) = (`Error 3));
  assert (R.of_presult (`Ok 3) = (Ok 3));
  assert (R.of_presult (`Error 3) = (Error 3));
  ()

let test_ignoring () =
  log "Test.[k]ignore_error";
  assert (R.ignore_error ~use:3 (Ok 4) = 4);
  assert (R.ignore_error ~use:3 (Error 4) = 3);
  assert (R.kignore_error ~use:(Ok 3) (Ok 4) = (Ok 4));
  assert (R.kignore_error ~use:(Ok 3) (Error 4) = (Ok 3));
  assert (R.kignore_error ~use:(Error 3) (Ok 4) = (Ok 4));
  assert (R.kignore_error ~use:(Error 3) (Error 4) = (Error 3));
  ()

let tests () =
  test_constructors ();
  test_reword_error ();
  test_gets ();
  test_bind ();
  test_map ();
  test_join ();
  test_msgs ();
  test_exn_trap ();
  test_is ();
  test_converting ();
  test_ignoring ();
  ()

let () =
  tests ();
  log "All tests succeeded."

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
