(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

type ('a, 'b) result = Ok of 'a | Error of 'b

module R = struct

  let err_error = "result value is (Error _)"
  let err_ok = "result value is (Ok _)"

  (* Results *)

  type ('a, 'b) t = ('a, 'b) result
  let ret v = Ok v
  let error e = Error e
  let get_ok = function Ok v -> v | Error _ -> invalid_arg err_error
  let get_error = function Error e -> e | Ok _ -> invalid_arg err_ok
  let reword_error reword = function
  | Ok _ as r -> r
  | Error e -> Error (reword e)

  let pp ~pp_ok ~pp_error ppf = function
  | Ok v -> Format.fprintf ppf "@[Ok %a@]" pp_ok v
  | Error e -> Format.fprintf ppf "@[Error %a@]" pp_error e

  let return = ret
  let fail = error

  (* Composing results *)

  let bind v f = match v with Ok v -> f v | Error _ as e -> e
  let map v f = match v with Ok v -> Ok (f v) | Error _ as e -> e
  let join r = match r with Ok v -> v | Error _ as e -> e
  let ( >>= ) = bind
  let ( >>| ) = map

  module Infix = struct
    let ( >>= ) = ( >>= )
    let ( >>| ) = ( >>| )
  end

  (* Error messages *)

  type err_msg = [ `Msg of string ]

  let pp_lines ppf s = (* hints new lines *)
    let left = ref 0 and right = ref 0 and len = String.length s in
    let flush () =
      Format.pp_print_string ppf (String.sub s !left (!right - !left));
      incr right; left := !right;
    in
    while (!right <> len) do
      if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
      incr right;
    done;
    if !left <> len then flush ()

  let pp_err_msg ppf (`Msg msg) = pp_lines ppf msg

  let err_msg fmt =
    let kerr _ = Error (`Msg (Format.flush_str_formatter ())) in
    Format.kfprintf kerr Format.str_formatter fmt

  let msg = Format.asprintf
  let reword_err_msg ?(replace = false) msg = function
  | Ok _ as r -> r
  | Error (`Msg e) ->
      if replace then Error (`Msg (msg e)) else
      Error (`Msg (Format.sprintf "%s\n%s" e (msg e)))

  let error_to_err_msg ~pp = function
  | Ok _ as r -> r
  | Error e -> err_msg "%a" pp e

  let err_msg_to_invalid_arg = function
  | Ok v -> v
  | Error (`Msg m) -> invalid_arg m

  (* Handling exceptions *)

  type err_exn = [ `Exn of Printexc.raw_backtrace ]
  let pp_err_exn ppf (`Exn e) =
    pp_lines ppf (Printexc.raw_backtrace_to_string e)

  let trap_exn f v = try Ok (f v) with
  | e ->
      let bt = Printexc.get_raw_backtrace () in
      Error (`Exn bt)

  let err_exn_to_msg = function
  | Ok _ as r -> r
  | Error (`Exn e) ->
      err_msg "Unexpected exception:\n%s" (Printexc.raw_backtrace_to_string e)

  (* Predicates *)

  let is_ok = function Ok _ -> true | Error _ -> false
  let is_error = function Ok _ -> false | Error _ -> true

  (* Converting *)

  let to_option = function Ok v -> Some v | Error e -> None
  let of_option ~none = function None -> none | Some v -> Ok v
  let to_presult = function Ok v -> `Ok v | Error e -> `Error e
  let of_presult = function `Ok v -> Ok v | `Error e -> Error e

  (* Ignoring errors *)

  let ignore_error ~use = function Ok v -> v | Error _ -> use
  let kignore_error ~use = function Ok _ as r -> r | Error _ -> use
end

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
