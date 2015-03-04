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
  let ok v = Ok v
  let error e = Error e
  let get_ok = function Ok v -> v | Error _ -> invalid_arg err_error
  let get_error = function Error e -> e | Ok _ -> invalid_arg err_ok
  let reword_error reword = function
  | Ok _ as r -> r
  | Error e -> Error (reword e)

  let pp ~pp_ok ~pp_error ppf = function
  | Ok v -> Format.fprintf ppf "@[Ok %a@]" pp_ok v
  | Error e -> Format.fprintf ppf "@[Error %a@]" pp_error e

  let return = ok
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

  type msg = [ `Msg of string ]
  let msg s = `Msg s
  let msgf fmt =
    let kmsg _ = `Msg (Format.flush_str_formatter ()) in
    Format.kfprintf kmsg Format.str_formatter fmt

  let pp_msg ppf (`Msg msg) = pp_lines ppf msg

  let error_msg s = Error (`Msg s)
  let error_msgf fmt =
    let kerr _ = Error (`Msg (Format.flush_str_formatter ())) in
    Format.kfprintf kerr Format.str_formatter fmt

  let reword_error_msg ?(replace = false) reword = function
  | Ok _ as r -> r
  | Error (`Msg e) ->
      let (`Msg e' as v) = reword e in
      if replace then Error v else error_msgf "%s\n%s" e e'

  let error_to_msg ~pp = function
  | Ok _ as r -> r
  | Error e -> error_msgf "%a" pp e

  let error_msg_to_invalid_arg = function
  | Ok v -> v
  | Error (`Msg m) -> invalid_arg m

  let open_error_msg = function Ok _ as r -> r | Error (`Msg m) as r -> r

  (* Handling exceptions *)

  type backtrace = [ `Backtrace of Printexc.raw_backtrace ]
  let pp_backtrace ppf (`Backtrace e) =
    pp_lines ppf (Printexc.raw_backtrace_to_string e)

  let trap_exn f v = try Ok (f v) with
  | e ->
      let bt = Printexc.get_raw_backtrace () in
      Error (`Backtrace bt)

  let error_backtrace_to_msg = function
  | Ok _ as r -> r
  | Error (`Backtrace bt) ->
      let bt = Printexc.raw_backtrace_to_string bt in
      error_msgf "Unexpected exception:\n%s" bt

  let open_error_backtrace = function
  | Ok _ as r -> r | Error (`Backtrace m) as r -> r

  (* Predicates *)

  let is_ok = function Ok _ -> true | Error _ -> false
  let is_error = function Ok _ -> false | Error _ -> true

  let equal ~ok ~error r r' = match r, r' with
  | Ok v, Ok v' -> ok v v'
  | Error e, Error e' -> error e e'
  | _ -> false

  let compare ~ok ~error r r' = match r, r' with
  | Ok v, Ok v' -> ok v v'
  | Error v, Error v' -> error v v'
  | Ok _, Error _ -> -1
  | Error _, Ok _ -> 1

  (* Converting *)

  let to_option = function Ok v -> Some v | Error e -> None
  let of_option ~none = function None -> none () | Some v -> Ok v
  let to_presult = function Ok v -> `Ok v | Error e -> `Error e
  let of_presult = function `Ok v -> Ok v | `Error e -> Error e

  (* Ignoring errors *)

  let ignore_error ~use = function Ok v -> v | Error _ -> use
  let kignore_error ~use = function Ok _ as r -> r | Error _ -> use

  (* Pervasives string conversion functions *)

  let k_of_string f s = try Some (f s) with Failure _ -> None
  let int_of_string s = k_of_string Pervasives.int_of_string s
  let nativeint_of_string s = k_of_string Nativeint.of_string s
  let int32_of_string s = k_of_string Int32.of_string s
  let int64_of_string s = k_of_string Int64.of_string s
  let float_of_string s = k_of_string float_of_string s
  let bool_of_string s = try Some (bool_of_string s) with
  | Invalid_argument _ (* sic *) -> None

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
