(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module C                    = Cmdliner

module API                  = Cmdlinergen.Gen ( )
module Plugin               = Cmdlinergen.Gen ( )
module LocalPlugin          = Cmdlinergen.Gen ( )
module InterdomainPlugin    = Cmdlinergen.Gen ( )
module HA                   = Cmdlinergen.Gen ( )

module API'                 = Rrd_idl.API(API)
module Plugin'              = Rrd_idl.Plugin(Plugin)
module LocalPlugin'         = Rrd_idl.LocalPlugin(LocalPlugin)
module InterdomainPlugin'   = Rrd_idl.InterdomainPlugin(InterdomainPlugin)
module HA'                  = Rrd_idl.HA(HA)


(* [rpc call] marshalls and unmarshalls an RPC call *)
let rpc (call:Rpc.call) : Rpc.response =
  if !Xcp_client.use_switch then
    Xcp_client.json_switch_rpc !Rrd_idl.queue_name call
  else
    failwith "Message switch not configured"

module CMD = struct
  let main =
    let doc = "Interface to the RRDD service" in
    let man =
      [`S "MORE HELP"
      ;`P "Use '$(mname) $(i,COMMAND) --help' for help on a single command."
      ;`Noblank
      ]
    in
    ( C.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ()))
    , C.Term.info "rrd-cli" ~version:"0.0" ~doc ~man
    )

  (* Provide each RRDD API call as a sub command. Commandliner supprts
   * sub-commands only on the top level. This makes it difficult
   * to expose the Plugin commands as sub command.  We therefore only
   * expose API calls that don't conflict in their names.
   *
   * (We can't rewwrite the names because the type for terms is
   * abstract.)
   *)

  let cmds =
    List.concat
    [ !API.terms
    ; !Plugin.terms
    ; !HA.terms
    ]
    |> List.map (fun term -> term rpc)

end

let main () =
  try
    match C.Term.eval_choice CMD.main CMD.cmds with
    | `Ok(_)      -> exit 0
    | `Error _    -> exit 1
    | _           -> exit 2
  with
    e -> Printf.eprintf "error: %s\n" (Printexc.to_string e); exit 2

(* only run main when we are not interactive *)
let () = if !Sys.interactive then () else main ()
