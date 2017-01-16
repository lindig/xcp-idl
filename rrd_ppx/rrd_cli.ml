
module RRD  = Rrd_idl
module C    = Cmdliner
module CG   = Cmdlinergen.Gen()
module CX   = RRD.API(CG)

let rpc (path:string) (call:Rpc.call) : Rpc.response =
  let socket    = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let ()        = Unix.connect socket (Unix.ADDR_UNIX path) in
  let ic        = Unix.in_channel_of_descr socket in
  let oc        = Unix.out_channel_of_descr socket in
  let msg_buf   = Jsonrpc.string_of_call call in
  let len       = Printf.sprintf "%016d" (String.length msg_buf) in
  output_string oc len;
  output_string oc msg_buf;
  flush oc;
  let len_buf = String.make 16 '\000' in
  really_input ic len_buf 0 16;
  let len = int_of_string len_buf in
  let msg_buf = String.make len '\000' in
  really_input ic msg_buf 0 len;
  let (response: Rpc.response) = Jsonrpc.response_of_string msg_buf in
  response

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

  (* provide each RRDD API call as a sub command *)
  let cmds =
    let rpc' = rpc !RRD.default_path in
      List.map (fun term -> term rpc') !CG.terms

end

let main () =
  match C.Term.eval_choice CMD.main CMD.cmds with
  | `Ok(_)      -> exit 0
  | `Error _    -> exit 1
  | _           -> exit 2

(* only run main when we are not interactive *)
let () = if !Sys.interactive then () else main ()
