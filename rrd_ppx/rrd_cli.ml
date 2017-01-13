
module C = Cmdliner

module RRDD = struct
  let has_vm_rrd vm_uuid = false (* dummy *)
end

let help_main man_format cmds = function
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
    let topics = "copyright" :: cmds in
    let conv, _ = C.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok true
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok t -> (* only reached when we add topics above *)
      let page = (topic, 7, "", "", ""),
                 [`S "OTHER"
                 ;`P "Here is room for online help texts"
                 ]
      in
      `Ok ( C.Manpage.print man_format Format.std_formatter page
          ; true
          )

module CMD = struct
  (** topic for help *)
  let topic =
    let doc = "Help topic" in
    C.Arg.(value
           & pos 0 (some string) None
           & info [] ~docv:"TOPIC" ~doc)

  let vm_uuid =
    let doc = "uuid" in
    C.Arg.(value
           & pos 0 (some string) None
           & info [] ~docv:"VM uuid" ~doc)

  let help =
    let doc = "help for sub commands" in
    let man =
      [ `S "DESCRIPTION"
      ; `P "provide help for a sub command"
      ; `S "BUGS"
      ; `P "Report bug on the github issue tracker"
      ]
    in
    ( C.Term.(ret
                (const help_main $ man_format $ choice_names $ topic))
    , C.Term.info "help" ~version:"1.0" ~doc ~man
    )


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

  let has_vm_rrd =
    let doc = "check if we have RRD data for this VM" in
    let man =
      [ `S "DESCRIPTION"
      ; `P "check if we have RRD data for this VM"
      ; `S "BUGS"
      ; `P "Report bug on the github issue tracker"
      ]
    in
    ( C.Term.(const RRDD.has_vm_rrd $ vm_uuid)
    , C.Term.info "has-vm-rrd" ~version:"0.0" ~doc ~man
    )

  let cmds =
    [ help
    ; has_vm_rrd
    ]

end

let () =
  match C.Term.eval_choice CMD.main CMD.cmds with
  | `Ok(_)      -> exit 0
  | `Error _    -> exit 1
  | _           -> exit 2

