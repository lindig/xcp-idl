(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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


module Mutex = struct
  include Mutex
  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock;
    let r = begin try f () with exn -> Mutex.unlock lock; raise exn end; in
    Mutex.unlock lock;
    r
end

module Cache = struct
  type 'a t = {
    mutable item: 'a option;
    fn: unit -> 'a;
    m: Mutex.t;
  }

  let make fn =
    let item = None in
    let m = Mutex.create () in
    { item; fn; m }

  let invalidate t =
    Mutex.execute t.m
      (fun () ->
        t.item <- None;
      )

  let get t =
    Mutex.execute t.m
      (fun () ->
        match t.item with
        | Some x -> x
        | None ->
          let x = t.fn () in
          t.item <- Some x;
          x
      )
end

let hostname = Cache.make Unix.gethostname
let invalidate_hostname_cache () = Cache.invalidate hostname

let rec split_c c str =
  try
    let i = String.index str c in
    String.sub str 0 i :: (split_c c (String.sub str (i+1) (String.length str - i - 1)))
  with Not_found -> [str]
    
module Backtrace = struct

  type backtrace = string list (* < OCaml 4.02.0 *)

  type t = {
    backtraces: backtrace array;
    exn_to_backtrace: exn Weak.t;
    mutable producer: int; (* free running counter *)
    m: Mutex.t;
  }

  (* Increasing this makes 'find_all' slower and increases the amount of
     memory needed. We maybe should make this a thread-local table. *)
  let max_backtraces = 100

  let get_backtrace_401 () = split_c '\n' (Printexc.get_backtrace ())

  let make () =
    let backtraces = Array.make max_backtraces [] in
    let exn_to_backtrace = Weak.create max_backtraces in
    let producer = 0 in (* free running *)
    let m = Mutex.create () in
    { backtraces; exn_to_backtrace; producer; m }

  let add t exn =
    Mutex.execute t.m
      (fun () ->
        let bt = get_backtrace_401 () in
        (* Deliberately clear the backtrace buffer *)
        (try raise Not_found with Not_found -> ());
        let slot = t.producer mod max_backtraces in
        t.producer <- t.producer + 1;
        Weak.set t.exn_to_backtrace slot (Some exn);
        t.backtraces.(slot) <- bt;
      )

  let remove_all t exn =
    (* work backwards from most recent backtrace, building the list
       in reverse *)
    let rec loop acc from =
      if from < 0 || t.producer - from > max_backtraces
      then acc
      else
        let slot = from mod max_backtraces in
        match Weak.get t.exn_to_backtrace slot with
        | Some exn' when exn' = exn ->
          let bt = t.backtraces.(slot) in
          Weak.set t.exn_to_backtrace slot None;
          t.backtraces.(slot) <- [];
          loop (bt :: acc) (from - 1)
        | _ -> loop acc (from - 1) in
    List.concat (loop [] (t.producer - 1))
end

let global_backtraces = Backtrace.make ()

let backtrace_is_important = Backtrace.add global_backtraces

let get_thread_id () =
    try Thread.id (Thread.self ()) with _ -> -1 

module ThreadLocalTable = struct
  type 'a t = {
    tbl: (int, 'a) Hashtbl.t;
    m: Mutex.t;
  }

  let make () =
    let tbl = Hashtbl.create 37 in
    let m = Mutex.create () in
    { tbl; m }

  let add t v =
    let id = get_thread_id () in
    Mutex.execute t.m (fun () -> Hashtbl.add t.tbl id v)

  let remove t =
    let id = get_thread_id () in
    Mutex.execute t.m (fun () -> Hashtbl.remove t.tbl id)
 
  let find t =
    let id = get_thread_id () in
    Mutex.execute t.m (fun () ->
      if Hashtbl.mem t.tbl id
      then Some (Hashtbl.find t.tbl id)
      else None
    )
end

let names = ThreadLocalTable.make ()

let tasks = ThreadLocalTable.make ()

let gettimestring () =
  let time = Unix.gettimeofday () in
  let tm = Unix.gmtime time in
  let msec = time -. (floor time) in
  Printf.sprintf "%d%.2d%.2dT%.2d:%.2d:%.2d.%.3dZ|"
    (1900 + tm.Unix.tm_year)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    (int_of_float (1000.0 *. msec))

let format include_time brand priority message =
  let host = Cache.get hostname in
  let name = match ThreadLocalTable.find names with Some x -> x | None -> "thread_zero" in
  let task = match ThreadLocalTable.find tasks with Some x -> x | None -> "" in

  let extra = Printf.sprintf "%s|%s|%s|%s" host name task brand in
  Printf.sprintf "[%s%.5s|%s] %s" (if include_time then gettimestring () else "") priority extra message

let print_debug = ref false
let log_to_stdout () = print_debug := true

let logging_disabled_for : (string * Syslog.level) list ref = ref []
let logging_disabled_for_m = Mutex.create ()

let is_disabled brand level =
  Mutex.execute logging_disabled_for_m (fun () ->
    List.mem (brand, level) !logging_disabled_for
  )

let facility = ref Syslog.Daemon
let facility_m = Mutex.create ()
let set_facility f = Mutex.execute facility_m (fun () -> facility := f)
let get_facility () = Mutex.execute facility_m (fun () -> !facility)

let output_log brand level priority s =
  if not(is_disabled brand level) then begin
    let msg = format false brand priority s in
    if !print_debug
    then Printf.printf "%s\n%!" (format true brand priority s);

    Syslog.log (get_facility ()) level msg
  end

let log_backtrace exn =
  let all = Backtrace.remove_all global_backtraces exn in
  let all' = List.length all in
  let rec loop i = function
  | [] -> ()
  | x :: xs ->
    output_log "backtrace" Syslog.Err "error" (Printf.sprintf "%d/%d: %s" i all' x);
    loop (i + 1) xs in
  output_log "backtrace" Syslog.Err "error" (Printexc.to_string exn);
  loop 1 all

let with_thread_associated task f x =
  ThreadLocalTable.add tasks task;
  try
    let result = f x in
    ThreadLocalTable.remove tasks;
    result
  with e ->
    (* This function is a top-level exception handler typically used on fresh
       threads. This is the last chance to do something with the backtrace *)
    backtrace_is_important e;
    ThreadLocalTable.remove tasks;
    log_backtrace e;
    raise e

let with_thread_named name f x =
  ThreadLocalTable.add names name;
  try
    let result = f x in
    ThreadLocalTable.remove names;
    result
  with e ->
    backtrace_is_important e;
    ThreadLocalTable.remove names;
    raise e

module StringSet = Set.Make(struct type t=string let compare=Pervasives.compare end)
let debug_keys = ref StringSet.empty 
let get_all_debug_keys () =
	StringSet.fold (fun key keys -> key::keys) !debug_keys []

let dkmutex = Mutex.create ()

module type BRAND = sig
  val name: string
end

let all_levels = [Syslog.Debug; Syslog.Info; Syslog.Warning; Syslog.Err]

let disable ?level brand =
	let levels = match level with
		| None -> all_levels
		| Some l -> [l]
	in
	Mutex.execute logging_disabled_for_m (fun () ->
		let disable' brand level = logging_disabled_for := (brand, level) :: !logging_disabled_for in
		List.iter (disable' brand) levels
	)

let enable ?level brand =
	let levels = match level with
		| None -> all_levels
		| Some l -> [l]
	in
	Mutex.execute logging_disabled_for_m (fun () ->
		logging_disabled_for := List.filter (fun (x, y) -> not (x = brand && List.mem y levels)) !logging_disabled_for
	)

module type DEBUG = sig
	val debug : ('a, unit, string, unit) format4 -> 'a

	val warn : ('a, unit, string, unit) format4 -> 'a

	val info : ('a, unit, string, unit) format4 -> 'a

	val error : ('a, unit, string, unit) format4 -> 'a

	val audit : ?raw:bool -> ('a, unit, string, string) format4 -> 'a

	val log_backtrace : unit -> unit

	val log_and_ignore_exn : (unit -> unit) -> unit
end

module Make = functor(Brand: BRAND) -> struct
  let _ =
    Mutex.execute dkmutex (fun () -> 
      debug_keys := StringSet.add Brand.name !debug_keys)

	let output level priority (fmt: ('a, unit, string, 'b) format4) =
		Printf.kprintf
			(fun s ->
				if not(is_disabled Brand.name level)
				then output_log Brand.name level priority s
			) fmt
    
	let debug fmt = output Syslog.Debug "debug" fmt
	let warn fmt = output Syslog.Warning "warn" fmt
	let info fmt = output Syslog.Info "info" fmt
	let error fmt = output Syslog.Err "error" fmt
	let audit ?(raw=false) (fmt: ('a, unit, string, 'b) format4) =
		Printf.kprintf
			(fun s ->
				let msg = if raw then s else format true Brand.name "audit" s in
				Syslog.log Syslog.Local6 Syslog.Info msg;
				msg
			) fmt

	let log_backtrace () =
		let backtrace = Printexc.get_backtrace () in
		debug "%s" (String.escaped backtrace)

	let log_and_ignore_exn f =
		try f () with _ -> log_backtrace ()
end
