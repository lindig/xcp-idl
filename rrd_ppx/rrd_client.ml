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

module API'                = Rrd_idl.API               (Idl.GenClientExn)
module Plugin'             = Rrd_idl.Plugin            (Idl.GenClientExn)
module LocalPlugin'        = Rrd_idl.LocalPlugin       (Idl.GenClientExn)
module Interdomain'        = Rrd_idl.InterdomainPlugin (Idl.GenClientExn)
module HA'                 = Rrd_idl.HA                (Idl.GenClientExn)

module type RPC = sig
  val rpc: Rpc.call -> Rpc.response
end

(* This functor creates a module with a signature similar to the old
 * Camlp4 RPC scheme. It avoids having to change all the call sites in a client
 * that used the old RPC scheme.
 *)

module Make (R :  RPC) = struct
  let add_host_ds ~ds_name =
    API'.add_host_ds R.rpc ds_name

  let add_sr_ds ~sr_uuid ~ds_name =
    API'.add_sr_ds R.rpc sr_uuid ds_name

  let add_vm_ds ~vm_uuid ~domid ~ds_name =
    API'.add_vm_ds R.rpc vm_uuid domid ds_name

  let archive_rrd ~vm_uuid ~remote_address =
    API'.archive_rrd R.rpc vm_uuid remote_address

  let archive_sr_rrd ~sr_uuid =
    API'.archive_sr_rrd R.rpc sr_uuid

  let backup_rrds ?remote_address x =
    API'.backup_rrds R.rpc remote_address x

  let forget_host_ds ~ds_name =
    API'.forget_host_ds R.rpc ds_name

  let forget_sr_ds ~sr_uuid ~ds_name =
    API'.forget_sr_ds R.rpc sr_uuid ds_name

  let forget_vm_ds ~vm_uuid ~ds_name =
    API'.forget_vm_ds R.rpc vm_uuid ds_name

  let has_vm_rrd ~vm_uuid =
    API'.has_vm_rrd R.rpc vm_uuid

  let migrate_rrd ?session_id ~remote_address ~vm_uuid ~host_uuid =
    API'.migrate_rrd R.rpc session_id remote_address vm_uuid host_uuid

  let push_rrd_local ~vm_uuid ~domid =
    API'.push_rrd_local R.rpc vm_uuid domid

  let push_rrd_remote ~vm_uuid ~remote_address =
    API'.push_rrd_remote R.rpc vm_uuid remote_address

  let push_sr_rrd ~sr_uuid ~path =
    API'.push_sr_rrd R.rpc sr_uuid path

  let query_host_ds ~ds_name =
    API'.query_host_ds R.rpc ds_name

  let query_possible_host_dss x =
    API'.query_possible_host_dss R.rpc x

  let query_possible_sr_dss ~sr_uuid =
    API'.query_possible_sr_dss R.rpc sr_uuid

  let query_possible_vm_dss ~vm_uuid =
    API'.query_possible_vm_dss R.rpc vm_uuid

  let query_sr_ds ~sr_uuid ~ds_name =
    API'.query_sr_ds R.rpc sr_uuid ds_name

  let query_vm_ds ~vm_uuid ~ds_name =
    API'.query_vm_ds R.rpc vm_uuid ds_name

  let remove_rrd ~uuid =
    API'.remove_rrd R.rpc uuid

  let send_host_rrd_to_master ~master_address =
    API'.send_host_rrd_to_master R.rpc master_address

  let set_cache_sr ~sr_uuid =
    API'.set_cache_sr R.rpc sr_uuid

  let unset_cache_sr x =
    API'.unset_cache_sr R.rpc x

  let update_use_min_max ~value =
    API'.update_use_min_max R.rpc value

  let update_vm_memory_target ~domid ~target =
    API'.update_vm_memory_target R.rpc domid target

  module Plugin = struct

    let deregister ~uid =
      Plugin'.deregister R.rpc uid

    let get_header x =
      Plugin'.get_header R.rpc x

    let get_path ~uid =
      Plugin'.get_path R.rpc uid

    let next_reading ~uid =
      Plugin'.next_reading R.rpc uid

    let register ~uid ~frequency =
      Plugin'.register R.rpc uid frequency

    module Local = struct
      let deregister ~uid =
        LocalPlugin'.deregister R.rpc uid

      let next_reading ~uid =
        LocalPlugin'.next_reading R.rpc uid

      let register ~uid ~info ~protocol =
        LocalPlugin'.register R.rpc uid info protocol
    end

    module Interdomain = struct
      let deregister ~uid =
        Interdomain'.deregister R.rpc uid

      let next_reading ~uid =
        Interdomain'.next_reading R.rpc uid

      let register ~uid ~info ~protocol =
        Interdomain'.register R.rpc uid info protocol
    end
  end

  module HA = struct
    let enable_and_update ~statefile_latencies ~heartbeat_latency ~xapi_latency =
      HA'.enable_and_update R.rpc statefile_latencies heartbeat_latency xapi_latency

    let disable x =
      HA'.disable R.rpc x
  end

  module Deprecated = struct
    let load_rrd ~uuid ~timescale ~master_address =
      API'.load_rrd R.rpc uuid timescale master_address
  end
end
