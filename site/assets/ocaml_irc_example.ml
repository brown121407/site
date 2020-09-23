(*
Copyright 2020 Alexandru-Sergiu Marton

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)
open Core


module Connection : sig
  type t
  val buffer : bytes
  val create : string -> int -> t
  val read : t -> string * int
  val write : t -> string -> int
end = struct 
  type t = Unix.File_descr.t
  let buffer = Bytes.make 512 '\000'
             
  let create (host : string) (port : int) : t =
    let open Unix in
    let sock = socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
    let addr = ADDR_INET (Inet_addr.of_string host, port) in
    connect sock ~addr;
    sock

  let read (conn : t) : string * int =
    Bytes.fill buffer ~pos:0 ~len:(Bytes.length buffer) '\000';
    let how_much = Unix.read conn ~buf:buffer in
    (Bytes.to_string buffer, how_much)

  let write (conn : t) (msg : string) : int =
    let buf = Bytes.of_string msg in
    let len = Bytes.length buf in
    Unix.write conn ~buf ~len
end

    
module Hostname : sig
  val get_ips : string -> int -> Unix.Inet_addr.t list
end = struct
  let get_ips (hostname : string) (port : int) : Unix.Inet_addr.t list =
    let open Unix in
    let filters = [AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM] in
    let service = Int.to_string port in
    getaddrinfo hostname service filters
    |> List.filter_map ~f:(fun x -> match x.ai_addr with
                                    | ADDR_INET (inet_addr, _) -> Some inet_addr
                                    | _ -> None)
end


let () =
  let (hostname, port) = ("chat.freenode.net", 6667) in
  let ip = Hostname.get_ips hostname port
           |> List.hd_exn
           |> Unix.Inet_addr.to_string in
  let conn = Connection.create ip port in
  let chan = Event.new_channel () in
  let receive_loop (conn, chan) =
    try
      while true do
        let () = match Event.poll (Event.receive chan) with
          | Some _ -> raise Exit
          | None -> ()
        in print_endline (fst (Connection.read conn))
      done
    with Exit -> ()
  in  
  let receive_thread = Thread.create receive_loop (conn, chan) ~on_uncaught_exn:`Kill_whole_process in
  try
    while true do
      let msg = In_channel.input_line In_channel.stdin in
      match msg with
      | Some msg ->
         begin
           Connection.write conn (msg ^ "\r\n") |> ignore;
           if String.is_prefix ~prefix:"QUIT" msg then
             raise Exit
         end
      | _ -> ()
    done
  with Exit ->
    begin
      Event.sync (Event.send chan ());
      Thread.join receive_thread
    end
