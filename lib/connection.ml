type t =
  { in_channel : In_channel.t
  ; out_channel : Out_channel.t
  }

let connect (host : string) (port : int) =
  let i, o =
    Unix.open_connection (Unix.ADDR_INET (Unix.inet_addr_of_string host, port))
  in
  { in_channel = i; out_channel = o }
;;

let send (connection : t) (frame : Frame.t) =
  Out_channel.output_string connection.out_channel (frame |> Frame.to_send);
  Out_channel.flush connection.out_channel
;;

let recv (connection : t) : (Frame.t, Reader.read_frame_error) result =
  match Reader.read connection.in_channel with
  | Ok (command, headers, body) -> Ok (Frame.create command headers (Some body))
  | Error e -> Result.error e
;;

let close (connection : t) = In_channel.close connection.in_channel
