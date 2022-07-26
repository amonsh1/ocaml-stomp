type read_frame_error =
  | EOF
  | BadHeader of string

let parse_header (header : string) : (string * string, read_frame_error) result =
  let splitted_header = (Str.bounded_split (Str.regexp ":") header) 2 in
  if List.length splitted_header = 2
  then Result.ok (List.nth splitted_header 0, List.nth splitted_header 1)
  else Result.error (BadHeader header)
;;

(** read headers [read_headers channel headers] *)
let rec read_headers (channel : In_channel.t) (headers : (string * string) list)
  : ((string * string) list, read_frame_error) result
  =
  let new_line = In_channel.input_line channel in
  match new_line with
  | Some "" -> Result.ok headers
  | Some line ->
    Result.bind (parse_header line) (fun (key, value) ->
      read_headers channel ((key, value) :: headers))
  | None -> Result.ok headers
;;

(** read data while end byte arrives (\x00)
    [read_to_null channel] *)
let rec read_to_null (channel : In_channel.t) (res : char list)
  : (string, read_frame_error) result
  =
  match In_channel.input_char channel with
  | Some chr ->
    if Char.equal chr '\x00'
    then Result.ok (res |> List.rev |> List.to_seq |> Bytes.of_seq |> Bytes.to_string)
    else read_to_null channel (chr :: res)
  | None -> Result.error EOF
;;

(** read command 
    [read_command channel] 
    read data until correct command arrives.*)
let rec read_command (channel : In_channel.t) : (Command.t, read_frame_error) result =
  (* читаем строки пока не придет допустимая команда *)
  let line = In_channel.input_line channel in
  let command =
    match line with
    | None -> Result.error EOF
    | Some v ->
      v
      |> String.trim
      |> Command.of_string
      |> (function
      | None -> read_command channel
      | Some c -> Result.ok c)
  in
  command
;;

(** read body
    [read_body channel length]
    if length is [Some] then data with this length will be read. *)
let read_body (channel : In_channel.t) (length : int option)
  : (string, read_frame_error) result
  =
  if Option.is_some length
  then (
    let buf = Bytes.create (Option.get length) in
    let read_result = In_channel.really_input channel buf 0 (Option.get length) in
    match read_result with
    | Some _ -> Result.ok (Bytes.to_string buf)
    | _ -> Result.error EOF)
  else read_to_null channel []
;;

(** read. [read channel] read {!type:Frame.t} from channel.*)
let read (connection : In_channel.t)
  : (Command.t * 'a Headers.HeadersMap.t * string, read_frame_error) result
  =
  match read_command connection with
  | Error e -> Error e
  | Ok command when command = Command.HEARTBEATH ->
    Result.ok (command, Headers.HeadersMap.empty, String.empty)
  | Ok command ->
    (match read_headers connection [] with
     | Error e -> Result.error e
     | Ok headers ->
       let content_length =
         List.find_opt (fun (h_key, _) -> String.equal h_key "content-length") headers
         |> fun header ->
         Option.bind header (fun header -> Some (int_of_string (snd header)))
       in
       (match read_body connection content_length with
        | Error e -> Result.error e
        | Ok body ->
          print_endline body;
          Result.ok (command, Headers.HeadersMap.of_seq (List.to_seq headers), body)))
;;
