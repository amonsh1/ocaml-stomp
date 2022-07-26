type t = {
  command : Command.t;
  headers : Headers.t;
  body : string option;
}

(** Used to generate a random string to use as messages ids e.t.c *)
let generate_random_id () = Random.bits64 () |> Int64.to_string

(** create Frame.t *)
let create command headers body : t = { command; headers; body }

(** Convert {!type:Frame.t} to string, which can be used to send to the server*)
let to_send (frame : t) : string =
  match frame.command with
  | Command.HEARTBEATH -> "\n"
  | _ ->
    [ frame.command |> Command.to_string ]
    |> (fun d ->
         if Headers.HeadersMap.is_empty frame.headers
         then "\n" :: d
         else
           (List.map
              (fun (k, v) -> Format.sprintf "%s:%s" k v)
              (Headers.HeadersMap.bindings frame.headers)
           |> String.concat "\n")
           :: d)
    |> List.cons "\n"
    |> (fun d -> Option.fold ~none:d ~some:(fun body -> ("\n" ^ body) :: d) frame.body)
    |> List.rev
    |> fun d -> String.concat "\n" d ^ "\x00"
;;

(** create message. [create_message destination message_id subscription headers body]. If
    [body] is exists and "content-type" doesn't exists in [headers] then it be sets as
    "text/plain" by default. *)
let create_message
  (destination : string)
  (message_id : string option)
  (subscription : string)
  (headers : Headers.t)
  (body : string option)
  : t
  =
  headers
  |> (fun hs ->
       if Option.is_some body && not (Headers.HeadersMap.mem "content-type" hs)
       then Headers.HeadersMap.add "content-type" "text/plain" hs
       else hs)
  |> (fun hs ->
       Option.fold
         ~none:hs
         ~some:(fun v ->
           Headers.HeadersMap.add "content-length" (string_of_int (String.length v)) hs)
         body)
  |> (fun hs ->
       Headers.HeadersMap.add
         "message-id"
         (Option.value message_id ~default:(generate_random_id ()))
         hs)
  |> Headers.HeadersMap.add "subscription" subscription
  |> Headers.HeadersMap.add "destination" destination
  |> fun hs -> create Command.MESSAGE hs body
;;

(** create receipt massage. [create_receipt receipt_id] *)
let create_receipt (receipt_id : string) : t =
  create Command.RECEIPT (Headers.of_pairs [ "receipt-id", receipt_id ]) None
;;

(** create error massage. [create_error message body headers] If [body] is exists and
    "content-type" doesn't exists in [headers] then it will be sets as "text/plain" by
    default *)
let create_error (message : string) (body : string option) (headers : Headers.t) : t =
  headers
  |> (fun hs ->
       if Option.is_some body && not (Headers.HeadersMap.mem "content-type" hs)
       then Headers.HeadersMap.add "content-type" "text/plain" hs
       else hs)
  |> (fun hs ->
       Option.fold
         ~none:hs
         ~some:(fun v ->
           Headers.HeadersMap.add "content-length" (string_of_int (String.length v)) hs)
         body)
  |> (fun hs -> Headers.HeadersMap.add "message" message hs)
  |> fun hs -> create Command.ERROR hs body
;;

type connection_version = V1_1

let connection_version_to_string = function
  | V1_1 -> "1.1"
;;

(** create connect massage. [create_connect host auth heart_beat] [auth] is a tuple
    ("login", "passcode"). [hearth_beat] is a tuple (<cx>,<cy>) *)
let create_connect
  (accept_version : connection_version)
  (host : string option)
  (auth : (string * string) option)
  (heart_beat : (int * int) option)
  : t
  =
  Headers.HeadersMap.empty
  |> (fun hs ->
       Headers.HeadersMap.add
         "accept-version"
         (connection_version_to_string accept_version)
         hs)
  |> (fun headers ->
       Option.fold
         ~none:headers
         ~some:(fun v -> Headers.HeadersMap.add "host" v headers)
         host)
  |> (fun headers ->
       Option.fold
         ~none:headers
         ~some:(fun v ->
           Headers.HeadersMap.add "login" (fst v) headers
           |> Headers.HeadersMap.add "passcode" (snd v))
         auth)
  |> (fun headers ->
       Option.fold
         ~none:headers
         ~some:(fun v ->
           Headers.HeadersMap.add
             "heart-beat"
             (Format.sprintf "%i,%i" (fst v) (snd v))
             headers)
         heart_beat)
  |> fun hs -> create Command.CONNECT hs None
;;

(** create send massage. create_send [destination body headers] If [body] is exists and
    "content-type" doesn't exists in [headers] then it will be sets as "text/plain" by
    default *)
let create_send (destination : string) (body : string option) (headers : Headers.t) : t =
  headers
  |> (fun hs ->
       if Option.is_some body && not (Headers.HeadersMap.mem "content-type" hs)
       then Headers.HeadersMap.add "content-type" "text/plain" hs
       else hs)
  |> (fun hs ->
       Option.fold
         ~none:hs
         ~some:(fun v ->
           Headers.HeadersMap.add "content-length" (string_of_int (String.length v)) hs)
         body)
  |> (fun hs -> Headers.HeadersMap.add "destination" destination hs)
  |> fun hs -> create Command.SEND hs body
;;

type ack_type =
  | Auto
  | Client
  | ClientIndividual

let ack_type_to_string = function
  | Auto -> "auto"
  | Client -> "client"
  | ClientIndividual -> "client-individual"
;;

let ack_type_of_string = function
  | "auto" -> Some Auto
  | "client" -> Some Client
  | "client-individual" -> Some ClientIndividual
  | _ -> None
;;

(** create subscribe massage. [create_subscribe destination ack id headers] if id is None,
    then it will be created automaticly *)
let create_subscribe
  (destination : string)
  (ack : ack_type)
  (id : string option)
  (headers : Headers.t)
  : t
  =
  headers
  |> Headers.HeadersMap.add "ack" (ack_type_to_string ack)
  |> Headers.HeadersMap.add "id" (Option.value id ~default:(generate_random_id ()))
  |> Headers.HeadersMap.add "destination" destination
  |> fun hs -> create Command.SUBSCRIBE hs None
;;

(** create unsubcribe massage. [create_unsubcribe id] *)
let create_unsubcribe (id : string) : t =
  create Command.UNSUBSCRIBE (Headers.of_pairs [ "id", id ]) None
;;

(** create ack massage. [create_ack id transaction] *)
let create_ack (id : string) (transaction : string option) : t =
  Headers.of_pairs [ "id", id ]
  |> (fun hs ->
       Option.fold
         ~none:hs
         ~some:(fun v -> Headers.HeadersMap.add "transaction" v hs)
         transaction)
  |> fun hs -> create Command.ACK hs None
;;

(** create nack massage. [create_nack id transaction] *)
let create_nack (id : string) (transaction : string option) : t =
  Headers.of_pairs [ "id", id ]
  |> (fun hs ->
       Option.fold
         ~none:hs
         ~some:(fun v ->
           Headers.HeadersMap.add "transaction" (string_of_int (String.length v)) hs)
         transaction)
  |> fun hs -> create Command.NACK hs None
;;

(** create commit massage. [create_commit transaction] *)
let create_commit (transaction : string) : t =
  create Command.COMMIT (Headers.of_pairs [ "transaction", transaction ]) None
;;

(** create abort massage. [create_abort transaction] *)
let create_abort (transaction : string) : t =
  create Command.ABORT (Headers.of_pairs [ "transaction", transaction ]) None
;;

(** create disconnect massage. [create_disconnect receipt] *)
let create_disconnect (receipt : string) : t =
  create Command.DISCONNECT (Headers.of_pairs [ "receipt", receipt ]) None
;;

(** create ping massage *)
let create_hearthbeath () : t =
  create Command.HEARTBEATH Headers.HeadersMap.empty None
;;
