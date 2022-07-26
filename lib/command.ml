type t =
  | MESSAGE
  | RECEIPT
  | ERROR
  | SEND
  | SUBSCRIBE
  | UNSUBSCRIBE
  | ACK
  | NACK
  | BEGIN
  | COMMIT
  | ABORT
  | DISCONNECT
  | CONNECTED
  | CONNECT
  | HEARTBEATH

let of_string (command : string) : t option =
  match String.uppercase_ascii command with
  | "MESSAGE" -> Some MESSAGE
  | "RECEIPT" -> Some RECEIPT
  | "ERROR" -> Some ERROR
  | "SEND" -> Some SEND
  | "SUBSCRIBE" -> Some SUBSCRIBE
  | "UNSUBSCRIBE" -> Some UNSUBSCRIBE
  | "ACK" -> Some ACK
  | "NACK" -> Some NACK
  | "BEGIN" -> Some BEGIN
  | "COMMIT" -> Some COMMIT
  | "ABORT" -> Some ABORT
  | "DISCONNECT" -> Some DISCONNECT
  | "CONNECTED" -> Some CONNECTED
  | "" -> Some HEARTBEATH
  | _ -> None
;;

let to_string (command : t) : string =
  match command with
  | MESSAGE -> "MESSAGE"
  | RECEIPT -> "RECEIPT"
  | ERROR -> "ERROR"
  | SEND -> "SEND"
  | SUBSCRIBE -> "SUBSCRIBE"
  | UNSUBSCRIBE -> "UNSUBSCRIBE"
  | ACK -> "ACK"
  | NACK -> "NACK"
  | BEGIN -> "BEGIN"
  | COMMIT -> "COMMIT"
  | ABORT -> "ABORT"
  | DISCONNECT -> "DISCONNECT"
  | CONNECT -> "CONNECT"
  | CONNECTED -> "CONNECTED"
  | HEARTBEATH -> ""
;;
