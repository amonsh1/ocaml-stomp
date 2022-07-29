# OCaml-stomp

Client library for [Stomp](https://stomp.github.io/index.html) in OCaml.

```ocaml
let rec listening connection =
  let m = Connection.recv connection in
  match m with
  | Ok m ->
    (match m.command with
     | Command.HEARTBEATH -> print_endline "HEARTBEATH"
     | _ -> m |> Frame.to_send |> print_endline);
    listening connection
  | Error e ->
    (match e with
     | EOF -> print_endline "connection closed"
     | BadHeader e -> print_endline (Format.sprintf "Parsing header error '%s'" e))
;;

let rec ping (connection : Connection.t) =
  Connection.send connection (Frame.create_hearthbeath ());
  Unix.sleepf 0.5;
  ping connection
;;

let () =
  let connection = Connection.connect "127.0.0.1" 61613 in
  Connection.send
    connection
    (Frame.create_connect V1_1 None (Some ("guest", "guest")) (Some (1000, 1000)));
  Connection.send
    connection
    (Frame.create_subscribe
       "/queue/test_queue"
       Frame.Auto
       (Some "1")
       Headers.HeadersMap.empty);
  let ping_tid = Thread.create ping connection in
  listening connection
;;
```
## TODO TESTS!
