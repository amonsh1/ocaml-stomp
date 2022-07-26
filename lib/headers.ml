module HeadersMap = Map.Make (String)

type t = string HeadersMap.t

let of_pairs (pairs : (string * string) list) : t = HeadersMap.of_seq (List.to_seq pairs)
