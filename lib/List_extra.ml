let split_n t_orig n =
  if n <= 0 then [], t_orig
  else
    (
      let rec loop n t accum =
        match t with
        | [] -> t_orig, [] (* in this case, t_orig = rev accum *)
        | hd :: tl -> if n = 0 then List.rev accum, t else loop (n - 1) tl (hd :: accum)
      in
      loop n t_orig []
    )

let take n l =
  if n <= 0 then []
  else
    (
      let rec loop n t accum =
        match t with
        | [] -> l
        | hd :: tl -> if n = 0 then List.rev accum else loop (n - 1) tl (hd :: accum)
      in
      loop n l []
    )

let chunks_of l ~length =
  if length <= 0 then failwith "List.chunks_of: Expected length > 0";
  let rec aux length acc l =
    match l with
    | [] -> List.rev acc
    | _ :: _ ->
      let sublist, l = split_n l length in
      aux length (sublist :: acc) l
  in
  aux length [] l
