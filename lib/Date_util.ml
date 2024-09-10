open CalendarLib
open Icalendar

let extract_date = fun (d : date_or_datetime) ->
    (
      match d with
      | `Date date -> date
      | `Datetime (`Utc dodt) -> dodt |> Ptime.to_date_time |> fst
      | `Datetime (`Local dodt) -> dodt |> Ptime.to_date_time |> fst
      | `Datetime (`With_tzid (ts, _)) -> ts |> Ptime.to_date_time |> fst
    )
    |> fun (i, j, k) -> Date.make i j k

let extract_ptime = fun (d : date_or_datetime) ->
    match d with
    | `Date date -> Option.get @@ Ptime.of_date date
    | `Datetime (`Utc dodt) -> dodt
    | `Datetime (`Local dodt) -> dodt
    | `Datetime (`With_tzid (ts, _)) -> ts

let summary : event -> string option = fun (Icalendar.{ props; _ }) ->
    match List.find_map
      (fun prop -> match prop with `Summary x -> Some x | _ -> None)
      props with
    | Some (_, str) -> Some str
    | None -> None

let location : event -> string option = fun (Icalendar.{ props; _ }) ->
    match List.find_map
      (fun prop -> match prop with `Location x -> Some x | _ -> None)
      props with
    | Some (_, str) -> Some str
    | None -> None

let compare_date_or_datetime (e1 : date_or_datetime) (e2 : date_or_datetime) =
  Ptime.compare (extract_ptime e1) (extract_ptime e2)
