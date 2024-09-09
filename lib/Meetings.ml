open Icalendar
open Pure_html
open CalendarLib
open Date_util
open HTML

let (let*) = Option.bind

let events : calendar -> event list =
  Fun.compose
    (
      List.filter_map
        (fun c -> match c with `Event e -> Some e | _ -> None)
    )
    snd

let upcoming : number: int -> calendar list -> node = fun ~number calendars ->
    let events =
      calendars
      |> List.map events
      |> List.concat
      |> List.sort (fun e1 e2 -> compare_date_or_datetime (snd e1.dtstart) (snd e2.dtstart))
      |> List_extra.take number
      |> List.map
        (
          fun event ->
            match event with
            | { props; dtstart; _ } ->
              let location =
                List.find_map
                  (fun prop -> match prop with `Location x -> Some x | _ -> None)
                  props
              in
              let date =
                (
                  match snd dtstart with
                  | `Date date -> date
                  | `Datetime (`Utc tstamp)
                  | `Datetime (`Local tstamp)
                  | `Datetime (`With_tzid (tstamp, _)) ->
                    tstamp
                    |> Ptime.to_date_time
                    |> fst
                )
                |> (fun (i, j, k) -> Date.make i j k)
                |> Printer.Date.to_string
              in
              match location with
              | Some (_, loc) ->
                div
                  []
                  [
                    txt "%s" date;
                    txt "%s" loc;
                  ]
              | None -> (div [] [])
        )
    in
    div [] events
