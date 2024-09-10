open Icalendar
open Pure_html
open CalendarLib
open Date_util
open HTML

let (let*) = Option.bind

let event = function `Event e -> Some e | _ -> None

let events : calendar -> event list =
  Fun.compose (List.filter_map event) snd

let on : Date.t -> calendar list -> node = fun date calendars ->
    let events =
      calendars
      |> List.map snd
      |> List.concat
      |> List.filter_map
        (
          fun c ->
            match c with
            | `Event ({ dtstart; _ } as e) ->
              let d = extract_date (snd dtstart) in
              if d = date then Some e else None
            | `Freebusy _
            | `Todo _
            | `Timezone _ ->
              None
        )
      |> List.map
        (
          fun d ->
            match Date_util.summary d with
            | Some s -> li [] [txt "%s" s]
            | None -> li [] [txt "unnamed event"]
        )
      |> function [] -> [txt "No events on this day"] | xs -> xs
    in
    div [] [h2 [] [txt "Schedule for %s" (Printer.Date.sprint "%B %d, %Y" date)]; ol [] events]

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
            | { dtstart; _ } ->
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
                |> Printer.Date.sprint "%a, %b %d"
              in
              let loc = match Date_util.location event with Some str -> div [][txt "%s" str] | None -> null [] in
              let summary = match Date_util.summary event with Some str -> div [][txt "%s" str] | None -> null [] in
                li
                  []
                  [
                    time [][txt "%s" date];
                    loc; summary
                    
                  ]
        )
    in
    ol [] events
