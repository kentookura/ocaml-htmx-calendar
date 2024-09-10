open CalendarLib
open Pure_html
open HTML

let first_of_month date =
  let m = Date.month date in
  let y = Date.year date in
  Date.make y (Date.int_of_month m) 1

let last_of_month date =
  let m = Date.month date in
  let y = Date.year date in
  Date.make y (Date.int_of_month m) (Date.days_in_month date)

let pack_date date =
  let year = Date.year date in
  let month = Date.int_of_month @@ Date.month date in
  let day = Date.day_of_month date in
  Format.sprintf "%d/%d/%d" year month day

let padding_start date = (Date.int_of_day @@ Date.day_of_week @@ first_of_month date) - 1
let padding_end date = 7 - (Date.int_of_day @@ Date.day_of_week @@ last_of_month date)

let daily start period f =
  let rec step (current : 'a Date.date) (computed : 'b list) (left : int) =
    let next_date = Date.next current `Day in
    match left with
    | 0 -> computed
    | _ -> step next_date (f current :: computed) (left - 1)
  in
  step start [] (Date.Period.nb_days period)
  |> List.rev

let month_select_button dir ~date =
  let (date, _text, icon) (* Should use text for screen reader *)
    =
    match dir with
    | `Previous ->
      (Date.prev date `Month),
      "Previous month",
      SVG.d
        "M12.79 5.23a.75.75 0 01-.02 1.06L8.832 10l3.938 3.71a.75.75 0 11-1.04 1.08l-4.5-4.25a.75.75 0 010-1.08l4.5-4.25a.75.75 0 011.06.02z";
    | `Next ->
      (Date.next date `Month),
      "Next month",
      SVG.d
        "M7.21 14.77a.75.75 0 01.02-1.06L11.168 10 7.23 6.29a.75.75 0 111.04-1.08l4.5 4.25a.75.75 0 010 1.08l-4.5 4.25a.75.75 0 01-1.06-.02z"
  in
  button
    [
      Hx.get "/calendar/%s" (pack_date date);
      Hx.target "#calendar";
      Hx.swap "outerHTML";
      class_ "month-nav-button";
      type_ "button";
    ]
    [
      (* span [] [txt "%s" text]; *)
      SVG.svg
        [
          SVG.viewbox ~min_x: 0 ~min_y: 0 ~width: 20 ~height: 20;
          SVG.fill "currentColor";
        ]
        [
          SVG.path
            [
              icon
            ]
            [];
        ];
    ]

let previous_month ~date = month_select_button `Previous ~date
let next_month ~date = month_select_button `Next ~date

let day_button (date : Date.t) =
  button
    [
      type_ "button";
      Hx.get "/events/%s" (pack_date date);
      Hx.target "#meetings-preview";
      Hx.target "innerHTML"
    ]
    [
      time
        [
          datetime "%s" (Printer.Date.to_string date);
        ]
        [txt "%d" (Date.day_of_month date)];
    ]

let view : Date.t -> Pure_html.node = fun date ->
    let days_in_current_month = Date.days_in_month date in
    let _days = days_in_current_month in

    (* M  T  W  T  F  S  S

       31 1  2  3  4  5  6
       ...
       29 30 31 1  2  3  4
    *)
    let weekday_header =
      tr [] @@
        List.map
          (fun s -> th [] [txt s])
          ["M"; "T"; "W"; "T"; "F"; "S"; "S"]
    in
    let days_till_first =
      let pd = Date.Period.day (padding_start date) in
      daily (Date.rem (first_of_month date) pd) pd day_button
    in
    let days_from_last =
      let pd = Date.Period.day (padding_end date) in
      day_button
      |> daily
        (Date.add (last_of_month date) (Date.Period.day 1))
        pd
    in
    let month =
      day_button
      |> daily
        (first_of_month date)
        (Date.Period.day (Date.days_in_month date))
    in
    let day_rows =
      days_till_first @ month @ days_from_last
      |> List.map (fun button -> td [] [button])
      |> List_extra.chunks_of ~length: 7
      |> List.map (fun row -> tr [] row)
    in
    let cal_table =
      table
        []
        (weekday_header :: day_rows)
    in
    div
      [
        id "calendar"
      ]
      [
        span
          [class_ "month-selection"]
          [
            div
              []
              [txt "%s" (Printer.name_of_month @@ Date.month date)];
            previous_month ~date;
            next_month ~date
          ];
        cal_table
      ];
