open Htmx_calendar
open CalendarLib
open Pure_html
open HTML

open Routes

let year_route = (s "calendar" / int /? nil)
let month_route = (s "calendar" / int / int /? nil)
let day_route = (s "calendar" / int / int / int /? nil)
let events = (s "events" / int / int / int /? nil)
let fonts = (s "fonts" / str /? nil)

let respond html = (Http.Response.make ~status: `OK (), Cohttp_eio.Body.of_string (to_string html))

let null_auth ?ip: _ ~host: _ _ =
  Ok None (* Warning: use a real authenticator in your code! *)

let https ~authenticator =
  match Tls.Config.client ~authenticator () with
  | Ok config ->
    fun uri raw ->
      let host =
        Uri.host uri
        |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
      in
      Tls_eio.client_of_flow ?host config raw
  | _ -> failwith "could not create tls client"

module Server = struct
  open Routes

  let style = Routes.((s "style.css" /? nil))

  let api ~env ~sw =
    let client = Cohttp_eio.Client.make ~https: (Some (https ~authenticator: null_auth)) env#net in
    let calendars =
      ["https://m2-ufind.univie.ac.at/courses/250029/2024W/1/ww.ics"]
      |> List.map
        (
          fun url ->
            let resp, body =
              Cohttp_eio.Client.get ~sw client (Uri.of_string url)
            in
            if Http.Status.compare resp.status `OK = 0 then
              Eio.Buf_read.(parse_exn take_all) body ~max_size: max_int
            else
              ""
        )
      |> List.filter_map (fun s -> match Icalendar.parse s with Ok cal -> Some cal | _ -> None)
    in
    let stylesheet = Eio.Path.(load (env#cwd / "assets/style.css")) in
    let load_font str = Eio.Path.(load (env#cwd / (Format.sprintf "assets/%s" str))) in
    [
      Routes.nil @-->
      respond @@
      html
        []
        [
          head
            []
            [
              meta [charset "utf-8"];
              script [src "https://unpkg.com/htmx.org@2.0.2/dist/htmx.js"] "";
              link [rel "stylesheet"; href "style.css"];
            ];
          body
            []
            [
              Month.view (Date.today ());
              section [ id "meetings-preview"; HTML.style_ "display: inline-block" ] [ ];
              section
                []
                [
                  h2 [] [txt "Upcoming events:"];
                  Meetings.upcoming ~number: 10 calendars;
                ];
            ];
        ];
      events @-->
        (
          fun i j k ->
            let date = Date.make i j k in
            respond @@ Meetings.on date calendars
        );
      style @-->
        Cohttp_eio.Server.respond ~status: `OK ~body: (Eio.Flow.string_source stylesheet) ();
      fonts @-->
        (fun path -> Cohttp_eio.Server.respond ~status: `OK ~body: (Eio.Flow.string_source (load_font path)) ());
      year_route @-->
        (
          fun i ->
            let date = Date.make i 1 1 in
            respond @@ Month.view date
        );
      month_route @-->
        (
          fun i j ->
            let date = Date.make i j 1 in
            respond @@ Month.view date
        );
      day_route @-->
        (
          fun i j k ->
            let date = Date.make i j k in
            respond @@ Month.view date
        )
    ]

  let router ~env ~sw = Routes.one_of (api ~env ~sw)

  let fourohfour = div [] [txt "Page not found"]

  let handler ~env ~sw = fun _socket request _body ->
      let uri = Cohttp.Request.uri request in
      let path = Uri.path uri in
      let _query = Uri.query uri in
      let matched_page = Routes.match' (router ~env ~sw) ~target: path in
      match matched_page with
      | Routes.FullMatch html
      | Routes.MatchWithTrailingSlash html ->
        html
      | Routes.NoMatch -> respond @@ fourohfour

  let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex)

  let run ~env ~port =
    Eio.Switch.run ?name: None @@
      fun sw ->
        let socket =
          Eio.Net.listen
            env#net
            ~sw
            ~backlog: 128
            ~reuse_addr: true
            (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
        and server = Cohttp_eio.Server.make ~callback: (handler ~env ~sw) ()
        in
        Cohttp_eio.Server.run socket server ~on_error: log_warning
end

let () =
  Eio_main.run @@
    fun env ->
      Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@
        fun () ->
          Server.run ~env ~port: 2387
