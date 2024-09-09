(* open CalendarLib *)


module Server = struct
  open Routes
  open Pure_html
  open HTML

  type api = Index

  let server_error = Http.Response.make ~status: `Internal_server_error ()

  let routes = 
    [route Routes.nil Index]

  let router = Routes.one_of routes

  let fourohfour = div [][txt "Page not found"]

  let api methd route _body =
    match methd, route with
    | `GET, Index -> div [][ txt "hello"]
    | _ -> fourohfour

  let handler = fun _socket request body ->
    let path = Http.Request.resource request in
    let matched_route = Routes.match' router ~target: path in
    let methd = Http.Request.(request.meth) in
    match matched_route with
    | Routes.FullMatch r
    | Routes.MatchWithTrailingSlash r ->
      let html = api methd r body |> to_string in
      (Http.Response.make ~status: `OK (), Cohttp_eio.Body.of_string html)
    | Routes.NoMatch -> (server_error, Cohttp_eio.Body.of_string "")


  let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex)

  let run ~env ~port =
    Eio.Switch.run ?name: None @@ fun sw ->
    let socket =
      Eio.Net.listen
        env#net
        ~sw
        ~backlog: 128
        ~reuse_addr: true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    and server = Cohttp_eio.Server.make ~callback: handler ()
    in
    Cohttp_eio.Server.run socket server ~on_error: log_warning
end

let () =
  (* let cal = Icalendar.parse Ical.schedule in *)
  (* match cal with *)
  (* | Ok (props, components) -> *)
  (*   Format.printf "Components: %d@." @@ List.length components; *)
  (*   Format.printf "Props: %d@." @@ List.length props; *)
  (* | Error s -> *)
  (*   Format.printf "Error: %s" s *)
  Eio_main.run @@ fun env ->
  Server.run ~env ~port:2387
