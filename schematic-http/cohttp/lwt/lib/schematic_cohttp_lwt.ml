open Lwt.Infix
module Logs = (val Logs_lwt.src_log Schematic_http.src)

let ( let* ) = ( >>= )
let ( let+ ) = ( >|= )
let ( >>=! ) = Lwt_result.( >>= )

type t = unit

let make () = ()

let with_timeout ?(cancel = false) ~method_ ~f name id uri timeout v =
  match timeout with
  | None -> v
  | Some timeout -> (
    Lwt.choose [ v >|= Base.Option.return; (timeout >|= fun () -> None) ]
    >>= function
    | Some v ->
      let () = if cancel then Lwt.cancel timeout in
      Lwt.return v
    | None ->
      let+ () =
        Logs.warn (fun m ->
            m "%atimeout %s: %s %a"
              (Fmt.option Schematic_http.Id.pp)
              id name
              Cohttp.Code.(string_of_method method_)
              Uri.pp uri)
      in
      f (Result.Error `Networking))

let error_of_exception id uri exn =
  Lwt_result.fail (Schematic_http.Private.error_of_exception id uri exn)

module Make
    (Client : Cohttp_lwt.S.Client)
    (Coding : Schematic.Coding.STR with type t = string) =
struct
  type nonrec t = t

  module Future = Nofuture_lwt

  let send_content_length = true

  module Coding = Coding

  module Body = struct
    type t = {
      cohttp : Cohttp_lwt.Body.t;
      id : Schematic_http.Id.t option;
      method_ : Http.Method.t;
      status : Http.Status.t option;
      timeout : unit Lwt.t option;
      uri : Uri.t;
    }

    let of_string method_ uri s =
      {
        cohttp = Cohttp_lwt.Body.of_string s;
        id = None;
        method_;
        status = None;
        timeout = None;
        uri;
      }

    let of_channel method_ uri s =
      let stream =
        Lwt_stream.from @@ fun () ->
        Lwt_io.read s >|= function "" -> None | chunk -> Some chunk
      in
      {
        cohttp = Cohttp_lwt.Body.of_stream stream;
        id = None;
        method_;
        status = None;
        timeout = None;
        uri;
      }

    let to_string { cohttp; method_; timeout; id; uri; _ } =
      let open Lwt in
      catch
        (fun () ->
          with_timeout ~cancel:true ~method_ ~f:Base.Fn.id "reading body" id uri
            timeout
          @@ let* body = Cohttp_lwt.Body.to_string cohttp in
             let* () =
               Logs.debug (fun m ->
                   m "%aread %d bytes of body"
                     (Fmt.option Schematic_http.Id.pp)
                     id (String.length body))
             in
             Lwt_result.return body)
        (error_of_exception id uri)

    let drain { cohttp; _ } =
      Lwt.catch
        (fun () -> Cohttp_lwt.Body.drain_body cohttp)
        (fun _ -> Lwt.return ())

    let to_channel ({ cohttp; method_; status; timeout; id; uri } as body) =
      let stream =
        Cohttp_lwt.Body.to_stream cohttp
        |> Lwt_stream.wrap_exn
        |> Lwt_stream.map_s (function
             | Result.Ok _ as ok -> Lwt.return ok
             | Result.Error e -> error_of_exception id uri e)
      and pending = ref None in
      Lwt_io.make ~close:(fun () -> drain body) ~mode:Lwt_io.input
      @@ fun buffer pos size ->
      match !pending with
      | Some (chunk, chunk_pos) ->
        let length = String.length chunk - chunk_pos in
        if length <= size then
          let () = Lwt_bytes.blit_from_string chunk chunk_pos buffer pos length
          and () = pending := None in
          Lwt.return length
        else
          let () = Lwt_bytes.blit_from_string chunk chunk_pos buffer pos size
          and () = pending := Some (chunk, chunk_pos + size) in
          Lwt.return size
      | None -> (
        with_timeout ~method_ ~f:Base.Option.return "reading body" id uri
          timeout (Lwt_stream.get stream)
        >>= function
        | Some (Result.Ok chunk) ->
          let length = String.length chunk in
          if length <= size then
            let () = Lwt_bytes.blit_from_string chunk 0 buffer pos length in
            Lwt.return length
          else
            let () = Lwt_bytes.blit_from_string chunk 0 buffer pos size
            and () = pending := Some (chunk, size) in
            Lwt.return size
        | Some (Result.Error reason) ->
          raise
            (Schematic_http.Channel
               (Schematic_http.Error.make ~method_ ?status ~reason uri))
        | None ->
          let+ () =
            Logs.info (fun m ->
                m "%a done streaming body" (Fmt.option Schematic_http.Id.pp) id)
          in
          0)
  end

  let raw () ~scope:_ ?timeout ~headers ?body id method_ uri =
    let open Lwt in
    let f () =
      let request =
        Client.call ~headers
          ?body:(Base.Option.map body ~f:(fun { Body.cohttp; _ } -> cohttp))
          method_ uri
        >|= Base.Result.return
      in
      let timeout =
        match timeout with
        | None -> None
        | Some delay -> Some (Timmy_lwt.sleep delay)
      in
      with_timeout ~method_ ~f:Base.Fn.id "reading headers" (Some id) uri
        timeout request
      >>=! fun (resp, body) ->
      match Cohttp.Response.status resp with
      | `Code 0 ->
        (* CORS or connectivity issue *)
        Lwt_result.fail `Networking
      | status ->
        Lwt_result.return
          ( resp,
            {
              Body.cohttp = body;
              method_;
              status = Some status;
              timeout;
              id = Some id;
              uri;
            } )
    in
    Lwt.catch f (error_of_exception (Some id) uri)
end
