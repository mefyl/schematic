open Lwt.Infix
module Logs = (val Logs_lwt.src_log Schematic_http.src)

let ( let* ) = ( >>= )
let ( let+ ) = ( >|= )
let ( >>=! ) = Lwt_result.( >>= )
let pp_uid = Fmt.(option @@ fmt "[%x] ")

type t = unit

let make () = ()

let with_timeout ?(cancel = false) ~method_ ~f name uid uri timeout v =
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
            m "%atimeout %s: %s %a" pp_uid uid name
              Cohttp.Code.(string_of_method method_)
              Uri.pp uri)
      in
      f
        (Result.Error
           Schematic_http.Error.(make ~method_ ~reason:`Networking uri)))

let uid = ref 0

module Make
    (Client : Cohttp_lwt.S.Client)
    (Coding : Schematic.Coding.STR with type t = string) =
struct
  type nonrec t = t
  type 'a result = 'a
  type error = Schematic_http.Error.never

  module Future = Nofuture_lwt

  let send_content_length = true

  module Coding = Coding

  module Monad = struct
    type ('a, 'b) t = ('a, 'b) Lwt_result.t

    include Base.Monad.Make2 (struct
      include Lwt_result

      let bind v ~f = bind v f
      let map v ~f = map f v
      let map = `Custom map
    end)
  end

  let exn method_ uri = function
    | Unix.Unix_error ((Unix.EBADF as e), _, _)
    | Unix.Unix_error ((Unix.ENOTCONN as e), _, _)
    | Unix.Unix_error ((Unix.EADDRNOTAVAIL (* Seen on iOS *) as e), _, _) ->
      let* () =
        Logs.info (fun m ->
            m "retryable unix exception requesting %a: %s" Uri.pp uri
              (Unix.error_message e))
      in
      Lwt_result.fail
        Schematic_http.Error.(
          make ~method_ ~status:(`Code 0) ~reason:`Networking uri)
    | (e : exn) ->
      (* FIXME: Cohttp has no API guarantees about exception that may escape
         depending on the backend. Unreleased recent version yield [Retry]
         exceptions, our own adds [Name_resolution_failed] on top of it, while
         older version may raise [Failure] or whatnot. Pinning such a
         fundamental libraries is a dependency nightmare, so we temporarily
         consider all exceptions as retryable for maximal compatibility and let
         final projects pin a specific version. *)
      let* () =
        Logs.warn (fun m ->
            m "treating cohttp exception as retryabale: %a" Base.Exn.pp e)
      in
      Lwt_result.fail
        Schematic_http.Error.(make ~method_ ~reason:`Networking uri)
  (* let* () =
   *   Logs.err (fun m ->
   *       m "fatal error requesting %a: exception escaped: %a" Uri.pp uri
   *         Base.Exn.pp e)
   * in
   * raise e *)

  module Body = struct
    type t = {
      cohttp : Cohttp_lwt.Body.t;
      method_ : Cohttp.Code.meth;
      timeout : unit Lwt.t option;
      uid : int option;
      uri : Uri.t;
    }

    let of_string method_ uri s =
      {
        cohttp = Cohttp_lwt.Body.of_string s;
        method_;
        timeout = None;
        uid = None;
        uri;
      }

    let of_channel method_ uri s =
      let stream =
        Lwt_stream.from @@ fun () ->
        Lwt_io.read s >|= function "" -> None | chunk -> Some chunk
      in
      {
        cohttp = Cohttp_lwt.Body.of_stream stream;
        method_;
        timeout = None;
        uid = None;
        uri;
      }

    let to_string { cohttp; method_; timeout; uid; uri } =
      let open Lwt in
      catch
        (fun () ->
          with_timeout ~cancel:true ~method_ ~f:Base.Fn.id "reading body" uid
            uri timeout
          @@ let* body = Cohttp_lwt.Body.to_string cohttp in
             let* () =
               Logs.info (fun m ->
                   m "%a read %d bytes of body" pp_uid uid (String.length body))
             in
             Lwt_result.return body)
        (exn method_ uri)

    let drain { cohttp; _ } =
      Lwt.catch
        (fun () -> Cohttp_lwt.Body.drain_body cohttp)
        (fun _ -> Lwt.return ())

    let to_channel ({ cohttp; method_; timeout; uid; uri } as body) =
      let stream =
        Cohttp_lwt.Body.to_stream cohttp
        |> Lwt_stream.wrap_exn
        |> Lwt_stream.map_s (function
             | Result.Ok _ as ok -> Lwt.return ok
             | Result.Error e -> exn method_ uri e)
      and pending = ref None in
      Lwt_io.make ~close:(fun () -> drain body) ~mode:Lwt_io.input
      @@ fun buffer pos size ->
      let* () = Logs.info (fun m -> m "read chunk") in
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
        with_timeout ~method_ ~f:Base.Option.return "reading body" uid uri
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
        | Some (Result.Error e) -> raise (Schematic_http.Channel e)
        | None ->
          let+ () =
            Logs.info (fun m -> m "%a done streaming body" pp_uid uid)
          in
          0)
  end

  let raw () ~scope:_ ?timeout ?headers ?body method_ uri =
    let () = uid := !uid + 1 in
    let uid = !uid in
    let open Lwt in
    let f () =
      let* () =
        Logs.info (fun m ->
            m "[%x] %s %a" uid Cohttp.Code.(string_of_method method_) Uri.pp uri)
      in
      let* () =
        let f (h, v) =
          Logs.info (fun m -> m "[%x]  %s: %s" uid (Base.String.capitalize h) v)
        in
        Lwt_list.iter_s f (Option.value ~default:[] headers)
      in
      let headers = Option.map Cohttp.Header.of_list headers in
      let request =
        Client.call ?headers
          ?body:(Base.Option.map body ~f:(fun { Body.cohttp; _ } -> cohttp))
          method_ uri
        >|= Base.Result.return
      in
      let timeout =
        match timeout with
        | None -> None
        | Some delay -> Some (Timmy_lwt.sleep delay)
      in
      with_timeout ~method_ ~f:Base.Fn.id "reading headers" (Some uid) uri
        timeout request
      >>=! fun (resp, body) ->
      match Cohttp.Response.status resp with
      | `Code 0 as status ->
        (* CORS or connectivity issue *)
        Lwt_result.fail
          Schematic_http.Error.(make ~method_ ~status ~reason:`Networking uri)
      | status ->
        let* () =
          Logs.info (fun m ->
              m "[%x] %s" uid @@ Cohttp.Code.string_of_status status)
        in
        Lwt_result.return
          (resp, { Body.cohttp = body; method_; timeout; uid = Some uid; uri })
    in
    Lwt.catch f (exn method_ uri)
end
