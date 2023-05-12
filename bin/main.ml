module C = Ntfy.Make (Cohttp_lwt_unix.Client)

let () = Lwt_main.run (
  let%lwt s = C.listen Sys.argv.(1) in
  Lwt_stream.iter_s (fun x -> Lwt_io.printf "id=%s\n" x.Ntfy.id) s
)
