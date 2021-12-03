open Lwt.Infix
open Lwt_io
open Lwt_unix

(** [log ()] is a promise for an [input_channel] that reads from
    the file named "log". *)
let log () : input_channel Lwt.t =
  openfile "log" [ O_RDONLY ] 0 >>= fun fd -> Lwt.return (of_fd ~mode:input fd)

(** [loop ic] reads one line from [ic], prints it to stdout,
    then calls itself recursively. It is an infinite loop. *)
let rec loop (ic : input_channel) =
  Lwt_io.read_line ic >>= fun s ->
  Lwt_io.printlf "%s" s >>= fun _ ->
  Lwt_io.(flush stdout) >>= fun _ -> loop ic
(* hint: use [Lwt_io.read_line] and [Lwt_io.printlf] *)

(** [monitor ()] monitors the file named "log". *)
let monitor () : unit Lwt.t = log () >>= loop

(** [handler] is a helper function for [main]. If its input is
    [End_of_file], it handles cleanly exiting the program by
    returning the unit promise. Any other input is re-raised
    with [Lwt.fail]. *)
let handler : exn -> unit Lwt.t = function
  | End_of_file -> Lwt.return ()
  | exc -> Lwt.fail exc

let main () : unit Lwt.t = Lwt.catch monitor handler

let _ = Lwt_main.run (main ())
