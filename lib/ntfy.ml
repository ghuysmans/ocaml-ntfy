module Assoc = struct
  type t = (string * string) list

  let of_yojson j =
    Ok (Yojson.Safe.Util.(List.map (fun (k, v) -> k, to_string v) (to_assoc j)))
end

type uri = Uri.t

let uri_of_yojson = function
  | `String s -> Ok (Uri.of_string s)
  | _ -> Error "uri"

module Action = struct
  type view = {
    label: string; (** Label of the action button in the notification *)
    url: uri; (** URL to open when action is tapped *)
    clear: bool [@default false]; (** Clear notification after action button is tapped *)
  } [@@deriving of_yojson {strict = false}]

  type broadcast = {
    label: string; (** Label of the action button in the notification *)
    intent: string [@default "io.heckel.ntfy.USER_ACTION"]; (** Android intent name *)
    extras: Assoc.t [@default []]; (** Android intent extras. Currently, only string extras are supported. *)
    clear: bool [@default false]; (** Clear notification after action button is tapped *)
  } [@@deriving of_yojson {strict = false}]

  type meth =
    | GET
    | POST
    | PUT
    | DELETE
    [@@deriving of_yojson]

  let meth_of_yojson y = meth_of_yojson (`List [y])

  type http = {
    label: string; (** Label of the action button in the notification *)
    url: uri; (** URL to which the HTTP request will be sent *)
    meth: meth [@default POST] [@key "method"]; (** HTTP method to use for request *)
    headers: Assoc.t [@default []]; (** HTTP headers to pass in request *)
    body: string [@default ""]; (** HTTP body *)
    clear: bool [@default false]; (** Clear notification after HTTP request succeeds. If the request fails, the notification is not cleared. *)
  } [@@deriving of_yojson {strict = false}]

  type t =
    | View of view (** Opens a website or app when the action button is tapped *)
    | Broadcast of broadcast (** Sends an Android broadcast intent when the action button is tapped (only supported on Android) *)
    | HTTP of http (** Sends HTTP POST/GET/PUT request when the action button is tapped *)

  let of_yojson y =
    match Yojson.Safe.Util.(member "action" y |> to_string) with
    | "view" -> Result.map (fun x -> View x) (view_of_yojson y)
    | "broadcast" -> Result.map (fun x -> Broadcast x) (broadcast_of_yojson y)
    | "http" -> Result.map (fun x -> HTTP x) (http_of_yojson y)
    | a -> Error ("action " ^ a)
end

type event =
  | Open [@name "open"]
  | Keepalive [@name "keepalive"]
  | Message [@name "message"]
  | Poll_request [@name "poll_request"]
  [@@deriving of_yojson]

let event_of_yojson y = event_of_yojson (`List [y])

type topics = string list (* FIXME split on comma *)

let topics_of_yojson = function
  | `String s -> Ok (String.split_on_char ',' s)
  | _ -> Error "topics"

type attachment = {
  name: string; (** Name of the attachment *)
  url: uri; (** URL of the attachment *)
  typ: string option [@key "type"] [@default None]; (** MIME type of the attachment, only defined if attachment was uploaded to ntfy server *)
  size: int option [@default None]; (** Size of the attachment in bytes, only defined if attachment was uploaded to ntfy server *)
  expires: int option [@default None]; (** Attachment expiry date as Unix time stamp, only defined if attachment was uploaded to ntfy server *)
} [@@deriving of_yojson]

type priority =
  | Max (** Really long vibration bursts, default notification sound with a pop-over notification *)
  | High (** Really long vibration bursts, default notification sound with a pop-over notification *)
  | Default (** Short default vibration and sound. Default notification behavior. *)
  | Low (** No vibration or sound. Notification will not visibly show up until notification drawer is pulled down. *)
  | Min (** No vibration or sound. The notification will be under the fold in "Other notifications". *)

let priority_of_yojson = function
  | `Int 5 -> Ok Max
  | `Int 4 -> Ok High
  | `Int 3 -> Ok Default
  | `Int 2 -> Ok Low
  | `Int 1 -> Ok Min
  | _ -> Error "priority"

let int_of_priority = function
  | Max -> 5
  | High -> 4
  | Default -> 3
  | Low -> 2
  | Min -> 1

type t = {
  id: string; (** Randomly chosen message identifier *)
  time: int; (** Message date time, as Unix time stamp *)
  expires: int option [@default None]; (** Unix time stamp indicating when the message will be deleted *)
  event: event; (** Message type, typically you'd be only interested in [Message] *)
  topic: topics; (** topics the message is associated with; only one for all [Message] events, but may be a list in [Open] events *)
  message: string option [@default None]; (** Message body; always present in [Message] events *)
  title: string option [@default None]; (** Message title; if not set defaults to ntfy.sh/<topic> *)
  tags: string list [@default []]; (** List of tags that may or not map to emojis *)
  priority: priority [@default Default]; (** Message priority *)
  click: uri option [@default None]; (** Website opened when notification is clicked *)
  actions: Action.t list [@default []]; (** Action buttons that can be displayed in the notification *)
  attachment: attachment option [@default None]; (** Details about an attachment *)
} [@@deriving of_yojson]

module Make (C : Cohttp_lwt.S.Client) = struct
  open Lwt.Infix

  let subscribe ?(host="ntfy.sh")
      ?(poll=false) ?since ?(scheduled=false)
      ?id ?message ?title ?priorities ?tags
      topic =
    let path = "/" ^ Uri.pct_encode topic ^ "/json" in
    let query =
      let add key cond l = if cond then (key, []) :: l else l in
      let add' key value l =
        match value with
        | None -> l
        | Some v -> (key, [v]) :: l
      in
      let string_of_priorities l =
        List.map (fun x -> string_of_int (int_of_priority x)) l |>
        String.concat ","
      in
      add "poll" poll [] |>
      add' "since" (Option.map string_of_int since) |>
      add "scheduled" scheduled |>
      add' "id" id |>
      add' "message" message |>
      add' "title" title |>
      add' "priority" (Option.map string_of_priorities priorities) |>
      add' "tags" (Option.map (fun l -> String.concat "," l) tags)
    in
    let uri = Uri.make ~scheme:"https" ~host ~path ~query () in
    C.get uri >>= fun (resp, body) ->
    let s = Cohttp_lwt.Body.to_stream body in
    match Cohttp.Response.status resp with
    | `OK ->
      Lwt_stream.map (fun x ->
        match of_yojson (Yojson.Safe.from_string x) with
        | Ok t -> t
        | Error e -> failwith e
      ) s |>
      Lwt.return
    | _ ->
      Lwt.fail_with "HTTP error"
end
