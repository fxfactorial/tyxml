(* Ocsigen
 * http://www.ocsigen.org
 * Module server.ml
 * Copyright (C) 2005 Vincent Balat, Denis Berthod, Nataliya Guts
 * Laboratoire PPS - CNRS Universit� Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception; 
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Lwt
open Messages
open Ocsimisc
open Extensions
open Http_frame
open Ocsiheaders
open Http_com
open Predefined_senders
open Ocsiconfig
open Parseconfig
open Error_pages
open Lazy


exception Ocsigen_403
exception Ocsigen_unsupported_media
exception Ssl_Exception
exception Ocsigen_upload_forbidden
exception Config_file_exn of exn


let () = Random.self_init ()

(* Without the following line, it stops with "Broken Pipe" without raising
   an exception ... *)
let _ = Sys.set_signal Sys.sigpipe Sys.Signal_ignore


external disable_nagle : Unix.file_descr -> unit = "disable_nagle"

let local_addr num = Unix.ADDR_INET (Unix.inet_addr_any, num)

let _ = Ssl.init ()
let sslctx = ref (Ssl.create_context Ssl.SSLv23 Ssl.Server_context)


let ip_of_sockaddr = function
    Unix.ADDR_INET (ip, port) -> ip
  | _ -> raise (Ocsigen_Internal_Error "ip of unix socket")

let port_of_sockaddr = function
    Unix.ADDR_INET (ip, port) -> port
  | _ -> raise (Ocsigen_Internal_Error "port of unix socket")


let get_boundary cont_enc =
  let (_,res) = Netstring_pcre.search_forward
      (Netstring_pcre.regexp "boundary=([^;]*);?") cont_enc 0 in
  Netstring_pcre.matched_group res 1 cont_enc

let find_field field content_disp = 
  let (_,res) = Netstring_pcre.search_forward
      (Netstring_pcre.regexp (field^"=.([^\"]*).;?")) content_disp 0 in
  Netstring_pcre.matched_group res 1 content_disp

type to_write = 
    No_File of string * Buffer.t 
  | A_File of (string * string * string * Unix.file_descr)

let counter = let c = ref (Random.int 1000000) in fun () -> c := !c + 1 ; !c


(* reading the request *)
let get_request_infos meth url http_frame filenames sockaddr port =

  try

    let (url, parsed_url, path, params, get_params) =
      Extensions.parse_url url
    in
    
    let headerhost = 
      match get_host_and_port http_frame with
      | None -> None
      | Some (h,_) -> Some h
    in
    (*  Here we don't trust the port information given by the request.
       We use the port we are listening on. *)
    Messages.debug
      ("- host="^(match headerhost with None -> "<none>" | Some h -> h));
(*XXX Servers MUST report a 400 (Bad Request) error if an HTTP/1.1
      request does not include a Host request-header. *)

    let useragent = get_user_agent http_frame in
    
    let cookies_string = lazy (get_cookie_string http_frame) in
    
    let cookies = 
      lazy (match (Lazy.force cookies_string) with
      | None -> []
      | Some s -> parse_cookies s) 
    in
   
    let ifmodifiedsince = get_if_modified_since http_frame in
    
    let ifunmodifiedsince =  get_if_unmodified_since http_frame in
    
    let ifnonematch = get_if_none_match http_frame in
    
    let ifmatch = get_if_match http_frame in
    
    let inet_addr = ip_of_sockaddr sockaddr in
    
    let ct = get_content_type http_frame in

    let cl = get_content_length http_frame in

    let referer = lazy (get_referer http_frame) in

    let accept = lazy (get_accept http_frame)   in

    let accept_charset = lazy (get_accept_charset http_frame) in

    let accept_encoding = lazy (get_accept_encoding http_frame) in

    let accept_language = lazy (get_accept_language http_frame) in



    let find_post_params = 
      lazy
        (if meth = Http_header.GET || meth = Http_header.HEAD 
        then return ([],[]) else 
          match http_frame.Http_frame.content with
          | None -> return ([], [])
          | Some body_gen ->
              try
                let ct = match ct with
                | None -> raise (Failure "Missing Content-Type")
                | Some ct -> ct
                in
                let body = Ocsistream.get body_gen in
                catch
                  (fun () ->
                    let ctlow = String.lowercase ct in
                    if ctlow = "application/x-www-form-urlencoded"
                    then 
                      catch
                        (fun () ->
                          Ocsistream.string_of_stream body >>=
                          (fun r -> return
                              ((Netencoding.Url.dest_url_encoded_parameters r),
                               [])))
                        (function
                          | Ocsistream.String_too_large -> 
                              fail Input_is_too_large
                          | e -> fail e)
                    else 
                      match
                        (Netstring_pcre.string_match 
                           (Netstring_pcre.regexp "multipart/form-data*")) ctlow 0
                      with 
                      | None -> fail Ocsigen_unsupported_media
                      | _ ->
                          let bound = get_boundary ct in
                          let params = ref [] in
                          let files = ref [] in
                          let create hs =
                            let cd = List.assoc "content-disposition" hs in
                            let st = try 
                              Some (find_field "filename" cd) 
                            with _ -> None in
                            let p_name = find_field "name" cd in
                            match st with 
                            | None -> No_File (p_name, Buffer.create 1024)
                            | Some store -> 
                                let now = 
                                  Printf.sprintf 
                                    "%s-%f-%d" 
                                    store (Unix.gettimeofday ()) (counter ())
                                in
                                match ((Ocsiconfig.get_uploaddir ())) with
                                | Some dname ->
                                    let fname = dname^"/"^now in
                                    let fd = Unix.openfile fname 
                                        [Unix.O_CREAT;
                                         Unix.O_TRUNC;
                                         Unix.O_WRONLY;
                                         Unix.O_NONBLOCK] 0o666 in
                                    (* Messages.debug "file opened"; *)
                                    filenames := fname::!filenames;
                                    A_File (p_name, fname, store, fd)
                                | None -> raise Ocsigen_upload_forbidden
                          in
                          let rec add where s =
                            match where with 
                            | No_File (p_name, to_buf) -> 
                                Buffer.add_string to_buf s;
                                return ()
                            | A_File (_,_,_,wh) ->
                                let len = String.length s in
                                let r = Unix.write wh s 0 len in
                                if r < len then
(*XXXX Inefficient if s is long *)
                                  add where (String.sub s r (len - r))
                                else
                                  Lwt_unix.yield ()
                          in
                          let stop size  = function 
                            | No_File (p_name, to_buf) -> 
                                return 
                                  (params := !params @
                                    [(p_name, Buffer.contents to_buf)])
                                  (* � la fin ? *)
                            | A_File (p_name,fname,oname,wh) -> 
                                (* Messages.debug "closing file"; *)
                                files := 
                                  !files@[(p_name, {tmp_filename=fname;
                                                    filesize=size;
                                                    original_filename=oname})];
                                 Unix.close wh;
                                return ()
                          in
                          Multipart.scan_multipart_body_from_stream 
                            body bound create add stop >>=
                          (fun () -> return (!params, !files)))
                  (fun e -> (*XXX??? Ocsistream.consume body >>= fun _ ->*) fail e)
              with e -> fail e)

(* AEFF *)              (*        IN-MEMORY STOCKAGE *)
              (* let bdlist = Mimestring.scan_multipart_body_and_decode s 0 
               * (String.length s) bound in
               * Messages.debug (string_of_int (List.length bdlist));
               * let simplify (hs,b) = 
               * ((find_field "name" 
               * (List.assoc "content-disposition" hs)),b) in
               * List.iter (fun (hs,b) -> 
               * List.iter (fun (h,v) -> Messages.debug (h^"=="^v)) hs) bdlist;
               * List.map simplify bdlist *)
    in
    {ri_url_string = url;
     ri_url = parsed_url;
     ri_method = meth;
     ri_path_string = string_of_url_path path;
     ri_path = path;
     ri_get_params_string = params;
     ri_host = headerhost;
     ri_get_params = get_params;
     ri_post_params = lazy (force find_post_params >>= fun (a, b) -> 
                            return a);
     ri_files = lazy (force find_post_params >>= fun (a, b) -> 
                      return b);
     ri_inet_addr = inet_addr;
     ri_ip = Unix.string_of_inet_addr inet_addr;
     ri_remote_port = port_of_sockaddr sockaddr;
     ri_port = port;
     ri_user_agent = useragent;
     ri_cookies_string = cookies_string;
     ri_cookies = cookies;
     ri_ifmodifiedsince = ifmodifiedsince;
     ri_ifunmodifiedsince = ifunmodifiedsince;
     ri_ifnonematch = ifnonematch;
     ri_ifmatch = ifmatch;
     ri_content_type = ct;
     ri_content_length = cl;
     ri_referer = referer;
     ri_accept = accept;
     ri_accept_charset = accept_charset;
     ri_accept_encoding = accept_encoding;
     ri_accept_language = accept_language;
     ri_http_frame = http_frame;
   }
      
  with e ->
    Messages.debug ("~~~ Exn during get_request_infos : "^
                    string_of_exn e);
    raise e



let service
    receiver
    sender_slot
    request
    meth
    url
    port
    sockaddr
    xhtml_sender
    empty_sender
    inputchan =
  (* sender_slot is here for pipelining:
     we must wait before sending the page,
     because the previous one may not be sent *)

  let head = meth = Http_header.HEAD in
  let clientproto = Http_header.get_proto request.Http_frame.header in

  let handle_service_errors e =
    (* Exceptions during page generation *)
    Messages.debug
      ("~~~ Exception during generation/sending: " ^ string_of_exn e);
    match e with
      (* EXCEPTIONS WHILE COMPUTING A PAGE *)
    | Ocsigen_404 ->
        Messages.debug "-> Sending 404 Not Found";
        send_error sender_slot ~clientproto ~head ~keep_alive:true
          ~code:404 xhtml_sender
    | Ocsigen_403 ->
        Messages.debug "-> Sending 403 Forbidden";
        send_error sender_slot ~clientproto ~head ~keep_alive:true
          ~code:403 xhtml_sender
    | Extensions.Ocsigen_malformed_url
    | Unix.Unix_error (Unix.EACCES,_,_)
    | Extensions.Ocsigen_403 ->
        Messages.debug "-> Sending 403 Forbidden";
        send_error sender_slot ~clientproto ~head ~keep_alive:true
          ~code:403 xhtml_sender (* Forbidden *)
    | Ocsistream.Interrupted Ocsistream.Already_read ->
        Messages.errlog
          "Cannot read the request twice. You probably have \
           two incompatible extensions, or the order of the \
           extensions in the config file is wrong.";
        send_error sender_slot ~clientproto ~head ~keep_alive:true
          ~code:500 xhtml_sender (* Internal error *)
    | Ocsigen_upload_forbidden ->
        Messages.debug "-> Sending 403 Forbidden";
        send_error sender_slot ~clientproto ~head ~keep_alive:true
          ~code:403 xhtml_sender
    | Http_error.Http_exception (_,_) ->
        send_error sender_slot ~clientproto ~head ~cookies:[] ~keep_alive:false
          ~http_exception:e xhtml_sender
    | Ocsigen_Bad_Request ->
        Messages.debug "-> Sending 400";
        send_error sender_slot ~clientproto ~head ~keep_alive:false
          ~code:400 xhtml_sender
    | Ocsigen_unsupported_media ->
        Messages.debug "-> Sending 415";
        send_error sender_slot ~clientproto ~head ~keep_alive:false
          ~code:415 xhtml_sender
    | Neturl.Malformed_URL ->
        Messages.debug "-> Sending 400 (Malformed URL)";
        send_error sender_slot ~clientproto ~head ~keep_alive:false
          ~code:400 xhtml_sender (* Malformed URL *)
    | e ->
        Messages.warning
          ("Exn during page generation: " ^ string_of_exn e ^" (sending 500)");
        Messages.debug "-> Sending 500";
        if get_debugmode () then
          send_xhtml_page
            ~content:(error_page
                        "error 500"
                        [XHTML.M.p
                           [XHTML.M.pcdata (string_of_exn e);
                            XHTML.M.br ();
                            XHTML.M.em
                             [XHTML.M.pcdata "(Ocsigen running in debug mode)"]
                          ]])
            sender_slot ~clientproto ~head ~keep_alive:true
            ~code:500 xhtml_sender
        else
          send_error sender_slot ~clientproto ~head ~keep_alive:true
            ~code:500 xhtml_sender
  in
  let finish_request () =
    (* We asynchronously finish to read the request contents if this
       is not done yet so that:
       - we can handle the next request
       - there is no dead-lock with the client writing the request and
         the server writing the response.
       We need to do this once the request has been handled before sending
       any reply to the client. *)
    match request.Http_frame.content with
      Some f ->
        ignore
          (Lwt.catch
             (fun () -> Ocsistream.consume f)
             (fun e ->

                (*XXX duplicated!*)
                let warn s =
                  let ip =
                    Unix.string_of_inet_addr (ip_of_sockaddr sockaddr) in
                  Messages.warning ("While talking to " ^ ip ^ ": " ^ s)
                in
                begin match e with
                  Http_com.Lost_connection _ ->
                    warn "connection abrutedly closed by peer \
                          while reading contents"
                | Http_com.Timeout ->
                    warn "timeout while reading contents"
                | Http_com.Aborted ->
                    warn "reading thread aborted"
                | Http_error.Http_exception (code, mesg) ->
                    warn (Http_error.string_of_http_exception e)
                | _ ->
                    Messages.unexpected_exception
                      e "Server.finish_request"
                end;
                Http_com.abort receiver;
                (* We unlock the receiver in order to resume the
                   reading loop.  As the connection has been aborted,
                   the next read will fail and the connection will be
                   closed properly. *)
                Http_com.unlock_receiver receiver;
                Lwt.return ()))
    | None ->
        ()
  in

  (* body of service *)
  if meth <> Http_header.GET &&
     meth <> Http_header.POST &&
     meth <> Http_header.HEAD
  then begin
    finish_request ();
    (* RFC 2616, sect 5.1.1 *)
    send_error
      sender_slot ~clientproto ~head ~keep_alive:true ~code:501 xhtml_sender
  end else begin
    let filenames = ref [] (* All the files sent by the request *) in

    Lwt.finalize (fun () ->
      (* *** First of all, we read all the request
         (that will possibly create files) *)
      Lwt.try_bind
        (fun () ->
           Lwt.return
             (get_request_infos meth url request filenames sockaddr port))
        (fun ri ->
           (* *** Now we generate the page and send it *)
           (* Log *)
           accesslog
             (Format.sprintf "connection%s from %s (%s) : %s"
                (match ri.ri_host with
                   None   -> ""
                 | Some h -> " for " ^ h)
                ri.ri_ip ri.ri_user_agent ri.ri_url_string);

           (* Generation of pages is delegated to extensions: *)
           Lwt.try_bind
             (fun () ->
                Extensions.do_for_host_matching
                  ri.ri_host ri.ri_port (Extensions.get_virthosts ()) ri)
             (fun (res, cookieslist) ->
                finish_request ();
(* RFC
   An  HTTP/1.1 origin  server, upon  receiving a  conditional request
   that   includes   both   a   Last-Modified  date   (e.g.,   in   an
   If-Modified-Since or  If-Unmodified-Since header field)  and one or
   more entity tags (e.g.,  in an If-Match, If-None-Match, or If-Range
   header  field) as  cache  validators, MUST  NOT  return a  response
   status of 304 (Not Modified) unless doing so is consistent with all
   of the conditional header fields in the request.
   -
   The result  of a request having both  an If-Unmodified-Since header
   field and  either an  If-None-Match or an  If-Modified-Since header
   fields is undefined by this specification.
*)
                let not_modified =
                  let etagalreadyknown =
                    match res.res_etag with
                    | None   -> false
                    | Some e -> List.mem e ri.ri_ifnonematch
                  in
                  match res.res_lastmodified, ri.ri_ifmodifiedsince with
                  | Some l, Some i when l <= i ->
                      ri.ri_ifnonematch = [] || etagalreadyknown
                  | _, None ->
                      etagalreadyknown
                  | _ ->
                       false
                in
                let precond_failed =
                  begin match
                    res.res_lastmodified, ri.ri_ifunmodifiedsince
                  with
                  | Some l, Some i -> i < l
                  | _              -> false
                  end
                    ||
                  begin match ri.ri_ifmatch, res.res_etag with
                  | None,   _      -> false
                  | Some _, None   -> true
                  | Some l, Some e -> not (List.mem e l)
                  end
                in
                if not_modified then begin
                  Messages.debug "-> Sending 304 Not modified ";
                  send_empty
                    ~content:() sender_slot ~clientproto ~keep_alive:true ~head
                    ~cookies:(List.map change_cookie
                                (res.res_cookies@cookieslist))
                    ?last_modified:res.res_lastmodified
                    ?etag:res.res_etag
                    ~code:304 (* Not modified *)
                    empty_sender
                end else if precond_failed then begin
                  Messages.debug
                    "-> Sending 412 Precondition Failed \
                     (if-unmodified-since header)";
                  send_empty
                    ~content:() sender_slot ~clientproto ~keep_alive:true ~head
                    ~cookies:(List.map change_cookie
                                (res.res_cookies@cookieslist))
                    ?last_modified:res.res_lastmodified
                    ~code:412 (* Precondition failed *)
                    empty_sender
                end else
                  res.res_send_page
                    ?filter:res.res_filter
                    ~cookies:(List.map change_cookie
                                (res.res_cookies@cookieslist))
                    sender_slot ~clientproto ~keep_alive:true ~head:head
                    ?last_modified:res.res_lastmodified
                    ?code:res.res_code
                    ?charset:res.res_charset ?etag:res.res_etag
                    (Http_com.create_sender
                       ~headers:res.res_headers ~server_name:server_name ()))
             (fun e ->
                finish_request ();
                match e with
                  Ocsigen_Is_a_directory ->
                    Messages.debug "-> Sending 301 Moved permanently";
                    send_empty
                      ~content:()
                      ~cookies:[]
                      sender_slot
                      ~clientproto
                      ~keep_alive:true
                      ~location:((Neturl.string_of_url
                                    (Neturl.undefault_url
                                       ~path:("/"::(ri.ri_path))
                                       ri.ri_url))^"/")
                      ~code:301 (* Moved permanently *)
                      ~head empty_sender
                | _ ->
                    handle_service_errors e))
        (fun e ->
            finish_request ();
            handle_service_errors e))
      (fun () ->
         (* We remove all the files created by the request
            (files sent by the client) *)
         if !filenames <> [] then
           Messages.debug "** Removing files";
         List.iter
           (fun a ->
              try
                Unix.unlink a
              with Unix.Unix_error _ as e ->
                Messages.warning
                  (Format.sprintf "Error while removing file %s: %s"
                                   a (string_of_exn e)))
           !filenames;
         Lwt.return ())
  end

let linger in_ch receiver =
  Lwt.catch
    (fun () ->
       (* We wait for 30 seconds at most and close the connection
          after 2 seconds without receiving data from the client *)
       let abort_fun () = Lwt_ssl.abort in_ch Exit in
       let long_timeout = Lwt_timeout.create 30 abort_fun in
       let short_timeout = Lwt_timeout.create 2 abort_fun in
       Lwt_timeout.start long_timeout;
       let s = String.create 1024 in

       let rec linger_aux () =
         Lwt_ssl.wait_read in_ch >>= fun () ->
         Lwt.try_bind
           (fun () ->
              Lwt_timeout.start short_timeout;
              Lwt_ssl.read in_ch s 0 1024)
           (fun len ->
              if len > 0 then linger_aux () else Lwt.return ())
           (fun e ->
              begin match e with
                Unix.Unix_error(Unix.ECONNRESET,_,_)
              | Ssl.Read_error (Ssl.Error_syscall | Ssl.Error_ssl)
              | Exit ->
                  Lwt.return ()
              | _ ->
                  Lwt.fail e
              end)
       in
       (* We start the lingering reads before waiting for the
          senders to terminate in order to avoid a deadlock *)
       let linger_thread = linger_aux () in
       Http_com.wait_all_senders receiver >>= fun () ->
       Messages.debug "** SHUTDOWN";
       Lwt_ssl.ssl_shutdown in_ch >>= fun () ->
       Lwt_ssl.shutdown in_ch Unix.SHUTDOWN_SEND;
       linger_thread >>= fun () ->
       Lwt_timeout.stop long_timeout;
       Lwt_timeout.stop short_timeout;
       Lwt.return ())
    (fun e ->
       Messages.unexpected_exception e "Server.linger";
       Lwt.return ())

let try_bind' f g h = Lwt.try_bind f h g

let handle_connection port in_ch sockaddr =
  let receiver = Http_com.create_receiver Query in_ch in
  let xhtml_sender =
    Http_com.create_sender
      ~headers:Predefined_senders.dyn_headers ~server_name:server_name () in
  let empty_sender =
    Http_com.create_sender
      ~headers:Http_headers.empty ~server_name:server_name () in

  let warn s =
    let ip = Unix.string_of_inet_addr (ip_of_sockaddr sockaddr) in
    Messages.warning ("While talking to " ^ ip ^ ": " ^ s)
  in

  let handle_write_errors e =
    begin match e with
      Lost_connection e' ->
        warn ("connection abrutedly closed by peer (" ^ string_of_exn e' ^ ")")
    | Http_com.Timeout ->
        warn "timeout"
    | Http_com.Aborted ->
        warn "writing thread aborted"
    | Ocsistream.Interrupted e' ->
        warn ("interrupted content stream (" ^ string_of_exn e' ^ ")")
    | _ ->
        Messages.unexpected_exception e "Server.handle_write_errors"
    end;
    Http_com.abort receiver;
    Lwt.fail Http_com.Aborted
  in

  let handle_read_errors e =
    begin match e with
      Http_com.Connection_closed ->
        (* This is the clean way to terminate the connection *)
        warn "connection closed by peer";
        Http_com.abort receiver;
        Http_com.wait_all_senders receiver
    | Http_com.Keepalive_timeout ->
        warn "keepalive timeout";
        Http_com.abort receiver;
        Http_com.wait_all_senders receiver
    | Http_com.Lost_connection _ ->
        warn "connection abrutedly closed by peer";
        Http_com.abort receiver;
        Http_com.wait_all_senders receiver
    | Http_com.Timeout ->
        warn "timeout";
        Http_com.abort receiver;
        Http_com.wait_all_senders receiver
    | Http_com.Aborted ->
        warn "reading thread aborted";
        Http_com.wait_all_senders receiver
    | Http_error.Http_exception (code, mesg) ->
        warn (Http_error.string_of_http_exception e);
        Http_com.start_processing receiver (fun slot ->
          (*XXX We should use the right information for clientproto
            and head... *)
          send_error slot
               ~clientproto:Http_frame.Http_header.HTTP10 ~head:false
               ~cookies:[] ~keep_alive:false
               ~http_exception:e xhtml_sender);
        linger in_ch receiver
    | _ ->
        Messages.unexpected_exception e "Server.handle_read_errors";
        Http_com.abort receiver;
        Http_com.wait_all_senders receiver
    end
  in

  let rec handle_request () =
    try_bind'
      (fun () ->
         Messages.debug "** Receiving HTTP message";
         Http_com.get_http_frame receiver)
      handle_read_errors
      (fun request ->
         let meth, url =
           match Http_header.get_firstline request.Http_frame.header with
           | Http_header.Query a -> a
           | _                   -> assert false
           (*XXX Should be checked in [get_http_frame] *)
         in
         Http_com.start_processing receiver (fun slot ->
           Lwt.catch
             (fun () ->
(*XXX Why do we need the port but not the host name?*)
                service
                  receiver slot request meth url port sockaddr
                  xhtml_sender empty_sender in_ch)
             handle_write_errors);
         if get_keepalive request.Http_frame.header then
           handle_request ()
         else (* No keep-alive => no pipeline *)
            (* We wait for the query to be entirely read and for
               the reply to be sent *)
            Http_com.lock_receiver receiver >>= fun () ->
            Http_com.wait_all_senders receiver)

  in (* body of handle_connection *)
  handle_request ()

let rec wait_connection use_ssl port socket =
  try_bind'
    (fun () -> Lwt_unix.accept socket)
    (fun e ->
       Messages.debug (Format.sprintf "Accept failed: %s" (string_of_exn e));
       wait_connection use_ssl port socket)
    (fun (s, sockaddr) ->
       debug "\n__________________NEW CONNECTION__________________________";
       incr_connected ();
       let relaunch_at_once =
         get_number_of_connected () < get_max_number_of_connections () in
       if relaunch_at_once then
         ignore (wait_connection use_ssl port socket)
       else
         warning (Format.sprintf "Max simultaneous connections (%d) reached."
                    (get_max_number_of_connections ()));
       Lwt.catch
         (fun () ->
            Lwt_unix.set_close_on_exec s;
            disable_nagle (Lwt_unix.unix_file_descr s);
            begin if use_ssl then
              Lwt_ssl.ssl_accept s !sslctx
            else
              Lwt.return (Lwt_ssl.plain s)
            end >>= fun in_ch ->
            handle_connection port in_ch sockaddr)
         (fun e ->
            Messages.unexpected_exception e
              "Server.wait_connection (handle connection)";
            Lwt.return ()) >>= fun () ->
       Messages.debug "** CLOSE";
       begin try
         Lwt_unix.close s
       with Unix.Unix_error _ as e ->
         Messages.unexpected_exception e "Server.wait_connection (close)"
       end;
       decr_connected ();
       if not relaunch_at_once then begin
         debug "Ok releasing one connection";
         ignore (wait_connection use_ssl port socket)
       end;
       Lwt.return ())

(** Thread waiting for events on a the listening port *)
let listen use_ssl port wait_end_init =
  let listening_socket =
    try
      let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt_unix.set_close_on_exec socket;
      Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
      Lwt_unix.bind socket (local_addr port);
      Lwt_unix.listen socket 1;
      socket
    with
    | Unix.Unix_error (Unix.EACCES, _, _) ->
        errlog (Format.sprintf
                  "Fatal - You are not allowed to use port %d." port);
        exit 7
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
        errlog (Format.sprintf "Fatal - The port %d is already in use." port);
        exit 8
    | exn ->
        errlog ("Fatal - Uncaught exception: " ^ string_of_exn exn);
        exit 100
  in
  wait_end_init >>= fun () ->
  wait_connection use_ssl port listening_socket

(* fatal errors messages *)
let errmsg = function
  | Dynlink.Error e -> 
      (("Fatal - Dynamic linking error: "^(Dynlink.error_message e)),
      6)
  | (Unix.Unix_error _) as e ->
      (("Fatal - "^(string_of_exn e)),
      9)
  | Ssl.Private_key_error ->
      (("Fatal - bad password"),
      10)
  | Config_file_exn exn ->
      (("Fatal - Error in configuration file: "^ string_of_exn exn),
      50)
  | Simplexmlparser.Xml_parser_error s ->
      (("Fatal - Error in configuration file: "^s),
       51)
  | Parseconfig.Dynlink_error (s, exn) ->
      (("Fatal - While loading "^s^": "^(string_of_exn exn)),
      52)
  | exn -> 
      try
        ((Extensions.get_init_exn_handler () exn),
        20)
      with
        exn ->
          (("Fatal - Uncaught exception: "^string_of_exn exn),
          100)
            
            
            

(* reloading the cmo *)
let reload () =

  (* That function cannot be interrupted *)
  warning "Reloading config file";

  (try
    match parse_config () with
    | [] -> ()
    | s::_ ->
        begin
          Extensions.start_initialisation ();
          
          parse_server true s;
          
          Extensions.end_initialisation ();
        end
  with e -> 
    Extensions.end_initialisation ();
    errlog (fst (errmsg e)));
  
  warning "Config file reloaded"
    


let _ = try

  let config_servers = 

    parse_config ()

  in

  let number_of_servers = List.length config_servers in

  if number_of_servers > 1
  then Messages.warning "Multiple servers not supported anymore";

  let ask_for_passwd sslports _ =
    print_string "Please enter the password for the HTTPS server listening \
      on port(s) ";
      print_string
      (match sslports with
        [] -> assert false
      | a::l -> List.fold_left
            (fun deb i -> deb^", "^(string_of_int i)) (string_of_int a) l);
    print_string ": ";
    let old_term= Unix.tcgetattr Unix.stdin in
    let old_echo = old_term.Unix.c_echo in
    old_term.Unix.c_echo <- false;
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
    try
      let r = read_line () in
      print_newline ();
      old_term.Unix.c_echo <- old_echo;
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
      r
    with exn ->
      old_term.Unix.c_echo <- old_echo;
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH old_term;
      raise exn
  in

  let run (user,group) (_, ports, sslports) (minthreads, maxthreads) s =

    Messages.open_files ();

    Lwt_unix.run 
      (let wait_end_init = wait () in
      (* Listening on all ports: *)
      List.iter 
        (fun i -> 
          ignore (listen false i wait_end_init)) ports;
      List.iter 
        (fun i ->
          ignore (listen true i wait_end_init)) sslports;
      
      let gid = match group with
      | None -> Unix.getgid ()
      | Some group -> (try
          (Unix.getgrnam group).Unix.gr_gid
      with e -> errlog ("Error: Wrong group"); raise e)
      in
      let uid = match user with
      | None -> Unix.getuid ()
      | Some user -> (try
        (Unix.getpwnam user).Unix.pw_uid
      with e -> errlog ("Error: Wrong user"); raise e) 
      in

      (* A pipe to communicate with the server *)
      let commandpipe = get_command_pipe () in 
      (try
        ignore (Unix.stat commandpipe)
      with _ -> 
        (try
          let umask = Unix.umask 0 in
          Unix.mkfifo commandpipe 0o660;
          Unix.chown commandpipe uid gid;
          ignore (Unix.umask umask)
        with e -> 
          Messages.errlog 
            ("Cannot create the command pipe: "^(string_of_exn e))));

      (* I change the user for the process *)
      (try
        Unix.setgid gid;
        Unix.setuid uid;
      with e -> errlog ("Error: Wrong user or group"); raise e);

      set_user user;
      set_group group;
            
      (* Je suis fou :
         let rec f () = 
         (* print_string "-"; *)
         Lwt_unix.yield () >>= f
         in f(); *)

      if maxthreads < minthreads
      then 
        raise
          (Config_file_error "maxthreads should be greater than minthreads");

      ignore (Preemptive.init minthreads maxthreads);
      
      (* Now I can load the modules *)
      Dynlink.init ();
      Dynlink.allow_unsafe_modules true;

      Extensions.start_initialisation ();

      parse_server false s;
      
      Dynlink.prohibit ["Extensions.R"];
      (* As libraries are reloaded each time the config file is read, 
         we do not allow to register extensions in libraries *)
      (* seems it does not work :-( *)


      (* Closing stderr, stdout stdin if silent *)
      if (Ocsiconfig.get_silent ())
      then begin
        (* redirect stdout and stderr to /dev/null *)
        let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
        Unix.dup2 devnull Unix.stdout;
        Unix.dup2 devnull Unix.stderr;
        Unix.close devnull;
        Unix.close Unix.stdin;
      end;
      
      (* detach from the terminal *)
      if (Ocsiconfig.get_daemon ())
      then ignore (Unix.setsid ());
          
      Extensions.end_initialisation ();

      (* Communication with the server through the pipe *)
      (try
        ignore (Unix.stat commandpipe)
      with _ -> 
          let umask = Unix.umask 0 in
          Unix.mkfifo commandpipe 0o660;
          ignore (Unix.umask umask);
          Messages.warning "Command pipe created");

      let pipe = Lwt_unix.in_channel_of_descr 
          (Lwt_unix.of_unix_file_descr
             (Unix.openfile commandpipe 
                [Unix.O_RDWR; Unix.O_NONBLOCK; Unix.O_APPEND] 0o660)) in
      let rec f () = 
        Lwt_chan.input_line pipe >>=
        (fun _ -> reload (); f ())
      in ignore (f ());

      wakeup wait_end_init ();
      
      warning "Ocsigen has been launched (initialisations ok)";
      
      wait ()
      )
  in

  let set_passwd_if_needed (ssl,ports,sslports) =
    if sslports <> []
    then
      match ssl with
        None
      | Some (None, None) -> ()
      | Some (None, _) -> raise (Ocsiconfig.Config_file_error
                            "SSL certificate is missing")
      | Some (_, None) -> raise (Ocsiconfig.Config_file_error 
                            "SSL key is missing")
      | Some ((Some c), (Some k)) -> 
          Ssl.set_password_callback !sslctx (ask_for_passwd sslports);
          Ssl.use_certificate !sslctx c k
  in

  let write_pid pid =
    match Ocsiconfig.get_pidfile () with
      None -> ()
    | Some p ->
        let spid = (string_of_int pid)^"\n" in
        let len = String.length spid in
        let f =
          Unix.openfile
            p
            [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o640 in
        ignore (Unix.write f spid 0 len);
        Unix.close f
  in

  let rec launch = function
      [] -> () 
    | [h] -> 
        let user_info, sslinfo, threadinfo = extract_info h in
        set_passwd_if_needed sslinfo;
        let pid = Unix.fork () in
        if pid = 0
        then run user_info sslinfo threadinfo h
        else begin
          Messages.console ("Process "^(string_of_int pid)^" detached");
          write_pid pid;
        end
    | _ -> () (* Multiple servers not supported any more *)

  in

  if (not (get_daemon ())) &&
    number_of_servers = 1 
  then
    let cf = List.hd config_servers in
    let (user_info, sslinfo, threadinfo) = extract_info cf in
    (set_passwd_if_needed sslinfo;
     write_pid (Unix.getpid ());
     run user_info sslinfo threadinfo cf)
  else launch config_servers

with e ->
  let msg, errno = errmsg e in
  errlog msg;
  exit errno
