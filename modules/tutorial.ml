(* Ocsigen Tutorial *)

open XHTML.M
open Ocsigen

(* ------------------------------------------------------------------ *)
(* To create a web page without parameter: *)
let plop1 = 
  register_new_url 
    ~path:["plop1"]
    ~params:_noparam 
    << <html>
         <head><title></title></head>
         <body><h1>plop</h1></body>
       </html> >>

let plop = 
  register_new_url 
    ~path:["plop"]
    ~params:_noparam 
    (html
       (head (title (pcdata "")) [])
       (body [h1 [pcdata "ploooop"]]))
    
(* Pages can have side effects: *)
let compt = 
  let next =
    let c = ref 0 in
      (fun () -> c := !c + 1; !c)
  in
  register_new_url 
    ~path:["compt"]
    ~params:_unit
    (fun () -> 
      (html
       (head (title (pcdata "counter")) [])
       (body [p [pcdata (string_of_int (next ()))]])))

(* As usual in OCaml, you can forget labels when the application is total: *)
let plip = 
  register_new_url 
    ["dir";"plip"]  (* the url dir/plip *)
    _noparam
    << <html> 
         <head><title>plip</title></head>
         <body><h1>plip</h1></body>
       </html> >>

let oups = 
  register_new_url 
    ["rep"]
    _noparam 
    (html
       (head (title (pcdata "")) [])
       (body [h1 [pcdata "The first one."]]))

let rep2 = 
  register_new_url 
    ["rep";"toto"] 
    _noparam 
    (html
       (head (title (pcdata "")) [])
       (body [h1 [pcdata "Here there was a page rep 
		    but it has been erased by the directory rep/."]]))
(* rep2 will erase oups in the table (with a warning) *)

(* the url rep/ is not equivalent to rep *)
let default = register_new_url ["rep";""] _noparam
  << <html>
       <head><title></title></head>
       <body>
         <p>default page. rep is redirected to rep/</p>
       </body>
     </html> >>


(* ------------------------------------------------------------------ *)
let writeparams entier chaine chaine2 = 
<< <html>
    <head><title></title></head>
    <body>
    <p>
      You sent to me: 
      <strong>$str:string_of_int entier$</strong> et 
      <strong>$str:chaine$</strong> et 
      <strong>$str:chaine2$</strong>
    </p>
    </body>
  </html> >>

(* you can register twice the same url, with different parameters names *)
let plop_params = register_new_url 
  ~path:["plop"]
  ~params:((_int "entier") ** (_string "chaine") ** (_string "chaine2"))
  writeparams
(* If you register twice exactly the same URL, the first one will 
   disappear! *)


(* ------------------------------------------------------------------ *)
(* A web page without parameter which has access to the user-agent 
   and which answers to any url of the form uaprefix/*
*)
let uaprefix = 
  register_new_url 
    ~path:["uaprefix"]
    ~params:(_useragent (_ip (_url_suffix (_string "s"))))
    ~prefix:true
    (fun ua ip suff s -> 
       << <html>
            <head><title></title></head>
            <body>
              <p>
	      The suffix of the url is <strong>$str:suff$</strong>
              and your user-agent is <strong>$str:ua$</strong>
              and your IP is <strong>$str:ip$</strong>
              and s is <strong>$str:s$</strong>
              </p>
            </body>
          </html> >>)

let iprefix = 
  register_new_url 
    ~path:["iprefix"] 
    ~prefix:true 
    ~params:(_url_suffix (_int "i"))
    (fun suff i -> 
<< <html>
     <head><title></title></head>
     <body><p>
       The suffix of the url is <strong>$str:suff$</strong> 
       and i is <strong>$str:string_of_int i$</strong>
     </p></body>
   </html> >>)


(* ------------------------------------------------------------------ *)
(* You can use your own types *)
type mysum = A | B
let mysum_of_string = function
    "A" -> A
  | "B" -> B
  | _ -> raise (Failure "mysum_of_string")
let string_of_mysum = function
    A -> "A"
  | B -> "B"

let mytype = register_new_url 
  ["mytype"]
  (_user_type mysum_of_string string_of_mysum "valeur")
  (fun x -> let v = string_of_mysum x in
    (html
       (head (title (pcdata "")) [])
       (body [p [pcdata v]])))



(* ------------------------------------------------------------------ *)
(* To create a link to a registered url, use the a function: *)

let links = register_new_url ["rep";"links"] (_current_url _noparam)
    (fun current_url ->
      (html
	 (head (title (pcdata "")) [])
	 (body 
	    [p
	       [a plop current_url [pcdata "plop"]; br ();
		a plip current_url [pcdata "plip"]; br ();
		a default current_url 
		  [pcdata "default page of the directory"]; br ();
                a uaprefix current_url 
		  [pcdata "uaprefix"] "suf" "toto"; br ();
                a plop_params current_url 
		  [pcdata "plop_params"] 45 "hello" "plpl"; br ();
                a
	          (new_external_url
		     ~path:["http://fr.wikipedia.org";"wiki"]
		     ~prefix:true
		     ~params:(_url_suffix _noparam) ()) 
                  current_url
	          [pcdata "ocaml on wikipedia"]
                  "Ocaml"]])))
    
(* Note that to create a link we need to know the current url, because:
   the link from toto/titi to toto/tata is "tata" and not "toto/tata"
*)



(* Note new_external_url to create a link on external url *)
(* If you want to put a link towards a page that is not already defined,
   you can break register_new_url into new_url and register_url.
   Do not forget to register the url!!!
 *)

let linkrec = new_url ["linkrec"] (_current_url _noparam) ()

let _ = register_url linkrec 
    (fun url -> 
      << <html>
          <head><title></title></head>
          <body><p>$a linkrec url <:xmllist< cli >>$</p></body>
         </html> >>)





(* ------------------------------------------------------------------ *)
(* To create a form, call the function form with the name of a registered
   url and a function that takes the names of the parameters and
   build the formular
*)
let create_form = 
  (fun entier chaine chaine2 ->
    <:xmllist< <p>Write an int: $int_input entier$ <br/>
    Write a string: $string_input chaine$ <br/>
    Write a string: $string_input chaine2$ <br/>
    $input ~a:[(a_input_type `Submit);(a_value "Click")] ()$</p>
    >>)

let form = register_new_url ["form"] (_current_url _noparam)
  (fun current_url -> 
     let f = form_get plop_params current_url create_form in
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)



(* ------------------------------------------------------------------ *)
(* By default parameters of a web page are in the URL (GET parameters).
   A web page may expect parameters from the http header (POST parameters)
   (that is parameters which are not in the URL)
   Use this if you don't want the user to be able to bookmark
   the URL with parameters, for example if you want to post some
   data which will change the state of the server (database, etc).
   When you register an URL with hidden parameters, you must register
   before an url (fallback) without these parameters (for example that will
   answer if the page is reloaded without the hidden parameters, or
   bookmarked).
 *)
let no_post_param_url = 
  register_new_url 
    ~path:["post"]
    ~params:_noparam 
    << <html><body><p>Version of the page without POST parameters</p></body></html> >>
    
let my_url_with_post_params = register_new_post_url
    ~fallback:no_post_param_url
    ~post_params:(_string "value")
    (fun value -> 
	       << <html><body><p>$str:value$</p></body></html> >>)

(* You can mix get and post parameters *)
let get_no_post_param_url = 
  register_new_url 
    ~path:["post2"]
    ~params:(_int "i")
    (fun i -> 
	       << <html><body><p>No POST parameter, i: <strong>$str:string_of_int i$</strong></p></body></html> >>)
    
let my_url_with_get_and_post = register_new_post_url 
  ~fallback:get_no_post_param_url
  ~post_params:(_string "value")
  (fun value i -> 
	       let i' = string_of_int i in
	       << <html><body><p>Value: <strong>$str:value$</strong> <br/> 
		 i: <strong>$str:i'$</strong>
	       </p></body></html> >>)


(* ------------------------------------------------------------------ *)
(* To create a POST form, use the form_post function,
   possibly applied to GET parameters (if any)
*)

let form2 = register_new_url ["form2"] (_current_url _noparam)
  (fun current_url -> 
     let f  = 
       (form_post my_url_with_post_params current_url
	  (fun chaine -> 
	    <:xmllist< <p> Write a string: $string_input chaine$ </p> >>)) in
       << <html><body>$f$</body></html> >>)

let form3 = register_new_url ["form3"] (_current_url _noparam)
  (fun current_url ->
     let f  = 
       (form_post my_url_with_get_and_post current_url
	  (fun chaine -> 
	    <:xmllist< <p> Write a string: $string_input chaine$ </p> >>)
	  222) in
       << <html><body>$f$</body></html> >>)

let form4 = register_new_url ["form4"] (_current_url _noparam)
  (fun current_url ->
     let f  = 
       (form_post
	  (new_external_post_url 
	     ~path:["http://www.petitspois.com";"form"]
	     ~params:(_int "i")
	     ~post_params:(_string "chaine") ()) current_url
	  (fun chaine -> 
	    <:xmllist< <p> Write a string: $string_input chaine$ </p> >>)
	  222) in
       << <html><body>$f$</body></html> >>)
       
(* note new_external_post_url *)


(* ------------------------------------------------------------------ *)
(* URL with state:
   You can define new urls that differs from public url only by a (hidden)
   parameter (internal state).
   To do that, use new_state_url and new_post_state_url.
   Actually the text of the URL need to be exactly the same as a public
   url, so that it doesn't fail if you bookmark the page.
   That's why new_state_url and new_post_state_url take a public
   URL as parameter (called fallback).
   The session URL will have the same GET parameters but may have
   different POST parameters.
   This session URL will be differenciated from the public one by an internal
   (hidden) parameter.

   This parameter is an hidden post
   parameter if possible... (but in the case of a GET form, it is not 
   possible :-( and for a link it is difficult to do...).

   ** I am not sure whether we should allow them in global table or not
   or only in session tables because of the problem of GET parameters ** 

   Solutions (?) : 
    - interdire les url avec �tat en dehors des sessions ?
    - trouver un moyen d'utiliser la redirection pour changer l'url au vol ?
    - ... ?
 *)

let ustate = new_url ["state"] (_current_url _noparam) ()

let ustate2 = new_state_url ustate

let _ = 
  let c = ref 0 in
  let page url = 
    let l3 = 
      form_post ustate2 url [<< <p> $submit_input "incr i (post)"$ </p> >>] in
    let l4 =
      form_get ustate2 url [<< <p> $submit_input "incr i (get)"$ </p> >>] in
    << <html>
         <body>
          <p> i vaut $str:string_of_int !c$ <br/> 
             $a ustate url <:xmllist< reload >>$ <br/> 
             $a ustate2 url <:xmllist< incr i >>$ 
          </p> 
          $l3$ $l4$ 
         </body>
       </html> >>
  in
    register_url ustate page;
    register_url ustate2 (fun url -> c := !c + 1; page url)


(* ------------------------------------------------------------------ *)
(* You can replace some public url by an url valid only for one session.
   To do that, use the optional parameter url_table of the make_page function.
   It contains a set of url you want to register only for this session.
   To create this value, use register_session_url or
   register_post_session_url
   (and session_table () for the empty table)

   Use this for example if you want two versions of each page,
   one public, one for connected users

   To close a session, use close_session ()
*)
let public_session_without_post_params = 
  new_url 
    ~path:["session"]
    ~params:(_current_url _noparam)
    ()

let public_session_with_post_params = 
  new_post_url 
    ~fallback:public_session_without_post_params
    ~post_params:(_string "login")

let accueil url = 
  let f = form_post public_session_with_post_params url
    (fun login -> 
	 <:xmllist< <p> login: $string_input login$ </p> >>) in
    << <html><body>$f$</body></html> >>

let _ = register_url
  ~url:public_session_without_post_params
  accueil

let rec launch_session login =
  let close = register_new_session_url (* See later *)
    ~fallback:public_session_without_post_params 
    (fun url -> close_session (); accueil url)
  in
  let new_main_page url =
    << <html>
    <body><p>
         Bienvenue $str:login$ ! <br/>
         $a plop url <:xmllist< plop >>$ <br/>
         $a plip url <:xmllist< plip >>$ <br/>
         $a links url <:xmllist< links >>$ <br/>
         $a close url <:xmllist< close session >>$ <br/>
    </p></body>
      </html> >>
  in
    register_session_url 
      ~url:public_session_without_post_params 
                               (* url is any public url already registered *)
      new_main_page;
    register_session_url 
      plop (* any public url already registered *)
      << <html><body><p> Plop $str:login$ ! </p></body></html> >>;
    register_session_url 
      plip
      << <html><body><p> Plop2 $str:login$ ! </p></body></html> >>;
    new_main_page
      
let _ =
  register_post_url
    ~url:public_session_with_post_params
    launch_session



(* ------------------------------------------------------------------ *)
(* You can register url with states in session tables.
   Use this if you want a link or a form which depends precisely on an
   instance of the web page, for example to buy something on an internet shop.
   UPDATE: Actually it is not a good example, because what we want in a shop
   is the same shoping basket for all pages. 
   SEE queinnec example instead.
*)
let shop_without_post_params =
  new_url
    ~path:["shop"]
    ~params:(_current_url _noparam)
    ()

let shop_with_post_params =
  new_post_url
    ~fallback:shop_without_post_params
    ~post_params:(_string "article")

let write_shop shop url  =
  (form_post shop url
     (fun article -> 
	let sb = string_input article in
	  <:xmllist< <p> What do you want to buy? $sb$ </p> >>))

let shop_public_main_page current_url =
  let f  = write_shop shop_with_post_params current_url in
    << <html><body>$f$</body></html> >>

let _ = 
  register_url shop_without_post_params shop_public_main_page
    

let write_shopping_basket shopping_basket =
  let rec aux = function
      [] -> [<< <br/> >>]
    | a::l -> let fol = aux l in <:xmllist< $str:a$ <br/> $list:fol$ >>
  in
  let ffol = aux shopping_basket in
    <:xmllist< Your shopping basket: <br/> $list:ffol$ >>

let rec page_for_shopping_basket url shopping_basket =
  let local_shop_with_post_params = 
    new_post_state_url
      ~fallback:shop_without_post_params
      ~post_params:(_string "article")
  and local_pay = new_state_url ~fallback:shop_without_post_params
  in
    register_post_session_url
      ~url:local_shop_with_post_params
      (fun article current_url -> 
		 page_for_shopping_basket 
		   current_url (article::shopping_basket));
    register_session_url
      local_pay
      (fun current_url ->
	   << <html><body>
	        <p>You are going to pay: 
                  $list:write_shopping_basket shopping_basket$ </p>
              </body></html> >>);
      << <html>
           <body> 
             <div>$list:write_shopping_basket shopping_basket$</div>
             $write_shop local_shop_with_post_params url$ 
             <p>$a local_pay url <:xmllist< pay >>$</p>
           </body>
         </html> >>

let _ = register_post_url
  ~url:shop_with_post_params
  (fun article url -> page_for_shopping_basket url [article])


(* Queinnec example: *)

let queinnec = 
  new_url
    ~path:["queinnec"]
    ~params:(_current_url _noparam)
    ()

let queinnec_post = 
  new_post_url 
    ~fallback:queinnec
    ~post_params:(_int "i")

let _ = 
  let create_form is = 
    (fun entier ->
      let ib = int_input entier in
      let b = submit_input "Somme" in
      <:xmllist< <p> $str:is$ + $ib$ <br/>
      $b$ </p>
      >>)
  in
  register_post_url
    ~url:queinnec_post
    (fun i current_url ->
      let is = string_of_int i in
      let queinnec_result = register_new_post_session_url
	  ~fallback:queinnec_post
	  ~post_params:(_int "j")
	  (fun j current_url -> 
	    let js = string_of_int j in
	    let ijs = string_of_int (i+j) in
	    << <html> <body><p> $str:is$ + $str:js$ = $str:ijs$ </p></body></html> >>)
      in
      let f  = 
	form_post queinnec_result current_url (create_form is) in
      << <html><body>$f$</body></html> >>)
    
let _ =   
  let create_form = 
    (fun entier ->
      <:xmllist< <p> Write a number: $int_input entier$ <br/>
      $submit_input "Envoyer"$ </p>
      >>)
  in
  register_url
    queinnec
    (fun current_url ->
      let f  = form_post queinnec_post current_url create_form in
      << <html><body>$f$</body></html> >>)
      


(* Actions: *)
(* Actions are like url but they do not generate any page.
   For ex, when you have the same form (or link) on several pages 
   (for ex a connect form),
   instead of making a version with post params of all these pages,
   you can use actions.
   Create and register an action with new_actionurl, register_actionurl,
     register_new_actionurl, register_session_actionurl,
     register_new_session_actionurl
   Make a form or a link to an action with action_form or action_link.
   By default, they print the page again after having done the action.
   But you can give the optional boolean parameter reload to action_form
   or action link to prevent reloading the page.
*)
let action_session = 
  new_url ~path:["action"] ~params:(_http_params _noparam) ()

let connect_action = new_actionurl ~params:(_string "login")

let accueil_action h = 
  let f = action_form connect_action h
    (fun login -> 
       let sb = string_input login in
	 <:xmllist< <p> login: $sb$ </p> >>) in
    << <html><body>$f$</body></html> >>

let _ = register_url
  ~url:action_session
  accueil_action

let rec launch_session login =
  let deconnect_action = register_new_actionurl _unit close_session in
  let deconnect_box h s = action_link deconnect_action h s in
  let new_main_page h =
    << <html>
       <body><p>
         Bienvenue $str:login$ ! <br/>
         $a plop h.current_url <:xmllist< plop >>$ <br/>
         $a plip h.current_url <:xmllist< plip >>$ <br/>
         $a links h.current_url <:xmllist< links >>$ </p>
         $deconnect_box h <:xmllist< Close session >>$
      </body>
      </html> >>
  in
    register_session_url ~url:action_session new_main_page;
    register_session_url plop << <html><body><p> Plop $str:login$ ! </p></body></html> >>;
    register_session_url plip << <html><body><p> Plop2 $str:login$ ! </p></body></html> >>
      
let _ = register_actionurl
    ~actionurl:connect_action
    ~action:(fun login -> launch_session login)


(* Main page for this example *)
let main = new_url [] (_current_url _noparam) ()

let _ = register_url main
  (fun url ->
     << 
       <html> 
       <!-- This is a comment! -->
       <head>
	 $css_link (make_uri static_dir url "style.css")$
	 <title>Ocsigen Tutorial</title>
       </head>
       <body>
       <h1>Ocsigen</h1>
       <h2>Examples</h2>
       <h3>Simple pages</h3>
       <p>
       Une page simple : $a plop url <:xmllist< plop >>$ <br/>
       Une page avec un compteur : $a compt url <:xmllist< compt >>$ <br/> 
       Une page simple dans un r�pertoire : 
	   $a plip url <:xmllist< dir/plip >>$ <br/>
       Page erased by another one: $a rep2 url <:xmllist< rep2 >>$ <br/>
       Default page of a directory:
	 $a oups url <:xmllist< rep >>$ will be redirected to
     $a default url <:xmllist< rep/ >>$</p>
       <h3>Parameters</h3>
       <p>
       Une page avec param�tres GET : 
	   $a plop_params url <:xmllist< plop avec params >> 45 "hello" "krokodile"$ (que se passe-t-il si le premier param�tre n'est pas un entier ?)<br/> 
       Une page avec URL "pr�fixe" qui r�cup�re l'IP et l'user-agent : 
	   $a uaprefix url <:xmllist< uaprefix >> "suf" "toto"$ <br/> 
       Une page URL "pr�fixe" avec des param�tres GET : 
	   $a iprefix url <:xmllist< iprefix >> "popo" 333$ <br/> 
       Une page qui r�cup�re un param�tre d'un type utilisateur : 
	     $a mytype url <:xmllist< mytype >> A$ </p>
       <h3>Links and Formulars</h3>
       <p>
       Une page avec des liens : $a links url <:xmllist< links >> $ <br/> 
       Une page avec un lien vers elle-m�me : 
	     $a linkrec url <:xmllist< linkrec >>$ <br/>
       The $a main url <:xmllist< default page >>$ 
	   of this directory (myself) <br/>
       Une page avec un formulaire GET qui pointe vers la page plop avec params : 
	     $a form url <:xmllist< form >>$ <br/> 
       Un formulaire POST qui pointe vers la page "post" : 
	     $a form2 url <:xmllist< form2 >>$ <br/> 
       La page "post" quand elle ne re�oit pas de param�tres POST : 
	     $a no_post_param_url url <:xmllist< post sans post_params >>$ <br/> 
       Un formulaire POST qui pointe vers une URL avec param�tres GET : 
	     $a form3 url <:xmllist< form3 >>$ <br/> 
       Un formulaire POST vers une page externe : 
	     $a form4 url <:xmllist< form4 >>$ </p> 
       <h3>Sessions</h3>
       <p>
       URL avec �tat : 
	     $a ustate url <:xmllist< state >>$ (probl�me des param�tres GET bookmark�s...) <br/> 
       Une session bas�e sur les cookies : 
	     $a public_session_without_post_params url <:xmllist< session >>$ <br/> 
       Une session avec des actions : 
	     $a action_session url <:xmllist< actions >>$ <br/>
       Une session avec "url de sessions" : 
	     $a queinnec url <:xmllist< queinnec >>$
       - (ancienne version : $a shop_without_post_params url <:xmllist< shop >>$)
       </p>
       </body>
     </html> >>)


(* WARNING! As we don't use threads, 
   the following page will stop the server 5 seconds!!!
let _ = 
  register_new_url 
    ~path:["loooooooooong"]
    ~params:_unit
    (fun () -> 
               Unix.sleep 5;
	       << <html><body><p>Ok now, you can read the page.</p></body></html> >>)
*)
