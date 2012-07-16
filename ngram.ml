(*{{{NGRAMTYPE*)
module type NGRAMTYPE = sig
  val num_elements : int 
  val pick_random : unit -> int
  (*in case two sequences have the same frequencies, referee will decide*)
  val referee : int list -> int
end
(*}}}*)
(*{{{NGRAM_PREDICTOR_TYPE*)
module type NGRAM_PREDICTOR_TYPE = sig
  type t
  (*the int parameter here is the window size: n*)
  (*if N is of size n then register_sequence takes arrays of size n+1 while*)
  (*predict_element accepts arrays of size n to predict +1 element*)
  val create_empty : int -> t
  val register_sequence : t -> int array -> unit
  val predict_element : t -> int array -> int
  (*val predict_with_freq : t -> int array -> int * int*)
end
(*}}}*)
(*{{{NGRAM_HIERARCHY*)
module type NGRAM_HIERARCHY= sig
  val window_sizes : int list
  (*referee is the final arbiter that decides which predictor to use.*)
  (*a simple maximum freq could picker could work here*)
  (*window_size * frequency = int * int*)
  (*NOTE that it's better to pass along the actual decision itself*)
  val referee : (int * int) list -> int
end
(*}}}*)
(*{{{ misc functions *)
let (|>) g f = f g
let subsequence e size = 
  let start_copy = (Array.length e) - size in
  let ret = Array.make size 0 in
  Array.blit e start_copy ret 0 size; ret
let max_list l = List.fold_right (fun e acc ->
  if e > acc then e
  else acc ) (List.tl l) (List.hd l)
(*
 *given an array arr, arr_max_index returns a list of indices where every index
 *points to a maximum value in the array
 *example: arr_max_index [|1;0;1|] => [0;2]
 *)
let arr_max_index arr =
  let current_max_ind = ref [0] in
  for i=1 to (Array.length arr)-1 do
    let e = arr.(i) in 
    match compare e arr.(List.hd !current_max_ind) with
    | 1 -> current_max_ind := [i]
    | 0 ->  current_max_ind := i :: !current_max_ind
    | -1 -> ()
    | _ -> failwith "just to kill the damn warning"
  done; !current_max_ind
(*}}}*)
(*{{{NgramPredictor*)
module NgramPredictor (N : NGRAMTYPE) : NGRAM_PREDICTOR_TYPE 
= struct
  type t = { store: (int array, int array) Hashtbl.t;
    (*example: if window_size=3. then we take LLRL and use it*)
    (*to predict L when given LLR again*)
             window_size: int }

  let create_empty window = 
    { store = Hashtbl.create 100;
      window_size = window }

  let register_sequence t earr = 
    match t with
    | { store=h; window_size=size } ->
        let value = earr.(size) in
        let key = Array.make size 0 in
        Array.blit earr 0 key 0 size;
        try match Hashtbl.find h key with
        | a -> a.(value) <- a.(value) + 1
        with Not_found ->
          let new_v = Array.make N.num_elements 0 in
          new_v.(value) <- 1;
          Hashtbl.add h key new_v

  (*if we have no information then we will return a random element*)
  (*earr is of length t.window_size*)
  let predict_element t earr = 
    match t with
    | { store=h; window_size } ->
        try match Hashtbl.find h earr with
        | a -> 
          (*
           *a is an array of frequencies where the index corresponds to element
           *and the value at the index corresponds to the frequency
           *)
            let max_freq = arr_max_index a in
            if (List.length max_freq) = 1
            then List.hd max_freq
            else N.referee max_freq
        with Not_found -> N.pick_random()
end
(*}}}*)
(*{{{HierarchicalNGramPredictor*)
module HierarchicalNGramPredictor (H : NGRAM_HIERARCHY) (N : NGRAM_PREDICTOR_TYPE)
: NGRAM_PREDICTOR_TYPE
= struct
  type t = ( int, N.t ) Hashtbl.t

  let create_empty _ = 
    let h = Hashtbl.create (List.length H.window_sizes) in
    List.iter ( fun ws -> Hashtbl.add h ws (N.create_empty ws))
    H.window_sizes; h
    
  let register_sequence t earr = 
    let l = Array.length earr in 
    H.window_sizes 
        |> List.filter (fun ws -> ws < l)
        |> List.iter ( fun ws ->
            let subseq = subsequence earr (ws + 1) in
            match Hashtbl.find t ws with
            | predictor -> N.register_sequence predictor subseq)

  let predict_element t earr = 
    let l = Array.length earr in
    H.window_sizes 
            |> List.filter ( fun ws -> ws <= l )
            |> List.map (fun ws ->
                let subseq = subsequence earr ws in
                match Hashtbl.find t ws with
                | predictor -> (ws, N.predict_element predictor subseq)
                ) |> H.referee
end
(*}}}*)
(*{{{RPS*)
module RPS = struct
  type rps = Rock | Paper | Scissors
  type player = Human | Computer
  type result = HumanWin | Tie | ComputerWin
  (*
   *first element of the tuple is what the human played
   *second element is what the computer played in response
   *)
  type t = rps * rps
  let beats = function
    | Rock -> Paper
    | Scissors -> Rock
    | Paper -> Scissors
  let fight (x,y) =
    if (beats x) = y then ComputerWin
    else if (beats y) = x then HumanWin
    else Tie
  (*{{{crap ton of boilerplate because we don't have derived typeclasses*)
  let rps_of_string = function
    | "r" -> Rock
    | "p" -> Paper
    | "s" -> Scissors

  let string_of_rps = function
    | Rock -> "Rock"
    | Paper -> "Paper"
    | Scissors -> "Scissors"

  let string_of_result = function
    | ComputerWin -> "Computer Wins"
    | HumanWin -> "Human Wins"
    | Tie -> "Tie"

  let to_int = function
    | (Rock, Rock) -> 0
    | (Rock, Paper) -> 1
    | (Rock, Scissors) -> 2
    | (Paper, Rock) -> 3
    | (Paper, Paper) -> 4
    | (Paper, Scissors) -> 5
    | (Scissors, Rock) -> 6
    | (Scissors, Paper) -> 7
    | (Scissors, Scissors) -> 8
  let of_int = function
    | 0 -> (Rock, Rock)
    | 1 -> (Rock, Paper)
    | 2 -> (Rock, Scissors)
    | 3 -> (Paper, Rock)
    | 4 -> (Paper, Paper)
    | 5 -> (Paper, Scissors)
    | 6 -> (Scissors, Rock)
    | 7 -> (Scissors, Paper)
    | 8 -> (Scissors, Scissors)
    | _ -> failwith "Trying to cast integer higher than 8 into rps"
    (*}}}*)
end
(*}}}*)
(*{{{RPS related ngram predictors*)
module RpsGram : NGRAMTYPE = struct
  let num_elements = 9
  let pick_random () = Random.int num_elements
  let referee a = List.hd a
end
module HierGram : NGRAM_HIERARCHY = struct
  let window_sizes = [1;2;3]
  (*
   *let the highest widow size overrule the prediction. In the future use more
   *way. Maybe weights?
   *)
  let referee l = 
    (List.fold_right (fun (w, pick) (best_w, best_pick) ->
      if w > best_w then (w, pick)
      else (best_w, best_pick)) (List.tl l) (List.hd l) ) |> snd
end
(*}}}*)

exception User_exit
let rps_match () = 
  let module OnePredictor = NgramPredictor(RpsGram) in
  let module RpsPredictor = HierarchicalNGramPredictor(HierGram)(OnePredictor)
  in
  let max_win_size = max_list HierGram.window_sizes in
  let predictor = RpsPredictor.create_empty 0 in
  let stats = ref (0,0,0) in
  (*
   *the sequence_buffer should grow to the maximum window_size in our hierachical
   *ngram predictor. 
   *)
  (*fix the following crap code*)
  let sequence_buffer = ref [] in
  let response = ref "" in 
  try while true do
    print_endline "Your move:... \n";
    (*no cheating... we don't use human_move*)
    response := input_line stdin;
    if (not (List.mem !response ["r";"p";"s"])) || !response = "q" then raise User_exit;
    let human_move = RPS.rps_of_string !response in
    let likely_move = 
      let likely_move_int = 
        if (List.length !sequence_buffer) > 0 then
          RpsPredictor.predict_element predictor (Array.of_list !sequence_buffer)
        else begin
          print_endline "Doing initial random guess...";
          RpsGram.pick_random()
        end
      in likely_move_int |> RPS.of_int |> fst
    in
    let computer_move = RPS.beats likely_move in
    let rps_play = (human_move, computer_move) in 
    let result = RPS.fight rps_play in
    Printf.printf "Computer played %s. Winner is: %s\n" (RPS.string_of_rps
    computer_move) (RPS.string_of_result result) ;
    (*report shit here*)
    ( stats := match !stats with (win,lose, tie) -> match result with
    | RPS.HumanWin -> (succ win, lose, tie)
    | RPS.ComputerWin -> (win, succ lose, tie)
    | RPS.Tie -> (win, lose, succ tie));
    (*update sequence_buffer here*)
    sequence_buffer := !sequence_buffer @ [rps_play |> RPS.to_int];
    (*in the first run of the program sequence_buffer might be too short*)
    if (List.length !sequence_buffer) > 1 then
        RpsPredictor.register_sequence predictor (Array.of_list !sequence_buffer);
    (*
     *want to chop the first element out of the sequence buffer whenever it grows
     *too long
     *)
    if (List.length !sequence_buffer) > max_win_size then
      sequence_buffer := List.tl !sequence_buffer;
  done;
  with User_exit -> print_endline "Ended input session";
  match !stats with (w, l ,t) ->
    begin 
      let total = w + l + t in
      let calc_p x = ((float_of_int x) /. (float_of_int total)) *. 100.0 in
      let (wp, lp, tp) = (calc_p w, calc_p l, calc_p t) in
      Printf.printf "Results (Wins,Loses ,Ties) = (%f%%,%f%%,%f%%)" wp lp tp
    end;


