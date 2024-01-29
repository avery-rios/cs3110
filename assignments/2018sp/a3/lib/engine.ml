module type Engine = sig
  type idx

  val index_of_words : (string * string list) list -> idx
  val index_of_dir : string -> idx
  val to_list : idx -> (string * string list) list
  val or_not : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end

module Path : sig
  type t = private string

  val of_string : string -> t
  (** [make s] convert [s] into a path *)

  val ( </> ) : t -> t -> t
  (** [d </> f] is path of [f] in dir [d] *)

  val has_extension : string -> t -> bool
  (** [has_extension e p] returns [true] if [p] has extension [e] *)

  val list_dir : t -> t list
  (** [list_dir p] returns filenames in directory [p] *)

  val read_file : t -> string
  (** [read_file f] returns content of file [f] *)

  val compare : t -> t -> [ `LT | `EQ | `GT ]
  val format : Format.formatter -> t -> unit
end = struct
  type t = string

  let of_string s = s
  let ( </> ) = Filename.concat
  let has_extension e p = Filename.extension p = e

  let rec list_dir_i acc fd =
    match Unix.readdir fd with
    | s -> list_dir_i (s :: acc) fd
    | exception End_of_file -> acc

  let list_dir d =
    let fd = Unix.opendir d in
    let cont = list_dir_i [] fd in
    Unix.closedir fd;
    cont

  let read_file p =
    let fd = Unix.openfile p [ Unix.O_RDONLY ] 0 in
    let size = (Unix.fstat fd).st_size in
    let chan = Unix.in_channel_of_descr fd in
    let ret = really_input_string chan size in
    close_in chan;
    ret

  let compare p1 p2 =
    let c = String.compare p1 p2 in
    if c < 0 then `LT else if c > 0 then `GT else `EQ

  let format = Format.pp_print_string
end

module Word = struct
  type t = string

  let compare s1 s2 =
    let c = String.compare s1 s2 in
    if c < 0 then `LT else if c > 0 then `GT else `EQ

  let format = Format.pp_print_string

  (** [prewords s] split [s] into a list of prewords *)
  let prewords s =
    let r = Str.regexp "[ \n\t]" in
    Str.split r s

  (** [word pw] returns [Some w] is [w] is word in preword [pw], returns [None] is
      [pw] contains no word  *)
  let word s =
    let bound = Str.regexp "[a-zA-Z0-9]" in
    let l = String.length s in
    try
      let sp = Str.search_forward bound s 0 in
      let ep = Str.search_backward bound s (l - 1) in
      Some (String.sub s sp (ep + 1 - sp))
    with Not_found -> None

  let split_words s =
    prewords s |> List.filter_map word
    |> List.map String.lowercase_ascii
    |> List.sort_uniq String.compare

  let fold_words f acc s =
    prewords s
    |> List.fold_left
         (fun a pw ->
           match word pw with
           | Some w -> f (String.lowercase_ascii w) a
           | None -> a)
         acc
end

module MakeEngine =
functor
  (S : Data.Set with type Elt.t = Path.t)
  (D : Data.Dictionary with type Key.t = string and type Value.t = S.t)
  ->
  struct
    type idx = D.t

    (** [add_word w p i] add word [w] in file [p] to index [i] *)
    let add_word p w i =
      D.insert w (D.find w i |> Option.value ~default:S.empty |> S.insert p) i

    (** [add_words p idx ws] is index with words [ws] in file [p]*)
    let add_words p i ws = List.fold_left (fun a w -> add_word p w a) i ws

    let index_of_words ws =
      List.fold_left
        (fun acc (p, ws) -> add_words (Path.of_string p) acc ws)
        D.empty ws

    let index_of_dir d =
      let open Path in
      let pd = Path.of_string d in
      list_dir pd
      |> List.filter (has_extension ".txt")
      |> List.map (fun f ->
             let p = pd </> f in
             (p, read_file p))
      |> List.fold_left
           (fun acc (p, c) -> Word.fold_words (add_word p) acc c)
           D.empty

    let to_list idx =
      D.to_list idx
      |> List.map (fun (w, ps) -> (w, (S.to_list ps :> string list)))

    (** [find idx w] returns [Some s] if [w] is contains in paths set [s], [None]
        is [s] is not in index *)
    let find idx w = D.find (String.lowercase_ascii w) idx

    (** [proc_not idx ws r] is path set with path of words [ws] in index [idx] removed *)
    let proc_not idx ns r =
      List.filter_map (find idx) ns |> List.fold_left S.difference r

    (** [list_of_path_set ps] is string list of path set [ps] *)
    let list_of_path_set ps = (S.to_list ps :> string list)

    let or_not idx ors nots =
      List.filter_map (find idx) ors
      |> List.fold_left S.union S.empty
      |> proc_not idx nots |> list_of_path_set

    (** [proc_ands idx s ws] returns [Some s], [s] is path set that contains 
        all of words [ws], returns [None] if no file contains all of words [ws] *)
    let rec proc_ands idx acc = function
      | [] -> Some acc
      | h :: t -> (
          match find idx h with
          | None -> None
          | Some s -> proc_ands idx (S.intersect acc s) t)

    let and_not idx ands nots =
      match ands with
      | [] -> []
      | h :: t -> (
          match Option.bind (find idx h) (fun it -> proc_ands idx it t) with
          | None -> []
          | Some s -> proc_not idx nots s |> list_of_path_set)

    let format fmt idx = D.format fmt idx
  end

module MakeE (DM : Data.DictionaryMaker) = struct
  module PathSet = Data.MakeSetOfDictionary (Path) (DM)
  module M = MakeEngine (PathSet) (DM (Word) (PathSet))
  include M
end

module ListEngine = MakeE (Data.MakeListDictionary)
(* TODO: replace [TrivialEngine] in the line above with
   an application of [MakeEngine] to some appropriate parameters. *)

module TreeEngine = MakeE (Data.MakeTreeDictionary)
(* TODO: replace [TrivialEngine] in the line above with
   an application of [MakeEngine] to some appropriate parameters. *)
