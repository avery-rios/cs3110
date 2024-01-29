(** An [Engine] indexes words found in text files and answers
  queries about which files contain which words. *)
module type Engine = sig
  type idx
  (** The type of an index *)

  val index_of_words : (string * string list) list -> idx
  (** [index_of_list [(f0, ws0); ...(fn,ws)]] is index of file [fn] with words [wsn] *)

  val index_of_dir : string -> idx
  (** [index_of_dir d] is an index of the files in [d].  Only files whose
    names end in [.txt] are indexed.  Only [d] itself, not any
    of its subdirectories, is indexed.
    raises: Not_found if [d] is not a valid directory. *)

  val to_list : idx -> (string * string list) list
  (** [to_list idx] is a list representation of [idx] as an association
    list.  The first element of each pair in the list is a word,
    the second element is a list of the files in which that word
    appears.  The order of elements in both the inner and outer
    lists is unspecified.  Likewise, it is unspecified whether
    the outer list contains multiple entries for words that
    are the same other than case, or just a single entry. *)

  val or_not : idx -> string list -> string list -> string list
  (** [or_not idx ors nots] is a list of the files that contain
    any of the words in [ors] and none of the words in [nots].
    requires: [ors] is not empty. *)

  val and_not : idx -> string list -> string list -> string list
  (** [and_not idx ands nots] is a list of the files that contain
    all of the words in [ands] and none of the words in [nots].
    requires: [ands] is not empty. *)

  val format : Format.formatter -> idx -> unit
  (** [format] is a printing function suitable for use
    with the toplevel's [#install_printer] directive.
    It outputs a textual representation of an index
    on the given formatter. *)
end

module Word : sig
  val split_words : string -> string list
  (** [split_words s] is words in string [s], in lower case and sorted,
    with no duplicate *)
end

module ListEngine : Engine
(** An engine implemented with list-based data structures.
  Its performance is likely to be slow. *)

module TreeEngine : Engine
(** An engine implemented with balanced-tree-based data structures.
  Its performance is asymptotically more efficient
  than [ListEngine]. *)
