type 'a t

val create : unit -> 'a t

val add : int -> 'a -> 'a t -> unit

val find : int -> 'a t -> 'a
