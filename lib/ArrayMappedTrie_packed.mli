type 'a t

val empty : 'a t

val add : int -> 'a -> 'a t -> 'a t

val find : int -> 'a t -> 'a
