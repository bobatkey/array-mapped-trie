type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val add : int -> 'a -> 'a t -> 'a t

val find : int -> 'a t -> 'a

val singleton : int -> 'a -> 'a t

val remove : int -> 'a t -> 'a t

val iter : (int -> 'a -> unit) -> 'a t -> unit

val fold : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
