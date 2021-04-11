A Standard ML library for parallel sequences, designed for the
[smlpkg](https://github.com/diku-dk/smlpkg) package manager. Sequences are
implemented by mutable arrays under the hood. The interface is purely
functional, and includes a variety of standard functions: `tabulate`, `map`,
`reduce`, `filter`, etc. A secondary interface is included to more directly
manipulate mutable arrays.

This library is well optimized and---if compiled with
[MPL](https://github.com/mpllang/mpl)---highly parallel.

## Library sources

There are two source files:
  - `lib/github.com/shwestrick/sml-parseq/sml-parseq.mlb`
  - `lib/github.com/shwestrick/sml-parseq/sml-parseq.mpl.mlb`

The `.mlb` is for use with normal SML (e.g. [MLton](http://mlton.org/))
and the `.mpl.mlb` is for use with [MPL](https://github.com/mpllang/mpl).
Both supply the same interface, described below.

## Interface

```sml
structure Seq:
sig
  type 'a t = 'a ArraySlice.slice
  type 'a seq = 'a t
  (** The sequence type. The representation is exposed for convenience,
    * although this is of course a bit dangerous if using parallelism.
    * Don't mutate unless you know what you're doing!
    *
    * The type name `seq` is for readability in the signature. When using
    * the library, it is recommended to keep the structure closed, and use
    * `Seq.t`, for example:
    *
    *   val x: int Seq.t = ...
    *)

  val nth: 'a seq -> int -> 'a
  val length: 'a seq -> int

  val empty: unit -> 'a seq
  val fromList: 'a list -> 'a seq
  val toList: 'a seq -> 'a list
  val equal: ('a * 'a -> bool) -> ('a seq * 'a seq) -> bool

  val toString: ('a -> string) -> 'a seq -> string

  val subseq: 'a seq -> (int * int) -> 'a seq
  val take: 'a seq -> int -> 'a seq
  val drop: 'a seq -> int -> 'a seq

  val tabulate: (int -> 'a) -> int -> 'a seq
  val map: ('a -> 'b) -> 'a seq -> 'b seq
  val rev: 'a seq -> 'a seq
  val append: 'a seq * 'a seq -> 'a seq
  val filter: ('a -> bool) -> 'a seq -> 'a seq
  val flatten: 'a seq seq -> 'a seq

  val iterate: ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b

  val reduce: ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a
  val scan: ('a * 'a -> 'a) -> 'a -> 'a seq -> ('a seq * 'a)
  val scanIncl: ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq

  val foreach: 'a seq -> (int * 'a -> unit) -> unit

  val binarySearch: ('a * 'a -> order) -> 'a seq -> 'a -> int
  val merge: ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq

  (** A few different sorting algorithms. *)
  val samplesort: ('a * 'a -> order) -> 'a seq -> 'a seq
  val mergesort: ('a * 'a -> order) -> 'a seq -> 'a seq
  val quicksort: ('a * 'a -> order) -> 'a seq -> 'a seq

  val countingsort: 'a seq
                 -> (int -> int)      (* bucket id of ith element *)
                 -> int               (* number of buckets *)
                 -> 'a seq * int seq  (* sorted, bucket offsets *)
end
```

```sml
structure SeqBasis:
sig
  type grain = int
  (** Granularity control parameters always come first, if the function
    * needs one.
    *)

  val for: (int * int)
        -> (int -> unit)
        -> unit

  val foldl: ('b * 'a -> 'b)
          -> 'b
          -> (int * int)
          -> (int -> 'a)
          -> 'b

  val tabulate: grain
             -> (int * int)
             -> (int -> 'a)
             -> 'a array
  (** `tabulate grain (i, j) f` produces the array [f(i), f(i+1), ..., f(j-1)].
    *)

  val reduce: grain
           -> ('a * 'a -> 'a)
           -> 'a
           -> (int * int)
           -> (int -> 'a)
           -> 'a
  (** `reduce grain f b (i, j) g` computes the sum of `g(k)` for `i <= k < j`,
    * where the sum is taken with respect to f. Note that function f must be
    * associative, i.e. f(x,f(y,z)) = f(f(x,y),z). The value `b` should be
    * a left-identity for f: f(b,x) = f(x)
    *)

  val scan: grain
         -> ('a * 'a -> 'a)
         -> 'a
         -> (int * int)
         -> (int -> 'a)
         -> 'a array
  (** Similar to reduce, but produces all prefix sums. The output is size
    * N+1, for both "inclusive" and "exclusive" versions of scan, i.e. the
    * 0th output element is the identity, and the Nth output element is the
    * same as the output of `reduce`.
    *)

  val filter: grain
           -> (int * int)
           -> (int -> 'a)
           -> (int -> bool)
           -> 'a array
  (** `filter grain (i, j) f p` is essentially the same as
    * `tabulate grain (i, j) f` except that each element f(k) is only included
    * in the output if the predicate p(k) is satisfied. The relative order of
    * the output elements is preserved.
    *)

  val tabFilter: grain
              -> (int * int)
              -> (int -> 'a option)
              -> 'a array
  (** A slightly different type for filter. The difference is that
    * `tabFilter grain (i, j) f` guarantees that each f(k) is only evaluated
    * exactly once, which is more efficient than a normal filter when the
    * function is expensive.
    *)
end
```
