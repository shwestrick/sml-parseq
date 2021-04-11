structure Seq:
sig
  type 'a t = 'a ArraySlice.slice
  type 'a seq = 'a t

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
end =
struct
  structure A = Array
  structure AS = ArraySlice
  structure SSU = SampleSortUtil

  type 'a t = 'a ArraySlice.slice
  type 'a seq = 'a t

  val gran = 10000

  fun nth s i = AS.sub (s, i)
  fun length s = AS.length s

  fun empty () = AS.full (A.fromList [])
  fun fromList xs = ArraySlice.full (Array.fromList xs)
  fun toList s = List.tabulate (length s, nth s)

  fun toString f s =
    String.concatWith "," (List.map f (toList s))

  fun subseq s (i, n) = AS.subslice (s, i, SOME n)
  fun take s k = subseq s (0, k)
  fun drop s k = subseq s (k, length s - k)

  fun tabulate f n = AS.full (SeqBasis.tabulate gran (0, n) f)

  fun map f s = tabulate (fn i => f (nth s i)) (length s)

  fun rev s = tabulate (fn i => nth s (length s - i - 1)) (length s)

  fun append (s, t) =
    tabulate (fn i => if i < length s then nth s i else nth t (i - length s))
      (length s + length t)

  fun iterate f b s =
    SeqBasis.foldl f b (0, length s) (nth s)

  fun foreach s f =
    ForkJoin.parfor gran (0, length s) (fn i => f (i, nth s i))

  fun scan f b s =
    let
      val n = AS.length s
      val r = SeqBasis.scan gran f b (0, n) (nth s)
    in
      (AS.slice (r, 0, SOME n), A.sub (r, n))
    end

  fun scanIncl f b s =
    let
      val n = AS.length s
      val r = SeqBasis.scan gran f b (0, n) (nth s)
    in
      AS.slice (r, 1, NONE)
    end

  fun reduce f b s =
    SeqBasis.reduce gran f b (0, length s) (nth s)

  fun filter p s =
    AS.full (SeqBasis.filter gran (0, length s) (nth s) (p o nth s))

  fun equal eq (s, t) =
    length s = length t andalso
    SeqBasis.reduce gran (fn (a, b) => a andalso b) true (0, length s)
      (fn i => eq (nth s i, nth t i))

  fun flatten s =
    let
      val offsets = AS.full (SeqBasis.scan 10000 op+ 0 (0, length s) (length o nth s))
      val total = nth offsets (length s)
      val output = ForkJoin.alloc total
    in
      ForkJoin.parfor 100 (0, length s) (fn i =>
        let
          val t = nth s i
          val off = nth offsets i
        in
          foreach t (fn (j, x) => A.update (output, off + j, x))
        end);

      AS.full output
    end

  val binarySearch = BinarySearch.search
  val merge = Merge.merge
  val mergesort = MergeSort.sort
  val quicksort = QuickSort.sort

  (** =======================================================================
    * Author: Guy Blelloch
    * Basically the cache-oblivious sorting algorithm from:
    *   Low depth cache-oblivious algorithms.
    *   Guy E. Blelloch, Phillip B. Gibbons and Harsha Vardhan Simhadri.
    *   SPAA 2010
    * The main difference is that it does not recurse (using quicksort instead)
    * and the merging with samples is sequential.
    *)
  fun samplesort cmp A =
    let
      val n = AS.length A

      (* parameters used in algorithm *)
      val bucket_quotient = 3
      val block_quotient = 2
      val sqrt = Real.floor(Math.sqrt(Real.fromInt n))
      val num_blocks = sqrt div block_quotient
      val block_size = ((n-1) div num_blocks) + 1
      val num_buckets = (sqrt div bucket_quotient) + 1
      val over_sample = 1 + ((n div num_buckets) div 500)
      val sample_size = num_buckets * over_sample
      val sample_stride = n div sample_size
      val m = num_blocks*num_buckets

      (* sort a sample of keys *)
      val sample = tabulate (fn i => AS.sub (A, i*sample_stride)) sample_size
      val _ = QuickSort.sortInPlace cmp sample

      (* take a subsample *)
      val sub_sample = tabulate (fn i => AS.sub (sample, (i+1)*over_sample))  (num_buckets-1)

      val counts = AS.full (ForkJoin.alloc m)
      val B = AS.full (ForkJoin.alloc n)

      (* sort each block and merge with the pivots, giving a count of the number
         of keys between each pivot in each block *)
      val _ =
      ForkJoin.parfor 1 (0,num_blocks) (fn i =>
        let
          val start = i * block_size
          val len = Int.min((i+1)* block_size,n) - start
          (* copy into B to avoid changing A *)
          val _ = SeqBasis.for (start, start+len) (fn j =>
            AS.update(B, j, AS.sub (A, j)))
          val B' = subseq B (start, len)
          val _ = QuickSort.sortInPlace cmp B'
          val counts' = subseq counts (i*num_buckets, num_buckets)
          val _ = SSU.mergeWithSamples cmp (B', sub_sample, counts')
        in () end)

      (*  scan across the counts to get offset of each source bucket within each block *)
      val (source_offsets,_) = scan op+ 0 counts

      (*  transpose and scan across the counts to get offset of each
          destination within each bucket *)
      val tcounts = SSU.transpose(counts,num_blocks,num_buckets)
      val (dest_offsets,_) = scan op+ 0 tcounts

      (*  move data to correct destination *)
      val C = SSU.transposeBlocks(B, source_offsets, dest_offsets,
                                  counts, num_blocks, num_buckets, n)

      (* get the start location of each bucket *)
      val bucket_offsets =
        tabulate (fn i => if (i = num_buckets) then n
                              else AS.sub (dest_offsets, i *  num_blocks))
          (num_buckets+1)
      (* sort the buckets *)
      val _ =
        ForkJoin.parfor 1 (0, num_buckets) (fn i =>
          let
            val start = AS.sub (bucket_offsets, i)
            val len = (AS.sub (bucket_offsets, i+1)) - start
          in
            QuickSort.sortInPlace cmp (subseq C (start,len))
          end)

    in
      C
    end

end
