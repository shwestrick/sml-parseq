(** Original author: Guy Blelloch
  * Refactored by: Sam Westrick
  *
  * Helper functions for a cache-oblivious samplesort.
  *)
structure SampleSortUtil:
sig
  type 'a seq = 'a ArraySlice.slice

  (* transpose (matrix, numRows, numCols) *)
  val transpose: 'a seq * int * int -> 'a seq

  (* transposeBlocks (blockMatrix, srcOffsets, dstOffsets, counts, numRows, numCols, n) *)
  val transposeBlocks:
    'a seq * int seq * int seq * int seq * int * int * int -> 'a seq

  val mergeWithSamples:
    ('a * 'a -> order) -> ('a seq * 'a seq * int seq) -> unit

end =
struct

  structure A = Array
  structure AS = ArraySlice

  type 'a seq = 'a ArraySlice.slice

  val sub = A.sub
  val update = A.update

  val for = SeqBasis.for
  val parallelFor = ForkJoin.parfor
  val par = ForkJoin.par

  fun for_l (lo, len) f = for (lo, lo + len) f

  fun matrixDandC baseCase (threshold, num_rows, num_cols) =
    let fun r(rs, rl, cs, cl) =
          if (rl*cl < threshold) then baseCase(rs, rl, cs, cl)
          else if (cl > rl) then
            (par (fn () => r(rs, rl, cs, cl div 2),
                  fn () => r(rs, rl, cs + (cl div 2), cl - (cl div 2))); ())
          else
            (par (fn () => r(rs, rl div 2, cs, cl),
                  fn () => r(rs + (rl div 2), rl - (rl div 2), cs, cl)); ())
    in r(0, num_rows, 0, num_cols) end

  (* transposes a matrix *)
  fun transpose(S, num_rows, num_cols) =
    let
      val seq_threshold = 8000
      val (SS, offset, n) = AS.base S
      val _ = if (AS.length S) <> (num_rows * num_cols) then raise Size else ()
      val R = ForkJoin.alloc (num_rows * num_cols)
      fun baseCase(row_start, row_len, col_start, col_len) =
          for_l (row_start, row_len) (fn i =>
            for_l (col_start, col_len) (fn j =>
               update(R, j * num_rows + i, sub(SS,(i*num_cols + j + offset)))))
    in (matrixDandC baseCase (seq_threshold, num_rows, num_cols);
        AS.full(R))
    end

  (* transposes a matrix of blocks given source and destination pairs *)
  fun transposeBlocks(S, source_offsets, dest_offsets, counts, num_rows, num_cols, n) =
    let
      val seq_threshold = 500
      val (SS, offset, n) = AS.base S
      val R = ForkJoin.alloc n
      fun baseCase(row_start, row_len, col_start, col_len) =
          for (row_start, row_start + row_len) (fn i =>
            for (col_start, col_start + col_len) (fn j => let
                   val pa = offset + AS.sub (source_offsets, i*num_cols + j)
                   val pb = AS.sub (dest_offsets, j*num_rows + i)
                   val l = AS.sub (counts, i*num_cols + j)
                 in for (0,l) (fn k => update(R,pb+k,sub(SS, pa + k))) end))
    in (matrixDandC baseCase (seq_threshold, num_rows, num_cols);
        AS.full(R))
    end

  (* merges a sequence of elements A with the samples S, putting counts in C *)
  fun mergeWithSamples cmp (A, S, C) =
    let
      val num_samples = AS.length S
      val n = AS.length A
      fun merge(i,j) =
        if (j = num_samples) then AS.update(C,j,n-i)
        else
          let fun merge'(i) = if (i < n andalso cmp(AS.sub (A, i), AS.sub (S, j)) = LESS)
                              then merge'(i+1)
                              else i
              val k = merge'(i)
              val _ = AS.update(C, j, k-i)
          in merge(k,j+1) end
  in merge(0,0) end

end
