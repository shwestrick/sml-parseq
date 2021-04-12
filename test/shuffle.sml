val n = valOf (Int.fromString (List.hd (CommandLine.arguments ())))
val _ = print (Int.toString n ^ "\n")

val elems = Seq.tabulate (fn i => i) n
val result = Seq.shuffle elems 15210

val flags = Seq.tabulate (fn i => false) n
val _ = Seq.foreach result (fn (_, x) => ArraySlice.update (flags, x, true))
val result = Seq.reduce (fn (a, b) => a andalso b) true flags

val _ = print ("all found? " ^ (if result then "yes" else "no") ^ "\n")
