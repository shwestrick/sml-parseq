.PHONY: shuffle shuffle.mpl

test: shuffle shuffle.mpl
	./shuffle 1000
	./shuffle 10000
	./shuffle 100000
	./shuffle 1000000
	./shuffle 10000000
	./shuffle.mpl @mpl procs 4 -- 1000
	./shuffle.mpl @mpl procs 4 -- 10000
	./shuffle.mpl @mpl procs 4 -- 100000
	./shuffle.mpl @mpl procs 4 -- 1000000
	./shuffle.mpl @mpl procs 4 -- 10000000

shuffle:
	mlton -default-type int64 shuffle.mlb

shuffle.mpl:
	mpl -default-type int64 shuffle.mpl.mlb
