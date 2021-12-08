.PHONY: all bench

all:
	dune build @runtest @all

bench:
	dune exec -- ./bench/bench_yield.exe
	dune exec -- ./bench/bench_promise.exe
	dune exec -- ./bench/bench_stream.exe
	dune exec -- ./bench/bench_semaphore.exe
	dune exec -- ./lib_eio_linux/tests/bench_noop.exe
