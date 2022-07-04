.PHONY: all bench

all:
	dune build @runtest @all

bench:
	dune exec -- ./bench/bench_buf_read.exe
	dune exec -- ./bench/bench_mutex.exe
	dune exec -- ./bench/bench_yield.exe
	dune exec -- ./bench/bench_promise.exe
	dune exec -- ./bench/bench_stream.exe
	dune exec -- ./bench/bench_semaphore.exe
	dune exec -- ./bench/bench_cancel.exe
	dune exec -- ./lib_eio_linux/tests/bench_noop.exe

test_luv:
	rm -rf _build
	EIO_BACKEND=luv dune runtest

docker:
	docker build -t eio .
