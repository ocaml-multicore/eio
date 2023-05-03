.PHONY: all bench stress

all:
	dune build @runtest @all

bench:
	dune exec -- ./bench/main.exe

test_posix:
	EIO_BACKEND=posix dune runtest

dscheck:
	dune exec -- ./lib_eio/tests/dscheck/test_rcfd.exe
	dune exec -- ./lib_eio/tests/dscheck/test_sync.exe
	dune exec -- ./lib_eio/tests/dscheck/test_semaphore.exe
	dune exec -- ./lib_eio/tests/dscheck/test_cells.exe

stress:
	dune exec -- ./stress/stress_proc.exe
	dune exec -- ./stress/stress_semaphore.exe

docker:
	docker build -t eio .
