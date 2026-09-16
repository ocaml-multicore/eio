.PHONY: all bench stress

all:
	dune build @runtest @all

docs:
	dune build @install && dune exec -- odoc_driver --remap --html-dir=_build/odoc eio eio_linux eio_posix eio_windows eio_main

bench:
	dune exec -- ./bench/main.exe

test_posix:
	EIO_BACKEND=posix dune runtest

dscheck:
	dune exec -- ./lib_eio/tests/dscheck/test_condition.exe
	dune exec -- ./lib_eio/tests/dscheck/test_rcfd.exe
	dune exec -- ./lib_eio/tests/dscheck/test_sync.exe
	dune exec -- ./lib_eio/tests/dscheck/test_semaphore.exe
	dune exec -- ./lib_eio/tests/dscheck/test_cells.exe

stress:
	dune exec -- ./stress/stress_proc.exe
	dune exec -- ./stress/stress_semaphore.exe
	dune exec -- ./stress/stress_release.exe

docker:
	docker build -t eio .
