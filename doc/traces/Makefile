all: both-posix.svg cancel-posix.svg

%.svg: %.fxt
	eio-trace render -f "$<" "$@"
