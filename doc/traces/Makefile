all: both-posix.svg cancel-posix.svg switch-mock.svg

%.svg: %.fxt
	eio-trace render -f "$<" "$@"
