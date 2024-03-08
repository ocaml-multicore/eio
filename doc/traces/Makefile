all: both-posix.svg cancel-posix.svg switch-mock.svg net-posix.svg multicore-posix.svg

%.svg: %.fxt
	eio-trace render "$<"
