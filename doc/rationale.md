This document collects some of the reasons behind various API choices in Eio.

# Scheduling order

When forking a new fibre, there are several reasonable scheduling behaviours:

1. Append the new fibre to the run queue and then continue running the parent.
2. Append the parent fibre to the run queue and start the child immediately.
3. Append both old and new fibres to the run-queue (in some order), then schedule the next task at the queue's head.
4. Prepend the old and new fibres to the *head* of the run-queue and resume one of them immediately.

And several desirable features:

- Especially for `Fibre.both`, putting both at the start or both at the end of the run-queue seems more consistent
  that starting one before everything in the queue and the other after.
- Adding both to the head of the queue is the most flexible, since the other behaviours can then be achieved by yielding.
- Putting both at the head may lead to starvation of other fibres.
- Running the child before the parent allows the child to e.g. create a switch and store it somewhere atomically.
- Scheduling new work to run next can make better use of the cache in some cases (domainslib works this way).

Therefore, `Fibre.fork f` runs `f` immediately and pushes the calling fibre to the head of the run-queue.
After making this change, the examples in the README seemed a bit more natural too.
