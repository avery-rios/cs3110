Since `@` operator is linear time to the length of queue, and `ListQueue.enqueue`
requires uses `@` operator once and other operation takes constant time, so
`ListQueue.enqueue` is linear time to the length of queue.

Adding one element to `ListQueue` of length $l$ takes $l$ time, adding $n$ element
takes $1 + 2 + \dots + n = \frac{n (n - 1)}{2}$ time, that's quadratic in $n$.

`BatchedQueue.enqueue` only use constant times of constant time operation, so it
takes constant time.

Adding one element to `BatchedQueue` is constant time, so adding $n$ element to it
takes linear time.
