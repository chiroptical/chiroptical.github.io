---
layout: post
title: 'C++ Recursive Template Metaprogramming: Fibonacci Numbers'
date:   2018-07-01
categories: jekyll blog
---

Background
---

After a brief dive into Scala, I am back to writing C++. However, I do have a
much better appreciation for functional programming and recursion. I am far
from an expert at either, but I am interested in increasing my programming
skills. I decided to revive my blog and try to post things I find fun or
interesting. I am currently reading "Effective Modern C++" by Scott Meyers and
continually come across Metaprogramming online. I was poking around Stack
Overflow and I found [this
post](https://stackoverflow.com/questions/22449902/tail-recursion-performance-on-template-meta-programming)
which asks about tail recursion in Template Metaprogramming (TMP). I thought
this was interesting and decided to see if I could write the naive recursive
Fibonacci number generator using TMP.

I had already written this in Scala, which looks like:

```scala
import scala.annotation.tailrec
def fib(n: Int): Int = {
    @tailrec
    def loop(iter: Int, prev: Int, next: Int): Int = {
        if (iter >= n) prev
        else loop(iter + 1, next, prev + next)
    }
    loop(0, 0, 1)
}
fib(10)
```

However, `fib(10)` will execute at runtime and the Java Virtual Machine occurs
additional runtime overhead each time you run the program. A neat benefit of
TMP in C++ is the compiler can compute `fib(10)` and then each invocation of
the program is as simple as printing an integer. My first implementation in
C++, looked like:

```cpp
#include <cstdint>
#include <iostream>

namespace impl {

    template<int64_t n, bool isPositive>
    struct fib_impl {
        static constexpr int64_t val = fib_impl<n - 1, isPositive>::val + fib_impl<n - 2, isPositive>::val;
    };

    template<>
    struct fib_impl<1, true> {
        static constexpr int64_t val = 1;
    };

    template<>
    struct fib_impl<0, true> {
        static constexpr int64_t val = 0;
    };

    // If calling fib<-1>::val it will try to do the recursion infinitely
    // -> this template short circuits that recursion
    template<int64_t n>
    struct fib_impl<n, false> {
        static constexpr int64_t val = -1;
    };

} // namespace impl

template<int64_t n>
struct fib {
    static_assert(n >= 0, "Error: fib can't be called with a negative integer");
    static constexpr int64_t val = impl::fib_impl<n, (n >= 0)>::val;
};

int main() {
//    static_assert(fib<-1>::val); // This will fail.
//    static_assert(fib<10>::val == 55); // Make sure it works at compile time!
    std::cout << fib<91>::val << '\n';
    return 0;
}
```

I want the interface of `fib` to accept only a positive integer, therefore we
abstract away whether, or not, the integer is positive with `impl::fib_impl`.
In this implementation, you need 3 template specializations. Two are the
termination conditions: 0 and 1; the other provides protection from an infinite
recursion when you give a negative number to `fib`. Even though you get an
error from the `static_assert(fib<-1>::val)`, the compiler still tries to
create infinite templates. Luckily, your compiler will protect you from
creating literally infinite templates (GCC 7.2.1 allowed 900 to be generated,
use `-ftemplate-depth=<value>` to change it). This implementation isn't tail
recursive because the recursion isn't in the tail position. The recursive call,

```cpp
fib_impl<n - 1, isPositive>::val + fib_impl<n - 2, isPositive>::val
```

is shaped like `recursive_template(...) + recursive_template(...)`, but must
look like: `recursive_template(...)` to be tail recursive. You can verify this
by modifying the Scala code. In C++, I believe the only way to find out if tail
recursion is actually applied is looking at the assembly for loops.
Unfortunately, this is done at compile time and you can't review the compile
time assembly (to my knowledge). The tail recursive implementation is:

```cpp
#include <cstdint>
#include <iostream>

namespace impl {

    template <int64_t n, int64_t prev, int64_t next, bool isPositive>
    struct fib_impl {
        static constexpr int64_t val = fib_impl<n - 1, next, prev + next, isPositive>::val;
    };

    template <int64_t prev, int64_t next>
    struct fib_impl<0, prev, next, true> {
        static constexpr int64_t val = prev;
    };

    template <int64_t n, int64_t prev, int64_t next>
    struct fib_impl<n, prev, next, false> {
        static constexpr int64_t val = -1;
    };

} // namespace impl


template <int64_t n>
struct fib {
    static_assert(n >= 0, "Error: fib can't be called with negative numbers!");
    static constexpr int64_t val = impl::fib_impl<n, 0, 1, (n >= 0)>::val;
};

int main() {
//    static_assert(fib<-1>::val); // This will fail.
//    static_assert(fib<10>::val == 55); // Make sure it works at compile time
    std::cout << fib<91>::val << '\n';
    return 0;
}
```

Great, now the recursive call is in the tail position. Additionally, we only
need 2 template specializations. The one where `n = 0` and the infinite
template recursion protection for negative integers. I compiled both versions
with GCC 7.2.1 using the C++11 standard (which is necessary for `constexpr`) 10
times and measured the average compile time. It was essentially the same (about
0.2s). The tail recursive version has a major downside though: it overflows a
`int64_t` faster than the non-tail recursive version. The largest value of `n`
for the non-tail recursive version was 92 and for the tail recursive version
was 91. The reason for this is because the template recursion for
`fib<92>::val` contains a `prev + next` which would contain a value to large to
fit in `int64_t`.

This code was an academic exercise, but I think it is neat. This is my first
experience with TMP and I am very interested to learn more. Feel free to
message me, or follow me, on Twitter with constructive criticism or for future
blog posts.
