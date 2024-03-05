---
layout: post
title: "Getting started with Erlang's `maybe_expr`"
date:   2024-03-04
categories: jekyll blog
---

## Assumptions

You are using [rebar3][rebar3] to build your project. You are using OTP 25.

## Notes

If you are on OTP 26, you can skip [Setting up rebar3][#setting-up-rebar3]

## Setting up rebar3

Credit to [this forum entry][erlangforums] for the details. First create the
file `config/vm.args` if it doesn't exist and add,

```
-enable-feature maybe_expr
```

Set this in your environment,

```
export ERL_FLAGS="-args_file config/vm.args"
```

Or, prepend `ERL_FLAGS="-args_file config/vm.args"` to your `rebar3` commands

## Enabling the enable

In your Erlang files you only need to add (after your module definition),

```erlang
-module(...).
-feature(maybe_expr, enable).
```

## What can I use `maybe_expr` for?

...

[rebar3]: https://rebar3.org
[erlangforums]: https://erlangforums.com/t/how-to-enable-maybe-expr/2154
[erlang-manual]: https://www.erlang.org/doc/reference_manual/expressions#maybe
