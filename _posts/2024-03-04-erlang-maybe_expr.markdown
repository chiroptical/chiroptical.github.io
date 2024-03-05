---
layout: post
title: "Getting started with Erlang's `maybe_expr`"
date:   2024-03-04
categories: jekyll blog
---

## Assumptions

You are using [rebar3][rebar3] to build your project. You are using OTP 25.

## Introduction

I am an Erlang beginner and I am currently building a Slack bot to learn more.
Here is some code I wrote recently,

```erlang
{ok, ChannelId} = map_utils:recursive_get([<<"channel">>, <<"id">>], Payload),
{ok, UserId} = map_utils:recursive_get([<<"user">>, <<"id">>], Payload),
{ok, TeamId} = map_utils:recursive_get([<<"team">>, <<"id">>], Payload),
{ok, ResponseUrl} = map_utils:recursive_get([<<"response_url">>], Payload),
```

The `Payload` here is a decoded JSON body from Slack. The `map_utils:recursive_get/2` function
takes the path to a JSON entry and extracts it if possible, given this JSON

```json
{
  "channel": {
    "id": "value"
  }
}
```

If we ran this JSON through my HTTP handlers, this code would succeed,

```erlang
{ok, <<"value">>} = map_utils:recursive_get([<<"channel">>, <<"id">>]),
{error, not_found} = map_utils:recursive_get([<<"hello">>, <<"world">>]),
```

When the `ChannelId`, `UserId`, etc are all extracted from the `Payload`
properly everything is great. However, if any of the pattern matches fails
everything seems to get dropped into the void. This is obviously problematic
when you are building an application. Thankfully, I discovered `maybe_expr`!

With `maybe_expr`, the code will look more like this,

```erlang
-record(interact_payload, {channel_id, user_id, team_id, response_url})

% ...

maybe
  {ok, ChannelId} ?= map_utils:recursive_get([<<"channel">>, <<"id">>], Payload),
  {ok, UserId} ?= map_utils:recursive_get([<<"user">>, <<"id">>], Payload),
  {ok, TeamId} ?= map_utils:recursive_get([<<"team">>, <<"id">>], Payload),
  {ok, ResponseUrl} ?= map_utils:recursive_get([<<"response_url">>], Payload)
  {ok, #interact_payload{channel_id = ChannelId, user_id = UserId, team_id = TeamId, response_url = ResponseUrl}}
else
  {error, not_found} ->
    logger:error(...),
    {error, not_found};
  {error, not_found, Reason} ->
    logger:error(...),
    {error, not_found}
end,
```

Instead of dropping anything into the void, the `else` clause can be used to pattern
match out any failure cases. Here, we match `{error, not_found}` and `{error,
not_found, Reason}` and log that we had an unexpected error.

This feature is currently ["experimental" in OTP 25][erlang-manual]. However,
it is becoming standardized over the next few OTP releases. See this
[highlight][otp-26-highlights] for more information.

## Setting up rebar3

Credit to [this forum entry][erlangforums] for the details. With OTP 25, first
create the file `config/vm.args` if it doesn't exist and add,

```
-enable-feature maybe_expr
```

Then set this in your environment,

```
export ERL_FLAGS="-args_file config/vm.args"
```

Or, prepend `ERL_FLAGS="-args_file config/vm.args"` to your `rebar3` commands.
Reminder, you can skip this step in OTP 26.

## Enabling the feature

In your Erlang files you only need to add (after your module definition),

```erlang
-module(...).
-feature(maybe_expr, enable).
```

Done. Supposedly in [OTP 27][otp-27] you won't need to do either of these steps!

## More information

You can read the Erlang Enhancement Process proposal [here][eep].

If you've ever written Haskell before this is essentially [`ExceptT e IO
a`][except-t] where the `e` can be literally anything. It is your job in Erlang
to catch all the cases. You can add type checking to your Erlang code with
something like [eqWAlizer][eqwalizer] and type annotations via dialyzer. I was
first exposed to type annotations in [LYAH's dialyzer
introduction][dialyzer-intro].

If there is anything you are curious about in Erlang, please ask me about it on
[BlueSky][bluesky] or the [Erlanger's Slack][erlangers-invite]. I would like to
write more blog posts and learn more about Erlang.

[rebar3]: https://rebar3.org
[erlangforums]: https://erlangforums.com/t/how-to-enable-maybe-expr/2154
[erlang-manual]: https://www.erlang.org/doc/reference_manual/expressions#maybe
[otp-26-highlights]: https://www.erlang.org/blog/otp-26-highlights/#no-need-to-enable-feature-maybe-in-the-runtime-system
[eep]: https://www.erlang.org/eeps/eep-0049
[otp-27]: https://www.erlang.org/news/167#compiler-and-jit-improvements
[except-t]: https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Except.html#g:2
[eqwalizer]: https://github.com/WhatsApp/eqwalizer
[dialyzer-intro]: https://learnyousomeerlang.com/dialyzer#plt
[bluesky]: https://bsky.app/profile/chiroptical.dev
[erlangers-invite]: https://erlef.org/slack-invite/erlanger
