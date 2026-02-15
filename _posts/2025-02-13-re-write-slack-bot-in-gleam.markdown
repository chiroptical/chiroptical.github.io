---
layout: post
title: "Why I re-wrote my Slack bot in Gleam"
date:   2025-02-13
categories: jekyll blog
---

Introduction
---

I started working on [Music Monday][musicmonday] awhile back. It is currently
only a Slack bot which, when installed into a channel, selects a user at
random to suggest a musical album to share. This blog post isn't about the
functionality, but the language change I made during implementation. Feel free
to check it out though, I need beta testers.

The bot was originally implemented in Erlang. I **really** like Erlang. The
language is quirky and simple. The BEAM is a gorgeous piece of technology to
build applications on. However, I'm a static types enjoyer. I write Haskell
professionally and I really really like having sum and product types.
To summarize, I want strong static types **and** I want [Erlang/OTP][erlang-otp].  

Enter [Gleam][gleam]. I want to go over some specific examples where I feel
Gleam made my developer experience better.

## Records

In Erlang, you can share records via header files, `.hrl`, e.g.,

```erlang
-record(slack_channel, {id, music_monday_id, channel_id}).
```

I used this record to denote a return type from a database query in `pgo`.
You can pattern match or access elements, e.g.,

```erlang
-include_lib("{filename}.hrl").

#slack_channel{id = Id} = function_that_returns_slack_channel(),
SlackChannel = function_that_returns_slack_channel(),
Id = SlackChannel#slack_channel.id,
```

I don't think this pattern is great _across_ modules. You can give the fields
types and they can be checked with dialyzer/eqwalizer. That just doesn't provide
**me** enough, I'm not a smart man. A compiler with expressive types, that
are checked, and easily shareable across modules saves me a lot of stress and
brainpower.

In Gleam, this record is defined,

```gleam
import youid/uuid

pub type SlackChannel {
  SlackChannel(id: uuid.Uuid, music_monday_id: uuid.Uuid, channel_id: String)
}
```

I can import this type from anywhere via `import path/to/module.{type
SlackChannel}`. I can use it qualified via `import path/to/module` with
`module.SlackChannel`. It is easy to pass this type around and it works for both
Erlang and Javascript targets.

## Database Queries

With `pgo`, here is how I complete a unique insert into `slack_channel`

```erlang
create_unique(ChannelId) ->
    #{command := insert, rows := Rows} = pgo:transaction(fun() ->
        pgo:query(
            <<
                "insert into slack_channel (channel_id)"
                " values ($1)"
                " on conflict (channel_id) do update"
                " set channel_id = excluded.channel_id"
                " returning id"
            >>,
            [ChannelId]
        )
    end),
    case Rows of
        [{Id}] -> {ok, Id};
        _ -> {error, impossible}
    end.
```

Is `ChannelId` a UUID or String? Is `Id` a UUID or String? Is this query even
sound? In my Erlang application, I explicitly tested **every** query because
of the number of mistakes I made. I could add `-spec` annotations to this to
inform the reader but it doesn't mean they are correct! Additionally, PostgreSQL
already knows this information! Why not just let PG figure out the types and
write the `{ok, Id}` and `{error, impossible}` logic ourselves. In Gleam, we
can use [squirrel][squirrel] for this. You add your query to a SQL file in the
module tree and squirrel will autogenerate the requisite Gleam code for you! For
the above query (with a slight modification) it will generate,

```gleam
pub type CreateUniqueRow {
  CreateUniqueRow(id: Uuid)
}

pub fn create_unique(
  db: pog.Connection,
  arg_1: Uuid,
  arg_2: String,
) -> Result(pog.Returned(CreateUniqueRow), pog.QueryError) {
  // ... generated code here
}
```

In my production application I needed an additional `Uuid` to create a
`slack_channel` (the internal Slack team identifier). This was partially why
I rewrote the application which I'll explain in the next section. Here, I need
a `Uuid` and `String` to call this function and I'll get back, effectively,
`Result(List(CreateUniqueRow), pog.QueryError)`. The `pog.Returned` type
also has a `count` field. You need to understand what `arg_1` and `arg_2` are
supposed to be, but the shape is generated automatically. You can, and probably
should, create a usable API around this function. For example, the logic above
expects only a single entry to come back from the query. Squirrel also provides
helpful error messages when your queries are broken.

Refactoring
---

The initial Slack developer experience is okay if you are installing into a
single Slack team, but Music Monday is intended to be installed in **many**
Slacks. This requires OAuth credentials. I've not built applications like this
before so I made an assumption my bot is basically the same entity across Slack
but that isn't true. Each team has its own credentials and even its own bot id.

When I built the Erlang application I was too tightly coupled with my
development Slack team. I needed to do a huge refactor to support Slack team ids
and OAuth credentials. In Erlang, there is no requirement to add dialyzer specs
so... I didn't. I was in a hell hole of refactoring with the tests I had (which
actually was non-zero but far from full coverage) and debugging everything else
at runtime. It was pain.

After a few hours of this, I had enough. You can say, "skill issue" or "bad
tests and use of specs" and... you are right. To me, this is why strong static
types are _the way_. I am forced to do this **and** the compiler will help me.

Using the example above, I added the `slack_team` table and modified the
`slack_channel` table via migrations, re-ran `gleam run -m squirrel`, and ran
`gleam build`. Now, **all** the places I need to change are revealed to me. No
magic, no remembering. I just need to line up the new types.

This is true of internal blocks of code as well. In Erlang, when I pull out a
chunk of code, I have to figure out what the spec is supposed to be. In Gleam,
it was known before and it is known now. There is even a code action to do it.

Programming the Happy Path
---

Let's start with an example using `maybe` from Erlang,

```erlang
    maybe
        {~"text", Arguments} ?= proplists:lookup(~"text", Proplist),
        {~"channel_id", ChannelId} ?= proplists:lookup(~"channel_id", Proplist),
        {~"user_id", UserId} ?= proplists:lookup(~"user_id", Proplist),
        {ok, {slack_command, Arguments, UserId, ChannelId}}
    else
        none -> {error, <<"Unable to construct slack_command from_proplist">>}
    end.
```

Here, I'm trying to lookup some keys in a "proplist" (a list of key-value
tuples). They all need to be present to succeed. If `proplists:lookup`
succeeds it returns `{Key, Value}` if it fails it returns `none`. This
API is actually quite friendly for `maybe` expressions, others aren't.

First, the `?=` syntax is saying, if the left side of the expression is
a successful pattern match continue otherwise go to the `else` block and
start pattern matching. Let's imagine that an update to the proplists API
caused `proplists:lookup` to `error` instead of `none` **or** `{error,
not_a_proplist}` if you don't provide a proplist. In either of those update
scenarios, my pattern match would fail. Tools like dialyzer won't catch this,
but I believe other projects are working on full compilers for Erlang, e.g.
https://github.com/etylizer/etylizer.

In Gleam, I just don't have this problem because I am using a compiler with
exhaustiveness checking. I have a few options for coding this in Gleam, e.g.

```
// Note: I could use type to distinguish between parse_query failure and lookup failure
// Nil is used for simplicity
case uri.parse_query(a_string) { // https://hexdocs.pm/gleam_stdlib/0.69.0/gleam/uri.html#parse_query
  Error(Nil) -> Error(Nil)
  Ok(a_proplist) -> {
    case list.key_find(a_proplist, "text"), list.key_find(a_proplist, "channel_id"), list.key_find(a_proplist, "user_id") {
      Ok(args), Ok(channel_id), Ok(user_id) -> Ok(#(args, channel_id, user_id))
      _, _, _ -> Error(Nil)
    }
  }  
}
```

I personally find the `Result` `use` style more readable, but I'll elide that
for simplicity. The key is that my pattern match **has** to be exhaustive,
I couldn't write,

```
    Ok(args), Ok(channel_id), Ok(user_id) -> Ok(#(args, channel_id, user_id))
    Error(Nil), Error(Nil), Error(Nil) -> Error(Nil)
```

the compiler will **tell me** I goofed up. I like that. Additionally, if I want
to use some other error type the compiler will help me refactor e.g. if I wanted
to use a validation monad (I had to sneak the 'm' word in here). Additionally,
if the shape is updated I'll get a compiler error. Note: be careful using blanket
pattern matches, e.g. `_, _, _ ->` above, because you could miss API updates!

Adding a Front-end
---

There is only a small footprint of front-end code for Music Monday today.
Essentially an install button, some frequently asked questions, and a page to
describe how to use the bot after it is installed. However, if I want to do
something more interesting there aren't many friendly and maintained Erlang
frameworks to build with. I think [Nova][nova] looks interesting though.

The only times I have ever really enjoyed writing front-end are with Elm.
Typescript is a tedious language and doesn't _really_ have a language server
outside of VSCode. Editing in Helix with the LSP is basically useless. React's
hooks are getting easier for me, but the number of times I've had to think
incredibly deeply about the runtime is brutal. I want a simpler language with a
better editor experience. Enter [Lustre][lustre].

It was very easy for me to get a server-side rendered application
together. The types were easy to figure out, examples exist, and a
decent amount of documentation is available. I can use the editor
I prefer with actually useful LSP functionality. I already have a
Tailwind Plus subscription so it was easy to drop the HTML into [this
converter](https://lpil.github.io/html-lustre-converter/) (thanks Louis!) and
get the Lustre representation.


Erlang/OTP
---

I'm not going to be able to convince you that Erlang/OTP rocks. There
are better posts that cover that in **way** more detail. You are just
going to have to believe me for now. With a combination of [factory
supervisors](https://hexdocs.pm/gleam_otp) (in Erlang, simple-one-for-one
supervisors) and [crew](https://hexdocs.pm/crew/index.html) I was able to
introduce services and back-pressure into my system with little effort. Slack
has team based API limits and I'd like to be able to build with this in mind.

Conclusion
---

Gleam gave me all the tools I needed to be successful. The language is simple
and can be picked up quickly. The community is stellar and super helpful. You
can build full-stack applications with one language. If you are looking for
a strong statically typed language, check out Gleam. You'll also, eventually,
learn about Erlang/OTP which has really nice patterns to help build robust
software.

Feel free to send me a direct message on [BlueSky][bluesky] if you have any
questions, corrections, or want to tell me I'm wrong. I'm always happy to learn
new things.

[bluesky]: https://bsky.app/profile/chiroptical.dev
[musicmonday]: https://musicmonday.app
[erlang-otp]: https://www.erlang.org/doc/system/design_principles.html
[gleam]: https://gleam.run
[squirrel]: https://github.com/giacomocavalieri/squirrel
[nova]: https://www.novaframework.org/
[elm]: https://elm-lang.org/
[lustre]: https://github.com/lustre-labs/lustre
