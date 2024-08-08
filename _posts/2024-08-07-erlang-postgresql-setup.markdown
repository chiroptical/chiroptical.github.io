---
layout: post
title: "Setting up Erlang CI with PostgreSQL"
date:   2024-08-07
categories: jekyll blog
---

# Overview

In this blog post, I'll quickly discuss how I got Erlang and PostgreSQL set up in GitHub CI.
Additionally, I discuss how I set up single use databases for my test infrastructure.

# Assumptions

You at least have some familiarity with GitHub Actions and Erlang.

## Erlang in GitHub Actions

This is actually very easy thanks to the Erlang Foundation's [setup-beam][setup-beam] action.
Here is the top of my `ci.yaml` file,

```yaml
on: push

jobs:
  test:
    runs-on: ubuntu-22.04
    name: Erlang/OTP ${{matrix.otp}} / rebar3 ${{matrix.rebar3}}
    strategy:
      matrix:
        otp: ['26.2.2']
        rebar3: ['3.22.1']
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{matrix.otp}}
          rebar3-version: ${{matrix.rebar3}}
```

I am building and testing an application here, so having an intensive matrix isn't particularly helpful.
Next, the actual checks and tests I want to run,

```yaml
      # The Erlang files should be formatted
      - run: rebar3 fmt --check
      # Run eunit
      - run: rebar3 eunit
      # Run common test
      - run: rebar3 ct
```

This is essentially all you need to do both [eunit][eunit] and [commontest][commontest] tests.

## Single use databases

With common test, I can have build-up and tear-down callbacks for every suite
and every test. Using these callbacks, I can create, migrate, and destroy a
single use database for each test that needs them. Let's look at an example Erlang
common test suite. Set up our Erlang module, include eunit macros, and export
all the functions in the module so common test can run them,

```erlang
-module(temporary_database_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
```
  
The `all/0` callback tells common test which tests to run. We'll look at the actual
test last.

```erlang
all() ->
    [the_current_database_is_the_temporary_one_and_contains_public_tables].
```

These callbacks initialize state for this test suite. In this case, I start
pgo and ensure all of the environment variables are set up for pgo. This
`Config` variable can be used to store state for your tests or explain to the
`end_per_suite/1` function what to destroy. This isn't really a tutorial on
common test.

```erlang
init_per_suite(Config) ->
    application:ensure_all_started(pgo),
    environment:setup_application_variables(),
    Config.

end_per_suite(_Config) ->
    application:stop(pgo),
    ok.
```

This is the meaty goodness, here we create a migrated database from a template (I'll explain
in the next section). Then initialize a connection to that database and store the details
in `Config`. Again, this is how you pass information to your end step.

```erlang
init_per_testcase(_TestCase, Config) ->
    #{temporary_database_atom := DatabaseAtom, temporary_pool_id := PoolId} =
        util_tests:create_migrated_database_pool(),
    [{temporary_database_atom, DatabaseAtom}, {temporary_pool_id, PoolId} | Config].
```

Finally, we pull out the `Config` information and destroy the temporary database
and the connection. 

```erlang
end_per_testcase(_TestCase, Config) ->
    DatabaseAtom = proplists:get_value(temporary_database_atom, Config),
    PoolId = proplists:get_value(temporary_pool_id, Config),
    util_tests:drop_database_and_pool(DatabaseAtom, PoolId).
```

Voila, single use database per test set up and tore down. In this suite, I just
do a test that the number of tables in the migrated database contains seven or
more tables. That was just how many tables I had when I set this up.
Seven isn't at all magical.

```erlang
the_current_database_is_the_temporary_one_and_contains_public_tables(Config) ->
    #{command := select, num_rows := 1, rows := [{NumberPublicTables}]} = pgo:query(
        <<"select count(*) from information_schema.tables where table_schema = 'public'">>
    ),
    ?assert(NumberPublicTables >= 7),

    DatabaseAtom = proplists:get_value(temporary_database_atom, Config),
    #{command := select, num_rows := 1, rows := [{CurrentDatabase}]} = pgo:query(
        <<"select current_database()">>
    ),
    ?assert(CurrentDatabase =:= atom_to_binary(DatabaseAtom)).
```

## Getting PostgreSQL in GitHub Actions CI

I'll go over more specific Erlang bits in the next section. First, let's get
PostgreSQL in GitHub actions.

```yaml
      - uses: ikalnytskyi/action-setup-postgres@v6
        id: postgres
        with:
          username: my_app
          password: my_app
          database: my_app_template
          port: 5432
```

Conveniently there is already a github action for this. The interesting part is
that I am defining the database as `my_app_template` which is just a separate
copy of my migrated database. This is because, locally, `my_app` is used to test
and I don't want to copy any data from it every time I create a temporary database.
Migrating the template database is also easy,

```yaml
      - uses: cachix/install-nix-action@v27
        with:
          nix_path: nixpkgs=channel:nixos-unstable
          github_access_token: ${{ secrets.GITHUB_TOKEN }}
      - run: nix-shell -p dbmate --run "unset PGSERVICEFILE && dbmate up"
        env:
          DATABASE_URL: ${{ steps.postgres.outputs.connection-uri }}?sslmode=disable
```

Here, I use [nix][nix] to get [dbmate][dbmate].
Both of these tools are nice to use.
One of them is **a lot** harder than the other.

## Internal Erlang Functions

I showed you the high level common test earlier. Some of the internals
are in this section,

```erlang
create_migrated_database_pool() ->
    % Create a temporary database and initialize a connection to it
    DatabaseAtom = util_pgo:create_temporary_database(),
    {ok, PoolId} = util_pgo:start_default_pool_with_name(DatabaseAtom),
    #{temporary_database_atom => DatabaseAtom, temporary_pool_id => PoolId}.
```

The implementation of the function in my per-test database and pool initializer,

```erlang
create_temporary_database() ->
    DatabaseName = get_random_database_name(),
    DatabaseAtom = list_to_atom(DatabaseName),
    Cmd = "createdb " ++ DatabaseName ++ " -T my_app_template",
    Output = ?cmd(Cmd),
    logger:notice(#{
        action => create_temporary_database, cmd => Cmd, output => Output
    }),
    DatabaseAtom.
```

I literally just shell out to `createdb`. The command macro is pretty slick.
Also, check out my [YouTube video][bits-logger] on the `logger` module.
Finally, we can start our connection via pgo,

```erlang
start_default_pool_with_name(Atom) ->
    pgo:start_pool(
        default,
        #{
            pool_size => 1,
            host => environment:get_application_variable(pgo_host),
            user => environment:get_application_variable(pgo_user),
            database => atom_to_binary(Atom),
            password => environment:get_application_variable(pgo_password)
        }
    ).
```

At our end step, we just shell out to `dropdb` and kill the pool with,

```erlang
    ok = supervisor:terminate_child(pgo_sup, PoolId),
```

## Summary

This was a super interesting thing to set up myself. It isn't a ton of work
and there are GitHub actions to help you out. I am really enjoying my time with
Erlang but testing even simple database actions is really important. I found
that almost all of my query code was wrong at first. The project I set this up for is
currently private but feel free to ask me questions on [Twitter][twitter].

[setup-beam]: https://github.com/erlef/setup-beam
[eunit]: https://www.erlang.org/doc/apps/eunit/chapter.html
[commontest]: https://www.erlang.org/doc/apps/common_test/basics_chapter.html
[nix]: https://nixos.org
[dbmate]: https://github.com/amacneil/dbmate
[bits-logger]: https://www.youtube.com/watch?v=xMvwYtUkT0A
[twitter]: https://x.com/chiroptical
