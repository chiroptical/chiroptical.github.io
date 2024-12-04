---
layout: post
title: "Leex and yecc by example: part 1"
date:   2024-12-03
categories: jekyll blog
---

# Background

[Advent of code][aoc] is here and every year I use a parser combinator library
to handle the inputs. Lately, I have been into Erlang but I couldn't find a
parser combinator library that I liked. However, Erlang does have [leex][leex]
and [yecc][yecc]. Leex is a lexical analyzer generator and yecc is a LALR-1
parser generator. I have never used tools like these before and it seemed
like a great opportunity. However, the documentation and examples are a little
sparse for getting started. Therefore, I decided to start this series as a
medium to present examples of lexers and parsers in Erlang using leex and yecc!

I am not going to dive into the theory of these tools. I really just want to jam
something examples into your brain piece so you can play with them. If you
want to dive into theory later, be my guest.

# Day 3: Mull It Over

This is the first example which isn't just numbers separated by spaces. It is
a great lexing and parsing example.  The [problem on advent of code][day3]. The
basic idea is you have an input,

```
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
```

In the text, you'll see some `mul(A,B)`. We want to find all of these in
the input, multiply the numbers `A` and `B` together, then sum all of the
multiplications. There are corrupted versions which you aren't allowed
to count, e.g. `mul(A, B)`, `mul(A,B`, `?(A,B)`. I think most people
will use regex to solve this problem, i.e. `mul\([0-9]+,[0-9]+\)`.
How would we do this with leex and yecc?

# Lexing

Lexing is a process of breaking a string into tokens. In our case, the
tokens would be `mul`, `(`, `,`, `)`, or a number. In part 2, we also need `do`,
`don't`. Finally, we also need to represent the rest of the input i.e. `space`,
`newline`, `skip`. The `skip` token is going to represent any token that isn't
relevant to us. For example, the tokenized version of `xmul(2,4)%&` is approximately `['skip',
'mul', '(', '2', ',', '4', ')', skip, skip]`. We'll feed the tokenization to the
parser in the next step. First, let's discuss how to describe the lexer using
leex.

```
Definitions.

INT         = [0-9]+
MULTIPLY    = mul
DO          = do\(\)
DONT        = don't\(\)
OPEN_PAREN  = \(
CLOSE_PAREN = \)
COMMA       = ,
SPACE       = \s+
NEWLINE     = \n
ELSE        = .
```

The first section of the leex file describes our tokens. It uses a relatively
simplistic regex language to describe the tokens. More documentation [here][leex-regex].
`INT = [0-9]+` means an `INT` is described as one or more of any single number
between 0 and 9. `SPACE = \s+` means a `SPACE` is described as one or more space
characters. `ELSE = .` means `ELSE` is described as any character.

```
Rules.

{INT}                 : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{MULTIPLY}            : {token, {mul, TokenLine}}.
{DO}                  : {token, {do, TokenLine}}.
{DONT}                : {token, {dont, TokenLine}}.
{OPEN_PAREN}          : {token, {open_paren, TokenLine}}.
{CLOSE_PAREN}         : {token, {close_paren, TokenLine}}.
{COMMA}               : {token, {comma, TokenLine}}.
{SPACE}               : {token, {space, TokenLine}}.
{NEWLINE}             : {token, {newline, TokenLine}}.
{ELSE}                : {token, {skip, TokenLine}}.  
```

The rules are evaluated in order from top to bottom. All of these rules are
generating tokens, but you could also `: skip_token` or return an error `:
{error, "..."}`. In `{token, X}`, `X` must be a tuple and it can be any length
but must start with an atom. We'll use that atom in the parser shortly. You can
also use Erlang code here! For example, we really want `2` and not `"2"` so we
use a builtin `list_to_integer/1` to convert. `TokenLine` is a leex variable
which is filled in with the line number from the input. There is also `TokenLoc`
which gives you `{Line, Column}`.

```
Erlang code.
```

This is the final section. I don't need any special Erlang code here, but we'll
use this section in the parser.

If you are using [rebar3][rebar3], and you stick these sections into a single file like
`src/lexer.xrl` it will auto-generate a file `src/lexer.erl` which contains
`lexer:lex/1`. If you pass that a string, it will try to lex it. For the
following example (the test input),

```erlang
lexer:lex("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))").
```

you get (truncated for article length),

```
[{skip,1},
 {mul,1},
 {open_paren,1},
 {int,1,2},
 {comma,1},
 {int,1,4},
 {close_paren,1},
 {skip,1},
 {skip,1},
 ...]
```

See how it is just a list of tokens! Let's parse it!

# Parser

Parsing will take the list of tokens and interpret them into a data structure
convenient for our computation. In our case, we only care about the numbers
(`int`), `do` and `don't`. In part 1, we only need the numbers. For example,
given the above lexed output we want `[{operands, {2, 4}}]` to come out of
parser. `operands` is arbitrary, we could have used `spaceship`. Our program
decides what to do with the output of the parser.

```
Nonterminals instruction memory computer else.

Terminals mul int open_paren close_paren comma space newline skip do dont.

Rootsymbol computer.
```

The terminals are just the atoms from our lexer, hopefully you recognize them.
The non-terminals describe the pieces we want to pull out,

- `instruction`: `{operand, {X, Y}}`, `enable`, `disable`
- `memory`: a list of `instruction`s
- `computer`: a list of `memory`s
- `else`: everything we want to skip

I chose to translate `do -> enable`, `don't -> disable` because it seemed
more obvious when writing the solution. The `Rootsymbol` determines the
starting point. In our case, we are parsing the `computer` as a list of list
of instructions. The next section of the yecc file is a bit large in this case
but I prefer to show code before the example. I like to put a blank line in
between each non-terminal for readability.

```
instruction -> mul open_paren int comma int close_paren : {ok, operands, {extract_integer('$3'), extract_integer('$5')}}.
instruction -> mul open_paren int comma int             : incomplete.
instruction -> mul open_paren int comma                 : incomplete.
instruction -> mul open_paren int                       : incomplete.
instruction -> mul open_paren                           : incomplete.
instruction -> do                                       : {ok, enable}.
instruction -> dont                                     : {ok, disable}.
instruction -> else                                     : incomplete.

memory -> instruction             : remove_incomplete('$1').
memory -> instruction memory      : remove_incomplete_rec('$1', '$2').

computer -> memory           : ['$1'].
computer -> memory computer  : ['$1'|'$2'].

else -> mul.
else -> int.
else -> open_paren.
else -> close_paren.
else -> comma.
else -> space.
else -> newline.
else -> skip.
```

This section describes all the rules. A single rule is constructed with a
non-terminal `instruction ->`, it's
definition `mul open_paren int comma int close_paren`, and what we do with
it `: {ok, operands, {extract_integer('$3'), extract_integer('$5')}}.`. The
definition is literally the atoms from the tokenized output in that exact order.
If you have a `space` in between any of those the rule will fail. Each token is
identified using this odd syntax, i.e. `'$3'` is the first `int` token.
The return is just an Erlang value. We can use built-ins or add code in an `Erlang code.` section (which I use in this case).
After the matching rule, we have to describe all the `incomplete` sequences.
If you don't do this, you get very weird errors. Those errors are trying to
explain to you that you **must** have these 6 tokens in order and I only found
e.g. `mul open_paren`, but I can't find an `int` after it. Try commenting those out
and check the error message, it isn't great. Finally, we have `do`, `dont`, and anything `else`.

The `else` non-terminals are just any singular token not matching our incomplete
`mul(1,2)` example. Note how I don't include `do` and `dont` there, I always
want to parse those.

The `memory` non-terminal is recursive and so you need a base case.
Our base case is just an `instruction` and the recursive case is
an `instruction` followed by `memory`. You'll see these recursive definitions
a lot with advent of code problems. The `computer` is very similar but replace
`instruction` with `memory`.

```
Erlang code.

extract_integer({_Token, _Line, Value}) -> Value.

remove_incomplete({ok, operands, {A, B}}) ->
	[{operands, {A, B}}];
remove_incomplete({ok, enable}) ->
	[enable];
remove_incomplete({ok, disable}) ->
	[disable];
remove_incomplete(incomplete) ->
	[].

remove_incomplete_rec({ok, operands, {A, B}}, Rest) ->
	[{operands, {A, B}} | Rest];
remove_incomplete_rec({ok, enable}, Rest) ->
	[enable | Rest];
remove_incomplete_rec({ok, disable}, Rest) ->
	[disable | Rest];
remove_incomplete_rec(incomplete, X) ->
	X.
```

Finally, our Erlang code to process. We are just removing the `incomplete`s
from our final result and reshaping the operands, enable, and disable to avoid
dealing with the `ok`s in our application code.

If you are using `rebar3` you will get auto-generation here too. A file
`src/parser.yrl` will generate `src/parser.erl` which has `parse/1`.

```erlang
{ok, Lex} = lexer:lex("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"),
parser:parse(Lex).
```

will output

```
[[{operands,{2,4}},
  {operands,{5,5}},
  {operands,{11,8}},
  {operands,{8,5}}]]
```

Amazing! Solving the problem is trivial from here!

# Conclusion

This was just our first example using leex and yecc. We are going to go over a
lot more examples in the series. For example, [2023 day 1][2023-day-1] makes use
of a neat leex feature. If you have any suggestions, questions, or corrections
hit me up on [bluesky][bluesky].

[aoc]: https://adventofcode.com
[leex]: https://www.erlang.org/doc/apps/parsetools/leex.html
[yecc]: https://www.erlang.org/doc/apps/parsetools/yecc.html
[rebar3]: https://rebar3.org
[bluesky]: https://bsky.app/profile/chiroptical.dev
[day3]: https://adventofcode.com/2024/day/3
[leex-regex]: https://www.erlang.org/doc/apps/parsetools/leex.html#module-regular-expressions
[2023-day-1]: https://adventofcode.com/2023/day/1
 
