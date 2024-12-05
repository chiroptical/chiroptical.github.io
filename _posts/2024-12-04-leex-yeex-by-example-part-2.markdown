---
layout: post
title: "Leex and yecc by example: part 2"
date:   2024-12-04
categories: jekyll blog
---

Welcome to Part 2!
---

[In part 1]({% link _posts/2024-12-03-leex-yeex-by-example-part-1.markdown %}),
we went over our first example of using leex and yecc. In
this blog post, I'll lex/parse [Advent of code 2024 day 4][day4] and simplify
day 3 using an observation from day 4. I am still learning too.

Day 4: Ceres Search
---

In this problem, we are given a matrix of `X`, `M`, `A`, and `S`s we are
basically doing a word search puzzle for `XMAS` horizontally, vertically, or
diagonally. It can also appear forward or backward. An abbreviated input,

```text
MMMSXX
MSAMXM
AMXSXM
MSAMAS
```

When I see problems like this I usually reach for `maps` because I need to
search in the vicinity of every element. Essentially, for the first `M` in that
input I want {% raw %}`{{1, 1}, m}`{% endraw %}. Conveniently, that is the exact
format I need to use [maps:from_list/2][from-list].

First, I'll drop the leex file and then discuss the various sections,

```
Definitions.

XMAS    = [XMAS]
NEWLINE = \n

Rules.

{XMAS}    : {token, {xmas, TokenLoc, to_atom(TokenChars)}}.
{NEWLINE} : skip_token.

Erlang code.

to_atom("X") ->
	x;
to_atom("M") ->
	m;
to_atom("A") ->
	a;
to_atom("S") ->
	s.
```

In the definitions section we only have `XMAS` which is any one of our
characters. Note, you would use `[A-Z]` if you wanted any capital letter
but we only care about the specific letters. We also have newlines
which you've seen before.

The rules show two nice things we have at our disposal. `TokenLoc` will tell us
exacly the row and column our token was found in the format `{Row, Column}`.
Additionally, `skip_token` is simply dropping the token entirely. We don't
really care about the newlines because we get the position from `TokenLoc`.
We can also run `to_atom/1` on our token to convert it into an atom. Amazing!
The output of our lexer looks like,

```text
[{xmas,{1,1},m},
 {xmas,{1,2},m},
 {xmas,{1,3},m},
 ...]
```

Much simpler than day 3. Next our yecc file which I'll discuss below,

{% raw %}
```text
Nonterminals chars.

Terminals xmas.

Rootsymbol chars.

chars -> xmas       : [reshape('$1')].
chars -> xmas chars : [reshape('$1')|'$2'].

Erlang code.

reshape({xmas, {Row, Col}, Xmas}) ->
	{{Row, Col}, Xmas}.
```
{% endraw %}

We only have one terminal this time `xmas`. Additionally, we only have
one non-terminal which I called `chars`. A `chars` can either be an `xmas`
(base case) or `xmas` followed by `chars` (recursive case). Finally, we just
`reshape/1` to get our desired shape for `maps:from_list/2` and we are done!

My solution is [here][day4-solution]. I plan on doing some refactoring
but it is currently working.

Refactor Day 3
---

If you haven't read [part 1]({% link
_posts/2024-12-03-leex-yeex-by-example-part-1.markdown %}) the following
explanation isn't going to make a lot of sense.

In the previous example, we completely ignored the newlines. In day 3, we also
don't care about the newlines! The "memory" described in the problem is just
a contiguous string of characters. We can make some basic changes to our lexer
and parser to simplify our application code from dealing with a list of list
of inputs.

In the lexer, we can either change the newline rule to,

1. `{NEWLINE} : {token, {skip, TokenLine}}.`
2. `{NEWLINE} : skip_token.`

I chose "1." because I don't really care about a few extra `skip`
tokens. Ideally, I could change the else definition to `ELSE =
[^all_unimportant_characters]`. The `[^abc]` syntax means inverse i.e. all
characters but `a`, `b`, `c`. The regex in leex is not powerful enough to
say "anything except a number, do, don't, parentheses, or comma".
I could just add a bunch of symbols to a list,
e.g. `[%^&*$#\s\n]` but I don't like brute forcing the possible symbols in the
input. I only point this out because it is a limitation you may want to know about.
Documentation is [here][leex-regex] for reference.

In the parser, we remove the non-terminal `computer`, the terminal `newline`,
and change the `Rootsymbol` to `memory`. That is literally it. You'll
end up with an input to your application like,

```
[{operands,{2,4}},
 {operands,{5,5}},
 {operands,{11,8}},
 {operands,{8,5}},
 ...]
```

With this simplification, I was able to remove about 20 lines from my
application code that just folded over that outer list. Links to the new files
for reference,

- [lexer][day3-lexer]
- [parser][day3-parser]
- [solution][day3-solution]

[from-list]: https://www.erlang.org/doc/apps/stdlib/maps.html#from_list/1
[day4]: https://adventofcode.com/2024/day/4
[day3-lexer]: https://github.com/chiroptical/advent_of_code_2024/blob/main/src/lexer_day_3_2024.xrl
[day3-parser]: https://github.com/chiroptical/advent_of_code_2024/blob/main/src/parser_day_3_2024.yrl
[day3-solution]: https://github.com/chiroptical/advent_of_code_2024/blob/main/src/solution_day_3_2024.erl
[day4-solution]: https://github.com/chiroptical/advent_of_code_2024/blob/main/src/solution_day_4_2024.erl
[leex-regex]: https://www.erlang.org/doc/apps/parsetools/leex.html#module-regular-expressions
