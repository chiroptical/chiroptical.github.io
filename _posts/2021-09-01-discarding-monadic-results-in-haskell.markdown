---
layout: post
title: 'Discarding monadic results in Haskell'
date:   2021-09-01
categories: jekyll blog
---

# Discarding Monadic Results in Haskell

I recently ran this poll on
[Twitter](https://twitter.com/chiroptical/status/1433059574370689027). The
original poll and results,

![poll]({{ "/assets/discarding-monadic-results-poll.png" | relative_url }})

I wasn't expecting much from this poll but the comments turned out to be fantastic!
Let's summarize the problem, options, and discuss them a bit. The focus of the
discussion will be if **I would use it in my personal project**. It isn't a suggestion.

## Motivation

```haskell
module Main where    
    
f :: IO Int    
f = pure 1    
    
main :: IO ()    
main = do    
  -- business...    
  f    
  -- more business...    
  pure ()
```

The following warning is generated when you compile this with `-Wall`,

```
src/Main.hs:9:3: warning: [-Wunused-do-bind]
    A do-notation statement discarded a result of type ‘Int’
    Suppress this warning by saying ‘_ <- f’
  |
9 |   f
  |   ^
```

Typically, I also use `-Werror` and therefore the warning becomes an error. What are our options in this case?

## Options

We need to discard the result of `f`. Here are all the suggested solutions (attributed to the first suggester),

- Disable the warning `-Wno-unused-do-bind` [@TechnoEmpress](https://twitter.com/TechnoEmpress/status/1433169858477371392)
- `void f` (in the poll)
- `() <$ f` [@alex_pir](https://twitter.com/alex_pir/status/1433279377786163200)
- `_ <- f` (in the poll)
- `_ ← f` [@toastal](https://twitter.com/toastal/status/1433075730347278336)
- `_descriptiveName <- f` [@MxLambda](https://twitter.com/MxLambda/status/1433072010846998532)
- `(_ :: ResultType) <- f `, in this case `Int` [@vincenthz](https://twitter.com/vincenthz/status/1433387638275260423)

## Breakdown

Let's look at a few of the options a bit. 

### void

```haskell
void :: Functor f => f a -> f ()
```

I have always used this in my personal projects. It gets the job done, but isn't particularly satisfying (hence the poll).
The win here is that it only requires a `Functor` constraint and can be used beyond `do` notation.
I wonder if `void` would be more compelling if it was named differently? Maybe `discard` or `ignore`?

### The const equivalent

```haskell
(<$) :: Functor f => a -> f b -> f a
(<$) = fmap . const
```

This is `const` lifted into a functorial context. It is more flexible than `void` and useful for the same reasons.
It is provided, for free, by the Functor typeclass and is one I often forget about. That being said, I don't feel
particularly compelled to start using `() <$ ...` over `void`.

### Underscores

The options,

```haskell
_ <- f
(_ :: Int) <- f
_descriptiveName <- f
```

The first line is saying "match something, but I don't care what". This is equivalent to `void` but preferred by more respondents.
There is one exception to this preference (expressed in the responses as well),

```
do
  -- business...
  _ <- finalMonadicComputation
  pure ()
```

I personally think this should be `void` in almost all cases.
The latter two lines are much more interesting to consider and make context even more important.
Specifying the underscore's type, i.e. `_ :: Int`, does add some additional type safety if the monadic computation changes.
However, in most cases, changing the monadic computation would at least point me to the underscore (thanks GHC)
so I can reconsider my choices. Adding a descriptive name is never a bad thing, but sometimes it is difficult to come
up with a **good** name or the function names are clear enough. I think both of these are interesting and I will probably
use some variation of them in the future.

Bonus: with `ScopedTypeVariables` you can remove the parentheses.

### Unicode

Honestly, I don't even know how to enter a unicode arrow on my keyboard. Cool suggestion nonetheless. 

### Disable the warning

Here I am appeasing the compiler for `-Wall -Werror` and Hécate is playing an entirely different game.
I think this is interesting and I might try it out in my personal projects. However, you **do** lose a signal
that the monadic computation returns something. In Haskell, we often use `descriptiveFunctionName_` to
indicate that a function returns `()` and if you follow that convention you could use that as a signal.
Do I really need this signal? I am not so sure anymore.

## Wrapping up

This poll generated a surprising response. The results were both fun,
interesting, and will hopefully make me think more carefully about context. I
hope you enjoyed it as much as I did.

Find typos or have suggestions? My DMs are always open
[@chiroptical](https://twitter.com/chiroptical).

Like the content? Follow me on [Twitch](https://twitch.tv/chiroptical) and
subscribe on [Youtube](https://youtube.com/chiroptical)
