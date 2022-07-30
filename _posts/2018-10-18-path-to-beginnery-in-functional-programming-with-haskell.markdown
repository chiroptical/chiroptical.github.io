---
layout: post
title: 'Path to Beginnery in Functional Programming with Haskell - 1'
date:   2018-10-18
categories: jekyll blog
---

Path To Beginnery in Functional Programming with Haskell
---

See [the first post in this
series](/blog/path-to-beginnery-in-functional-programming-with-haskell) for an
introduction to this series. Quick recap: I am trying to track my path
completing Haskell programming projects from books I am reading. Feel free to
message me on Twitter [@chiroptical](https://twitter.com/chiroptical) with any
corrections or suggestions on new topics.

Project 1
---

This is a short problem, but I was getting stuck on a `foldr` implementation.
I wanted to write down the problem, reductions, correct solution, and some
alternate implementations to increase my understanding.

#### Definition of Problem

Implement `myMaximumBy` using a fold. `myMaximumBy` takes a comparison
function, of type `(a -> a -> Ordering)`, and returns the greatest element of
the list based on the last value in the list which returned `GT`. Some
examples:

```haskell
Prelude> myMaximumBy (\_ _ -> GT) [1..10]
1
Prelude> myMaximumBy (\_ _ -> LT) [1..10]
10
Prelude> myMaximumBy compare [1..10]
10
```

#### Solving The Problem

The base case, or accumulator, is simply the first value in the list. My
initial thought is that given an empty list our function should return an
error. Side note: after additional thought I decided to implement a version which
returns `Maybe a`, but I will show that in the __Practical Considerations__
section. If given a list with one element, simply return that element. Next we
need to define our folding function (for a `foldr`),

```haskell
folder :: (a -> a -> Ordering) -> a -> a -> a
folder f x acc = if f x acc == GT then x else acc
```

and the full `foldr` with pattern matches for empty and single-item lists,

```haskell
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Cannot myMaximumBy on empty list!"
myMaximumBy _ [x] = x
myMaximumBy f xs = foldr (folder f) (head xs) $ tail xs
```

For a novice, this might look like working code as it will type check! However,
it doesn't work correctly. A `foldr` breaks down like this for `[a]` with 3 items:

```haskell
foldr g acc [a, a', a'']
-- ==
-- g a (g a' (g a'' acc))
```

Let's take the example where,

```haskell
-- Omitting types
g = folder f
f = \_ _ -> GT

-- Reduction (g x x' = x, always!)
-- 1. g a (g a' a'')
-- 2. g a a'
-- 3. a
```

Which is not what we are looking for! We actually want to return `a''`. To
do that, we need `foldl`,

```haskell
folder :: (a -> a -> Ordering) -> a -> a -> a
folder f acc x = if f acc x == GT then acc else x

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Cannot myMaximumBy on empty list!"
myMaximumBy _ [x] = x
myMaximumBy f xs = foldl (folder f) (head xs) $ tail xs

-- Reduction
-- 1. f (f a a') a''
-- 2. f a' a''
-- 3. a''
```

Practical Considerations
---

#### Implement `... -> Maybe a` Version

Let's remove the version of `myMaximumBy` which errors out by returning
`Nothing` when given an empty list and a `Maybe a` otherwise.

```haskell
folder :: (a -> a -> Ordering) -> Maybe a -> a -> Maybe a
folder f (Just acc) x = if f acc x == GT then (Just acc) else (Just x)
folder _ _ _ = Nothing

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy _ [] = Nothing
myMaximumBy _ [x] = Just x
myMaximumBy f xs = foldl (folder f) (Just $ head xs) $ tail xs
```

I don't think `folder f _ x` pattern in necessary, but it definitely doesn't
hurt.

#### Implement `myMinimumBy`

For `myMinimumBy` you simply replace `GT` in `folder` with `LT`. With a little
abstraction, you can write both in a nice point-free style.

```haskell
folder :: Ordering -> (a -> a -> Ordering) -> Maybe a -> a -> Maybe a
folder o f (Just acc) x = if f acc x == o then (Just acc) else (Just x)
folder _ _ _ _ = Nothing

myOrderingBy :: Ordering -> (a -> a -> Ordering) -> [a] -> Maybe a
myOrderingBy _ _ [] = Nothing
myOrderingBy _ _ [x] = Just x
myOrderingBy o f as = foldl (folder o f) (Just $ head as) $ tail as

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy = myOrderingBy GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy = myOrderingBy LT
```

Wrapping Up
---

This wasn't a particularly difficult problem or solution, but it was one of the
first cases where my code looked correct, type-checked, and failed. It is
really important to understand the difference between `foldr` and `foldl`. I am
starting to really enjoy point-free style in Haskell. When understood, it is
terse and beautiful.

Edits made on 10/18/18 cleaning up patterns with unneccesary named parameters.
Replace `(x:[])` with `[x]`.
