---
layout: post
title: 'First day with typst, a markup based typesetting system'
date:   2023-03-31
categories: jekyll blog
---

I came across [typst](https://github.com/typst/typst) recently which looks like
an interesting replacement to LaTeX. I don't really do much collaborative
editing anymore, but I really enjoy plain text presentations. I tried
[pollen](https://docs.racket-lang.org/pollen/) as well, but I didn't like the
unicode symbols. What was my first presentation like using `typst`?

`typst` is available on [the unstable nix
channel](https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=typst)
and you can likely get it with `nix-shell -p typst` or follow the instructions
on [their github](https://github.com/typst/typst).

One annoying thing about LaTeX is you have to compile a bunch of times for your
PDF to be correct. With `typst`, you can `typst -w document.typ` and it will
watch the document for changes and recompile automatically. This is a really
nice productivity boost. 

Setting up the presentation,

```
#set page(                                                                 
  paper: "presentation-16-9",    
  margin: (    
    rest: 25pt    
  )    
)   
```

Here, we are setting parameters for the
[page](https://typst.app/docs/reference/layout/page/). `#` denotes a "code
expression". I believe I could also build my own template to define margins, spacing,
font, etc. in a separate file.

Next, the font,

```
#set text(    
  font: "JetBrains Mono",    
  size: 22pt    
)
```

This syntax is a bit odd, but it is syntax and LaTeX isn't necessarily nicer in
any way. You can lay out a slide like so,

```
= The slides title

// The slides content

#pagebreak()
```

The `=` denotes a header, you can generate smaller headers with additional `=`,
i.e. `===`. The `//` denotes a comment, most of what you need is from
markdown, see [the syntax guide](https://typst.app/docs/reference/syntax/).
Interestingly, you can make this into a named function,

```
let slide(title, content) = [
  = #title
  #content
  #pagebreak()
]
```

The function syntax is a bit weird to me, but I also don't fully understand the
type system yet. As an example, here is another function,

```
  #let fig(location, width, gap, caption) = [
    #figure(
      image(location, width: width),
      numbering: none,
      gap: gap,
      caption: caption
    )
  ]
```

Note the difference in how I refer to the parameters in the body. I think the
former `#x` are inserting "content blocks" and the latter are plain values and
don't require the `#`. Not exactly sure yet.

From here, you could generate a slide with a figure pretty easy.

```
#slide(
  [The slides title],
  [
    - Some unordered list item
    - Some other unordered list item
    fig(
      "figures/image.png", 
      350pt, // the width of the image, see function definition
      -2pt, // the captions are a bit far away from the images by default
      [ The caption for the figure. ]
    )
  ]
)
```

From here, you can build a basic presentation! Pretty cool.

I also wrote two other functions for links:

```
#let l(location) = link(location)[#text(blue)[#location]]
#let ld(location, description) = link(location)[#text(blue)[#description]]
```

I did try a two column `#grid` but the alignment was a bit wonky. I would like
to spend a bit more time handling columnar layouts before attempting to show
some code. Let me know what you think on [Twitter](https://twitter.com/chiroptical)
