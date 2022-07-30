---
layout: post
title: 'Simple Scaleable Preprocessing with PyTorch and Ray - 0'
date:   2020-05-20
categories: jekyll blog
---

Simple Scaleable Preprocessing With Pytorch and Ray
---

#### Background

I have been using [PyTorch](https://pytorch.org/) for a few months now and I
really like the `Dataset` and `DataLoader` workflow (see
[`torch.utils.data`](https://pytorch.org/docs/stable/data.html)). I realized I
might be able to use this workflow for every step in my Machine Learning
pipeline, i.e. preprocessing, training, and inference. I further realized I
could use [Ray](https://docs.ray.io/en/stable/index.html) to coordinate
multi-node parallelism with little changes to my original code.

__Escape Hatch__: if you would rather explore the code with no explanation
there is a [Jupyter Notebook on
Github](https://github.com/chiroptical/pytorch-ray-workflow/blob/master/PyTorchDatasetExample.ipynb)

I believe most folks are using `Dataset/DataLoader` to handle training and
inference pipelines but let's consider a more general preprocessing workflow. A
data scientist needs to write a function which processes their entire data set,
the function has the approximate signature:

```
InputFile -> (OutputFiles, Metadata)
```

Here, `InputFile` is an input file in your dataset. The function may produce
one, or more, `OutputFiles` and some `Metadata` related to the operation
performed. As a practical example, I often have to split large audio files into
multiple audio files of a fixed size and retain some metadata (source audio,
destination audio, labels).

In this blog post, I'll discuss how to get PyTorch's `DataSet` and `DataLoader`
workflow running in parallel for this general use case. I will also go over
some of the mistakes I made while first exploring this workflow. I will assume
the reader knows basic Python.

### Why should you care?

I believe this workflow is really easy to teach to beginners. A user only needs
to know how to write a function to process an input file and the relationship
between batches and parallelism. With the exception of the `collate_fn`
(explained later) the code is essentially boilerplate. If you can implement a
`Dataset` the parallelism comes almost for free which is a __massive__ win for
beginners.

#### Up and Running

I am going to build an example data set which mimics the audio splitting
example I introduced. I will have a `dataset.csv` file which contains the
following:

```
input
a.txt
b.txt
c.txt
d.txt
```

Each TXT file will contain a word (`simple`, `scaleable`, `preprocessing`, and
`pytorch` respectively). The files will be located in an `inputs/` directory.
The goal is to split each word into parts of a certain number of characters and
overlap, e.g.

```python
a = "hello"
b = split_word(a, num_chars=2, overlap=1)
assert b == ["he", "el", "ll", "lo"]
c = split_word(a, num_chars=3, overlap=2)
assert c == ["hel", "ell", "llo"]
```

We can build a `Dataset` which performs this action on all of the input files.
First, let's generate a list of input files. I'll use the built-in CSV library:

```python
import csv

with open("dataset.csv", "r") as csv_file:
    reader = csv.DictReader(csv_file)
    input_files = [f"inputs/{row['input']}" for row in reader]

assert input_files == ["inputs/a.txt", "inputs/b.txt", "inputs/c.txt", "inputs/d.txt"]
```

To use `Dataset`, you'll need PyTorch (e.g. `pip3 install torch==1.5.0`)

```python
from torch.utils.data import Dataset

class WordSplitter(Dataset):
    def __init__(self, inputs, num_chars=2, overlap=1):
        self.inputs = inputs
        self.num_chars = num_chars
        self.overlap = overlap
        
    def __len__(self):
        return len(self.inputs)
    
    def __getitem__(self, idx):
        filename = self.inputs[idx]
        
        with open(filename, "r") as f:
            word = f.read().strip()
        
        return split_word(
            word,
            num_chars=self.num_chars,
            overlap=self.overlap
        )
```

For the `Dataset` to work, we need to define 3 "dunder" methods `__init__,
__len__, and __getitem`. The `__init__` function stores the input files and
parameters needed to run `split_word`. The `__len__` function returns the
length of `input_files`. The `__getitem__` function is where the computation
happens. First, we extract the file at the given index. Second, we read the
word from the file and remove any whitespace sorrounding the word. Finally, we
feed our word to `split_word` with the appropriate parameters. Let's see if it
works:

```python
word_splitter = WordSplitter(input_files, num_chars=3, overlap=2)
assert word_splitter[0] == ['sim', 'imp', 'mpl', 'ple']
```

Awesome. It is really important to make sure your `Dataset` works before moving
on to the next steps. Remember our signature from before:

```
InputFile -> (OutputFiles, Metadata)
```

Think of the `__getitem__` method in `WordSplitter` as inputting an
`InputFile`, not writing any `OutputFiles`, and producing `Metadata` related to
the operation. In the realistic audio splitting example the `OutputFiles` could
be written to an `outputs/` directory. We can now wrap this into a `DataLoader`
and run our analysis in parallel!

```python
from torch.utils.data import DataLoader

loader = DataLoader(
    word_splitter,
    batch_size=1,
    shuffle=False,
    num_workers=len(word_splitter),
)
```

The `DataLoader` bundles our work into batches to be operated on. The
`DataLoader` takes in the `word_splitter` `Dataset` object we initialized
previously. When we set `batch_size=1`, the `loader` will split our work into 4
total batches where each batch contains 1 file (`batch_size=2` means 2 batches
each with 2 files). With 4 batches it is possible to split the work over 4
cores on our machine by setting `num_workers=len(word_splitter)`. __Important:
with `batch_size=4` there is only 1 batch to process and therefore no
parallelism can be extracted (i.e. setting `num_workers` will have no
effect)__. The `shuffle=False`  argument asks the loader to process inputs in
order (the default). The `loader` object behaves like other iterators, i.e. we
can print the results in a `for` loop:

```python
for metadata in loader:
    print(metadata)
```

Let's look at the output:

```
[('sim',), ('imp',), ('mpl',), ('ple',)]
[('sca',), ('cal',), ('ale',), ('lea',), ('eab',), ('abl',), ('ble',)]
[('pre',), ('rep',), ('epr',), ('pro',), ('roc',), ('oce',), ('ces',), ('ess',), ('ssi',), ('sin',), ('ing',)]
[('pyt',), ('yto',), ('tor',), ('orc',), ('rch',)]
```

Hmm... Something looks weird, each string is embedded in a tuple. The issue is
PyTorch uses a collation function which is designed for their `Tensor` type. It
doesn't work great in this case. Luckily, we can define our own to fix this! In
the following code I will use `...` to represent code shown above. First, we
need to figure out what the input to `collate_fn` even looks like. Add the
`collate_fn` to `WordSplitter`

```python
 class WordSplitter(Dataset):
 	...
    
    @classmethod
    def collate_fn(*batch):
        print(f"BATCH: {batch}")
        return []
```

The `@classmethod` decorator allows us to call `WordSplitter.collate_fn`
(you'll see it in a moment). I use `*batch` to tuple up all of the inputs if
the arity is greater than one. The `collate_fn` isn't complete but this allows
us to inspect our inputs to the function. Second, we add our new function to
the `DataLoader`:

```python
loader = DataLoader(
	...,
    collate_fn=WordSplitter.collate_fn,
)
```

Note, you don't want to run this test over your entire data set. I would
suggest doing this on a small subset of inputs. If we loop over the loader
again,

```
BATCH: (<class '__main__.WordSplitter'>, [['sim', 'imp', 'mpl', 'ple']])
BATCH: (<class '__main__.WordSplitter'>, [['sca', 'cal', 'ale', 'lea', 'eab', 'abl', 'ble']])
BATCH: (<class '__main__.WordSplitter'>, [['pre', 'rep', 'epr', 'pro', 'roc', 'oce', 'ces', 'ess', 'ssi', 'sin', 'ing']])
BATCH: (<class '__main__.WordSplitter'>, [['pyt', 'yto', 'tor', 'orc', 'rch']])
[]
[]
[]
[]
```

Let's modify `batch_size=2` in the `loader` and see what happens when there is actual batching,

```
BATCH: (<class '__main__.WordSplitter'>, [['sim', 'imp', 'mpl', 'ple'], ['sca', 'cal', 'ale', 'lea', 'eab', 'abl', 'ble']])
BATCH: (<class '__main__.WordSplitter'>, [['pre', 'rep', 'epr', 'pro', 'roc', 'oce', 'ces', 'ess', 'ssi', 'sin', 'ing'], ['pyt', 'yto', 'tor', 'orc', 'rch']])
[]
[]
```

Okay, so PyTorch returns something like `(DatasetObject, [metadata0, metadata1,
...])`. All we need to do is extract the list of metadata from the tuple and
return it, i.e.

```python
@classmethod
def collate_fn(*batch):
    return batch[1]
```

In the `for` loop we need to additionally loop over the returned list of metadata, i.e.

```python
for metadatas in loader:
    for metadata in metadatas:
        print(metadata)
```

Result with `batch_size=1`,

```
['sim', 'imp', 'mpl', 'ple']
['sca', 'cal', 'ale', 'lea', 'eab', 'abl', 'ble']
['pre', 'rep', 'epr', 'pro', 'roc', 'oce', 'ces', 'ess', 'ssi', 'sin', 'ing']
['pyt', 'yto', 'tor', 'orc', 'rch']
```

With `batch_size=2`,

```
['sim', 'imp', 'mpl', 'ple']
['sca', 'cal', 'ale', 'lea', 'eab', 'abl', 'ble']
['pre', 'rep', 'epr', 'pro', 'roc', 'oce', 'ces', 'ess', 'ssi', 'sin', 'ing']
['pyt', 'yto', 'tor', 'orc', 'rch']
```

With `batch_size=4`,

```
['sim', 'imp', 'mpl', 'ple']
['sca', 'cal', 'ale', 'lea', 'eab', 'abl', 'ble']
['pre', 'rep', 'epr', 'pro', 'roc', 'oce', 'ces', 'ess', 'ssi', 'sin', 'ing']
['pyt', 'yto', 'tor', 'orc', 'rch']
```

Heck yes, this is exactly what we want! You could easily write this metadata
somewhere for further use. The key thing to remember here is that the
parallelism happens over batches, in this case the maximum possible cores used
with varying batch sizes:

|`batch_size`|cores|
|---|---|
|1|4|
|2|2|
|4|1|

The full code is available in a Jupyter Notebook on
[Github](https://github.com/chiroptical/pytorch-ray-workflow). This concludes
part 0. Next time we'll look into Ray and let it coordinate the
`Dataset/DataLoader` workflow over multiple nodes!

If you have any suggestions or improvements please message me on Twitter
[@chiroptical](https://twitter.com/chiroptical) or submit an issue on
[Github](https://github.com/chiroptical/pytorch-ray-workflow).

##### Edits

- 05/20/2020: Use snake-case over camel-case for `wordSplitter`
