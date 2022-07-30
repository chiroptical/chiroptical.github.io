---
layout: post
title: 'Building Tensorflow GPU Images for HPC'
date:   2018-01-21
categories: jekyll blog
---

Background
---

I am an HPC administrator for Pitt. A common trend lately is users asking for
the newest Tensorflow release the second it is available. However, as many of
you probably know, compiling Tensorflow can be a bear. My daily Linux
distribution of choice is Arch Linux which is a bleeding edge distribution and
Tensorflow 1.4.1 is as easy as `sudo pacman -S python-tensorflow-opt-cuda`
(note, I use the `pip` package below). However, I have found that building GPU
enabled containers is a little tricky because if the underlying NVIDIA
libraries don't match it will never run on the GPU. First, we should talk about
building HPC containers

Enter Singularity
---

Singularity ([http://singularity.lbl.gov](http://singularity.lbl.gov)) is a
very powerful tool for reproducible research as well as portable software. It
is available on the Arch Linux User Repository (AUR) as `singularity-container`
and can be installed with `yaourt -S singularity-container` (I have always used
`yaourt` AUR package manager, others exist). If you need to build from source:

```bash
git clone https://github.com/singularityware/singularity.git
cd singularity
./autogen.sh
./configure
make
make install
make test
```

I have never had an issue compiling and installing this code. On Red Hat
Enterprise Linux, you will need to `sudo make install` for everything to work.
If you don't have access to install with `sudo`, add `
--prefix=<somewhere_you_have_access> --disable-suid` to the configure line (via
[Issue 1258](https://github.com/singularityware/singularity/issues/1258)). I am
not going to go over the basics of Singularity, check out their documentation
for that (or hit me up on Twitter and I'll write about it).

Setting Up
---

As I mentioned previously, the tricky part is getting the libraries to match. I
installed NVIDIA Drivers/CUDA using an orchestration tool called Warewulf (same
developer as Singularity!). On my compute nodes, these libraries are included
in the following packages:

1. `NVIDIA-Linux-x86_64-384.59.run`
2. `cuda_8.0.44_linux.run` (ships with CuDNN 5)

For TF 1.4.1, you also need CUDNN 6: `cucudnn-8.0-linux-x64-v6.0.tgz`. Ideally,
you would download all of these from the web. In the case of the slightly older
Driver/CUDA, I had a harder time finder these online. I keep the source on my
Warewulf master for safe keeping. You may need to ask your HPC administrator
for these packages.

Build File
---

With Singularity, you have some options for building containers and I chose to
use a bootstrap file. By convention it is titled `Singularity` and is
essentially a Bash script. First, I will paste the entire file and then break
it down section by section.

```bash
Bootstrap: docker
From: base/archlinux

%runscript
    exec python $*

%setup
    # Mirror list
    echo 'Server = http://mirror.cs.pitt.edu/archlinux/$repo/os/$arch' > $SINGULARITY_ROOTFS/etc/pacman.d/mirrorlist
    echo 'Server = http://mirrors.rit.edu/archlinux/$repo/os/$arch' >> $SINGULARITY_ROOTFS/etc/pacman.d/mirrorlist
    echo 'Server = http://mirror.es.its.nyu.edu/archlinux/$repo/os/$arch' >> $SINGULARITY_ROOTFS/etc/pacman.d/mirrorlist
    echo 'Server = http://mirrors.rutgers.edu/archlinux/$repo/os/$arch' >> $SINGULARITY_ROOTFS/etc/pacman.d/mirrorlist

    # NVidia
    VERSION=384.59
    sh NVIDIA-Linux-x86_64-$VERSION.run -x
    mv NVIDIA-Linux-x86_64-$VERSION $SINGULARITY_ROOTFS/usr/local
    cp links.sh $SINGULARITY_ROOTFS/root

    # CuDNN
    mkdir $SINGULARITY_ROOTFS/usr/local/cuda
    cp -R cudnn/* $SINGULARITY_ROOTFS/usr/local/cuda

    # CUDA
    dir=$(pwd)
    sh $dir/cuda_8.0.44_linux.run -extract=$dir/cuda
    $dir/cuda/cuda-linux64-rel-8.0.44-21122537.run --noexec --keep
    cp -R $dir/pkg/lib64/* $SINGULARITY_ROOTFS/usr/local/cuda/lib64

    # Cleanup
    rm -rf cuda pkg

%environment
    export LD_LIBRARY_PATH=/usr/local/cuda/lib64:/usr/local/NVIDIA-Linux-x86_64-384.59:$LD_LIBRARY_PATH
    export PATH=/usr/local/NVIDIA-Linux-x86_64-384.59:$PATH
    unset XDG_RUNTIME_DIR

%labels
    AUTHOR barrymoo

%post
    # Process NVIDIA links
    sh /root/links.sh 384.59

    # Install python and pip
    pacman -Syy --noconfirm python python-pip

    # Install tensorflow
    pip install --upgrade tensorflow-gpu
```

Metadata
---

This is the only section which doesn't start with a `%<section>`. In this case
I use `Bootstrap: docker` and `From: base/archlinux`. I am telling singularity
to start with an Arch Linux base image from DockerHub. In the next sections, I
will modify that container.

%runscript
---

This tells the container that when a user runs `singularity run
tensorflow-gpu.img ...` to run `python` (within the container) with `...` as
arguments. There is a lot more clever things one can do with this section, but
my users basically need to run `python <some_script.py>`

%setup
---

I will break this down into a few steps:

1. Generate a list of mirrors for `pacman`. In the `%post` section, we need to
   install things via `pacman` and will need to refresh mirrors. Note the use
   of `$SINGULARITY_ROOTFS`! In this section we are running from _outside_ the
   container! To refer to the root filesystem of the container we use this
   environment variable.
2. Install the NVIDIA driver which matches the compute nodes. Copy in the
   script `links.sh`, which I borrowed from [@clemsonciti on
   GitHub](https://github.com/clemsonciti/singularity-images/blob/master/dl/links.sh)
   (thanks!), which we need for inside the container.
3. I already had CuDNN extracted in this directory, simply make the
   `/usr/local/cuda` directory and copy CuDNN in.
4. Install CUDA. CUDA comes with 3 components, I only want the CUDA libraries.
5. Finally clean up the stuff we no longer need.

%environment
---

This generates a file `/environment` inside the container which singularity
runs when setting up the environment.

%labels
---

More metadata. There is probably more useful information I could put in here,
but I don't plan to distribute this container (it is specific to my current
compute node environment).

%post
---

Unlike `%setup` this section is executed _inside_ the container! Again the steps:

1. Run the `links.sh` script for our driver version.
2. Install `python` and `pip`.
3. Install `tensorflow-gpu` via `pip`. 

Here, you could install other packages. For example, I know my users will use
`cython` (installed via `pip`) and `gcc` (installed via `pacman`).

Bootstrap the Container
---

You will need root to build the container. Therefore, it makes sense to use
your own computer: `sudo singularity build tensorflow-gpu.img Singularity`.
After it is done building, 

```bash
$ singularity run tensorflow-gpu.img hello-world.py # a stupid simple hello TF script
... Errors complaining it can't run on GPU due to driver mismatch ...
b'Hello TF'
42
```

From my cluster:

```bash
$ singularity run tensorflow-gpu.img hello-world.py
...
Creating TensorFlow device (/device:GPU:0) -> (device: 0, name: GeForce GTX
1080, pci bus id: 0000:81:00.0, compute capability: 6.1)
...
b'Hello TF'
42
```

Fantastic! We are running TF 1.4.1 on a compute node without compiling anything!

Wrap Up
---

I think Singularity is fantastic. I spent a lot of time mucking around with
compiling TF by hand in our HPC environment. To be fair, I also spent a lot of
time mucking around with building Singularity images on GPUs. However, every
time a new release of TF comes around I can simply update the container and
stop compiling it by hand. As usual, if anyone thinks what I am doing is stupid
and you have a better way. Message me on Twitter.
