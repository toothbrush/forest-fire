# forest-fire [![Build Status](https://travis-ci.org/toothbrush/forest-fire.svg?branch=master)](https://travis-ci.org/toothbrush/forest-fire)

This is a little command-line tool with an ill-advised name, to easily
tear down CloudFormation stacks which have outputs that other stacks
depend on.  In the AWS Console this is rather annoying, since you have
to manually chase up dependencies.

This tool simply interrogates the `aws-cli` tool about the stack
you're trying to delete, finds out its outputs, and checks whether any
currently-active stacks are importing them.  The output is a
dependency tree, which trivially tells us the order of deletion for it
to succeed.  If you're feeling adventurous, you may also let
`forest-fire` do the actual deletion for you.

# Installation

If you're not interested in hacking on this project, you can simply
download and install it.  Run:

```sh
cabal update; cabal install forest-fire
```

## Prerequisites for hacking

You'll need the following installed and available to be able to hack
on or contribute to this software:

### [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

You'll want to install Stack using your local package manager (yes,
it's available on Homebrew as `haskell-stack`), or if you're
adventurous, using their `curl | bash` method...

You'll need to add `~/.local/bin` to your `$PATH`.

### AWS CLI interface

I'm guessing that this is a thing you'll already have.

# Usage

If you simply run the tool without arguments, it'll print usage
information.  Here's the down-low, however.

## Find out what depends on a stack

Note that by default `forest-fire` performs a **dry run** (read-only).  The order in which
you'd have to perform deletions will be printed, but nothing will be
executed.

One or more stack names can be provided as command-line arguments.

```sh
forest-fire "kubernetes-dynamic-91acf0ef-lifecycle"
```

## Perform the deletions if you're satisfied with the results

```sh
forest-fire "kubernetes-dynamic-91acf0ef-lifecycle" --delete
```

# Do you Docker?

Some people don't believe in native executables.  For them, i present
the Dockerised version:

```sh
docker container run --rm \
    -e AWS_SECRET_ACCESS_KEY \
    -e AWS_ACCESS_KEY_ID \
    -e AWS_DEFAULT_REGION \
    paulrb/forest-fire:master yourstack
```

It is hosted on Docker Hub: https://hub.docker.com/r/paulrb/forest-fire
and built with Travis CI: https://travis-ci.org/toothbrush/forest-fire
from this repository.

# Credits

Thanks Redbubble, i totally should've been doing other things instead
of shaving this yak.
