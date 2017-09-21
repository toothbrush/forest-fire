# forest-fire

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

## Prerequisites

You'll need the following installed and available to be able to use
this software:

* Haskell Stack
* AWS CLI interface

## Instructions

tba

# Usage

If you simply run the tool without arguments, it'll print usage
information.  Here's the down-low, however.

## Find out what depends on a stack

```sh
stack exec forest-fire -- "kubernetes-dynamic-91acf0ef-lifecycle"
```

## Perform the deletions if you're satisfied with the tree

```sh
stack exec forest-fire -- "kubernetes-dynamic-91acf0ef-lifecycle" --delete
```

# Credits

Thanks Redbubble, i totally should've been doing other things instead
of shaving this yak.
