# Project for FP course at TU Delft

A clone of `jq` in Haskell.

Read `ASSIGNMENT.md` for task description and detailed instructions

## Build
```
> stack build
```

## Install

```
> stack install
```

this installs your executable to `~/.local/bin` by default (on *nix), make sure it's in $PATH

## Use

```
> echo '{"this" : "that"}' | jq-clone '.this'
```

or

```
> echo '{"this" : "that"}' | stack run -- '.this'
```

## Test `jq` online

[jqplay.org](https://jqplay.org/)

## Docs

[stedolan.github.io/jq/manual](https://stedolan.github.io/jq/manual)
