# foo-lang

This was simply an attempt at creating the base of a simple language. Currently it has bugs, even known bugs
(hence the failing tests) and I am not planning on fixing them. This was meant to be an experiment and not
more than that.

## Install

1. Install GHC and Stack on your system (with [https://www.haskell.org/ghcup/](GHCup))

2. Run `stack install` to install it under to `~/.local/bin`

3. The executable is called `foo-lang-exe` by default.

## Usage

`foo-lang-exe <file>`

For example `foo-lang-exe ./examples/fib2.foo`

