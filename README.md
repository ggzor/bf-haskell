<!-- vim:set tw=80 spell: -->

# bf-haskell

A throwaway [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) interpreter
that is:
- Slow as `s/Brainfuck/Brain//`, because of heavy use of monads.
- Humongous (~120 lines): relative to shorter bf interpreters.
- Not dependency-free: uses [mtl](https://hackage.haskell.org/package/mtl) and
  [containers](https://hackage.haskell.org/package/containers) (both
  almost-standard packages, but dependencies nonetheless).

## Usage

```bash 
cabal run bf-haskell -- PROGRAM 
```

## Examples

```bash 
cabal run bf-haskell -- ./examples/hello_world.bf
```

