# Haskeller Helper
- Haskell Cli Sandbox

### Compile
#### First Time
```
$ ghc -o build/run build/Main.hs
$ build/run
```
#### Otherwise
```
$ build/run
```

### Run
- `bin/hh`
  - default
- `bin/hh -h`
  - help

#### Notes
- `bin/hh -n <name>`
  - Create new note `<name>.txt`
  - If note exists, displays note.
- `bin/hh -na <name> <item>`
  - Add new item to note

- eg:
```
$ bin/hh -n todo
$ bin/hh -na todo "rake leaves"
$ bin/hh -n todo
```

#### Primes
- `bin/hh -p x`
  - find prime numbers up to integer `x`

