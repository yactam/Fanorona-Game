# Fanorona

## Description

Fanorona is our first project using <b>The OCaml programming language</b> which is a strategy board game for two players. The game is indigenous to Madagascar..

## Building Fanorona

To build the project just lunch this command in the terminal:

```bash
    $ dune build
```

For continuous build, use:

```bash
    $ dune build --watch
```

## Execute the game

After building just run:

```bash
    $ dune exec fanorona
```

## Running tests

To run the tests provided in the project use:

```bash
    $ dune runtest
```

This can be combined with continuous build & test, using:

```bash
    $ dune runtest --watch
```
