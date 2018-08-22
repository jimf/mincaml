# mincaml

MinCaml is an educational compiler for a minimal subset of OCaml. The
original, by Sumii et al., was written in ~2000 lines of OCaml, and
compiled down to SPARC assembly. This project is my attempt to implement
a MinCaml compiler in Rust. The output target is still TBD, though I'm
currently leaning toward Mach-O only because I develop on a Mac.

*Work in progress*

## Todo

 - [x] Lexical Analysis
 - [x] Parsing
 - [ ] Type Inference (In progress)
 - [ ] K-Normalization
 - [ ] α-Conversion
 - [ ] β-Reduction
 - [ ] Reduction of Nested let
 - [ ] Inline Expansion
 - [ ] Constant Folding
 - [ ] Elimination of Unnecessary Definitions
 - [ ] Closure Conversion
 - [ ] Virtual Machine Code Generation
 - [ ] 13-Bit Immediate Optimization
 - [ ] Register Allocation
 - [ ] Assembly Generation

## How to run

Assuming cargo/rustc are installed, the project can be run via cargo:

    $ cargo build
    $ cargo test
    $ cargo run

Right now, main.rs fires up a very basic REPL that will take MinCaml
expressions as input and outputs the respective AST. This will evolve as
the project moves forward.

## Motivation

I was looking for another compiler project, specifically with a small language
that was statically typed. I found [this similar question](https://stackoverflow.com/questions/1913621/is-there-a-simple-compiler-for-a-small-language)
on Stack Overflow, and was inspired by the [top comment](https://stackoverflow.com/questions/1913621/is-there-a-simple-compiler-for-a-small-language#1931417).
As for why Rust, I've been playing with it lately and wanted to do a more
substantial project in it.

## Resources

- [Original paper by Eijiro Sumii](http://esumii.github.io/min-caml/paper.pdf)
- [MinCaml website](http://esumii.github.io/min-caml/index-e.html)
- [min-caml GitHub](https://github.com/esumii/min-caml)

## License

MIT
