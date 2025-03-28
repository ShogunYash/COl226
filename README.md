# COl226
 Sem 2402
# Matrix/Vector Language Interpreter

This repository contains a custom interpreter for a Matrix/Vector language. The project implements a lexer, parser, and type checker, allowing you to write, analyze, and run code written in this domain-specific language. The interpreter supports arithmetic operations, variable assignments, conditionals, loops, vector operations, function calls, and both single-line and multi-line comments.

## Features

- **Lexical Analysis & Parsing:**  
  Converts source code into an Abstract Syntax Tree (AST) using OCamllex and OCamlyacc.

- **Type Checking:**  
  Implements a type checker that verifies the correctness of programs. It reports errors with detailed messages including line and column numbers.

- **Language Constructs:**  
  Supports common programming constructs such as:
  - Variable assignments (e.g., `x := 10;`)
  - Arithmetic expressions (e.g., `5 + 10`)
  - Conditional statements (e.g., `if ... then ... else ... end`)
  - For loops (e.g., `for i := 1 to 10 do { ... } end`)
  - Vector operations and function calls
  - Comment handling (both `//` single-line and `/* ... */` multi-line)

- **Testing:**  
  Built-in test cases can be run to validate the functionality of the parser and type checker.

## Prerequisites

Make sure you have the following installed:

- [OCaml](https://ocaml.org)
- [OPAM](https://opam.ocaml.org)
- OCaml packages: `str` (Install with `opam install str`)
- `ocamlfind`, `ocamllex`, and `ocamlyacc`

## Installation

Clone this repository and navigate to its directory:

```bash
git clone https://github.com/yourusername/your-repository.git
cd your-repository
