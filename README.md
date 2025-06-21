# 🧠 OCaml Expression Language Interpreter

This project is a small interpreter written in OCaml for evaluating arithmetic and logical expressions with support for variables, control flow, and future vector/matrix extensions. It uses a traditional compilation pipeline with a **lexer**, **parser**, **AST**, and **evaluator**.

---

## 📁 Project Structure

```
├── ast.ml          # Abstract Syntax Tree definitions
├── lexer.mll       # Lexical analyzer (using ocamllex)
├── parser.mly      # Parser specification (using menhir/ocamlyacc)
├── eval.ml         # Evaluation logic
├── main.ml         # Entry point (if applicable)
└── README.md       # You’re reading this
```

---

## ✅ Features

* [x] Integer and float arithmetic with auto-promotion
* [x] Boolean logic (`&&`, `||`, `!`)
* [x] Comparison operations (`=`, `<>`, `<`, `>`, `<=`, `>=`)
* [x] Statement and block execution
* [x] Mutable variable environment
* [x] Vector and matrix expressions
* [x] Control flow: `if`, `while`

---

## 🛠️ Build Instructions

Ensure you have OCaml and `menhir` installed. Then run:

```bash
make
```

Or manually:

```bash
ocamllex lexer.mll
menhir --ocamlc 'ocamlc -g' --infer parser.mly
ocamlc -g -c ast.ml
ocamlc -g -c parser.mli
ocamlc -g -c parser.ml
ocamlc -g -c lexer.ml
ocamlc -g -c eval.ml
ocamlc -g -o main ast.cmo parser.cmo lexer.cmo eval.cmo main.ml
```

---

## 📆 Example Usage

You can write an input program and run it via:

```bash
./main < input.txt
```

Sample `input.txt`:

```
x = 5 + 3;
y = x * 2.0;
!(x = y);
```

---

## 👨‍💻 Author

* **Nihal** (2023CS10058) – BTech CSE @ IIT Delhi
