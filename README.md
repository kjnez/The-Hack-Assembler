# The Hack Assembler

This is a project to translate assembly code to machine code in the Hack system. It follows the contract described in **The Elements of Computing Systems**.

## Dependencies

The project is implemented using Common Lisp (SBCL). The only external package I used is uiop, you can install it through quicklisp:

```lisp
(ql-quickload :uiop)
```

## Usage

You can use it by providing assembly source file and the output file as follows:

```lisp
(assembler:from-assembly-to-binary "test/Rect.asm" "Rect.hack")
```

## Note
The assembly source files in `test` folder are borrowed from [nand2tetris](https://www.nand2tetris.org/software) website. You can test the translation accuracy by using the provided assembler on that website.

## License

MIT
