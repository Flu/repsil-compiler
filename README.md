# Repsil compiler

Repsil is a very unoriginal name for a strongly-typed, interpreted, Lisp-1 LISP dialect inspired mostly by Common Lisp.
It runs on its own VM, called the RVM (WIP). This file serves as both a quick guide on what the compiler can and cannot do, as well as the features of the language. The features are not necessarrily implemented, but should rather be seen as a wishlist for the compiler, as the project evolves. Hopefully, by the end, the list will also serve as documentation.

# Language features

## Typing
Being a strongly-typed and statically-typed language, it differs somewhat from the majority of LISPs, which are usually dynamic. Types are not inferred (although they could be by adding a Hindley-Milner system) and can be specified using atoms, like this: `:int` or `:string` or `:custom-data-type`. Parametrized data types should include paranthesis around them: `(:some-data-type :int :float)` is a data type parametrized by an integer type and a float type. This feature will probably arrive pretty late in the construction of this compiler.

## Functions

Functions are defined by specifying the name of the function, its return type, the arguments and their return type.
```lisp
(defun foo :int ((:int bar) (:int baz))
	(+ bar baz))
```

Optional arguments can be specified by adding a default value:
```lisp
(defun foo :int ((:int bar) (:int baz 0))
	(+ bar baz))
```

By default, a function's body can contain multiple expressions, desugaring into a `progn`. As such, the last executed expression of a function is also implicitly its return value.

## Packages

Packages are defined at the directory level, similar to Python. Take for example this directory structure:
```
project-foo
├── main.rpsl
└── mod
    ├── mod.rpsl
    └── other.rpsl
```

The directory `mod` is automatically considered a package if it has a valid `mod.rpsl` inside it and all functions in it must be namespaced with `mod:` if they are to be used in `main.rspl`. In this package structure, `mod.rpsl` should contain the export list of that package. In other words, `other.rpsl` may contain 10 functions, only 2 of which a user might want to export so they can be used in `main.rspl`. As such, `mod.rspl` would look like this:
```lisp
(import :other)

(export other:func-foo)
(export other:func-bar)
```

Here it is shown that it is also possible to import individual files as packages. Indeed, if a package is not big or complex enough to merit being in its own directory, it can just be a single file in the top-level, in which case all of its symbols will be exported.

If we wanted to use those exported functions in `main.rpsl`, it would look like this:
```lisp
(import :mod)

(defun main :int ()
	(mod:func-foo)
	(mod:func-bar))
```

One more complication, of course, is the case where there's another file called `mod.rpsl` in the top-level directory:
```
project-foo
├── main.rpsl
├── mod
│   ├── lib.rpsl
│   └── other.rpsl
└── mod.rpsl
```

This is disallowed and the compiler will raise an error because of the ambiguity of importing one file module and one directory module with the same name. In short, these are the rules for resolving an import:
For each directory `search_dir` in `search_paths`:

    1. Check for single-file module:
       Does 'search_dir / foo.rpsl' exist?
       -> If yes: Compile this file as module 'foo'. Done!

    2. Check for directory-level package:
       Does 'search_dir / foo/' exist?
       -> If yes: 
          Does 'search_dir / foo / foo.rpsl' exist?
          -> If yes: Compile 'foo/foo.rpsl' as module 'foo' (exporting its public API). Done!
          -> If no: Throw Compiler Error ("Package 'foo' is missing its entry point 'foo/foo.rpsl'").

    3. Raise error: "Module 'foo' not found."
