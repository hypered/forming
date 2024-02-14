# Forming

Forming is an attempt to make it easy to describe user interfaces to collect
data. These can be for instance HTML forms or command-line interfaces.

It has some resemblance with Publicodes, which is based on YAML and implemented
in TypeScript: https://github.com/betagouv/mon-entreprise/publicodes.

The main idea of Forming is that it uses an AST to describe computations
similar to a simple expression language (additions, if-then-else expressions,
...) with two important things:

- Computed values can be objects, which thus can represent a filled form.
- Variables used in the computed expression can be "unset", meaning they have
  to be filled when the expression is evaluated.

This means the following: a form is a simple lazily evaluated expression that
returns a structured representation of some of its variables, together with
unset variables, which thus can be required and given as needed.

Computations can be used to describe e.g. payrolls, but also to implement
business rules regarding e.g. the creation of contracts (e.g. what VAT rate can
be used, if a risk declaration is required, ...).


# Features

The CLIs produced with Forming:

- Can represent simple computations (additions, if-then-else expressions)
- Can represent forms by "computing" an object (i.e. a dict, or map)
- User inputs can be given by `--set` arguments or as JSON with `--json`
- Can list the rules involved in a computation
- Can list the unset variables, possibly limited to the ones necessary for a
  given rule
- Can display a help message

The HTTP server can:

- List the available forms
- For each, either display a description, or the form itself
- For each, compute a result and display it (although a bit crudely)

Concrete syntax:

- Most of the examples below are hard-coded expressions in `forming-example.hs`
  but I'm trying to provide a concrete syntax
- Some example files are in `examples/` and they can be used by the
  `forming.hs` program
- That program requires the
  [`syntactical`](https://github.com/noteed/syntactical) library


# Development

I use `scripts/ghci.sh`. This loads `shell.nix` automatically.

If necessary, I can use a Nix shell provided by `shell.nix` manually:

```
$ nix-shell
```

I also need to bring code from my
[`design`](https://github.com/hypered/design) repository, which
is expected to live at `../design`, relative to this repository. So e.g.
when using `runghc`:

```
$ runghc -XOverloadedStrings -i../design/src -isrc/ bin/add.hs --html
$ find generated/
```

The HTTP server needs to know the location of the `static/` directory. This is
given through an environment variable:

```
$ FORMING_SITE_DIR=../design runghc -XOverloadedStrings -i../design/src -isrc/ bin/forming-examples.hs serve
```

This is being changed to use another version, called Struct (not yet ready at
all):

```
$ nix-build -A struct --out-link _site
$ FORMING_SITE_DIR=_site runghc -XOverloadedStrings -i../design/src -isrc/ bin/forming-examples.hs serve
```

It is written using the Snap Framework. An alternative using Servant has been
started (but is almost empty). Replace `serve` with `servant` to use it.


# Tests

Tests can be run with the following command:

```
$ runghc -XOverloadedStrings -i../design/src -isrc/ -itests/ tests/run-tests.hs
```

They can be read to learn the behavior of Forming.


# Example usage (binary built using the library)

It's possible to run Forming as a server:

```
$ nix-shell
$ scripts/serve-examples.sh
```

The current homepage at `/` is re-used from another project. Instead, navigate
to `/noteed` to see a list of forms.

The same program can also be used as a command-line tool:

```
$ runghc -XOverloadedStrings -i../design/src -isrc/ bin/forming-examples.hs run --help
```

```
$ runghc -XOverloadedStrings -i../design/src -isrc/ bin/forming-examples.hs run trivial-1
1

$ runghc -XOverloadedStrings -i../design/src -isrc/ bin/forming-examples.hs run trivial-a
ERROR: missing user inputs.
This computation expects the following user inputs:

  a

Use `--set '("a","1")'` to provide the value 1 to the input "a".

$ runghc -XOverloadedStrings -i../design/src -isrc/ bin/forming-examples.hs run trivial-a --set '("a","1")'
1

$ runghc -XOverloadedStrings -i../design/src -isrc/ bin/forming-examples.hs run trivial-a --json '{"a": 1}'
{"ouput":1}
```


# Example usage (concrete syntax)

```
$ runghc -XOverloadedStrings -i../design/src -i../syntactical -isrc/ bin/forming.hs run --file examples/trivial-1.fg
1
```

```
$ FORMING_SITE_DIR=../design runghc -XOverloadedStrings -i../design/src -i../syntactical -isrc/ bin/forming.hs serve --file examples/trivial-a.fg
```

Then visit `http://127.0.0.1:8000/examples/TODO` or
`http://127.0.0.1:8000/examples/TODO/+view`. (The `TODO` should be `trivial-a` or
`examples/trivial-a` in the future.)


# See also

Source of inspiration could be:

- Netlify, in particular their Forms offering
- Publicodes
- Typeform


# Decimal

```
Prelude Data.Decimal> (3500 :: Decimal)
3500
Prelude Data.Decimal> (3500.00 :: Decimal)
3500
Prelude Data.Decimal> (read "3500.00" :: Decimal)
3500.00
Prelude Data.Decimal> Decimal 2 350000
3500.00
Prelude Data.Decimal> roundTo 2 3500
3500.00
```

```
Prelude Data.Decimal> (read "3500.00" :: Decimal)
3500.00
Prelude Data.Decimal> (read "3500.00" :: Decimal) * 1.08
3780
```


# Notes

It seems in Publicodes that a simple rule:

```
a: 5
```

is a short form for:

```
a:
  formule: 5
```
