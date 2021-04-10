# Forming

Forming is an attempt to make it easy to describe user interfaces to collect
data. These can be for insantance HTML forms or command-line interfaces.

It has some ressemblance with Publicodes, which is based on YAML and
implemented in TypeScript:
https://github.com/betagouv/mon-entreprise/publicodes.

The main idea of Forming is that it uses an AST to describe computations
similar to a simple expression language (addition, if-then-else expressions,
...) with two important things:

- Computed values can be objects, which thus can represent a filled form.
- Variables used in the computed expression can be "unset", meaning they have
  to be filled when the expression is evaluated.

This means the following: a form is a simple lazily evaluated expression that
returns a structured representation of some of its variables, together with
unset variables, which thus can be required and given as needed.

Computations can be used to describe payrolls, but also to implement business
rules regarding e.g. the creation of contract (e.g. what VAT rate can be used,
if a risk declaration is required, ...).


# Features

The CLIs produced with Forming:

- Can represent simple computation (additions, if-then-else expressions)
- Can represent forms by "computing" an object (i.e. a dict, or map)
- User inputs can be given by `--set` arguments or as JSON with `--json`
- Can list the rules involved in a computation
- Can list the unset variables, possibly limited to the ones necessary for a
  given rule
- Can display a help message


# Development

I currently use this Nix shell:

```
$ nix-shell -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.aeson])"
```


# Tests

Tests can be run with the following command:

```
$ runghc bin/run-tests.hs
```


# Example usage

```
$ runghc bin/trivial-1.hs 
1

$ runghc bin/trivial-a.hs 
ERROR: missing user inputs.
This computation expects the following user inputs:

  a

Use `--set a 1` to provide the value 1 to the input "a".

$ runghc bin/trivial-a.hs --set a 1
1

$ runghc bin/trivial-a.hs --json '{"a": 1}'
1
```

```
$ runghc bin/play.hs a
Result (Int 5)

$ runghc bin/play.hs e
UnsetVariables ["e"]

$ runghc bin/play.hs --set e 4 e
Input "e" (Int 4)
Result (Int 4)
```

In the following example, if `i` is `True`, the value is taken from `a`, which
is 5. But if it is set to `False`, the value comes from `e`, which is unset and
thus must be set.

```
$ runghc bin/play.hs --set i True l
Input "i" (Bool True)
Result (Int 5)

$ runghc bin/play.hs --set i False l
Input "i" (Bool False)
UnsetVariables ["e"]

$ runghc bin/play.hs --set i False --set e 4 l
Input "i" (Bool False)
Input "e" (Int 4)
Result (Int 4)
```

Assertions:

```
$ runghc bin/play.hs --set e 1 r
Input "e" (Int 1)
Error (AssertionIntError (GreaterThan 1))

$ runghc bin/play.hs --set e 2 r
Input "e" (Int 2)
Result (Int 2)
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
