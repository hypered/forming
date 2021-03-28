# Publicodes, in Haskell

Publicodes seems very interesting, even though it's based on YAML:
https://github.com/betagouv/mon-entreprise/publicodes.

It's written in TypeScript and I wonder if I could re-implement it in Haskell.

I think this could be used to compute payrolls, but also to implement business
rules regarding e.g. the creation of contract (e.g. what VAT rate can be used,
if a risk declaration is required, ...).

In addition, I think it can be used to create forms, by allowing to return
objects (also called attribute sets, maps, dictionaries) that represent filled
forms.

In other words, it seems a form can be described as: a simple lazily evaluated
expression that returns a structured representation of its input, possibly with
a list of missing inputs.


# Example usage

```
$ runghc script.hs a
Result (Int 5)

$ runghc script.hs e
UnsetVariables ["e"]

$ runghc script.hs --set e 4 e
Input "e" (Int 4)
Result (Int 4)
```

In the following example, if `i` is `True`, the value is taken from `a`, which
is 5. But if it is set to `False`, the value comes from `e`, which is unset and
thus must be set.

```
$ runghc script.hs --set i True l
Input "i" (Bool True)
Result (Int 5)

$ runghc script.hs --set i False l
Input "i" (Bool False)
UnsetVariables ["e"]

$ runghc script.hs --set i False --set e 4 l
Input "i" (Bool False)
Input "e" (Int 4)
Result (Int 4)
```


# Notes

A simple rule:

```
a: 5
```

is a short form for:

```
a:
  formule: 5
```
