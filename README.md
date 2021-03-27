# Publicodes, in Haskell

Publicodes seems very interesting, even though it's based on YAML:
https://github.com/betagouv/mon-entreprise/publicodes.

It's written in TypeScript and I wonder if I could re-implement it in Haskell.

I think this could be used to compute payrolls, but also to implement business
rules regarding e.g. the creation of contract (e.g. what VAT rate can be used,
if a risk declaration is required, ...).

```
$ runghc script.hs a
Result (Int 5)

$ runghc script.hs e
UnsetVariables ["e"]

$ runghc script.hs --set e 4 e
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
