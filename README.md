# glue

To play with template haskell in GHCi:

```
ghci -XTemplateHaskell
> :m +Language.Haskell.TH
> runQ [| 1 |]
```
