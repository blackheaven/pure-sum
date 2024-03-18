# pure-sum

Allow deriving pure sum types string to be statically integrated to other
instances derivation mechanisms (e.g. [aeson](https://hackage.haskell.org/package/aeson)'s
`FromJSONKey`/`ToJSONKey`).

See [pure-sum-aeson](https://hackage.haskell.org/package/pure-sum-aeson).

## Example:

```haskell
data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToSumText, FromSumText) via (PureSum WeekDay)
```
