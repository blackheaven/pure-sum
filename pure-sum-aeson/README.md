# pure-sum-aeson

Correct [aeson](https://hackage.haskell.org/package/aeson)'s
`FromJSONKey`/`ToJSONKey` pure sum types derivations.

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
  deriving (ToSumText, FromSumText, FromJSON, FromJSONKey, ToJSON, ToJSONKey) via (PureSum WeekDay)
```
