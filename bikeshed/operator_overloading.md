```
operator +. (This, Other)
operator -. (This, Other)
operator *. (This, Other)
operator /. (This, Other)
operator %. (This, Other)
operator &. (This, Other)
operator |. (This, Other)
operator ^. (This, Other)
operator &[]. (&This, Other)
operator &mut []. (&mut This, Other)
operator -.(This)
operator !.(This)
operator <<.(This, Other)
operator >>.(This, Other)

OneOf:
operator <=> (&This, Other) -> StrongOrdering or (&This, Other) -> PartialOrdering
operator == (&This, Other) -> bool

+=, -=, *=, /=, %=, >>=, <<=, &=, ^=, |= are all generated
eg (val += other) is expanded to val = val + other (val is moved)

Consider:
deref and deref_mut

```