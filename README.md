# A Haskell module for JSON

Developed step by step as a learning exercise

## Step 1
## Define a data type for JSON Values

This is pretty straightforward:

```haskell
data JValue =
    JNull 
    | JString String
    | JInt Int
    | JReal Double
    | JTrue
    | JFalse
    | JArray [JValue]
    | JObject [(String, JValue)]
    deriving (Read, Show, Eq)
```

You'll see that I decided to use separate value constructors **JTrue** and **JFalse**, rather than **JBool Boolean**. It isn't clear to me yet which would be better.
I also felt it would be better to handle **Ints** and **Doubles** separately.
And I'm not going to worry about escaped unicode values in strings.
