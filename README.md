# A Haskell module for JSON

Developed step by step as a learning exercise

## Step 1
## Define a data type for JSON Values

This is pretty straightforward:

```haskell
module Json
    ( JValue (JNull, JString, JInt, JReal, JTrue, JFalse, JArray, JObject) ) where

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
The module declaration at the top of the file just exports our data type to make it visible to the outside world.

You'll see that I decided to use separate value constructors **JTrue** and **JFalse**, rather than **JBool Boolean**. It isn't clear to me yet which would be better.

I also felt it would be better to handle **Ints** and **Doubles** separately.

And I'm not going to worry about escaped unicode values in strings.

Ok. Let's try it out:

```console
*Json> let j = JNull
*Json> j
JNull
*Json> :t j
j :: JValue

*Json> let j = JArray [JTrue, JFalse, JNull, JReal 1.23]
*Json> j
JArray [JTrue,JFalse,JNull,JReal 1.23]

*Json> let j = JObject [("name", JString "Paul"), ("age", JInt 7)]
*Json> j
JObject [("name",JString "Paul"),("age",JInt 7)]
*Json> :t j
```
