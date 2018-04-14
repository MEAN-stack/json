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

Cool, it works. The next step will be to write a **stringify** function.

## Step 2
## print and prettyprint

We'll write the equivalent of JavaScript's **JSON.stringify** function:

```haskell
stringify :: JValue -> String
stringify JNull = "null"
stringify (JString s) = show s
stringify (JInt i) = show i
stringify (JReal r) = show r
stringify JTrue = "true"
stringify JFalse = "false"
stringify (JArray vs) = "[" ++ (intercalate "," $ map stringify vs) ++ "]"
stringify (JObject os) = "{" ++ (intercalate "," $ map (\(key, val) -> show key ++ ":" ++ stringify val) os) ++ "}"
```

we're using the function **intercalate** from **Data.List**. Its type is:

```shell
intercalate :: [a] -> [[a]] -> [a]
```
In our case the type a is Char, so the signature is:

```bash
String -> [String] -> String
```
intercalate takes a list of strings, inserts a given string in between each string in the list, and flattens the result. Like this:

```shell
Prelude Data.List> intercalate "," ["one", "two", "three"]
"one,two,three"
```

