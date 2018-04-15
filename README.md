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

Notice that we asked the compiler to derive an instance of **Eq**. That means that the order of key-value pairs in a JObject will be significant, whereas in JavaScript it isn't.

You'll see that I decided to use separate value constructors **JTrue** and **JFalse**, rather than **JBool Boolean**. It isn't clear to me yet which would be better.

I also felt it would be better to handle **Ints** and **Doubles** separately.

And I'm not going to worry about escaped unicode values in strings.

Ok. Let's try it out:

```shell
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

```shell
String -> [String] -> String
```
intercalate takes a list of strings, inserts a given string in between each string in the list, and flattens the result. Like this:

```shell
Prelude Data.List> intercalate "," ["one", "two", "three"]
"one,two,three"
```
Before we can test the stringify function we will need to add it to the list of exports and we'll import Data.List.

```shell
*Json> putStrLn $ stringify $ JObject [("name", JString "Paul"), ("id", JReal 1.23), ("beer", JTrue)]
{"name":"Paul","id":1.23,"beer":true}
```
Ok, great. Now let's try pretty-printing.

First we'll write a helper function which will take a string parameter which we will use for indentation.

```haskell
prettify' :: String -> JValue -> String
prettify' spacer (JArray vs)  = "\n" ++ spacer ++ "[\n" ++ (intercalate ",\n" $ map (prettify' sp) vs) ++ "\n" ++ spacer ++ "]"
    where sp = spacer ++ "    "

prettify' spacer (JObject os) = "\n" ++ spacer ++ "{\n" ++ sp ++ (intercalate (",\n" ++ sp) $ map (\(key, val) -> show key ++ " : " ++ rend sp val) os) ++ "\n" ++ spacer ++ "}"
    where sp = spacer ++ "    "
          rend sp (JArray vs)  = prettify' sp (JArray vs)
          rend sp (JObject os) = prettify' sp (JObject os)
          rend sp x            = stringify x

prettify' spacer x = spacer ++ stringify x
```
We deal with three patterns:
Arrays, objects and everything else

Now, for the real **prettify** function which we will export. We just call **prettify'** with an empty string, so our output won't be indented at the outermost level.

```haskell
prettify :: JValue -> String
prettify = prettify' ""
```
