Print pure [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) declarations as
[roc](https://www.roc-lang.org/) code.

```elm
import ElmSyntaxPrint
import ElmSyntaxParserLenient

"""module Sample exposing (..)

plus2 n =
    n + ([ 2 ] |> List.sum)
"""
    |> Elm.Parser.parseToFile
    |> Result.mapError (\_ -> "failed to parse elm source code")
    |> Result.map
        (\syntaxModule ->
            [ syntaxModule ]
                |> ElmSyntaxToRoc.modules
                |> Result.map ElmSyntaxToRoc.rocDeclarationsToModuleString
        )
-->
Ok """module [
    sample_plus2
]

sample_plus2 =
    \n ->
        Num.add (n) (List.sum ([ 2 ]))

..and some default declarations..
"""
```

To try it out, you can
run [this node script](https://github.com/lue-bird/elm-syntax-to-roc/tree/main/node-elm-to-roc).

### be aware

-   only a subset of elm is currently supported. not supported:
    -   `elm/regex`, `elm/file`, `elm/bytes`, `elm/http`, `elm/random`, `elm/url`, `elm/json`, `elm/parser`, `elm/virtual-dom`,
        `elm/html`, `elm/svg`, `elm/browser`, `elm/time`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`, `elm-explorations/linear-algebra`
    -   `Platform`, `Platform.Cmd`, `Platform.Sub`, `Task`, `Process`
    -   ports, glsl, the prefix operator functions `(>>)`, `(<<)`,
        curried functions like `List.map (Basics.remainderBy 2)` or `Result.map2 Tuple.pair a b`
    -   `String` uses UTF-8 instead of UTF-16 internally which means some helpers relying on code point offsets (like `String.length`) can return different results
    - `Char.toUpper`, `Char.toLower`, `String.toUpper`, `String.toLower` only convert latin letters a-Z and keep any other letter unchanged
    -   `++` will default to `List.append` unless one of the arguments is a string literal. So e.g. use `a ++ b ++ ""` to append string variables (which is also faster in elm).
        Similarly, sorting operations `<`, `>`, `<=` `>=`, `Basics.min`, `Basics.max`, `Basics.compare`, `List.sort` will only work on numbers,
        and `^` will not work if either argument is a concrete `Int` (not `Float` or `number` variable)
    -   using True or False in a `case`-`of` pattern
    -   potential future candidates: `Basics.(<<)`, `Basics.(>>)`, `Basics.truncate`, `Basics.modBy`, `Basics.clamp`, `Basics.degrees`, `Basics.turns`,
        `Basics.radians`, `Basics.logBase`, `Basics.atan2`, `Basics.toPolar`, `Basics.fromPolar`, `Basics.never`, `String.reverse`, `List.map5`, `Char.toLocaleLower`, `Char.toLocaleUpper`, `Char.isAlpha`, `Char.isAlphaNum`, `Char.isDigit`, `Char.isOctDigit`, `Char.isHexDigit`, `List.sortBy`, `List.head`, `List.tail`, `List.unzip`, `List.partition`, `Dict.update`, `Dict.merge`, `Dict.intersect`, `Dict.partition`, `Bitwise`, `Set`, `Array`. Any help appreciated!
-   no checks are performed before transpiling to roc. So if you don't add a compile check of your elm input,
    you might e.g. get a running program that circumvents an elm opaque type or a roc program that can't be run
-   not much care has been put into making the resulting code readable or even conventionally formatted
    and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-format/issues/new) you notice <3

### why roc?

-   it runs decently fast and can target Wasm
-   it's pretty much a superset of elm and doesn't require any types which makes transpiling trivial (for the record I failed at transpiling to rust because I was not skilled enough to generate types for types like curried rc'd closures)
