app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import pf.Stdin
import pf.Stderr
import Elm

main =
    Task.await
        (Stdin.readToEnd {})
        \input ->
            when Str.fromUtf8 input is
                Ok inputString ->
                    Stdout.write (inputString |> Elm.formatSingleModuleFormatSingleModule)

                Err _ ->
                    Stderr.line "could not decode incoming input into UTF-8"

