# Parsing expression grammar (PEG) in elm

# Elm PEG Parser

Elm PEG Parser is a library for parsing text using Parsing Expression Grammar (PEG).
It is implemented in Elm, a functional programming language for building web applications.

## Installation

Add the package to your Elm project using the following command:

```bash
elm install microelm/elm-peg
```

## Getting Started

To get started with Elm PEG Parser, you first need to define a PEG grammar using the `Grammar` type. Once you have
defined
your grammar, you can use the parse function to parse input text using the grammar.

```elm

import Html
import Peg exposing (Error, fromString, parse)


grammarString : String
grammarString =
    """
    start <- <digit+> {action}
    digit <- [0-9]
    """


{-| Parse a string with the grammar
-}
result : Result Error String
result =
    let
        actions _ found _ =
            Ok found

        predicate _ _ state =
            ( True, state )
    in
    grammarString
        |> fromString
        |> Result.andThen (\grammar -> parse grammar "" actions predicate "123")


{-| Check if the parse succeeded
-}
main =
    case result of
        Ok value ->
            Html.text ("Parsed value: " ++ value)

        Err error ->
            Html.text ("Parse error: " ++ error.message)
```

## API Documentation

The API documentation is available on the package [website](https://package.elm-lang.org/packages/microelm/elm-peg/latest/).

## Known Issues

- Detailed error handling is not fully implemented. Error messages are currently basic and may not be informative.
- The `ConditionalPredicate` rule is not fully implemented and may not behave as expected in certain cases.

## Acknowledgements

This library is based on the work of Ian Piumarta, who first introduced the PEG (Parsing Expression Grammar) formalism.
We would like to acknowledge his contributions to the field of programming language design and parsing.

Our library extends Ian's work by providing an Elm implementation of PEG parsing that is easy to use and customizable.
We have also added several new features, such as support for action functions and conditional predicates.

To learn more about Ian Piumarta's work, please visit [his website](https://www.piumarta.com/software/peg/).

## Contributing

Contributions are welcome!
Please read the [contributing guidelines](https://github.com/microelm/elm-peg/blob/release/CONTRIBUTING.md)
before submitting a pull request.

## License

This project is licensed under the Public Domain - see
the [LICENSE](https://github.com/microelm/elm-peg/blob/release/LICENSE) file for details.
