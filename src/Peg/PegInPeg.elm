module Peg.PegInPeg exposing (..)

import Array exposing (Array)
import Dict
import Peg.Parse
import Peg.Rule exposing (Rule(..))


fromString input =
    Peg.Parse.parse grammar initialState actions (\_ _ state -> ( True, state )) input
        |> Result.map .grammar


{-| peg program for peg it self

     Grammar         <- Spacing Definition+ EndOfFile
     Definition      <- Identifier {RuleName} LEFTARROW Expression {AddGrammar}
     Expression      <- {ChoiceStart} Sequence {AddRule} (SLASH Sequence {AddRule})* {ChoiceEnd}
     Sequence        <- {SequenceStart} (Prefix {AddRule})* {SequenceEnd}
     Prefix          <- AND Action {SetConditionalPredicate}
                     / {PositiveLookaheadStart} AND Suffix {AddRule} {PositiveLookaheadEnd}
                     / {NegativeLookaheadStart} NOT Suffix {AddRule} {NegativeLookaheadEnd}
                     / Suffix
     Suffix          <- Primary ( QUERY {SetOptional} / STAR {SetZeroOrMore} / PLUS {SetOneOrMore} )?
     Primary         <- Identifier !LEFTARROW {SetRuleRef}
                     / OPEN Expression CLOSE
                     / Literal {SetMatchLiteral}
                     / Class {SetRange}
                     / DOT {SetMatchAny}
                     / Action {SetAction}
                     / BEGIN {CollectStart}
                     / END {CollectEnd}

     Identifier      <- < IdentStart IdentCont* > Spacing
     IdentStart      <- [a-zA-Z_]
     IdentCont       <- IdentStart / [0-9]
     Literal         <- ['] < ( !['] Char  )* > ['] Spacing
                      / ["] < ( !["] Char  )* > ["] Spacing
     Class           <- '[' < ( !']' Range )* > ']' Spacing
     Range           <- Char '-' Char / Char
     Char            <- '\\' [abefnrtv'"\[\]\\]
                      / '\\' [0-3][0-7][0-7]
                      / '\\' [0-7][0-7]?
                      / '\\' '-'
                      / !'\\' .
     LEFTARROW       <- '<-' Spacing
     SLASH           <- '/' Spacing
     AND             <- '&' Spacing
     NOT             <- '!' Spacing
     QUERY           <- '?' Spacing
     STAR            <- '*' Spacing
     PLUS            <- '+' Spacing
     OPEN            <- '(' Spacing
     CLOSE           <- ')' Spacing
     DOT             <- '.' Spacing
     Spacing         <- ( Space / Comment )*
     Comment         <- '#' ( !EndOfLine . )* EndOfLine
     Space           <- ' ' / '\t' / EndOfLine
     EndOfLine       <- '\r\n' / '\n' / '\r'
     EndOfFile       <- !.
     Action          <- '{' < [^}]* > '}' Spacing
     BEGIN           <- '<' Spacing
     END             <- '>' Spacing

-}
grammar =
    [ ( "Grammar", Sequence [ RuleRef "Spacing", OneOrMore (RuleRef "Definition"), RuleRef "EndOfFile" ] )
    , ( "Definition", Sequence [ RuleRef "Identifier", Action "RuleName", RuleRef "LEFTARROW", RuleRef "Expression", Action "AddGrammar" ] )
    , ( "Expression", Sequence [ Action "ChoiceStart", RuleRef "Sequence", Action "AddRule", ZeroOrMore (Sequence [ RuleRef "SLASH", RuleRef "Sequence", Action "AddRule" ]), Action "ChoiceEnd" ] )
    , ( "Sequence", Sequence [ Action "SequenceStart", ZeroOrMore (Sequence [ RuleRef "Prefix", Action "AddRule" ]), Action "SequenceEnd" ] )
    , ( "Prefix", Choice [ Sequence [ RuleRef "AND", RuleRef "Action", Action "SetConditionalPredicate" ], Sequence [ Action "PositiveLookaheadStart", RuleRef "AND", RuleRef "Suffix", Action "AddRule", Action "PositiveLookaheadEnd" ], Sequence [ Action "NegativeLookaheadStart", RuleRef "NOT", RuleRef "Suffix", Action "AddRule", Action "NegativeLookaheadEnd" ], RuleRef "Suffix" ] )
    , ( "Suffix", Sequence [ RuleRef "Primary", Optional (Choice [ Sequence [ RuleRef "QUERY", Action "SetOptional" ], Sequence [ RuleRef "STAR", Action "SetZeroOrMore" ], Sequence [ RuleRef "PLUS", Action "SetOneOrMore" ] ]) ] )
    , ( "Primary", Choice [ Sequence [ RuleRef "Identifier", NegativeLookahead (RuleRef "LEFTARROW"), Action "SetRuleRef" ], Sequence [ RuleRef "OPEN", RuleRef "Expression", RuleRef "CLOSE" ], Sequence [ RuleRef "Literal", Action "SetMatchLiteral" ], Sequence [ RuleRef "Class", Action "SetRange" ], Sequence [ RuleRef "DOT", Action "SetMatchAny" ], Sequence [ RuleRef "Action", Action "SetAction" ], Sequence [ RuleRef "BEGIN", Action "CollectStart" ], Sequence [ RuleRef "END", Action "CollectEnd" ] ] )
    , ( "Identifier", Sequence [ Sequence [ RuleRef "IdentStart", ZeroOrMore (RuleRef "IdentCont") ] |> Collect, RuleRef "Spacing" ] )
    , ( "IdentStart", range "a-zA-Z_" )
    , ( "IdentCont", Choice [ RuleRef "IdentStart", range "0-9" ] )
    , ( "Literal", Choice [ Sequence [ MatchLiteral "'", ZeroOrMore (Sequence [ NegativeLookahead (MatchLiteral "'"), RuleRef "Char" ]) |> Collect, MatchLiteral "'", RuleRef "Spacing" ], Sequence [ MatchLiteral "\"", ZeroOrMore (Sequence [ NegativeLookahead (MatchLiteral "\""), RuleRef "Char" ]) |> Collect, MatchLiteral "\"", RuleRef "Spacing" ] ] )
    , ( "Class", Sequence [ MatchLiteral "[", ZeroOrMore (Sequence [ NegativeLookahead (MatchLiteral "]"), RuleRef "Range" ]) |> Collect, MatchLiteral "]", RuleRef "Spacing" ] )
    , ( "Range", Choice [ Sequence [ RuleRef "Char", MatchLiteral "-", RuleRef "Char" ], RuleRef "Char" ] )
    , ( "Char", Choice [ Sequence [ MatchLiteral "\\", range "abefnrtv'\"[]\\" ], Sequence [ MatchLiteral "\\", range "0-3", range "0-7", range "0-7" ], Sequence [ MatchLiteral "\\", range "0-7", Optional (range "0-7") ], Sequence [ MatchLiteral "\\", MatchLiteral "-" ], Sequence [ NegativeLookahead (MatchLiteral "\\"), MatchAny ] ] )
    , ( "LEFTARROW", Sequence [ MatchLiteral "<-", RuleRef "Spacing" ] )
    , ( "SLASH", Sequence [ MatchLiteral "/", RuleRef "Spacing" ] )
    , ( "AND", Sequence [ MatchLiteral "&", RuleRef "Spacing" ] )
    , ( "NOT", Sequence [ MatchLiteral "!", RuleRef "Spacing" ] )
    , ( "QUERY", Sequence [ MatchLiteral "?", RuleRef "Spacing" ] )
    , ( "STAR", Sequence [ MatchLiteral "*", RuleRef "Spacing" ] )
    , ( "PLUS", Sequence [ MatchLiteral "+", RuleRef "Spacing" ] )
    , ( "OPEN", Sequence [ MatchLiteral "(", RuleRef "Spacing" ] )
    , ( "CLOSE", Sequence [ MatchLiteral ")", RuleRef "Spacing" ] )
    , ( "DOT", Sequence [ MatchLiteral ".", RuleRef "Spacing" ] )
    , ( "Spacing", ZeroOrMore (Choice [ RuleRef "Space", RuleRef "Comment" ]) )
    , ( "Comment", Sequence [ MatchLiteral "#", ZeroOrMore (Sequence [ NegativeLookahead (RuleRef "EndOfLine"), MatchAny ]), RuleRef "EndOfLine" ] )
    , ( "Space", Choice [ MatchLiteral " ", MatchLiteral "\t", RuleRef "EndOfLine" ] )
    , ( "EndOfLine", Choice [ MatchLiteral "\u{000D}\n", MatchLiteral "\n", MatchLiteral "\u{000D}" ] )
    , ( "EndOfFile", NegativeLookahead MatchAny )
    , ( "Action", Sequence [ MatchLiteral "{", ZeroOrMore (MatchNot '}') |> Collect, MatchLiteral "}", RuleRef "Spacing" ] )
    , ( "BEGIN", Sequence [ MatchLiteral "<", RuleRef "Spacing" ] )
    , ( "END", Sequence [ MatchLiteral ">", RuleRef "Spacing" ] )
    ]


initialState =
    { ruleName = ""
    , stack = []
    , rules = []
    , rule = Nothing
    , prefix = Nothing
    , grammar = []
    }


pushToStack _ state =
    { state | stack = state.rules :: state.stack, rules = [], rule = Nothing }


popFromStack fn _ state =
    case state.stack of
        [] ->
            state

        x :: xs ->
            { state
                | stack = xs
                , rules = x
                , rule =
                    state.rules |> applyIfMany fn
            }


popFromStackRule rule _ state =
    case state.stack of
        [] ->
            state

        x :: xs ->
            { state
                | stack = xs
                , rules = x
                , rule =
                    state.rules |> applyIfMany Sequence |> Maybe.map rule
            }


textToRule rule string state =
    { state | rule = Just (rule string) }


wrapRule wrap _ state =
    { state | rule = Maybe.map wrap state.rule }


addRule _ state =
    case state.rule of
        Just rule ->
            { state | rules = state.rules ++ [ rule ], rule = Nothing }

        Nothing ->
            state


matchLiteral string state =
    let
        unescape : String -> String
        unescape str =
            str
                |> String.replace "\\n" "\n"
                |> String.replace "\\r" "\u{000D}"
                |> String.replace "\\t" "\t"
                |> String.replace "\\b" "\u{0008}"
                |> String.replace "\\f" "\u{000C}"
                |> String.replace "\\\"" "\""
                |> String.replace "\\\\" "\\"
    in
    { state | rule = Just (MatchLiteral (unescape string)) }


actions key value inputState =
    Dict.fromList
        [ ( "SetConditionalPredicate", textToRule ConditionalPredicate )
        , ( "SetRange", textToRule range )
        , ( "SetOptional", wrapRule Optional )
        , ( "SetAction", textToRule Action )
        , ( "SetZeroOrMore", wrapRule ZeroOrMore )
        , ( "SetOneOrMore", wrapRule OneOrMore )
        , ( "SetMatchAny", \_ state -> { state | rule = Just MatchAny } )
        , ( "SetRuleRef", textToRule RuleRef )
        , ( "SetMatchLiteral", matchLiteral )
        , ( "CollectStart", pushToStack )
        , ( "CollectEnd", popFromStackRule Collect )
        , ( "ChoiceStart", pushToStack )
        , ( "ChoiceEnd", popFromStack Choice )
        , ( "SequenceStart", pushToStack )
        , ( "SequenceEnd", popFromStack Sequence )
        , ( "PushToStack", pushToStack )
        , ( "PositiveLookaheadStart", pushToStack )
        , ( "PositiveLookaheadEnd", popFromStackRule PositiveLookahead )
        , ( "NegativeLookaheadStart", pushToStack )
        , ( "NegativeLookaheadEnd", popFromStackRule NegativeLookahead )
        , ( "AddRule", addRule )
        , ( "RuleName", \string state -> { state | ruleName = string } )
        , ( "AddGrammar"
          , \_ state ->
                { state | grammar = state.grammar ++ [ ( state.ruleName, state.rule |> Maybe.withDefault (MatchLiteral "Grammar Add Error") ) ] }
          )
        ]
        |> (\dict ->
                case Dict.get key dict of
                    Just act ->
                        act value inputState |> Ok

                    Nothing ->
                        Err "No action found"
           )


range : String -> Rule
range inputString =
    let
        charToIndex =
            String.toList >> List.head >> Maybe.withDefault ' ' >> getIndex

        charRanges =
            String.indexes "-" inputString
                |> List.foldl
                    (\index acc ->
                        let
                            ( startChar, endChar ) =
                                ( String.slice (index - 1) index inputString
                                , String.slice (index + 1) (index + 2) inputString
                                )

                            ( from, to ) =
                                ( startChar, endChar )
                                    |> Tuple.mapBoth charToIndex ((+) 1 << charToIndex)
                        in
                        { acc
                            | rangeChars = Array.slice from to allChars |> Array.append acc.rangeChars
                            , nonRangeChars = String.replace (startChar ++ "-" ++ endChar) "" acc.nonRangeChars
                        }
                    )
                    { rangeChars = Array.empty
                    , nonRangeChars = inputString
                    }

        unescapeInRange : String -> String
        unescapeInRange str =
            str
                |> String.replace "\\n" "\n"
                |> String.replace "\\r" "\u{000D}"
                |> String.replace "\\t" "\t"
                |> String.replace "\\b" "\u{0008}"
                |> String.replace "\\f" "\u{000C}"
                |> String.replace "\\\"" "\""
                |> String.replace "\\\\" "\\"
                |> String.replace "\\[" "["
                |> String.replace "\\]" "]"
    in
    if String.startsWith "^" inputString then
        String.dropLeft 1 inputString
            |> String.toList
            |> List.head
            |> Maybe.withDefault ' '
            |> MatchNot

    else
        charRanges.nonRangeChars
            |> unescapeInRange
            |> String.toList
            |> Array.fromList
            |> Array.append
                charRanges.rangeChars
            |> Array.foldr (\item -> (::) (MatchLiteral (String.fromChar item))) []
            |> applyIfMany Choice
            |> Maybe.withDefault (MatchLiteral "ERROR")


allChars : Array Char
allChars =
    let
        numberChars =
            [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

        lowercaseChars =
            [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]

        uppercaseChars =
            [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' ]
    in
    numberChars ++ lowercaseChars ++ uppercaseChars |> Array.fromList


getIndex : Char -> Int
getIndex start =
    case start of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'a' ->
            10

        'b' ->
            11

        'c' ->
            12

        'd' ->
            13

        'e' ->
            14

        'f' ->
            15

        'g' ->
            16

        'h' ->
            17

        'i' ->
            18

        'j' ->
            19

        'k' ->
            20

        'l' ->
            21

        'm' ->
            22

        'n' ->
            23

        'o' ->
            24

        'p' ->
            25

        'q' ->
            26

        'r' ->
            27

        's' ->
            28

        't' ->
            29

        'u' ->
            30

        'v' ->
            31

        'w' ->
            32

        'x' ->
            33

        'y' ->
            34

        'z' ->
            35

        'A' ->
            36

        'B' ->
            37

        'C' ->
            38

        'D' ->
            39

        'E' ->
            40

        'F' ->
            41

        'G' ->
            42

        'H' ->
            43

        'I' ->
            44

        'J' ->
            45

        'K' ->
            46

        'L' ->
            47

        'M' ->
            48

        'N' ->
            49

        'O' ->
            50

        'P' ->
            51

        'Q' ->
            52

        'R' ->
            53

        'S' ->
            54

        'T' ->
            55

        'U' ->
            56

        'V' ->
            57

        'W' ->
            58

        'X' ->
            59

        'Y' ->
            60

        'Z' ->
            61

        _ ->
            0


applyIfMany : (List Rule -> Rule) -> List Rule -> Maybe Rule
applyIfMany f list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ ->
            Just (f list)
