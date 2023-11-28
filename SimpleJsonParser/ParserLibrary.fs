module SimpleJsonParser.ParserLibrary

open System

type ParserLabel = string
type ParserError = string

type ParseResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

type Parser<'a> =
    { parseFn: InputState -> ParseResult<'a * InputState>
      label: ParserLabel }

let parserPositionFromInputState (inputState: InputState) =
    { currentLine = Input.currentLine inputState
      line = inputState.position.line
      column = inputState.position.column }


let printResult result =
    match result with
    | Success(value, _) -> printfn $"%A{value}"
    | Failure(label, error, parserPos) ->
        let errorLine = parserPos.currentLine
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        printfn $"Line:%i{linePos} Col:%i{colPos} Error parsing %s{label}\n%s{errorLine}\n%s{failureCaret}"

let satisfy predicate label =
    let innerFn input =
        let remainingInput, charOpt = Input.nextChar input

        match charOpt with
        | None ->
            let err = "No more input"
            let pos = parserPositionFromInputState input
            Failure(label, err, pos)
        | Some first ->
            if predicate first then
                Success(first, remainingInput)
            else
                let err = $"Unexpected '%c{first}'"
                let pos = parserPositionFromInputState input
                Failure(label, err, pos)

    { parseFn = innerFn; label = label }

let pChar charToMatch =
    let predicate ch = (ch = charToMatch)
    let label = (sprintf $"%c{charToMatch}")
    satisfy predicate label

let runOnInput parser input = parser.parseFn input

let run (parser: Parser<_>) input =
    runOnInput parser (Input.fromString input)

let bindP f p =
    let label = "unknown"

    let innerFn input =
        let result1 = runOnInput p input

        match result1 with
        | Failure(label, err, pos) -> Failure(label, err, pos)
        | Success(value1, remainingInput) ->
            let p2 = f value1
            runOnInput p2 remainingInput

    { parseFn = innerFn; label = label }

let (>>=) p f = bindP f p

let setLabel parser newLabel =
    let newInnerFn input =
        let result = parser.parseFn input

        match result with
        | Success s -> Success s
        | Failure(_, err, pos) -> Failure(newLabel, err, pos)

    { parseFn = newInnerFn
      label = newLabel }

let (<?>) = setLabel

let getLabel parser = parser.label

let returnP x =
    let label = $"%A{x}"
    let innerFn input = Success(x, input)

    { parseFn = innerFn; label = label }

let andThen p1 p2 =
    p1 >>= (fun p1Result -> p2 >>= (fun p2Result -> returnP (p1Result, p2Result)))

let (.>>.) = andThen

let applyP fP xP =
    fP >>= (fun f -> xP >>= (fun x -> returnP (f x)))

let orElse parser1 parser2 =
    let label = $"%s{getLabel parser1} orElse %s{getLabel parser2}"

    let innerFn (input: InputState) =
        let result1 = runOnInput parser1 input

        match result1 with
        | Success(r1, remaining1) -> Success(r1, remaining1)
        | Failure _ -> runOnInput parser2 input

    { parseFn = innerFn; label = label }

let (<|>) = orElse

let choice listOfParsers = List.reduce (<|>) listOfParsers

let anyOf listOfChars =
    let label = $"anyOf %A{listOfChars}"
    listOfChars |> List.map pChar |> choice <?> label

let mapP f = bindP (f >> returnP)


let (<!>) = mapP

let (|>>) x f = mapP f x

let (<*>) f x = applyP f x

let lift2 f xP yP = returnP f <*> xP <*> yP

let addP = lift2 (+)

let startsWith (str: string) (prefix: string) = str.StartsWith(prefix)

let startsWithP = lift2 startsWith

let rec sequence parserList =
    let cons head tail = head :: tail

    let consP = lift2 cons

    match parserList with
    | [] -> returnP []
    | head :: tail -> consP head (sequence tail)

let rec parseZeroOrMore parser (input: InputState) =
    let firstResult = runOnInput parser input

    match firstResult with
    | Failure _ -> ([], input)
    | Success(firstValue, inputAfterFirstParse) ->
        let subsequentValues, remainingInput = parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue :: subsequentValues
        (values, remainingInput)

let many parser =
    let label = $"many %s{getLabel parser}"
    let innerFn input = Success(parseZeroOrMore parser input)

    { parseFn = innerFn; label = label }

let many1 p =
    let label = $"many1 %s{getLabel p}"

    p >>= (fun head -> many p >>= (fun tail -> returnP (head :: tail))) <?> label

let charListToString charList = charList |> List.toArray |> String

let manyChars cp = many cp |>> charListToString

let manyChars1 cp = many1 cp |>> charListToString

let opt p =
    let label = $"opt %s{getLabel p}"
    let some = p |>> Some
    let none = returnP None
    (some <|> none) <?> label

let chooseFirst pF pS = pF .>>. pS |> mapP fst
let (.>>) = chooseFirst

let chooseSecond pF pS = pF .>>. pS |> mapP snd
let (>>.) = chooseSecond

let between p1 p2 p3 = p1 >>. p2 .>> p3

let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP |>> fun (p, pList) -> p :: pList

let sepBy p sep = sepBy1 p sep <|> returnP []

let pString (str: string) =
    str |> List.ofSeq |> List.map pChar |> sequence |>> charListToString


let whitespaceChar =
    let predicate = Char.IsWhiteSpace
    let label = "whitespace"
    satisfy predicate label

let spaces = many whitespaceChar

let spaces1 = many1 whitespaceChar

let digitChar =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label

let pInt =
    let label = "integer"

    let resultToInt (sign, digits) =
        let i = digits |> int

        match sign with
        | Some _ -> -i
        | None -> i

    let digits = manyChars1 digitChar

    opt (pChar '-') .>>. digits |> mapP resultToInt <?> label

let pFloat =
    let label = "float"

    let resultToFloat (((sign, digits1), _), digits2) =
        let fl = sprintf $"%s{digits1}.%s{digits2}" |> float

        match sign with
        | Some _ -> -fl
        | None -> fl

    let digits = manyChars1 digitChar

    let p = opt (pChar '-') .>>. digits .>>. pChar '.' .>>. digits
    p |> mapP resultToFloat <?> label

let rec readAllChars input =
    [ let remainingInput, charOpt = Input.nextChar input

      match charOpt with
      | None -> ()
      | Some ch ->
          yield ch
          yield! readAllChars remainingInput ]
