module SimpleJsonParser.JParser

open System
open ParserLibrary

type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list

let (>>%) p x = p |>> (fun _ -> x)

let jNull = pString "null" >>% JNull <?> "null"

let jBool =
    let jTrue = pString "true" >>% JBool true
    let jFalse = pString "false" >>% JBool false

    jTrue <|> jFalse <?> "bool"

let jUnescapedChar =
    let label = "char"
    satisfy (fun ch -> ch <> '\\' && ch <> '\"') label

let jEscapedChar =
    [ ("\\\"", '\"')
      ("\\\\", '\\')
      ("\\/", '/')
      ("\\b", '\b')
      ("\\f", '\f')
      ("\\n", '\n')
      ("\\r", '\r')
      ("\\t", '\t') ]
    |> List.map (fun (toMatch, result) -> pString toMatch >>% result)
    |> choice
    <?> "escaped char"

let jUnicodeChar =
    let backslash = pChar '\\'
    let uChar = pChar 'u'
    let hexDigit = anyOf ([ '0' .. '9' ] @ [ 'A' .. 'F' ] @ [ 'a' .. 'f' ])
    let fourDigits = hexDigit .>>. hexDigit .>>. hexDigit .>>. hexDigit

    let convertToChar (((h1, h2), h3), h4) =
        let str = $"%c{h1}%c{h2}%c{h3}%c{h4}"
        Int32.Parse(str, Globalization.NumberStyles.HexNumber) |> char

    backslash >>. uChar >>. fourDigits |>> convertToChar

let quotedString =
    let quote = pChar '\"' <?> "quote"
    let jChar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar

    quote >>. manyChars jChar .>> quote

let jString = quotedString |>> JString <?> "quoted string"

let jNumber =
    let optSign = opt (pChar '-')
    let zero = pString "0"
    let digitOneNine = satisfy (fun ch -> Char.IsDigit ch && ch <> '0') "1-9"
    let digit = satisfy (fun ch -> Char.IsDigit ch) "digit"
    let point = pChar '.'
    let e = pChar 'e' <|> pChar 'E'
    let optPlusMinus = opt (pChar '+' <|> pChar '-')

    let nonZeroInt =
        digitOneNine .>>. manyChars digit |>> fun (first, rest) -> string first + rest

    let intPart = zero <|> nonZeroInt

    let fractionPart = point >>. manyChars1 digit

    let exponentPart = e >>. optPlusMinus .>>. manyChars1 digit

    let (|>?) opt f =
        match opt with
        | None -> ""
        | Some x -> f x

    let convertToJNumber (((optSign, intPart), fractionPart), expPart) =
        let signStr = optSign |>? string

        let fractionPartStr = fractionPart |>? (fun digits -> "." + digits) // e.g. ".456"

        let expPartStr =
            expPart
            |>? fun (optSign, digits) ->
                let sign = optSign |>? string
                "e" + sign + digits // e.g. "e-12"

        (signStr + intPart + fractionPartStr + expPartStr) |> float |> JNumber

    optSign .>>. intPart .>>. opt fractionPart .>>. opt exponentPart
    |>> convertToJNumber
    <?> "number"

let jNumber' = jNumber .>> spaces1

let createParserForwardedToRef<'a> () =

    let dummyParser: Parser<'a> =
        let innerFn _ = failwith "unfixed forwarded parser"
        { parseFn = innerFn; label = "unknown" }

    let parserRef = ref dummyParser

    let innerFn input = runOnInput parserRef.Value input
    let wrapperParser = { parseFn = innerFn; label = "unknown" }

    wrapperParser, parserRef

let jValue, jValueRef = createParserForwardedToRef<JValue> ()
jValueRef.Value <- jNumber

let jArray =
    let left = pChar '[' .>> spaces
    let right = pChar ']' .>> spaces
    let comma = pChar ',' .>> spaces
    let value = jValue .>> spaces

    let values = sepBy value comma

    between left values right |>> JArray <?> "array"

let jObject =
    let left = spaces >>. pChar '{' .>> spaces
    let right = pChar '}' .>> spaces
    let colon = pChar ':' .>> spaces
    let comma = pChar ',' .>> spaces
    let key = quotedString .>> spaces
    let value = jValue .>> spaces

    let keyValue = (key .>> colon) .>>. value
    let keyValues = sepBy keyValue comma

    between left keyValues right |>> Map.ofList |>> JObject <?> "object"


jValueRef.Value <- choice [ jNull; jBool; jNumber; jString; jArray; jObject ]
