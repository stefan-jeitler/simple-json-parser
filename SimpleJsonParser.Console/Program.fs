open SimpleJsonParser.ParserLibrary
open SimpleJsonParser.JParser

// run jNull "null" |> printResult
// run jNull "nulp" |> printResult

// run jBool "true" |> printResult
// run jBool "false" |> printResult
// run jBool "truX" |> printResult

//run jUnescapedChar "a" |> printResult
//run jUnescapedChar "\\" |> printResult

//run jEscapedChar "\\\\" |> printResult // Success '\\'
//run jEscapedChar "\\t" |> printResult  // Success '\009'

// run jEscapedChar @"\\" |> printResult  // Success '\\'
// run jEscapedChar @"\n" |> printResult  // Success '\010'
// run jEscapedChar "a" |> printResult

//run jUnicodeChar "\\u263A" |> printResult

// run jString "\"\"" |> printResult
// run jString "\"a\"" |> printResult
// run jString "\"ab\"" |> printResult
// run jString "\"ab\\tde\"" |> printResult
// run jString "\"ab\\u263Ade\"" |> printResult

run jNumber' "00.1" |> printResult
run jNumber "123.4e5" |> printResult
