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

// run jNumber' "00.1" |> printResult
// run jNumber "123.4e5" |> printResult

//run jArray "[ 1, 2 ]" |> printResult

// run jObject """{ "a":1, "b"  :  2 }""" |> printResult
// run jObject """{ "a":1, "b"  :  2, }""" |> printResult

let example1 = """{
  "name" : "Stefan",
  "isMale" : true,
  "bday" : {"year":2001, "month":12, "day":25 },
  "favouriteColors" : ["blue", "green"],
  "emptyArray" : [],
  "emptyObject" : {}
}"""
run jValue example1 |> printResult

let example2= """{"widget": {
  "debug": "on",
  "window": {
    "title": "Sample Konfabulator Widget",
    "name": "main_window",
    "width": 500,
    "height": 500
  },
  "image": {
    "src": "Images/Sun.png",
    "name": "sun1",
    "hOffset": 250,
    "vOffset": 250,
    "alignment": "center"
  },
  "text": {
    "data": "Click Here",
    "size": 36,
    "style": "bold",
    "name": "text1",
    "hOffset": 250,
    "vOffset": 100,
    "alignment": "center",
    "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
  }
}}  """

run jValue example2 |> printResult