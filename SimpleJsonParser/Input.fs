namespace SimpleJsonParser

open System
open Position

type InputState =
    { lines: string array
      position: Position }

module Input =
    let fromString str =
        if String.IsNullOrEmpty(str) then
            { lines = [||]
              position = initialPosition }
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = str.Split(separators, StringSplitOptions.None)

            { lines = lines
              position = initialPosition }

    let currentLine inputState =
        let linePos = inputState.position.line

        if linePos < inputState.lines.Length then
            inputState.lines[linePos]
        else
            "EOF"

    let nextChar input =
        let linePos = input.position.line
        let colPos = input.position.column

        if linePos >= input.lines.Length then
            input, None
        else
            let currentLine = currentLine input

            if colPos < currentLine.Length then
                let char = currentLine[colPos]
                let newPos = incrementColumn input.position
                let newState = { input with position = newPos }
                newState, Some char
            else
                let char = '\n'
                let newPos = incrementLine input.position
                let newState = { input with position = newPos }
                newState, Some char