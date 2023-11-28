namespace SimpleJsonParser

type ParserPosition =
    { currentLine: string
      line: int
      column: int }

module Position =

    type Position = { line: int; column: int }

    let initialPosition = { line = 0; column = 0 }

    let incrementColumn (pos: Position) = { pos with column = pos.column + 1 }

    let incrementLine pos = { line = pos.line + 1; column = 0 }


