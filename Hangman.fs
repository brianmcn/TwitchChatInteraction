module Hangman

let BASEX, BASEY, BASEZ = 64, 254, 64

type WOOL =
    | White 
    | Orange 
    | Magenta 
    | LightBlue 
    | Yellow 
    | Lime 
    | Pink 
    | Gray 
    | LightGray 
    | Cyan 
    | Purple 
    | Blue 
    | Brown 
    | Green 
    | Red 
    | Black 
    member this.ToDamageValue() =
        match this with
        | White -> 0
        | Orange -> 1
        | Magenta -> 2
        | LightBlue -> 3
        | Yellow -> 4
        | Lime -> 5
        | Pink -> 6
        | Gray -> 7
        | LightGray -> 8 
        | Cyan -> 9
        | Purple -> 10
        | Blue -> 11
        | Brown -> 12
        | Green -> 13
        | Red -> 14
        | Black -> 15

let THE_GRID =
    [|
        "............"
        "............"
        "............"
        "............"
        "............"  |]
        // then blank line, then 'choices', then 3 lines of alpha

let charToPixels(c) = 
    if c = '_' then 
        Alphabet.underscore
    elif c = ' ' then 
        Alphabet.space
    else
        let n = int(System.Char.ToLowerInvariant(c)) - int('a')
        if n < 0 || n > 25 then failwith "bad letter input"
        let letter = Alphabet.letters.[n]
        letter

let putLetterAt(letter, row, col, wool:WOOL, minecraftStdin : System.IO.StreamWriter) =
    if row < 0 || row > 9 then failwith "bad row input"
    if col < 0 || col > 11 then failwith "bad col input"
    let letter = charToPixels(letter)
        
    let x = BASEX + 10*col
    let y = BASEY
    let z = BASEZ + 12*row
    for i = 0 to 9 do
        for j = 0 to 11 do
            minecraftStdin.WriteLine(sprintf "setblock %d %d %d %s" (x+i) y (z+j) (if letter.[j].[i] = 'X' then "wool "+wool.ToDamageValue().ToString() else "wool 0"))
    minecraftStdin.Flush()

let putLineAt(line:string, row, wool, minecraftStdin) =
    if line.Length > 12 then failwith "line too long"
    for i = 0 to line.Length-1 do
        putLetterAt(line.[i], row, i, wool, minecraftStdin)

let drawInitPuzzle(puzzle:string[], minecraftStdin:System.IO.StreamWriter) =
    if puzzle.Length > 5 then failwith "puzzle has too many lines"
    for i = 0 to puzzle.Length-1 do if puzzle.[i].Length > 12 then failwithf "puzzle line %d too long" i

    let toUnderscores(s) =
        let sb = System.Text.StringBuilder()
        for c in s do 
            if c=' ' then sb.Append(c) |> ignore
            else sb.Append('_') |> ignore
        sb.ToString()

    minecraftStdin.WriteLine(sprintf "fill %d %d %d %d %d %d wool 0" BASEX BASEY BASEZ (BASEX+63) BASEY (BASEZ+63))
    minecraftStdin.WriteLine(sprintf "fill %d %d %d %d %d %d wool 0" (BASEX+64) BASEY BASEZ (BASEX+127) BASEY (BASEZ+63))
    minecraftStdin.WriteLine(sprintf "fill %d %d %d %d %d %d wool 0" BASEX BASEY (BASEZ+64) (BASEX+63) BASEY (BASEZ+127))
    minecraftStdin.WriteLine(sprintf "fill %d %d %d %d %d %d wool 0" (BASEX+64) BASEY (BASEZ+64) (BASEX+127) BASEY (BASEZ+127))
    minecraftStdin.Flush()

    for i = 0 to puzzle.Length-1 do
        putLineAt(toUnderscores(puzzle.[i]), i, WOOL.Blue, minecraftStdin)

    putLineAt("       used", 6, WOOL.Pink, minecraftStdin)
    putLineAt("avail",       6, WOOL.Lime, minecraftStdin)
    putLineAt("abcdefghi",   7, WOOL.Lime, minecraftStdin)
    putLineAt("jklmnopqr",   8, WOOL.Lime, minecraftStdin)
    putLineAt("stuvwxyz",    9, WOOL.Lime, minecraftStdin)

let updatePuzzleDraw(c, puzzle:string[], minecraftStdin:System.IO.StreamWriter) =
    let mutable foundLetter = false
    // draw in correct guess letters
    for i = 0 to puzzle.Length-1 do
        for j = 0 to puzzle.[i].Length-1 do
            if c=puzzle.[i].[j] then
                putLetterAt(c, i, j, WOOL.Blue, minecraftStdin)
                foundLetter <- true
    // mark the 'used' letter off
    let puzzle = ["abcdefghi"; "jklmnopqr"; "stuvwxyz"]
    for i = 0 to puzzle.Length-1 do
        for j = 0 to puzzle.[i].Length-1 do
            if c=puzzle.[i].[j] then
                putLetterAt(c, i+7, j, WOOL.Pink, minecraftStdin)
    foundLetter

    
