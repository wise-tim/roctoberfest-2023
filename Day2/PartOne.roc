app "roctoberfest-day-2-part-1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout, pf.Stderr, pf.Path, pf.File, pf.Task.{ Task }]
    provides [main] to pf

main : Task {} I32
main =
    total =
        input <- Path.fromStr "./Day2/test_input.txt"
            |> File.readUtf8
            |> Task.await

        gameResults <- input
            |> Str.split "\n"
            |> List.map \line ->
                when Str.split line " " is
                    [first, second] ->
                        f <- decodeFirst first |> Result.try
                        s <- decodeSecond second |> Result.map

                        outcomePoints =
                            when (f, s) is
                                (Rock, Rock) -> 3
                                (Paper, Paper) -> 3
                                (Scissors, Scissors) -> 3
                                (Rock, Scissors) -> 0
                                (Paper, Rock) -> 0
                                (Scissors, Paper) -> 0
                                (Rock, Paper) -> 6
                                (Paper, Scissors) -> 6
                                (Scissors, Rock) -> 6

                        shapePoints =
                            when s is
                                Rock -> 1
                                Paper -> 2
                                Scissors -> 3

                        outcomePoints + shapePoints

                    _ -> Err (BadLine line)
            |> List.mapTry \v -> v
            |> Task.fromResult
            |> Task.await

        gameResults
        |> List.sum
        |> Num.toStr
        |> Stdout.line

    err <- total |> Task.onErr
    _ <- Stderr.line
            (
                when err is
                    FileReadErr path _ -> "Unable to read file \(Path.display path)"
                    FileReadUtf8Err path _ -> "Unable to read file as utf8 \(Path.display path)"
                    BadFirstValue value -> "\(value) is not a valid first value, please choose one of A, B or C."
                    BadSecondValue value -> "\(value) is not a valid second value, please choose one of X, Y or Z."
                    BadLine line -> "\nI expected a line like:\n\nA Y\n\nBut you gave me:\n\n\(line)"
            )
        |> Task.await

    Task.err -1

decodeFirst : Str -> Result [Rock, Paper, Scissors] [BadFirstValue Str]
decodeFirst = \str ->
    when str is
        "A" -> Ok Rock
        "B" -> Ok Paper
        "C" -> Ok Scissors
        other -> Err (BadFirstValue other)

decodeSecond : Str -> Result [Rock, Paper, Scissors] [BadSecondValue Str]
decodeSecond = \str ->
    when str is
        "X" -> Ok Rock
        "Y" -> Ok Paper
        "Z" -> Ok Scissors
        other -> Err (BadSecondValue other)

