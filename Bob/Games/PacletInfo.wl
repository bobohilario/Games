(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "Bob/Games",
    "Description" -> "A collection of games",
    "Creator" -> "Bob Sandheinrich",
    "License" -> "MIT",
    "PublisherID" -> "Bob",
    "Version" -> "1.2.1",
    "WolframVersion" -> "13+",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {"Bob`Games`"},
        "Symbols" -> {
          "Bob`Games`Bingo",
          "Bob`Games`Wordle",
          "Bob`Games`Wolfle",
          "Bob`Games`Minesweeper",
          "Bob`Games`Play2048",
          "Bob`Games`LightsOut",
          "Bob`Games`CircleTheDrain",
          "Bob`Games`Tetris"
        }
      },
      {"Documentation", "Language" -> "English"},
      {
        "Asset",
        "Assets" -> {{"WolfleData", "./Data/wolfle.wxf"}}
      }
    }
  |>
]
