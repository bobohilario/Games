(* ::Package:: *)

BeginPackage["Bob`Games`"];

(* Declare your packages public symbols here. *)
Bob`Games`Play2048
Bob`Games`Tetris
Bob`Games`LightsOut
Bob`Games`CircleTheDrain
Bob`Games`Minesweeper

Begin["`Private`"];

Play2048[args___]:=ResourceFunction["Play2048",ResourceSystemBase->"https://www.wolframcloud.com/obj/resourcesystem/api/1.0"][args]

Tetris[args___]:=ResourceFunction["PolyominoesGame",ResourceSystemBase->"https://www.wolframcloud.com/obj/resourcesystem/api/1.0"][args]

LightsOut[]:=LightsOut[3]
LightsOut[args___]:=ResourceFunction["LightsOutGame",ResourceSystemBase->"https://www.wolframcloud.com/obj/resourcesystem/api/1.0"][args]

CircleTheDrain[args___]:=ResourceFunction["CircleTheDrain",ResourceSystemBase->"https://www.wolframcloud.com/obj/resourcesystem/api/1.0"][args]

Minesweeper[args___]:=ResourceFunction["Minesweeper",ResourceSystemBase->"https://www.wolframcloud.com/obj/resourcesystem/api/1.0"][args]

End[]; (* End `Private` *)

EndPackage[];
