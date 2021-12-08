(* ::Package:: *)

BeginPackage["Bob`Games`"];

(* Declare your packages public symbols here. *)
Bob`Games`Bingo

Begin["`Private`"];

(* Define your public and private symbols here. *)

Bingo[args___]:=Catch[Catch[bingo[args],"Bingo"]]

Options[Bingo]=Options[bingo]=Join[Options[Grid],{"FreeSpace"->Automatic,"Headers"->None}]

bingo[]:=bingo[Partition[Range[75],15],5,"Headers"->{"B","I","N","G","O"}]

bingo[items:{_List..},opts:OptionsPattern[]]:=bingo[items,Min[Append[Length/@items,Length[items]]],opts]

bingo[items_List,opts:OptionsPattern[]]:=bingo[items,Floor[Sqrt[Length[items]]],opts]

bingo[items_,n_,opts:OptionsPattern[]]:=
    bingoBoard[setFreeSpace[OptionValue["FreeSpace"],items,n],opts]/;MatchQ[Dimensions[items],{n,n,___}]


bingo[items_List,n_,opts:OptionsPattern[]]:=bingo[RandomSample[#,n]&/@items,n,opts]/;Length[items]===n&&Min[Length/@items]>=n

bingo[items:{_List..},n_,opts:OptionsPattern[]]:=bingo[Flatten[items],n,opts]

bingo[items_List,n_,opts:OptionsPattern[]]:=bingo[Take[Partition[RandomSample@items,UpTo[n]],n],n,opts]/;Length[items]>=n^2

bingoBoard[items_,opts:OptionsPattern[Bingo]]:=Style[Grid[addHeaders[Transpose[items],OptionValue["Headers"]],

FilterRules[Flatten[{opts}],Options[Grid]],$defaultGridOptions
], $defaultStyleRules]

$defaultGridOptions={
    Frame->All,
    ItemSize->{10,{5,{10}}},
    Alignment->{Center,Center}

};

$defaultStyleRules={
    FontFamily->"Source Sans Pro",
    FontSize->18
};

addHeaders[board_,None]:=board

addHeaders[board_,headers_List]:=Prepend[board,Style[#,Bold]&/@PadRight[headers,Length[board]]]

setFreeSpace[Automatic,items_,_]:=items

setFreeSpace[expr_,items_,n_]:=Module[{mid=Ceiling[n/2],new=items},
    new[[mid,mid]]=expr;
    new
]

End[]; (* End `Private` *)

EndPackage[];
