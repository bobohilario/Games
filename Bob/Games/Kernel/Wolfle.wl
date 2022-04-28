(* ::Package:: *)

BeginPackage["Bob`Games`"];

(* Declare your packages public symbols here. *)
Bob`Games`Wolfle

Begin["`Private`"];

$wolfledata:=$wolfledata=Import[PacletObject["Bob/Games"]["AssetLocation", "WolfleData"]]

$wolflekeys={
 EntityProperty["WolframLanguageSymbol", "Name"],
 EntityProperty["WolframLanguageSymbol", "CharacterCount"], 
 EntityProperty["WolframLanguageSymbol", "VersionIntroduced"], 
 EntityProperty["WolframLanguageSymbol", "Attributes"], 
 EntityProperty["WolframLanguageSymbol", "FunctionalityAreas"], 
 EntityProperty["WolframLanguageSymbol", "RelatedSymbols"],
 EntityProperty["WolframLanguageSymbol", "EntityClasses"]}

wolfleCompare[guess_Association, target_Association] :=
    AssociationMap[wolfleCompare[#, guess[#], target[#]]&, 
        $wolflekeys]

wolfleCompare[guess_Entity, target_Entity] :=
    AssociationMap[wolfleCompare[#, 
        $wolfledata[guess][#], $wolfledata[target][#]]&, 
        $wolflekeys]

wolfleCompare[_,x_,x_]:=correct[elidevalues@x]/.EntityClass["WolframLanguageSymbol", val_]:>val
wolfleCompare[prop_,x_,y_]:=wolflecompare[prop,x,y]

wolflecompare[
    HoldPattern[EntityProperty]["WolframLanguageSymbol", 
    "Attributes"|"EntityClasses"|"FunctionalityAreas"|"RelatedSymbols"],x_,y_]:=With[{i=Intersection[x,y]},
    If[i==={},
        incorrect[elidevalues@x],
        close[elidevalues@x]
    ]/.EntityClass["WolframLanguageSymbol", val_]:>val
]

wolflecompare[EntityProperty["WolframLanguageSymbol", "CharacterCount"],x_,y_]:=
    If[Abs[x-y]>1,
        incorrect[x],
        close[x]
    ]

wolflecompare[EntityProperty["WolframLanguageSymbol", "Name"],x_,y_]:=With[{seq=LongestCommonSubsequence[x,y]},
    If[StringLength[seq]>2,
        close[x],
        incorrect[x]
    ]
]

wolflecompare[EntityProperty["WolframLanguageSymbol", "VersionIntroduced"],x_,y_]:=
    If[Abs[x-y]<=1,
        close[x],
        incorrect[x]
    ]

visualizeComparison[as_]:=
   Lookup[as,$wolflekeys]/.{
       correct->(Item[#,Background->Green,FontFamily->"Source Sans Pro"]&),
       close->(Item[#,Background->Yellow,FontFamily->"Source Sans Pro"]&),
       incorrect->(Item[#,Background->None,FontFamily->"Source Sans Pro"]&)
   }

elidevalues[l_List]:=Append[Take[l,UpTo[4]],"\[Ellipsis]"]/;Length[l]>5
elidevalues[expr_]:=expr
   
wolfleSummary[data_]:=Panel[Grid[Prepend[visualizeComparison/@data,Style[headerHint[#],18]&/@$wolflekeys], Frame->{None,All}],
    BaseStyle->{FontFamily->"Source Sans Pro"}]

headerHint[
 EntityProperty["WolframLanguageSymbol", "Name"]]:=
    Tooltip["Name","Yellow means they share a sequence of at least three characters"]


headerHint[
 EntityProperty["WolframLanguageSymbol", "VersionIntroduced"]]:=
    Tooltip["Version Introduced","Yellow means the version numbers are within 1.0 \n(7.1 and 8.0 ->yellow, 7.0 and 8.1 -> not yellow)"]

headerHint[
 EntityProperty["WolframLanguageSymbol", "RelatedSymbols"]]:=
    Tooltip["Related","\"RelatedSymbols\" From WolframLanguageData. \nYellow means only some values are in common. "]

headerHint[
 EntityProperty["WolframLanguageSymbol", "CharacterCount"]]:=
    Tooltip["Length","Yellow means the names are one character length apart"]

headerHint[
 EntityProperty["WolframLanguageSymbol", "Attributes"]]:=
    Tooltip["Attributes","Green is an exact match, \nyellow means some attribute is in common"]

headerHint[
 EntityProperty["WolframLanguageSymbol", "EntityClasses"]]:=
    Tooltip["Classes","\"EntityClasses\" From WolframLanguageData. \nYellow means only some values are in common. \nThis is helpful with option symbols"]

headerHint[
 EntityProperty["WolframLanguageSymbol", "FunctionalityAreas"]]:=
    Tooltip["Areas","\"FunctionalityAreas\" From WolframLanguageData. \nYellow means only some values are in common."]

headerHint[expr_]:=expr
Clear[Wolfle,wolfle]

Wolfle[args___]:=Catch[wolfle[args],"wolfle"]

wolfle[]:=wolfle[selectDailyWord[]]

Wolfle["ClearResults"]:=DeleteObject/@PersistentObjects[ FileNameJoin[{"Bob","Games","Wolfle","CompletionData","*"}]]

Wolfle["Random"]:=wolfle[selectRandomWord[]]

selectDailyWord[]:=(SeedRandom[AbsoluteTime[Today]]; RandomChoice[$wolfledata])
selectRandomWord[]:=(RandomChoice[$wolfledata])

$nomessage="";
wolfle[targetdata_Association]:=(checkCompleted[targetdata,Today];
    DynamicModule[{guess="",results={},message=$nomessage, i=0,complete=False},


        Panel@Dynamic@Grid[{
            {
                Style[message,Red,Italic,18]
            },
            {
                wolfleSummary@results
            },
            {
                If[complete,$nomessage,
                    InputField[Dynamic[guess], String,FieldCompletionFunction -> (If[StringLength[ToString@#]>1,
                    Names["System`" <> ToString[#] <> "*"],{}] &),
                    BaseStyle -> Directive["Code",24],FieldHint -> "Put Symbol Name Here"]
                ]
            },
            {
                If[complete,shareMessage[results],
                Button["Submit", ({results,message,complete,i}=wolfleGuess[guess,targetdata,results,i];
                guess="";
                If[TrueQ[complete],storeCompleteWolfle[targetdata, {guess,targetdata, results,message, i, complete}]]),ImageSize->Automatic,Method->"Queued"]
                ]
            },

            {Item["\"Wolfle\" Created By Bob Sandheinrich",FontSize->6,Alignment->Left]}
        }
        ]
        ,SaveDefinitions -> False
    ]

)

staticGrid[{guess_,targetdata_,results_,message_,i_,complete_}]:=Panel@Grid[{
            {
                Style[message,Red,Italic,18]
            },
            {
                wolfleSummary@results
            },
            {
                If[complete,$nomessage,
                    InputField[Dynamic[guess], String,FieldCompletionFunction -> (If[StringLength[ToString@#]>1,
                    Names["System`" <> ToString[#] <> "*"],{}] &),
                    BaseStyle -> Directive["Code",24],FieldHint -> "Put Symbol Name Here"]
                ]
            },
            {
                If[complete,shareMessage[results],
                Button["Submit", ({results,message,complete,i}=wolfleGuess[guess,targetdata,results,i];
                guess="";
                If[TrueQ[complete],storeCompleteWolfle[targetdata, {guess, results, i, complete}]]),
                ImageSize->Automatic,Method->"Queued"]
                ]
            },

            {Item["\"Wolfle\" Created By Bob Sandheinrich",FontSize->6,Alignment->Left]}
        }
        ]


shareMessage[as_]:=With[{colors=Lookup[as,$wolflekeys]/.{
       correct->(("\|01F7E9")&),
       close->(("\|01F7E8")&),
       incorrect->(("\:2B1C")&)
   }},
Column[{"Share: ",
Panel@ClickToCopy[
   "Wolfle "<>DateString[{"Year", "-", "Month", "-", "Day"}]<>"\n"<>
   StringRiffle[StringJoin/@colors,"\n"]]
}]
]

checkCompleted[targetdata_,date_]:=With[{comp=PersistentSymbol[completionlocation[targetdata,date]]},
    If[!MissingQ[comp],
        Throw[staticGrid@comp,"wolfle"]
    ]
]

completionlocation[targetdata_,date_]:=
    FileNameJoin[{"Bob","Games","Wolfle","CompletionData",Hash[{targetdata,date}, "SHA256", "HexString"]}]


$maxguesses=10;
Clear[wolfleGuess];
wolfleGuess[guess_String,targetdata_,results_,i_]:=With[{guessdata=$wolfledata[Entity["WolframLanguageSymbol", guess]]},
    If[AssociationQ[guessdata],
        wolfleGuess[guessdata ,targetdata,results,i],
        {results,"The given symbol \""<>guess<>"\" is not included in Wolfle.",False,i}
    ]
]

wolfleGuess[guess:KeyValuePattern[{EntityProperty["WolframLanguageSymbol", "Name"]->name_}],
    targetdata:KeyValuePattern[{EntityProperty["WolframLanguageSymbol", "Name"]->name_}],results_,i_]:=  (
    {Append[results,wolfleCompare[guess,targetdata]],
    Style["CORRECT: "<>name,32,Bold], True,i+1}
)

wolfleGuess[guess_Association,targetdata_,results_,i_]:=
If[
    guess[EntityProperty["WolframLanguageSymbol", "Name"]]===
    targetdata[EntityProperty["WolframLanguageSymbol", "Name"]]
    ,
    {Append[results,wolfleCompare[guess,targetdata]],
        Style["CORRECT: "<> targetdata[EntityProperty["WolframLanguageSymbol", "Name"]],32,Bold], True,i+1}
    ,

    With[{res=wolfleCompare[guess,targetdata],ii=i+1},
        If[AssociationQ[res],
            If[ii===$maxguesses,
                {Append[results,res],"You lost. The symbols was: "<>targetdata[EntityProperty["WolframLanguageSymbol", "Name"]],True,ii}
                ,
                {Append[results,res],$nomessage,False,ii}
            ],
            {results,"Something failed",False,i}
        ]
    ]
]

wolfleGuess[_,_,results_,i_]:={results,"That guess is not supported in Wolfle.",False,i}

storeCompleteWolfle[targetdata_, dynamicvalues_]:=
        PersistentSymbol[completionlocation[targetdata,Today]]=dynamicvalues;


Wolfle["WebForm"]:=With[{names=Names["System`*"], alldata=$wolfledata},
    Delayed[
        FormPage[{"Guess"-><|"Interpreter" -> "String", "Default" -> "", "Autosubmitting" -> True|>},
        Module[{targetdata=(SeedRandom[AbsoluteTime[Today]]; RandomChoice[alldata]),
            results={},message=$nomessage,complete=False,i=0,resultdata
            },
            resultdata=getCloudWolfleResults[targetdata];
            If[ListQ[resultdata],
                 {results,message,complete,i}=resultdata
            ];
            If[#Guess=!="",
                {results,message,complete,i}=wolfleGuess[#Guess,targetdata,results,i];  
                storeCloudWolfleResults[targetdata, {results,message,complete,i}];
            ];
            cloudResultsPanel[#Guess,results,message,i,complete]
        ]&,
        AppearanceRules-><|"Title" -> "Wolfle", "Description" -> "Wordle but for Wolfram Language symbols. Enter a WL symbol. Mouseover the headers in the results for more information."|>
    ]

    ]
]

getCloudWolfleResults[targetdata_Association]:=getCloudWolfleResults[cloudWolfleResultsLocation[$RequesterCloudUserUUID,Today,targetdata]]

getCloudWolfleResults[loc_CloudObject]:=With[{res=Import[loc]},
    If[ListQ[res],
        res,
        Missing[]
    ]
]

cloudWolfleResultsLocation[user_,date_,targetdata_]:=CloudObject[
    FileNameJoin[{"Bob","Games","Wolfle","CompletionData",Hash[{user,date,targetdata}, "SHA256", "HexString"]}]<>".wxf",Permissions->"Private"]

storeCloudWolfleResults[targetdata_, data_]:=Export[cloudWolfleResultsLocation[$RequesterCloudUserUUID,Today,targetdata],data,"WXF"]

cloudResultsPanel[guess_,results_,message_,i_,complete_]:=Panel@Grid[{
           {
                Style[message,Red,Italic,18]
            },
            {
                wolfleSummary@results
            },
            {
                If[complete,cloudShareMessage[results],$nomessage]
            },

            {Item["\"Wolfle\" Created By Bob Sandheinrich",FontSize->9,Alignment->Left]}
},Background->GrayLevel[.9],Spacings->0,ItemSize->10,Frame->True
        ]


cloudShareMessage[as_]:=With[{colors=Lookup[as,$wolflekeys]/.{
       correct->(("\|01F7E9")&),
       close->(("\|01F7E8")&),
       incorrect->(("\:2B1C")&)
   }},
Column[{"Share: ",
Panel[
   "Wolfle "<>DateString[{"Year", "-", "Month", "-", "Day"}]<>"<br>"<>
   StringRiffle[StringJoin/@colors,"<br>"]<>"<br>https://wolfr.am/wolfle"]
}]
]

wolfle[___]:=$Failed
End[]; (* End `Private` *)

EndPackage[];
