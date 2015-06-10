(* ::Package:: *)

BeginPackage["RandomSchedule"]

TestScaleOfRandomVsOptimal[]::usage=
	"Test the latency expectation of different sensor scale, "<>
	"with random scheduling and optimal scheduling"

Begin["`Private`"]
(*
	Generate discrete events following Poission process in periodic timelne	
	Input: 
		number of sensors, lambda of each sensor
	Output: 
		time table
*)



generate[sensors_,maxwperiod_,minwperiod_,maxwRange_,minwRange_,timeRange_]:=Module[
	{i,j,cycles,aList={},lList={},$tt={}},
	For[i=0,i<sensors,i++;
		cycles=RandomInteger[{minwperiod,maxwperiod}];
		aList=Sort[RandomSample[Range[timeRange],cycles]];
		lList=Array[0&,{cycles}];
		For[j=0,j<cycles,j++;
			If[j==cycles,
				lList[[j]]=Max[RandomInteger[Min[maxwRange,timeRange-aList[[j]]]],minwRange],
				lList[[j]]=Max[RandomInteger[Min[maxwRange,aList[[j+1]]-aList[[j]]]],minwRange]
			]];
		AppendTo[$tt,Transpose[{aList,lList}]]];
	$tt]
End[]
EndPackage[]
