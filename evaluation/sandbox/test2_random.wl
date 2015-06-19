(* ::Package:: *)

BeginPackage["Benchmark`",{"OptimalScheduleAdhocDp`"}]
(*
Only expectation function is implied as a final measurement,
to check the actually event version, see test_benchmark_discrete
*)
TestSensorScale::usage="TestSensorScale[sensorSettings_,minRange_,timeRange_]"
TestTotalCrawlNumber::usage="TestTotalCrawlNumber[sensorSettings_,minCrawl_,maxCrawl_]"
TestDiscretization::usage="TestDiscretization[sensorSettings_, minEps_, maxEps_]"

Begin["`Private`"]

TestSensorScale[sensorSettings_,scaleList_]:=Module[
	{ret,sensors,lList,crawlLimList,TimeTable,
	\[Theta],\[Delta],\[Epsilon],maxiter,maxwp,minwp,maxwr,minwr,tr,maxc,minc,maxcs,mincs,maxl,minl},
	Print["Start sensor scale test..."];	
	crawlLimList={};
	\[Delta]=sensorSettings["\[Delta]"];
	\[Epsilon]=sensorSettings["\[Epsilon]"];
	tr=sensorSettings["timeRange"];
	maxc=sensorSettings["maxcrawl"];
	minc=sensorSettings["mincrawl"];
	maxiter=sensorSettings["maxIteration"];
	maxwp=sensorSettings["maxwperiod"];
	minwp=sensorSettings["minwperiod"];
	maxwr=sensorSettings["maxwRange"];
	minwr=sensorSettings["minwRange"];
	maxcs=sensorSettings["maxcrawlsingle"];
	mincs=sensorSettings["mincrawlsingle"];
	maxl=sensorSettings["maxLambda"];
	minl=sensorSettings["minLambda"];
	
	Do[
		Print["Scale = "<> ToString[sensors]];
		minc=minc*sensors;
		maxc=maxc*sensors;
		lList=RandomReal[maxl-minl,{sensors}]+minl;
		crawlLimList=RandomInteger[maxcs-mincs,{sensors}]+mincs;
		\[Theta]=RandomInteger[{minc,minc}];(*sum of crawl numbers*)		
		TimeTable=TimeTableMaker[sensors,maxwp,minwp,maxwr,minwr,tr];
		ret["opt"]=\[Infinity];ret["arrange"]={};		
		ret=OptimalScheduleAdhocDp`DebugSchedule[
			lList,TimeTable,crawlLimList,\[Theta],\[Delta],\[Epsilon],maxiter,ret];
		retArr[sensors]=ret["opt"];
		sensors++,{sensors,scaleList}];
	retArr]

TestTotalCrawlNumber[sensorSettings_,minCrawl_,maxCrawl_]:=Module[{
	retArr,TimeTable,crawl,ret,lList,crawlLimList,
	sensors,\[Delta],\[Epsilon],maxiter,maxwp,minwp,maxwr,minwr,tr,maxc,minc,maxcs,mincs,maxl,minl},
	Print["No \[Theta] need to be set, start crawl numbers test..."];
	maxiter=sensorSettings["maxIteration"];
	maxwp=sensorSettings["maxwperiod"];
	minwp=sensorSettings["minwperiod"];
	maxwr=sensorSettings["maxwRange"];
	minwr=sensorSettings["minwRange"];
	\[Delta]=sensorSettings["\[Delta]"];
	\[Epsilon]=sensorSettings["\[Epsilon]"];(*step length of discretize*)
	tr=sensorSettings["timeRange"];
	maxcs=sensorSettings["maxcrawlsingle"];
	mincs=sensorSettings["mincrawlsingle"];
	maxl=sensorSettings["maxLambda"];
	minl=sensorSettings["minLambda"];
	sensors=sensorSettings["sensorScale"];
	(*Use the same dataset for crawl number test*)
	lList=RandomReal[maxl-minl,{sensors}]+minl;
	crawlLimList=RandomInteger[maxcs-mincs,{sensors}]+mincs;	
	TimeTable=TimeTableMaker[sensors,maxwp,minwp,maxwr,minwr,tr];

	For[crawl=minCrawl-1,crawl<maxCrawl,crawl++;
		ret["opt"]=\[Infinity];ret["arrange"]={};		
		ret=OptimalScheduleAdhocDp`DebugSchedule[
			lList,TimeTable,crawlLimList,crawl,\[Delta],\[Epsilon],maxiter,ret];
		retArr[crawl]=ret["opt"];
	];
	retArr]

TestDiscretization[sensorSettings_, minEps_, maxEps_]:=Module[
	{retArr,ret,eps,lList,crawlLimList,
	sensors,\[Delta],\[Theta],\[Epsilon],maxiter,maxwp,minwp,maxwr,minwr,tr,maxc,minc,maxcs,mincs,maxl,minl},
	maxiter=sensorSettings["maxIteration"];
	maxwp=sensorSettings["maxwperiod"];
	minwp=sensorSettings["minwperiod"];
	maxwr=sensorSettings["maxwRange"];
	minwr=sensorSettings["minwRange"];
	\[Delta]=sensorSettings["\[Delta]"];	
	\[Theta]=sensorSettings["\[Theta]"];
	tr=sensorSettings["timeRange"];
	maxcs=sensorSettings["maxcrawlsingle"];
	mincs=sensorSettings["mincrawlsingle"];
	maxl=sensorSettings["maxLambda"];
	minl=sensorSettings["minLambda"];
	sensors=sensorSettings["sensorScale"];	
	lList=RandomReal[maxl-minl,{sensors}]+minl;
	crawlLimList=RandomInteger[maxcs-mincs,{sensors}]+mincs;
	TimeTable=TimeTableMaker[sensors,maxwp,minwp,maxwr,minwr,tr];
	(*use the same dataset to test discretization*)
	For[eps=minEps-1,eps<maxEps,eps++;
		ret["opt"]=\[Infinity];ret["arrange"]={};
		ret=OptimalScheduleAdhocDp`DebugSchedule[
			lList,TimeTable,crawlLimList,\[Theta],\[Delta],eps,maxiter,ret];
		retArr[eps]=ret["opt"]];
	retArr]

(*maxwperiod is the max count of work cycles*)
TimeTableMaker[sensors_,maxwperiod_,minwperiod_,maxwRange_,minwRange_,timeRange_]:=Module[
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

SetDirectory[NotebookDirectory[]];
<<"optimal_schedule_adhocdp.wl";

(*Sensor Scale Test*)
sensorSettings["maxIteration"]=50;
sensorSettings["maxwperiod"]=10;(*the counter of wake time*)
sensorSettings["minwperiod"]=3;
sensorSettings["maxwRange"]=10;
sensorSettings["minwRange"]=1;
sensorSettings["\[Delta]"]=0.1;
sensorSettings["\[Epsilon]"]=2;
sensorSettings["timeRange"]=200;
sensorSettings["maxcrawl"]=2.0;(*rate of sum of sensors*)
sensorSettings["mincrawl"]=1.5;
sensorSettings["maxcrawlsingle"]=6;
sensorSettings["mincrawlsingle"]=1;
sensorSettings["maxLambda"]=1.0;
sensorSettings["minLambda"]=0.1;


(*retMatrix={};*)
(*
minScale=1;
maxScale=100;
averList=Array[0&,maxScale-minScale+1];
maxList=Array[0&,maxScale-minScale+1];
minList=Array[\[Infinity]&,maxScale-minScale+1];*)
scaleList = Range[10,100,10];
averList=Array[0&,Length[scaleList]];
maxList=Array[0&,Length[scaleList]];
minList=Array[\[Infinity]&,Length[scaleList]];

For[counts=0,counts<5,counts++;
	retArr=Benchmark`TestSensorScale[sensorSettings,scaleList];
	For[i=1,i<Length[scaleList],i++;
		averList[[i]]+=retArr[scaleList[[i]]];
		maxList[[i]]=Max[maxList[[i]],retArr[scaleList[[i]]]];
		minList[[i]]=Min[minList[[i]],retArr[scaleList[[i]]]];];
	Print[averList];
	Print[maxList];
	Print[minList]
]
averList/=5;
(*DiscretePlot[retArr[x],{x,minScale,maxScale}]*)
retTable=Transpose[{averList,maxList-minList}];
Needs["ErrorBarPlots`"];
ErrorBarPlots`ErrorListPlot[retTable]


(*Crawl Numbers Test*)
sensorSettings["sensorScale"]=8;
minCrawl=10;
maxCrawl=50;
retArr=Benchmark`TestTotalCrawlNumber[sensorSettings,minCrawl,maxCrawl];
DiscretePlot[retArr[x],{x,minCrawl,maxCrawl}]


(*
Discretization Test, which show negative relation.
Indeed, discretization result in very little effects on the final result, 
because the crawl time remains, and yet the last position of a heavily discretized work period
can not be recognized better than the lighter one.
*)
sensorSettings["timeRange"]=200;
sensorSettings["\[Theta]"]=10;(*crawl times*)
sensorSettings["maxwperiod"]=20;(*the counter of wake time*)
sensorSettings["minwperiod"]=2;
sensorSettings["maxwRange"]=50;
sensorSettings["minwRange"]=5;
minEps=1;
maxEps=50;
retArr=Benchmark`TestDiscretization[sensorSettings,minEps,maxEps];
DiscretePlot[retArr[x],{x,minEps,maxEps}]



