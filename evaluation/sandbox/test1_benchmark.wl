(* ::Package:: *)

BeginPackage["Benchmark`",{"Easi`"}]
(*
Only expectation function is implied as a final measurement,
to check the actually event version, see test_benchmark_discrete
*)
BestExpectScale::usage="BestExpectScale"
(*
TestTotalCrawlNumber::usage="TestTotalCrawlNumber[sensorSettings_,minCrawl_,maxCrawl_]"
TestDiscretization::usage="TestDiscretization[sensorSettings_, minEps_, maxEps_]"
*)
Begin["`Private`"]
(*
Best latency expectation under different sensor scales
---
\[Delta], the grantity of discretization.
\[Epsilon], stop threshold of the iterative method.
maxiter, maximal iteration times.
scaleList, the avaliable value for the sum of sensors
maxwRange, max work cycle of time range
maxwPeriod, max count of work cycles
---
retArr, N*{"opt":1,"arrange":N}, N is sensor count.
*)
BestExpectScale[
	\[Delta]_,\[Epsilon]_,maxiter_,scaleList_,
	maxwPeriod_,minwPeriod_,maxwRange_,minwRange_,timeRange_,
	maxCrawl_,minCrawl_,maxCrawlSingle_,minCrawlSingle_,maxLambda_,minLambda_]:=Module[
	{\[Theta],ret,lList,crawlLimList,timeTable,retArr},
	Print["Start sensor scale test..."];
	Do[
		Print["Scale = "<> ToString[sensors]];
		minCrawl=minCrawl*sensors;
		maxCrawl=maxCrawl*sensors;
		lList=RandomReal[maxLambda-minLambda,{sensors}]+minLambda;
		crawlLimList=RandomInteger[maxCrawlSingle-minCrawlSingle,{sensors}]+minCrawlSingle;
		\[Theta]=RandomInteger[{minCrawl,minCrawl}];
		timeTable=TimeTableMaker[sensors,maxwPeriod,minwPeriod,maxwRange,minwRange,timeRange];		
		ret=Easi`EasiCrawl[lList,timeTable,crawlLimList,\[Theta],\[Delta],\[Epsilon],maxiter];
		retArr[sensors]=ret["opt"];
		Print[ret["opt"]],
		{sensors,scaleList}];
	retArr]
(*
TestTotalCrawlNumber[sensorSettings_,minCrawl_,maxCrawl_]:=Module[{
	retArr,timeTable,crawl,ret,lList,crawlLimList,
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
	timeTable=TimeTableMaker[sensors,maxwp,minwp,maxwr,minwr,tr];
	For[crawl=minCrawl-1,crawl<maxCrawl,crawl++;
		ret["opt"]=\[Infinity];ret["arrange"]={};		
		ret=Easi`EasiCrawl[
			lList,timeTable,crawlLimList,crawl,\[Delta],\[Epsilon],maxiter,ret];
		retArr[crawl]=ret["opt"];
	];
	retArr]

TestDiscretization[sensorSettings_, minEps_, maxEps_]:=Module[
	{retArr,ret,eps,lList,crawlLimList,timeTable,
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
	timeTable=TimeTableMaker[sensors,maxwp,minwp,maxwr,minwr,tr];
	(*use the same dataset to test discretization*)
	For[eps=minEps-1,eps<maxEps,eps++;
		ret["opt"]=\[Infinity];ret["arrange"]={};
		ret=Easi`EasiCrawl[
			lList,timeTable,crawlLimList,\[Theta],\[Delta],eps,maxiter,ret];
		retArr[eps]=ret["opt"]];
	retArr]
*)
(*
Generate timeTable which contains the work cycle list for each sensor
---
maxwPeriod, the max count of work cycles
maxwRange, max time length of work cycle
---
$tt, N*2*M_i, N is sensor number & M_i is the cycle for each sensor
*)
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

(*Sensor Scale Test*)
Module[{
	\[Delta],\[Epsilon],maxiter,maxwPeriod,minwPeriod,maxwRange,minwRange,timeRange,
	maxCrawl,minCrawl,maxCrawlSingle,minCrawlSingle,maxLambda,minLambda,minScale,maxScale,
	averList,maxList,minList,scaleList,counts,
	retArr,retTable,i},
	\[Delta]=0.1;
	\[Epsilon]=2;
	maxiter=50;
	maxwPeriod=10;
	minwPeriod=3;
	maxwRange=10;
	minwRange=1;
	timeRange=200;
	maxCrawl=2.0;
	minCrawl=1.5;
	maxCrawlSingle=6;
	minCrawlSingle=1;
	maxLambda=1.0;
	minLambda=0.1;
	minScale=1;
	maxScale=100;
	averList=Array[0&,maxScale-minScale+1];
	maxList=Array[0&,maxScale-minScale+1];
	minList=Array[\[Infinity]&,maxScale-minScale+1];
	scaleList = Range[10,100,10];
	averList=Array[0&,Length[scaleList]];
	maxList=Array[0&,Length[scaleList]];
	minList=Array[\[Infinity]&,Length[scaleList]];
	For[counts=0,counts<5,counts++;
		retArr=Benchmark`BestExpectScale[
			\[Delta],\[Epsilon],maxiter,scaleList,
			maxwPeriod,minwPeriod,maxwRange,minwRange,timeRange,
			maxCrawl,minCrawl,maxCrawlSingle,minCrawlSingle,maxLambda,minLambda];
		Print[retArr];
		For[i=1,i<Length[scaleList],i++;
			averList[[i]]+=retArr[scaleList[[i]]];
			maxList[[i]]=Max[maxList[[i]],retArr[scaleList[[i]]]];
			minList[[i]]=Min[minList[[i]],retArr[scaleList[[i]]]];];
		Print[averList];
		Print[maxList];
		Print[minList]
	];
	averList/=5;
	(*DiscretePlot[retArr[x],{x,minScale,maxScale}]*)
	retTable=Transpose[{averList,maxList-minList}];
	Needs["ErrorBarPlots`"];
	ErrorBarPlots`ErrorListPlot[retTable]
]


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



