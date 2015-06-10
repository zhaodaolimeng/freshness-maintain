(* ::Package:: *)

BeginPackage["OptimalScheduleAdhocDp`"]

DebugSchedule::usage=
	"DebugSchedule[lambdaList_,timeTable_,crawlLimitsList_,\[Theta]_,\[Delta]_,\[Epsilon]_,iteratorLimit_,$R_] "<>
	"return the optimal schedule distribution f[opt]=?, f[arrange]=?"

(*TestSensorScale::usage="TestSensorScale[sensorSettings_,minRange_,timeRange_]"*)

DebugIncremental::usage=
	"DebugIncremental[expTable_,crawlLimitsList_,\[Theta]_,\[Delta]_,iteratorLimit_] "<>
	"return the optimal schedule distribution"

DebugPreCompute::usage="DebugPreCompute[timeTable_,lambdaList_,\[Epsilon]_,\[Theta]_] "<>
	"return expTable, the expectation of each sensor to crawl different times"

Begin["`Private`"]

(*Main entry*)
DebugSchedule[lambdaList_,timeTable_,crawlLimitsList_,\[Theta]_,\[Delta]_,\[Epsilon]_,iteratorLimit_,$R_]:=Module[
	{oldOpt,sensors,t,i,dist={},expTable,ret},	
	Print["Discretize timeline."];
	dist=DiscretizeTimeline[timeTable,lambdaList,\[Epsilon]];
	Print["Begin to compute expectation."];
	$R["opt"]=\[Infinity];
	$R["arrange"]=EvenlyDivide[\[Theta],Dimensions[lambdaList][[1]],crawlLimitsList];	
	expTable={};
	sensors=Dimensions[lambdaList][[1]];
	t=Dimensions[dist][[1]];
	oldOpt=0;
	For[i=0,i<sensors,i++;AppendTo[expTable,Array[-1&,{crawlLimitsList[[i]]}]];];
	For[i=0,Abs[$R["opt"]-oldOpt]>=\[Delta]&&i<iteratorLimit,i++;
		oldOpt=$R["opt"];
		ret=ImproveSolution[$R,expTable,dist,crawlLimitsList];
		$R=ret["$R"];
		If[i==1, Print["Before improve, Opt is "<>ToString[$R["opt"]]]];
		expTable=ret["expTable"]];
	Print["After improve, Opt is "<> ToString[$R["opt"]]];
	Print["Arrange is " <> ToString[$R["arrange"]]];
	$R]

DebugIncremental[expTable_,crawlLimitsList_,\[Theta]_,\[Delta]_,iteratorLimit_]:=Module[
	{i=1,$oldR,$R},
	(*$R=<|opt->\[Infinity],arrange->{}|>;*)
	$R["arrange"]=EvenlyDivide[\[Theta],Dimensions[expTable][[1]]];
	While[Abs[$R["opt"]-$oldR["opt"]]>=\[Delta]&&i<iteratorLimit,
		$oldR=$R;
		$R=ImproveSolution[$R,expTable,crawlLimitsList];
		Print["Iteration No."<>ToString[i]<>" opt is "<>ToString[$R["opt"]]];i++;];
	$R]

DebugPreCompute[timeTable_,lambdaList_,\[Epsilon]_,\[Theta]_]:=Module[
	{dist,expTable},
	dist=DiscretizeTimeline[timeTable,lambdaList,\[Epsilon]];		
	expTable=PrecomputeCostMatrix[dist,\[Theta]];
	expTable]

(*Utils function*)
EvenlyDivide[N_,L_,crawlLimitsList_]:=Module[
	{$arr,t,i},
	(*$arr=Array[Floor[N/L]&,{L}];For[t=Mod[N,L];i=1,t>0,t--;$arr[[i]]++;i++];*)
	If[Total[crawlLimitsList]<=N,
		$arr=crawlLimitsList,
		$arr=Array[0&,{L}];
		For[i=N;t=1,i>=0,t++;
			If[t>L,t=1];
			If[crawlLimitsList[[t]]>$arr[[t]],i--;$arr[[t]]++];]];
	$arr]

(*Discretization*)
DiscretizeTimeline[timeTable_,lambdaList_,stepLength_]:=Module[
	{$dist={},candicateCnt,i,j,k,nodesCnt,
	sensors,sensor,cycles,timeline,timeNode,workCycle,distanceMap},
	sensors=Dimensions[timeTable][[1]];
	For[sensor=0,sensor<sensors,sensor++;
		timeNode={};
		candicateCnt=0;
		timeline=timeTable[[sensor]];
		cycles=Dimensions[timeline][[1]];
		For[i=0,i<cycles,i++;
			workCycle=timeline[[i]];
			timeNode=Join[timeNode,
				Range[workCycle[[2]]+workCycle[[1]],workCycle[[1]],-stepLength]]];
		timeNode=Sort[timeNode];
		nodesCnt=Dimensions[timeNode][[1]];
		distanceMap=Array[0&,{nodesCnt,nodesCnt}];
		For[i=0,i<nodesCnt,i++;
			For[j=i,j<nodesCnt,j++;
				distanceMap[[i,j]]=DistanceBetweenTimeNode[
					timeline,lambdaList[[sensor]],timeNode[[i]],timeNode[[j]]];];];
		AppendTo[$dist,distanceMap]];
	$dist]

(*Distance*)
DistanceBetweenTimeNode[timeLine_,\[Lambda]_,start_,end_]:=Module[
	{$dd,cycles,pos},
	cycles=Dimensions[timeLine][[1]];
	pos=Position[timeLine,Select[timeLine,#[[1]]==start&]];
	If[Length[pos]!=0,
		$dd=EG[end-pos[[1]],\[Lambda]],
		$dd=EG[end-start,\[Lambda]]];
	$dd]
EG[t_,\[Lambda]_]:=(E^(-t \[Lambda])-1+t \[Lambda])t;

(*Precompute the cost of ALL possible arrangements for sensors*)
PrecomputeCostMatrix[dist_,maxCrawl_]:=Module[
	{ret={},forone,crawl,sensor,ut},
	For[sensor=0,sensor<Dimensions[dist][[1]],sensor++;
		forone={};
		For[crawl=0,crawl<maxCrawl,crawl++;
			ut=CrawlUtility[crawl,dist[[sensor]]];
			AppendTo[forone,ut["opt"]];];
		AppendTo[ret,forone]];
	ret]

(*Incremental Method*)
ImproveSolution[solv_,exptable_,dist_,crawlLimitsList_]:=Module[
	{$R,s1,s2,tmpArr,minExp,sensors,t,res,ret,fetchRet,expcopy,untouched},
	expcopy=exptable;
	minExp=\[Infinity];
	res=Array[0&,{2}];
	tmpArr=solv["arrange"];
	sensors=Dimensions[tmpArr][[1]];
	untouched=True;
	For[s1=0,s1<sensors,s1++;
		For[s2=0,s2<sensors,s2++;
			If[s1!=s2&&tmpArr[[s1]]+1<=crawlLimitsList[[s1]]&&tmpArr[[s2]]>=1,
				fetchRet=FetchOrSolve[s1,tmpArr[[s1]]+1,dist[[s1]],expcopy];
				expcopy=fetchRet["expTable"];t=fetchRet["$R"];
				fetchRet=FetchOrSolve[s1,tmpArr[[s1]],dist[[s1]],expcopy];
				expcopy=fetchRet["expTable"];t-=fetchRet["$R"];
				fetchRet=FetchOrSolve[s2,tmpArr[[s2]]-1,dist[[s2]],expcopy];
				expcopy=fetchRet["expTable"];t+=fetchRet["$R"];
				fetchRet=FetchOrSolve[s2,tmpArr[[s2]],dist[[s2]],expcopy];
				expcopy=fetchRet["expTable"];t-=fetchRet["$R"];
				If[t<minExp,minExp=t;res[[1]]=s1;res[[2]]=s2;untouched=False]
			]];];		
	If[untouched==True,
		For[t=0;s1=0,s1<sensors,s1++;
			fetchRet=FetchOrSolve[s1,crawlLimitsList[[s1]],dist[[s1]],expcopy];
			expcopy=fetchRet["expTable"];t+=fetchRet["$R"]],
		If[minExp<0,
			tmpArr=solv["arrange"];
			tmpArr[[res[[1]]]]++;
			tmpArr[[res[[2]]]]--;];
		For[t=0;s1=0,s1<sensors,s1++;
			t+=expcopy[[s1,tmpArr[[s1]]]];];
	];
	solv["opt"]=t;solv["arrange"]=tmpArr;
	ret["$R"]=solv;ret["expTable"]=expcopy;(*Update Memo*)
	ret]

FetchOrSolve[sensor_,crawls_,dist_,exptable_]:=Module[
	{ret,t,expcopy},
	expcopy=exptable;
	If[exptable[[sensor,crawls]]==-1,
		t=CrawlUtility[crawls,dist];
		expcopy[[sensor,crawls]]=t["opt"]];	
	(*Print[ToString[expcopy[[sensor,crawls]]]];*)
	ret["$R"]=expcopy[[sensor,crawls]];
	ret["expTable"]=expcopy;
	ret]

(*DP method for optimal scheduling for a single sensor*)
(*N is crawls allowed to use, w is a distance matrix*)
CrawlUtility[N_,w_]:=Module[
	{$R,M,f,p,n,m,k,minv=0,temp=0},	
	M=Dimensions[w][[1]];	
	$R["opt"]=\[Infinity];$R["arrange"]={};
	f=Array[\[Infinity]&,{M,N}];
	p=Array[0&,{M,N}];
	For[m=1,m<=M,m++,f[[m,1]]=w[[1,m]];p[[m,1]]=1];
	For[n=2,n<=N,n++,
		For[m=2,m<=M,m++,
			For[k=2,k<=m-1,k++,
				temp=f[[k,n-1]]+w[[k,m]];
				If[temp<f[[m,n]],f[[m,n]]=temp;p[[m,n]]=k]]]];
	$R["opt"]=f[[M,N]];
	For[n=N;m=M,n>=1;m>=1,n--,
		AppendTo[$R["arrange"],m];m=p[[m,n]]];
	$R]
End[]
EndPackage[]

(*
sensors=10;
lambdaList=RandomReal[0.9,{sensors}]+0.1;
crawlLimitsList=RandomInteger[8,{sensors}]+1;
Print[ToString[crawlLimitsList]];
\[Theta]=RandomInteger[{25,30}];(*sum of crawl numbers*)
\[Delta]=0.1;
\[Epsilon]=2;
maxIteration=50;
TimeTable=Module[{
	i,j,cycles,maxwperiod=10,minwperiod=3,maxwRange=10,minwRange=1,timeRange=200,
	aList={},lList={},$tt={}},
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
	$tt];
tmpR["opt"]=\[Infinity];tmpR["arrange"]={};
tmpR=DebugSchedule[lambdaList,TimeTable,crawlLimitsList,\[Theta],\[Delta],\[Epsilon],maxIteration,tmpR];
Print[tmpR["opt"]];
*)
