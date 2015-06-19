(* ::Package:: *)

BeginPackage["Easi`"]
EasiCrawl::usage="EasiCrawl return the optimal schedule distribution f[opt]=?, f[arrange]=?"
Begin["`Private`"]
(*
Main entry
---
lambdaList, parameter of Poission Process,
timeTable, n*m*2, n is sensors, m[[1]] is duty cycles & m[[2]] is working time
crawlLimitsList, maximum times that a device can be crawled for each sensor.
\[Theta], maximal crawl amount that is OK for server.
\[Delta], the grantity of discretization.
\[Epsilon], stop threshold of the improvement iterative method
iteratorLimit, maximal iteration times
$R, parameter for function style return.
---
$R, {"opt": 1, "arrange": N*1}, N is sensor number
*)
EasiCrawl[lambdaList_,timeTable_,crawlLimitsList_,\[Theta]_,\[Delta]_,\[Epsilon]_,iteratorLimit_]:=Module[{	
	oldOpt,sensors,i,distanceMatrix,dp,ret,$R},
	Print["Starting ..."];
	distanceMatrix=DiscretizeTimeline[timeTable,lambdaList,\[Epsilon]];
	$R["opt"]=\[Infinity];
	$R["arrange"]=EvenlyDivide[\[Theta],crawlLimitsList];
	dp={};(*Memo of time table, n*m, the minimal expectation can achieved*)
	sensors=Dimensions[lambdaList][[1]];
	oldOpt=0;
	For[i=0,i<sensors,i++;
		AppendTo[dp,Array[-1&,{crawlLimitsList[[i]]}]];];
	For[i=0,Abs[$R["opt"]-oldOpt]>=\[Delta]&&i<iteratorLimit,i++;
		oldOpt=$R["opt"];
		ret=ImproveSolution[$R,dp,distanceMatrix,crawlLimitsList];
		$R=ret["$R"];
		If[i==1, Print["Before improve, Opt is "<>ToString[$R["opt"]]]];
		dp=ret["dp"]
	];	
	Print["After improve, Opt is "<> ToString[$R["opt"]]];
	Print["Arrange is " <> ToString[$R["arrange"]]];
	$R]

(*
Frist arrangement, divide the crawls as even as possible
---
totalCrawls, total number allowed
crawlLimitsList, maximal crawls can be taken to each sensor
---
$arr, N*1, N is sensor number
*)
EvenlyDivide[totalCrawls_,crawlLimitsList_]:=Module[
	{$arr,t,i,sensors},
	sensors = Dimensions[crawlLimitsList][[1]];
	If[Total[crawlLimitsList]<=totalCrawls,
		$arr=crawlLimitsList,
		$arr=Array[0&,{sensors}];
		For[i=totalCrawls;t=1,i>=0,t++;
			If[t>sensors,t=1];
			If[crawlLimitsList[[t]]>$arr[[t]],i--;$arr[[t]]++];]];
	$arr]

(*
Discretization is to find the key check points for a single sensor's timeline
---
sleepPlan, the same data structure as the upper introduction.
lambdaList, controls the density of events
stepLength, granularity of discretization
---
$dist, N*M_i*M_i, N is sensor number & M_i*M_i is the time matrix of sensor i
*)
DiscretizeTimeline[sleepPlan_,lambdaList_,stepLength_]:=Module[
	{$dist={},candicateCnt,i,j,k,nodesCnt,
	sensors,sensor,cycles,timeline,timeNode,workCycle,distanceMap},
	sensors=Dimensions[sleepPlan][[1]];
	For[sensor=0,sensor<sensors,sensor++;		
		timeNode={};
		candicateCnt=0;
		timeline=sleepPlan[[sensor]];
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

(*
Compute the distance between 2 different data points for a single sensor
---
timeLine, m*2, m is duty cycles
lambda, density of events
start, expectation begin time
end, expectation end time
---
$dd, distance value 
*)
DistanceBetweenTimeNode[timeLine_,lambda_,start_,end_]:=Module[
	{$dd,cycles,pos,EG},
	EG[t_,\[Lambda]_]:=(E^(-t \[Lambda])-1+t \[Lambda])t;
	cycles=Dimensions[timeLine][[1]];
	pos=Position[timeLine,Select[timeLine,#[[1]]==start&]];
	If[Length[pos]!=0,
		$dd=EG[end-pos[[1]],lambda],
		$dd=EG[end-start,lambda]];
	$dd]

(*
Incremental Method
---
solv, {"opt": value, "arrange": N*1}, N is sensor number
dp, N*M_i, N is sensor number & M_i is maximal crawls for sensor i
dist, N*M_i*M_i, N is sensor number & M_i is time nodes for sensor i
crawlLimitsList, N*1, N is sensor number, improvements can be violate crawl number limit
---
ret, {"$R": solv, "dp": dp}, solv is feasiable solution & dp is the memo
*)
ImproveSolution[solv_,dp_,dist_,crawlLimitsList_]:=Module[
	{s1,s2,tmpArr,minExp,sensors,t,res,ret,fetchRet,dpcopy,untouched},
	dpcopy=dp;
	minExp=\[Infinity];
	res=Array[0&,{2}];
	tmpArr=solv["arrange"];
	sensors=Dimensions[tmpArr][[1]];
	untouched=True;
	For[s1=0,s1<sensors,s1++;
		For[s2=0,s2<sensors,s2++;
			If[s1!=s2&&tmpArr[[s1]]+1<=crawlLimitsList[[s1]]&&tmpArr[[s2]]>=1,
				fetchRet=FetchOrSolve[s1,tmpArr[[s1]]+1,dist[[s1]],dpcopy];
				dpcopy=fetchRet["dp"];t=fetchRet["opt"];
				fetchRet=FetchOrSolve[s1,tmpArr[[s1]],dist[[s1]],dpcopy];
				dpcopy=fetchRet["dp"];t-=fetchRet["opt"];
				fetchRet=FetchOrSolve[s2,tmpArr[[s2]]-1,dist[[s2]],dpcopy];
				dpcopy=fetchRet["dp"];t+=fetchRet["opt"];
				fetchRet=FetchOrSolve[s2,tmpArr[[s2]],dist[[s2]],dpcopy];
				dpcopy=fetchRet["dp"];t-=fetchRet["opt"];
				If[t<minExp,minExp=t;res[[1]]=s1;res[[2]]=s2;untouched=False]				
			]];];		
	(*CAUTION: If any expectaion of single sensor is infinity, the program will terminate.*)
	If[untouched==True,
		For[t=0;s1=0,s1<sensors,s1++;
			fetchRet=FetchOrSolve[s1,crawlLimitsList[[s1]],dist[[s1]],dpcopy];
			dpcopy=fetchRet["dp"];t+=fetchRet["opt"]],
		If[minExp<0,
			tmpArr=solv["arrange"];
			tmpArr[[res[[1]]]]++;
			tmpArr[[res[[2]]]]--;];
		For[t=0;s1=0,s1<sensors,s1++;
			t+=dpcopy[[s1,tmpArr[[s1]]]];];
	];	
	solv["opt"]=t;solv["arrange"]=tmpArr;
	ret["$R"]=solv;ret["dp"]=dpcopy;(*Update Memo*)
	ret]

(*
Compute the distance to crawl a certain sensor with a fixed time with memo
---
sensor, index of target sensor
crawls, number of total crawl of this sensor
dist, M*M, M is time node of sensor
dp, N*M, N is number of sensor & M is crawl number
---
ret, {"opt": 1, "$dp": N*M_i*M_i}, "opt" is the minimal value & dp is the meno
*)
FetchOrSolve[sensor_,crawls_,dist_,dp_]:=Module[
	{ret,t,dpcopy},
	dpcopy=dp;
	If[dp[[sensor,crawls]]==-1,
		t=CrawlUtility[crawls,dist];
		dpcopy[[sensor,crawls]]=t["opt"]];
	ret["opt"]=dpcopy[[sensor,crawls]];
	ret["dp"]=dpcopy;
	ret]

(*
DP method for optimal scheduling for a single sensor
---
N, 1, crawls allowed to use
w, M_i*M_i, a distance matrix of this sensor i
---
$R, {"opt": 1, "trajectory", T*1}, opt is minimal value & trajectory is the schedule for best
*)
CrawlUtility[N_,w_]:=Module[
	{$R,M,f,p,n,m,k,minv=0,temp=0},	
	M=Dimensions[w][[1]];	
	$R["opt"]=\[Infinity];$R["trajectory"]={};
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
		AppendTo[$R["trajectory"],m];m=p[[m,n]]];
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
tmpR=EasiCrawl[lambdaList,TimeTable,crawlLimitsList,\[Theta],\[Delta],\[Epsilon],maxIteration];
Print[tmpR["opt"]];
*)
