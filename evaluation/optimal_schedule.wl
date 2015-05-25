(* ::Package:: *)

BeginPackage["OptimalSchedule`"]

DebugSchedule::usage=
	"MainEntry[lambdaList_,timeTable_,crawlLimitsList_,\[Theta]_,\[Delta]_,\[Epsilon]_,maxIter] "<>
	"returns the optimal schedule distribution f[opt]=?, f[arrange]=?, \n"<>
	"lambdaList is the generation rates of interesting points of different sensors, \n"<>
	"timeTable stores the future work schedule for different sensors, \n"<>	
	"crawlLimitsList is the limit of crawls for a single sensor, \n"<>
	"\[Theta] is the count of global crawls, \n"<>
	"\[Delta] is iterator stop checker, \[Epsilon] is granularity for discretization, \n"<>
	"maxIter is the max iterator steps for incremental optimization method."
DebugIncremental::usage="DebugIncremental[expTable_,crawlLimitsList_,\[Theta]_,\[Delta]_,iteratorLimit_] "<>
	"returns the optimal schedule distribution <|opt->?, arrange->?|>"
DebugPreCompute::usage="DebugPreCompute[timeTable_,lambdaList_,\[Epsilon]_,\[Theta]_] "<>
	"return expTable, the expectation of each sensor to crawl different times"

Begin["`Private`"]

(*Main entry*)
DebugSchedule[lambdaList_,timeTable_,crawlLimitsList_,\[Theta]_,\[Delta]_,\[Epsilon]_,iteratorLimit_,$R_]:=Module[
	{oldOpt,sensors,t,i,dist={},expTable},	
	Print["Discretize timeline."];
	dist=DiscretizeTimeline[timeTable,lambdaList,\[Epsilon]];	
	Print["Begin to computate expectation."];
	$R["arrange"]=EvenlyDivide[\[Theta],Dimensions[lambdaList][[1]]];
	(*Print["Initialize arrange."];*)
	expTable=ExpectationForSensors[dist,\[Theta]];
	Print["Expectation computation done."];
	Print["Before improve, Opt is "<>ToString[$R["opt"]]];
	For[i=0,Abs[$R["opt"]-oldOpt]>=\[Delta]&&i<iteratorLimit,i++;
		oldOpt=$R["opt"];
		$R=ImproveSolution[$R,expTable,crawlLimitsList]];
	Print["After improve, Opt is "<>ToString[$R["opt"]]];
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
	expTable=ExpectationForSensors[dist,\[Theta]];
	expTable]

(*Utils function*)
EvenlyDivide[N_,L_]:=Module[
	{$arr,t,i},
	$arr=Array[Floor[N/L]&,{L}];
	For[t=Mod[N,L];i=1,t>0,t--;$arr[[i]]++;i++];
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
ExpectationForSensors[dist_,maxCrawl_]:=Module[
	{ret={},forone,crawl,sensor,ut},
	For[sensor=0,sensor<Dimensions[dist][[1]],sensor++;
		forone={};
		For[crawl=0,crawl<maxCrawl,crawl++;
			ut=CrawlUtility[crawl,dist[[sensor]]];
			AppendTo[forone,ut["opt"]];];
		AppendTo[ret,forone]];
	ret]

(*DP method for optimal scheduling for a single sensor*)
CrawlUtility[N_,w_]:=Module[
	{$R,M,f,p,n,m,k,minv=0,temp=0},
	(*$R=<|opt->\[Infinity],arrange->{}|>;*)
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

(*Incremental Method*)
ImproveSolution[solv_,exptable_,crawlLimitsList_]:=Module[
	{$R,s1,s2,tmpArr,minExp,sensors,t,res},
	minExp=\[Infinity];
	(*$R=<|opt->\[Infinity],arrange->{}|>;*)
	res=Array[0&,{2}];
	tmpArr=solv["arrange"];
	sensors=Dimensions[solv["arrange"]][[1]];
	For[s1=0,s1<sensors,s1++;
		For[s2=0,s2<sensors,s2++;
			If[s1!=s2&&tmpArr[[s1]]+1<=crawlLimitsList[[s1]]&&tmpArr[[s2]]>=1,				
				t=exptable[[s1,tmpArr[[s1]]+1]]-exptable[[s1,tmpArr[[s1]]]]
				+exptable[[s2,tmpArr[[s2]]-1]]-exptable[[s2,tmpArr[[s2]]]];
				If[t<minExp,minExp=t;res[[1]]=s1;res[[2]]=s2]]];];
	If[minExp<0,
		tmpArr=solv["arrange"];
		tmpArr[[res[[1]]]]++;
		tmpArr[[res[[2]]]]--;];	
	For[t=0;s1=0,s1<sensors,s1++;
		t+=exptable[[s1,tmpArr[[s1]]]];];
	solv["opt"]=t;solv["arrange"]=tmpArr;
	(*<|opt->t,arrange->tmpArr|>*)
	solv]

End[]

EndPackage[]
