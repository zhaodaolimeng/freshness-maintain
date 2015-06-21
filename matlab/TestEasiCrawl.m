sensors = 10;
lambdaList = rand(1,sensors)*0.9+0.1;
crawlLimitList = randi([1,9],1,sensors);
sumOfCrawl = randi([25,30]);
discreteStep = 2;
eps = 10;
iteratorLimit = 20;

% Build time table
maxwp = 10;
minwp=3;
maxwr=10;
minwr=1;
timeRange=200;
timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);

[opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit);
