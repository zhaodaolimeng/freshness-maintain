% Simple test of EasiCrawl

sensors = 10;
lambdaList = rand(1,sensors)*0.9+0.1;
crawlLimitList = randi([1,9],1,sensors);
sumOfCrawl = randi([25,30]);
discreteStep = 2;
eps = 10;
iteratorLimit = 20;
sensorWeight = ones(1,sensors);

% Build time table
maxwp = 10;
minwp=3;
maxwr=10;
minwr=1;
timeRange=200;
timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);

% For different sensor type
disp('Random');
[opt] = RandomCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep);
disp('DP');
sensorType = ones(1,sensors);
[opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);
disp('Greedy');
sensorType = ones(1,sensors)+1;
[opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);

