% Convergence speed are indicated by statistics of count of iterations.
% 2 different situations are tested, for many times.

clear;clc;
sensors = 10;
lambdaList = rand(1,sensors)*0.9+0.1;
crawlLimitList = randi([10,30],1,sensors);
discreteStep = 2;
eps = 10;
iteratorLimit = 100;
sensorWeight = ones(1,sensors);

% Build time table
maxwp = 20;
minwp=5;
maxwr=20;
minwr=2;
timeRange=400;
timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);
sensorType = ones(1,sensors);

resultList1 = zeros(size(crawlLimitList));
resultList2 = zeros(size(crawlLimitList));

sumOfCrawl = 50;
for i = 1:200
    crawlLimitList = randi([10,30],1,sensors);
    lambdaList = rand(1,sensors)*0.9+0.1;
    timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);    
    [opt,arrange,rate,plans,iteratecnt] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);    
    resultList1(i) = iteratecnt;
end
sumOfCrawl = 100;
for i = 1:200
    crawlLimitList = randi([10,30],1,sensors);
    lambdaList = rand(1,sensors)*0.9+0.1;
    timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);    
    [opt,arrange,rate,plans,iteratecnt] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);    
    resultList2(i) = iteratecnt;
end

save('Test6.mat','resultList1','resultList2');
