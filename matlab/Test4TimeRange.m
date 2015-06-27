% Benckmark of EasiCrawl with real events instead of LatencyExpectation

clear;clc;

sensors = 10;
lambdaList = rand(1,sensors)*0.9+0.1;
maxcs = 10; mincs = 2; % crawl per each sensor
maxwp = 10; minwp = 3; % count of working cycle 
maxwr = 8; minwr = 2; % length of working range
crawlLimitList = randi([mincs,maxcs],1,sensors);
% timeRange = 500;
% timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);
eps = 10;
iteratorLimit = 20;
sumOfCrawl = 100;
sensorWeight = ones(1,sensors);
sensorType = ones(1,sensors);

% Time Range
timeRangeList = 1:20;
timeRangeStep = 200;
discreteStep = 2;
% [opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit);

% Total count of crawls
resultList = zeros(size(timeRangeList));
timeList = zeros(size(timeRangeList));


for i = timeRangeList
    timeRange = timeRangeStep * i;
    timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);
    startTime = cputime;
    [opt] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);
    duration = cputime - startTime;
    resultList(i) = opt;
    timeList(i) = duration;
%     [opt] = RandomCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep);    
end

plot([resultList;timeList]');
save('Test4.mat','resultList','timeList');
