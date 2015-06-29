% What's our strategies' performance for different types of sensors?
% Different sensors, under different numbers of nodes.
% Three lines are plot: 
% 1. When one 100% of sensors are greedy based
% 2. 50% of sensors are greedy
% 3. 100% of sensors are dp based

clear;clc;
discreteStep = 2;
eps = 10;
iteratorLimit = 20;

maxwp = 10; minwp = 3; % count of working cycle 
maxwr = 10; minwr = 1; % length of working range
timeRange = 200;
maxc = 2.0; minc = 1.0; % times of crawls
maxcs = 6; mincs = 1; % crawl per each sensor
maxLambda = 1.0; minLambda = 0.1;
scaleList = 1:1:10; sensorstep = 10;

resultListAll = zeros(size(scaleList));
resultListHalf = zeros(size(scaleList));
resultListNone = zeros(size(scaleList));
resultListRand = zeros(size(scaleList));

for i = scaleList    
    sensors = i * sensorstep;
    lambdaList = rand(1,sensors)*(maxLambda-minLambda)+minLambda;    
    sensorWeight = ones(1,sensors); % weight for computing expectation
    sensorType = ones(1,sensors); % all dp method
    crawlLimitList = randi([mincs,maxcs],1,sensors);
    sumOfCrawl = randi([minc*sensors, maxc*sensors]);
    
    timeTable = MakeTimeTable(sensors, maxwp, minwp, maxwr, minwr, timeRange);    
    opt = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);
    resultListAll(i) = opt;
    
    sensorType(1:sensors/2) = 2;
    opt = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);
    resultListHalf(i) = opt;
    
    sensorType(1:sensors) = 2;
    opt = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);
    resultListNone(i) = opt;
    
    [opt] = RandomCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep);
    resultListRand(i) = opt;
    
end

save('Test5.mat','resultListAll','resultListHalf','resultListNone','resultListRand');