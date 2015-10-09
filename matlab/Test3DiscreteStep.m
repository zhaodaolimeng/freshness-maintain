% Test the impact of discretization step length, with the same timetable
% Error bar needed
% Computation cost should be evaluated

clear;clc % Caution

sensors = 10;
lambdaList = rand(1,sensors)*0.9+0.1;
maxcs = 20; mincs = 2; % crawl per each sensor
maxwp = 20; minwp = 10; % count of working cycle 
maxwr = 20; minwr = 5; % length of working range
timeRange = 500;
crawlLimitList = randi([mincs,maxcs],1,sensors);
timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);
eps = 10;
iteratorLimit = 20;
sumOfCrawl = 100;

sensorWeight = ones(1,sensors);
sensorType = ones(1,sensors);

% Total count of crawls
discreteStepList = 1:20;
resultList = zeros(size(discreteStepList));
durationList = zeros(size(discreteStepList));
resultRList = zeros(size(discreteStepList));
for discreteStep = discreteStepList     
    startTime = cputime;
    [opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);
    duration = cputime - startTime;
    durationList(discreteStep) = duration;
    resultList(discreteStep) = opt;
    [opt,arrange] = RandomCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep);
    resultRList(discreteStep) = opt;    
end

plot([resultList;resultRList]');
save('Test3.mat','resultList','resultRList','durationList');
