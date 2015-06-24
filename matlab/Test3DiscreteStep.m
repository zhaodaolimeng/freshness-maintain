% Test the impact of discretization step length, with the same timetable
% Error bar needed

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

discreteStep = 2;
[opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit);

% Total count of crawls
discreteStepList = 1:10;
resultList = zeros(size(discreteStepList));
rateList = zeros(size(discreteStepList));
for discreteStep = discreteStepList     
    [opt,arrange,rate] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit);
    resultList(discreteStep) = opt;
    rateList(discreteStep) = rate;
end
plot(resultList);
%plot(rateList);
