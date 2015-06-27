% Benckmark of EasiCrawl with real events instead of LatencyExpectation

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

% Time Range
timeList = 1:20;
timeRangeStep = 50;


discreteStep = 2;
[opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit);

% Total count of crawls

for i = timeList
    timeRange = timeRangeStep * i;
    
end
