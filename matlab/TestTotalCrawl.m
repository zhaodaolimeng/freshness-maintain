% Benchmark of same situation with different total crawls


% crawlLimitList = randi([1,9],1,sensors);
% sumOfCrawl = randi([25,30]);
% 
% % Build time table
% maxwp = 10;
% minwp=3;
% maxwr=10;
% minwr=1;
% timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);
% 
% [opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit);

sensors = 20;
lambdaList = rand(1,sensors)*0.9+0.1;
maxcs = 6; mincs = 1; % crawl per each sensor
maxwp = 10; minwp = 3; % count of working cycle 
maxwr = 10; minwr = 1; % length of working range
timeRange = 200;

crawlLimitList = randi([mincs,maxcs],1,sensors);
timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);

discreteStep = 2;
eps = 10;
iteratorLimit = 20;

% Total count of crawls
crawlStep = 10;
crawlList = 2:5;

resultList = zeros(size(crawlList));

for i = crawlList 
    sumOfCrawl = i * crawlStep;
    [opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit);
    resultList(i) = opt;
end

plot(resultList);
