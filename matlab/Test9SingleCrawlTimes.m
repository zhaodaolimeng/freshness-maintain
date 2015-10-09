% Benchmark of same situation with different total crawls

sensors = 1;
lambdaList = rand(1,sensors)*0.9+0.1;
maxwp = 50; minwp = 3; % count of working cycle 
maxwr = 15; minwr = 1; % length of working range
timeRange = 1000;
timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);
discreteStep = 1;
eps = 10;
iteratorLimit = 20;
crawlLimitList = 100;
sensorWeight = ones(1,sensors);
sensorType = ones(1,sensors);

% Total count of crawls
crawlStep = 1;
crawlList = 1:50;

resultList = zeros(size(crawlList));
resultRList = zeros(size(crawlList));
for i = crawlList 
    sumOfCrawl = i * crawlStep;
    [opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);    
    resultList(i) = opt;
    [opt,arrange] = RandomCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep);
    resultRList(i) = opt;
end
plot([resultList;resultRList]');
save('Test9.mat','resultList','resultRList');