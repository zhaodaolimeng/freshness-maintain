% Benchmark of same situation with different total crawls

sensors = 10;
lambdaList = rand(1,sensors)*0.9+0.1;
maxcs = 10; mincs = 2; % crawl per each sensor
maxwp = 10; minwp = 3; % count of working cycle 
maxwr = 10; minwr = 1; % length of working range
timeRange = 200;
timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);
discreteStep = 2;
eps = 10;
iteratorLimit = 20;
crawlLimitList = randi([mincs,maxcs],1,sensors);
sensorWeight = ones(1,sensors);
sensorType = ones(1,sensors);

% Total count of crawls
crawlStep = 1;
crawlList = 10:50;

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
save('Test2.mat','resultList','resultRList');