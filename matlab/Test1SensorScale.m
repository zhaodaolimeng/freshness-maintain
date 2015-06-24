% This file the benchmark of EasiCrawl under different sensor scale.

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

aveList = zeros(1,length(scaleList));
maxList = zeros(1,length(scaleList));
minList = zeros(1,length(scaleList));

for i = scaleList
    sensors = i * sensorstep;
    maxopt = 0;
    minopt = inf;
    for count = 1:5        
        lambdaList = rand(1,sensors)*(maxLambda-minLambda)+minLambda;
        crawlLimitList = randi([mincs,maxcs],1,sensors);
        sumOfCrawl = randi([minc*sensors, maxc*sensors]);
        timeTable = MakeTimeTable(sensors, maxwp, minwp, maxwr, minwr, timeRange);
        [opt, arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit);
        if opt > maxopt ;maxopt=opt; end
        if opt < minopt ;minopt=opt; end
        aveList(i) = aveList(i) + opt;
    end    
    maxList(i) = maxopt;
    minList(i) = minopt;
end
aveList = aveList / 5;

plot([aveList; minList; maxList]');

