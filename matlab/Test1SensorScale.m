% This file the benchmark of EasiCrawl under different sensor scale.

discreteStep = 1;
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

% Random Strategy for crawl schedule
aveListRnd = zeros(1,length(scaleList));
maxListRnd = zeros(1,length(scaleList));
minListRnd = zeros(1,length(scaleList));


for i = scaleList
    sensors = i * sensorstep;
    maxopt = 0;
    minopt = inf;
    maxoptr = 0;
    minoptr = inf;
    for count = 1:5        
        sensorWeight = ones(1,sensors); % weight for computing expectation
        sensorType = ones(1,sensors); % all dp method
        lambdaList = rand(1,sensors)*(maxLambda-minLambda)+minLambda;
        crawlLimitList = randi([mincs,maxcs],1,sensors);
        sumOfCrawl = randi([minc*sensors, maxc*sensors]);
        timeTable = MakeTimeTable(sensors, maxwp, minwp, maxwr, minwr, timeRange);
        
        % Dp method for crawl, optimal
        [opt,arrange] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);
        if opt > maxopt ;maxopt=opt; end
        if opt < minopt ;minopt=opt; end
        aveList(i) = aveList(i) + opt;
        
        % Random method for crawl scheduling, far from optimal
        [opt] = RandomCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep);
        if opt > maxoptr ;maxoptr=opt; end
        if opt < minoptr ;minoptr=opt; end
        aveListRnd(i) = aveListRnd(i) + opt;
    end    
    maxList(i) = maxopt;
    minList(i) = minopt;    
    maxListRnd(i) = maxoptr;
    minListRnd(i) = minoptr;    
end
aveList = aveList / 5;
aveListRnd = aveListRnd / 5;

figure(1);
plot([aveList; minList; maxList]');
figure(2);
plot([aveListRnd; minListRnd; maxListRnd]');

save('Test1.mat','aveList','minList','maxList','aveListRnd','minListRnd','maxListRnd');