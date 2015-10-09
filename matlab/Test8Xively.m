% Events are generated from raw data, which are then stored into Lists

clear;clc;
load('XivelyInput.mat');

coList = []; 
huList = [];
noList = [];
teList = [];

colock = 1;
hulock = 1;
nolock = 1;
telock = 1;
for i = 2:336
    t = mod(i,48);  
    
    if CO(i)-CO(i-1) > 5000
        coList = [coList i];
    elseif CO(i)>=25000 && colock == 1
        coList = [coList i];
        colock = 0;
    elseif CO(i)<25000 && colock == 0
        colock = 1;
    end
    
    if t < 18 || t > 36        
        if abs(Humidity(i) - Humidity(i-1)) > 3
            huList = [huList i];
        elseif (Humidity(i)>50||Humidity(i)<20) && hulock == 1
            huList = [huList i];
            hulock = 0;
        elseif Humidity(i)>=50 && Humidity(i)<=20
            hulock = 1;
        end
    end
    
    if NO2(i) - NO2(i-1) > 10
        noList = [noList i];
    elseif NO2(i) >= 80
        noList = [noList i];
        nolock = 0;
    elseif NO2(i) < 80
        nolock = 1;
    end
          
    if t < 18 || t > 36
        if Temperature(i) - Temperature(i-1) > 1
            teList = [teList i];
        elseif (Temperature(i) > 28 || Temperature(i) < 24)&& telock == 1
            teList = [teList i];
            telock = 0;
        elseif Temperature(i) <= 28 && Temperature(i) >= 24
            telock = 1;
        end
    end
    
end

sensors = 4;
timeTable(1).value=[0,336];
timeTable(2).value=[(0:6)*48+24;zeros(1,7)+24]';
timeTable(3).value=[0,336];
timeTable(4).value=[(0:6)*48+24;zeros(1,7)+24]';

sumOfCrawl = 20;
discreteStep = 2;
eps = 10;
iteratorLimit = 20;
sensorWeight = ones(1,4);
lambdaList = [length(coList),length(huList),length(noList),length(teList)]/336;
crawlLimitList = [10,10,10,10];

disp('Random');
[ropt,rarrange,rplans] = RandomCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep);
disp('Evenly')
[eopt,earrange,eplans] = EvenlyCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep);

disp('DP');
sensorType = ones(1,sensors);
[opt,arrange,rate,plans] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit,sensorWeight,sensorType);

eventTable(1).value = coList;
eventTable(2).value = huList;
eventTable(3).value = noList;
eventTable(4).value = teList;
totalLatency = 0;
for i = 1:4
    crawls = sort(rplans(i).value);
    for co = eventTable(i).value
        for crawl = crawls
            if crawl >= co
                totalLatency = totalLatency + (crawl - co);
                break;
            end
        end
    end
end
disp(['Latency = ' num2str(totalLatency)]);

totalLatency = 0;
for i = 1:4
    crawls = sort(eplans(i).value);
    for co = eventTable(i).value
        for crawl = crawls
            if crawl >= co
                totalLatency = totalLatency + (crawl - co);
                break;
            end
        end
    end
end
disp(['Latency = ' num2str(totalLatency)]);

totalLatency = 0;
for i = 1:4
    crawls = sort(plans(i).value);
    for co = eventTable(i).value
        for crawl = crawls
            if crawl >= co
                totalLatency = totalLatency + (crawl - co);
                break;
            end
        end
    end
end
disp(['Latency = ' num2str(totalLatency)]);



