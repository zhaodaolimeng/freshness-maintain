function [opt,arrange,plans] = EvenlyCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep)
%% 
% A random method to generate a schedule strategy for sensors
sensors = length(lambdaList);
disp('Starting EvenlyCrawl ...');
distanceMatrix = DiscretizeTimeline(timeTable,lambdaList,discreteStep);
% random arrange number of crawls
arrange = EvenlyDivide(crawlLimitList,sumOfCrawl);
opt = 0;
for sensor = 1:sensors
    dist = distanceMatrix(sensor).value;
    numberOfNodes = size(dist,1);
    
    d = numberOfNodes/(arrange(sensor)-1);    
    crawls = d:d:numberOfNodes-1;    
    crawls = ceil(crawls);
    
    expect = 0;
    lastNode = 1;
    for crawl = crawls
        expect = expect + dist(lastNode, crawl);
        lastNode = crawl;
    end
    expect = expect + dist(lastNode, numberOfNodes);
    opt = opt + expect;    
    
    plantime=zeros(1,length(crawls)+1);
    for i=crawls
        plantime(i)=distanceMatrix(sensor).timeNode(i);
    end
    plantime(end) = distanceMatrix(sensor).timeNode(end);
    plans(sensor).value = plantime;
end
disp(['opt = ' num2str(opt) ' arrange = ' mat2str(arrange)]);
end

