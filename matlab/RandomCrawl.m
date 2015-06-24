function [opt,arrange] = RandomCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep)
%% 
% A random method to generate a schedule strategy for sensors
sensors = length(lambdaList);
disp('Starting ...');
distanceMatrix = DiscretizeTimeline(timeTable,lambdaList,discreteStep);
% random arrange number of crawls
arrange = EvenlyDivide(crawlLimitList,sumOfCrawl);
opt = 0;
for sensor = 1:sensors
    dist = distanceMatrix(sensor).value;
    numberOfNodes = size(dist,1);
    crawls = sort(randsample(1:numberOfNodes-1, arrange(sensor)-1));    
    expect = 0;
    lastNode = 1;
    for crawl = crawls
        expect = expect + dist(lastNode, crawl);
    end    
    opt = opt + expect;    
end
disp(['opt = ' num2str(opt) ' arrange = ' mat2str(arrange)]);
end

