function [opt,route] = CrawlPlanning(crawls,dist,sensorType)
%%
% Method for calculate the best crawling plan for single sensor
% ---
% crawls, 1, number of crawls allowed for this sensor
% dist, {value:M*M,timeNode:K_i}, M is the time points available
% sensorType, 1, 1==DP & 2==Greedy & 3==Evenly

switch nargin
    case 2
        disp('WARNING: sensorType is unset');
        sensorType = 1;
end
switch sensorType
    case 1
        [opt,route] = dp(crawls,dist);
    case 2
        [opt,route] = greedy(crawls,dist);
    case 3
        [opt,route] = evenly(crawls,dist);
end
end

function [opt,route] = dp(crawls,dist)
%% 
% DP method for crawls planning

nodes = size(dist.value,1);
f = Inf(nodes, crawls);
p = zeros(nodes, crawls);
route = zeros(1,crawls);
for node = 1:nodes
    f(node, 1) = dist.value(1,node);
    p(node, 1) = 1;
end
for crawl = 2:crawls
    for node = 2:nodes
        for mid = 2: node - 1
            tmp = f(mid,crawl-1) + dist.value(mid,node);
            if tmp < f(node,crawl)
                f(node,crawl) = tmp;
                p(node,crawl) = mid;
            end
        end % Built on relationship between sub-problems
    end
end
opt = f(nodes, crawls);
prenode = nodes;
for crawl = crawls:-1:1
    route(crawl) = prenode;
    if prenode == 0 ;break;end
    prenode = p(prenode, crawl);
end
end

function [opt,route] = greedy(crawls,dist)
%%
% Greedy method for periodic sleep sensors
% Buggy due to lack of value validation

timeline = dist.timeline;
timeNode = dist.timeNode;
distance = dist.value;
cycle = size(timeline,1);

countList = zeros(1,cycle);
if crawls <= cycle
    cyclePerCrawl =  floor(cycle/crawls);
    remain = mod(cycle,crawls);     
    r = remain;
    for flag = cycle:-cyclePerCrawl:remain + cyclePerCrawl
        if flag == 0 ;break; end
        countList(flag) = 1;
        if r > 0
            countList(floor(flag+cyclePerCrawl/2)) = 1;
            r = r - 1;
        end
    end
    route = [];
    flag = 1; 
    % Go over countList, put index of timeNode into route    
    for index = 1:length(timeNode)
        node = timeNode(index);
        while true
            if countList(flag) == 1                
                break;                            
            end
            flag = flag + 1;
        end % flag to a end of cycle
        endTime = timeline(flag,1) + timeline(flag,2);
        if node == endTime
            route = [route index];
            flag = flag + 1;
        end
    end
else
    crawlPercycle = floor(crawls/cycle);
    outlaw = mod(crawls, cycle);    
    r = outlaw;
    route = [];    
    for i = cycle:-1:1
        crawlCnt = crawlPercycle;
        if r>0
            crawlCnt = crawlCnt + 1;
            r = r - 1;
        end
        % Choose the best timeNode in each work cycle
        index = length(timeNode);        
        endOfCycle = timeline(i,1)+timeline(i,2);
        while timeNode(index) ~= endOfCycle
            index = index - 1;
        end % move index to the tail of a cycle
        route = [index route];
        crawlCnt = crawlCnt - 1;
        if crawlCnt == 0 ;continue; end        
        
        startOfNextCycle = index; % Find start of cycle
        while timeNode(startOfNextCycle) < timeline(i,1)
            startOfNextCycle = startOfNextCycle - 1;
        end
        workNodeCnt = index - startOfNextCycle; % nodes in work cycle
        nodesPerCrawl = workNodeCnt/crawlCnt;
        remain = mod(timeline(i,2),crawlCnt);            
        for t = index-nodesPerCrawl:-nodesPerCrawl:startOfNextCycle
            if remain > 0
                route = [(t+floor(nodesPerCrawl/2)) route];
                remain = remain - 1;
            end
            route = [t route];            
        end
    end
end

lastCheckpoint = 1;
opt = 0;
for checkpoint = route
    opt = opt + distance(lastCheckpoint, checkpoint);
    lastCheckpoint = checkpoint;
end
end


function [opt,route] = evenly(crawls,dist)
%%
% When sensor's sleeping plan is unknown, nothing except for evenly crawl

timeNode = dist.timeNode;
distance = dist.value;

nodesPerCrawl = floor(length(timeNode)/crawls);
remain = mod(length(timeNode),crawls);

for flag = length(timeNode):-nodesPerCrawl:remain
    if remain > 0
        route = [(flag - floor(nodesPerCrawl/2)) route];
    end
    route = [flag route];
end
lastCheckpoint = 1;
for checkpoint = route
    opt = opt + distance(lastCheckpoint, checkpoint);
    lastCheckpoint = checkpoint;
end

end
