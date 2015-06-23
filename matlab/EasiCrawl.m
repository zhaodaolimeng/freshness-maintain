function [opt,arrange,rate] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit)
%%
% Compute best schedule given timetable and event density predication
% --
% lambdaList, N, parameter of Poission
% timeTable, N*{value:M_i*2}, N is sensors & M[1] is duty cycle & M[2] is working time
% crawlLimitList, N, max times that sensor can be crawled
% sumOfCrawl, 1, total crawl number
% discreteSize, 1, the grantity of discretization
% eps, threshold of improvement method
% iteratorLimit, maximal iteration times
% ---
% opt, 1, minimal value
% arrange, N, best arrangement for each sensor

opt = inf;
sensors = length(lambdaList);
if sumOfCrawl < sensors
    disp('ERROR: sum of crawls are less than sensors!')
    return;
end
disp('Starting ...');
distanceMatrix = DiscretizeTimeline(timeTable,lambdaList,discreteStep);
arrange = EvenlyDivide(crawlLimitList,sumOfCrawl);
% disp(['arrange = ' mat2str(arrange)]);
for i=1:sensors
    memo(i).value=zeros(1,crawlLimitList(i))-1;
end
optFirst = 0;
optLast = 0;
for i=1:iteratorLimit
    oldopt = opt;
    [memo, opt,arrange] = ImproveSolution(memo,arrange,distanceMatrix,crawlLimitList);    
    disp(['opt = ' num2str(opt) ' arrange = ' mat2str(arrange)]);
    if i == 1 ;optFirst = opt; end
    if i == iteratorLimit ;optLast = opt; end
    if abs(opt-oldopt)<eps
        optLast = opt;
        break;
    end
end
rate = (optFirst - optLast)/optFirst;
end

function [arr] =  EvenlyDivide(crawlLimitList,sumOfCrawls)
%%
% Frist arrangement, divide the crawls as even as possible
% ---
% totalCrawls, total number allowed
% crawlLimitsList, maximal crawls can be taken to each sensor
% ---
% arr, N*1, N is sensor number

sensors = length(crawlLimitList);
arr = crawlLimitList;
if sum(crawlLimitList)>sumOfCrawls    
    arr = zeros(1,sensors);
    i = sumOfCrawls;
    t = 1;
    while i>=0
        if t>sensors
            t=1;
        end
        if crawlLimitList(t)>arr(t)
            i=i-1;
            arr(t)=arr(t)+1;
        end
        t=t+1;
    end        
end
end

function [dist] = DiscretizeTimeline(timeTable,lambdaList,discreteStep)
%%
% Discretization is to find the key check points for a single sensor's timeline
% ---
% timeTable, N*{value:M_i*2}, M[1] is start time of cycle & M[2] is working length
% lambdaList, N, parameter of Poission
% discreteStep, 1, granularity of discretization
% ---
% dist, N*{value:M_i*M_i}, N is sensor number & M_i*M_i is the time matrix of sensor i

sensors = length(lambdaList);
for sensor = 1:sensors
    timeline = timeTable(sensor).value;
    timeNode = [];
    for i =1:length(timeline(:,1))
        workCycle = timeline(i,:);
        timeNode = horzcat(timeNode, workCycle(2)+workCycle(1):-discreteStep:workCycle(1));
    end
    timeNode = sort(timeNode);
    if timeNode(1) ~= 0;timeNode = horzcat(0, timeNode);end        
    nodeCnt = length(timeNode);
    distanceMap = zeros(nodeCnt);
    for i=1:nodeCnt
        for j=i+1:nodeCnt
            distanceMap(i,j) = LatencyExpectation(timeline,lambdaList(sensor),timeNode(i),timeNode(j));
        end
    end
    dist(sensor).value = distanceMap;
end
end

function [expect] = LatencyExpectation(timeline,lambda,stime,etime)
%%
% Latency expectation for a period on timeline
% ---
% timeline, M*2, M is counter of duty cycles & M[1] is start time of cycle
% lambda, 1, density of events
% stime, 1, begin time
% etime, 1, end time
% ---
% dd, 1, latency expectation

expect = 0;
done = false;
for lastCycle=length(timeline):-1:1
    if timeline(lastCycle,1)<=etime
        if timeline(lastCycle, 1) <= stime
            expect = lambda * (etime - stime)^2;
            done = true;
        else
            expect = lambda*(etime-timeline(lastCycle,1))^2;
        end
        break;
    end
end
if ~done
    for i = lastCycle - 1:-1:1
        if stime>=timeline(i,1)
            tw = timeline(i,2) - (stime - timeline(i,1));
            expect = expect + lambda*2*((etime-stime)-tw)*tw;
            expect = expect + lambda*tw^2;
            break;
        end
        tw = timeline(i,2);
        expect = expect + lambda*2*(etime-timeline(i,1)-tw)*tw;
        expect = expect + lambda*tw^2;
    end
end
% if expect < 0 ;expect = 0; end
end

function [memo, opt, arrange] = ImproveSolution(memo,arrange,dist,crawlLimitList)
%%
% Incremental Method
% ---
% opt, 1
% arrange, N*1, N is sensor number
% memo, N*{value:M_i}, N is sensor number & M_i is maximal crawls for sensor
% i & the value stored is the best value that can achieved with exactly the
% number of crawls
% dist, N*{value:M_i*M_i}, N is sensor number & M_i is time nodes for sensor i
% crawlLimitsList, N*1, N is sensor number

sensors = length(crawlLimitList);
minExp = inf;
mins1 = 0;
mins2 = 0;
untouched = true;
for s1 = 1:sensors
    for s2 = 1:sensors
        if s1 ~= s2 && arrange(s1)+1<=crawlLimitList(s1) && arrange(s2)>1
            % At least crawl once
            [memo,topt] = FetchOrSolve(memo,s1,arrange(s1)+1,dist(s1)); t = topt;            
            [memo,topt] = FetchOrSolve(memo,s1,arrange(s1),dist(s1)); t = t - topt;
            [memo,topt] = FetchOrSolve(memo,s2,arrange(s2)-1,dist(s2)); t = t + topt;
            [memo,topt] = FetchOrSolve(memo,s2,arrange(s2),dist(s2)); t = t - topt;
            if t < minExp
                minExp = t; mins1 = s1; mins2 = s2; untouched = false;               
            end
        end
    end
end
t = 0;
if untouched    
    for i = 1:sensors
        [memo, topt] = FetchOrSolve(memo,i,arrange(i),dist(s1));
        t = t + topt;
    end    
else
    if minExp<0
        arrange(mins1) = arrange(mins1) + 1;
        arrange(mins2) = arrange(mins2) - 1;        
    end
    for s1=1:sensors
        t = t + memo(s1).value(arrange(s1));
    end
end
opt = t;
end

function [memo, opt] = FetchOrSolve(memo,sensor,crawls,dist)
%%
% Memo as a cache
% ---
% memo, N*{value:M}, N is number of sensor & M is crawl number
% sensor, 1, index of target sensor
% crawls, 1, number of total crawl of this sensor
% dist, {value:M*M}, M is time node of sensor
% ---
% opt, 1, optimal value latency

% disp(['sensor = ' num2str(sensor) ' crawls = ' num2str(crawls)]);
if memo(sensor).value(crawls) == -1
    [opt] = CrawlPlanning(crawls,dist);
    memo(sensor).value(crawls) = opt;
else
    opt = memo(sensor).value(crawls);
end
end

function [opt] = CrawlPlanning(crawls, dist)
%%
% DP method for calculate the best value for single sensor
% ---
% crawls, 1, number of crawls allowed for this sensor
% dist, {value:M*M}, M is the time points available

% BUGGY!!!!!

nodes = size(dist.value,1);
f = Inf(nodes, crawls);
% p = zeros(sensors, crawls);
% route = zeros(crawls);
for node = 1:nodes
    f(node, 1) = dist.value(1,node);
%     p(sensor, 1) = 1;
end
for crawl = 2:crawls
    for node = 2:nodes
        for mid = 2: node - 1
            tmp = f(mid,crawl-1) + dist.value(mid,node);
            if tmp < f(node,crawl)
                f(node,crawl) = tmp;
%                 p(sensor,crawl) = mid;
            end
        end
    end
end
opt = f(nodes, crawls);
% sensor = sensors;
% for crawl = crawls:-1:1
%     route(crawl) = sensor;
%     sensor = p(sensor, crawl);
% end
end

