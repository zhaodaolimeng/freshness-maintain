function [opt,arrange,rate,plans] = EasiCrawl(lambdaList,timeTable,crawlLimitList,sumOfCrawl,discreteStep,eps,iteratorLimit)
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
for i=1:sensors
    memo(i).value=zeros(1,crawlLimitList(i))-1;
end
optFirst = 0;
optLast = 0;
for i=1:iteratorLimit
    oldopt = opt;
    [memo, opt, arrange] = ImproveSolution(memo,arrange,distanceMatrix,crawlLimitList);    
    disp(['opt = ' num2str(opt) ' arrange = ' mat2str(arrange)]);
    if i == 1 ;optFirst = opt; end
    if i == iteratorLimit ;optLast = opt; end
    if abs(opt-oldopt)<eps
        optLast = opt;
        break;
    end
end
rate = (optFirst - optLast)/optFirst;
% Specific plans for each sensor
for sensor = 1:sensors
    [topt,nodes] = CrawlPlanning(arrange(sensor),distanceMatrix(sensor));
    crawlTimes = [];
    for crawl = nodes
        crawlTimes = [crawlTimes distanceMatrix(sensor).timeNode(crawl)];
    end
    plans(sensor).value = crawlTimes;
end
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
% Memorize the accelerate
% ---
% memo, N*{value:M}, N is number of sensor & M is crawl number
% sensor, 1, index of target sensor
% crawls, 1, number of total crawl of this sensor
% dist, {value:M*M}, M is time node of sensor
% ---
% opt, 1, optimal value latency

if memo(sensor).value(crawls) == -1
    opt = CrawlPlanning(crawls,dist);
    memo(sensor).value(crawls) = opt;
else
    opt = memo(sensor).value(crawls);
end
end
