function [dist] = DiscretizeTimeline(timeTable,lambdaList,discreteStep)
%%
% Discretization is to find the key check points for a single sensor's timeline
% ---
% timeTable, N*{value:M_i*2}, M[1] is start time of cycle & M[2] is working length
% lambdaList, N, parameter of Poission
% discreteStep, 1, granularity of discretization
% ---
% dist, N*{value:M_i*M_i, timeNode:1*K_i}, 
%	N is sensor number & M_i*M_i is the time matrix of sensor i &
%   K_i is exact number of candidates time points

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
    dist(sensor).timeNode = timeNode;
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