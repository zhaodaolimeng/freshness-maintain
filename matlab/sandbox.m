% Latency expectation for a period on timeline
% ---
% timeline, M*2, M is counter of duty cycles & M[1] is start time of cycle
% lambda, 1, density of events
% stime, 1, begin time
% etime, 1, end time
% ---
% dd, 1, latency expectation
% timeNodeList = [11,13,77,79,102];
% timeline = [10,3;76,3;101,1];
timeline = [98,9;107,7;141,1;157,1;162,6];
timeNodeList = [99,101,103,105,107,108,110,112,114,142,158,162,164,166,168];

lambda = 0.8057;
stime = timeNodeList(1);
etime = timeNodeList(2);

resultList = zeros(size(timeNodeList));

index=1;

for etime = timeNodeList
    stime = timeNodeList(1);
    expect = 0;
    done = false;
    
    disp(['etime = ' num2str(etime) ' stime = ' num2str(stime)]);
    
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
        for i = lastCycle:-1:1
            if stime>=timeline(i,1)
                tw = timeline(i,2) - (stime - timeline(i,1));
                expect = expect + lambda*(2*(etime-stime)-tw)*tw;
                expect = expect + lambda*tw^2;
                break;
            end
            tw = timeline(i,2);
            expect = expect + lambda*(2*(etime-timeline(i,1))-tw)*tw;
            expect = expect + lambda*tw^2;
        end
    end
    disp(expect);
    resultList(index)=expect;index=index+1;
end

plot(timeNodeList, resultList);

for i = 2:length(timeNodeList)
    r =(resultList(i)-resultList(i-1))/(timeNodeList(i)-timeNodeList(i-1));
    disp(['rate =' num2str(r)]);
end
