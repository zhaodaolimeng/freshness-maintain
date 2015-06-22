timeline = [98,9;107,7;141,1;157,1;162,6];
timeNodeList = [99,101,103,105,107,108,110,112,114,142,158,162,164,166,168];
lambda = 0.8057;

timeline = [69,9;79,1;82,1;102,1;150,5;156,6;162,5];
timeNodeList = [70,72,74,76,78,80,83,103,151,153,155,156,158,160,162,163,165,167];
lambda = 0.2510;

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
    disp(expect);
    resultList(index)=expect;
    index=index+1;
end

plot(timeNodeList, resultList);

for i = 2:length(timeNodeList)
    r =(resultList(i)-resultList(i-1))/(timeNodeList(i)-timeNodeList(i-1));
    disp(['rate =' num2str(r)]);
end


