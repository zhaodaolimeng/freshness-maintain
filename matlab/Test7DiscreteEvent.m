% Real discrete event sequence test for different crawl planning method

sensors = 10;
lambdaList = rand(1,sensors)*0.9+0.1;
crawlLimitList = randi([1,9],1,sensors);
sumOfCrawl = randi([25,30]);
discreteStep = 2;
eps = 10;
iteratorLimit = 20;

% Build time table
maxwp = 10;
minwp=3;
maxwr=10;
minwr=1;
timeRange=200;
timeTable = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange);
eventTable = []; % Event sequence, 1*N, N is count of events

% Generate events for each sensors
for sensor = sensors
    eventList = [];    
    timeline = timeTable(sensor).value;    
    now = 0;
    cycle = 1;
    while now < timeRange
        interval = exprnd(lambdaList(sensor));
        now = now + interval;        
        unchange = true;
        while cycle <= size(timeline,1)
            if now > timeline(cycle,1) && now <= timeline(cycle,2)+timeline(cycle,1)
                eventList = [eventList, now];
                unchange = false;
                break;
            elseif now < timeline(cycle,1)
                unchange = false;
                break;
            end
            cycle = cycle + 1;
        end
        if unchange
            break;
        end        
    end
    eventTable(1).value = eventList; % eventList for each sensor done
end

% 
