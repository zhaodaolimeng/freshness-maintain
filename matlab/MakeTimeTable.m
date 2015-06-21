function [ timeTable ] = MakeTimeTable(sensors,maxwp,minwp,maxwr,minwr,timeRange)
%%
% maxwp, 1, max count of work cycles
% maxwr, 1, max time length of working cycle
% timeRange, 1, total time length

for i=1:sensors
    cycles = randi([minwp,maxwp]);
    aList = sort(randsample(1:timeRange,cycles));
    aList = aList';
    lList = zeros(cycles,1);
    for j=1:cycles-1
        lList(j) = max(randi([0,min(maxwr,aList(j+1)-aList(j))]),minwr);
    end
    j = j + 1;
    lList(j) = max(randi([0,min(maxwr,timeRange-aList(j))]),minwr);    
    timeTable(i).value = [aList,lList];
end
end

