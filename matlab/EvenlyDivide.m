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
