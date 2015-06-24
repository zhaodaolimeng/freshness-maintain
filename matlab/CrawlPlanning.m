function [opt,route] = CrawlPlanning(crawls, dist)
%%
% DP method for calculate the best value for single sensor
% ---
% crawls, 1, number of crawls allowed for this sensor
% dist, {value:M*M}, M is the time points available

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
        end
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
