
coList = [];
huList = [];
noList = [];
teList = [];

% 
% for i = 2:336
%     if CO(i)>25000 || CO(i)-CO(i-1) >   
%         coList = [coList i];       
%     end
% end

t=0;
for i=2:336
    if CO(i)-CO(i-1)>5000
        t = t+1;
    end
end


