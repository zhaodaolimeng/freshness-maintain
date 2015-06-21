% Latency expectation for a period on timeline
% ---
% timeline, M*2, M is counter of duty cycles & M[1] is start time of cycle
% lambda, 1, density of events
% stime, 1, begin time
% etime, 1, end time
% ---
% dd, 1, latency expectation

timeline = [];

expect = 0;
for lastCycle=length(timeline):-1:1
    if timeline(lastCycle,1)<=etime
        expect = expect + lambda*(etime-timeline(lastCycle,1))^2;
        break;
    end
end
for i = lastCycle:-1:1
    if stime>timeline(lastCycle,1)
        tw = timeline(lastCycle,2) - stime + timeline(lastCycle,1);
        expect = expect - lambda*(2*(etime-stime)-tw)*tw;
        break;
    end
    tw = timeline(lastCycle,2);
    expect = expect + lambda*(2*(etime-stime)-tw)*tw;
end