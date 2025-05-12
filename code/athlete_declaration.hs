let athlete YanniKakouris, 
    events: 100m, 200m, 400m,
    prs: 100m: 11.01, 200m: 22.42, 400m: 55;

let athlete AsaShepard, 
    events: 200m, 400m, 800m, 
    prs: 200m: 21.89, 400m: 48.6, 1500: 0:25.00, somelongevent: 0:1.0;

let roster Williams, 
    athletes: YanniKakouris, AsaShepard;

let athlete AmherstDude, 
    events: 100m, 200m, 
    prs: 100m: 10.7, 200m: 21.9;

let roster Amherst;
add AmherstDude to Amherst;

let meet NESCACs, 
    events: 100m, 200m, 400m,
    scoring: 1st: 10, 2nd: 5, 3rd: 3,
    teams: Williams, Amherst,
    maxEventsPerAthlete: 1;

output meet NESCACs;
output roster Williams;