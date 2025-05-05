let athlete YanniKakouris, 
    events: 100m, 200m, mile,
    prs: 100m: 11.01, 200m: 22.42, mile: 5:00.00;
let athlete AsaShepard, 
    events: 400m, 800m, 4x400m, 
    prs: 400m: 48.6, 800m: 1:55.00;
let roster Williams, 
    athletes: YanniKakouris, AsaShepard;




let athlete AmherstDude, 
    events: 100m, 200m, 
    prs: 100m: 10.7, 200m: 22.9;
let roster Amherst;
add AmherstDude to Amherst;



let meet NESCACs, 
    events: 100m, 200m, 400m, 800m, mile,
    scoring: 1st: 10, 2nd: 5, 3rd: 3,
    teams: Williams;

include Amherst in NESCACs;

optimize Williams for NESCACs;