let athlete YanniKakouris,
    events: 100m, 200m, 400m,
    prs: 100m: 11.01, 200m: 22.5, 400m: 20.0;

let athlete ColinStone,
    events: 200m, 400m,
    prs: 200m: 22.97, 400m: 49.12;

let roster Williams,
    athletes: YanniKakouris, ColinStone;

let athlete Amherst1,
    events: 200m, 400m,
    prs: 200m: 22.97, 400m: 49.13;

let athlete Amherst2,
    events: 100m, 200m, 400m, 800m,
    prs: 100m: 11.10, 200m: 22.49, 400m: 44, 800m: 1:55;

let roster Amherst;
add Amherst1 to Amherst;
add Amherst2 to Amherst;

let meet NESCACs,
    events: 100m, 200m, 400m,
    scoring: 1st: 10, 2nd: 5, 3rd: 1,
    maxEventsPerAthlete: 2;

include Williams in NESCACs;
include Amherst in NESCACs;

duplicate athlete YanniKakouris to YanniKakouris2;
add YanniKakouris2 to Williams;
output roster Williams to "williams_roster1";
optimize Williams for NESCACs;
