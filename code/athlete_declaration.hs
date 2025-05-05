let athlete YanniKakouris, 
    events: 100m, 200m, 4x100m, 
    prs: 100m: 11.01, 200m: 22.42;

let roster Williams, athletes: YanniKakouris;

let meet NESCACs, 
    events: 100m, 200m, 300m, 5k, 
    scoring: 1st: 10, 2nd: 5, 3rd: 3,
    teams: Williams;

add Amherst to NESCACs;
add ConnCollege to NESCACs;

optimize Williams for NESCACs;