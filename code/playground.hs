let athlete genericSprinter,
    events: 100m, 200m, 400m;

duplicate athlete genericSprinter to YanniKakouris;
duplicate athlete genericSprinter to AsaShepard;

set YanniKakouris to 11.01 in 100m;
set YanniKakouris to 22.07 in 200m;
set AsaShepard to 22.5 in 200m;
set AsaShepard to 48.6 in 400m;
set AsaShepard to 4:25.3 in mile;

update athlete YanniKakouris,
    events: 100m, 200m, 400m,
    prs: 100m: 11.01, 200m: 22.07, 400m: 51.1;

let roster WilliamsTrack,
    athletes: YanniKakouris, AsaShepard;

output roster WilliamsTrack to "williams_track_roster";

remove AsaShepard from Williams;