let athlete alice, events: 100m,200m, prs: 100m:12.34,200m:25.67;
let athlete bob, events: 100m,400m, prs: 100m:11.89,400m:54.32;
let athlete charlie, events: 800m,1600m, prs: 800m:2:10.5,1600m:4:45.2;
let roster varsity, athletes: alice,bob,charlie;
let meet states, events: 100m,200m,400m,800m,1600m, scoring: 1st:10,2nd:8,3rd:6,4th:5,5th:4,6th:3,7th:2,8th:1;
include varsity in states;
optimize varsity for states;
output roster varsity;
output meet states;
