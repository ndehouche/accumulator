set Outcomes;
set Matches;
set Bookmakers; 

param O{k in Outcomes,i in Matches,j in Bookmakers};
param P{k in Outcomes, i in Matches};
param Pmin;





var x {k in Outcomes, i in Matches,j in Bookmakers},  binary;


maximize Odds:
prod{k in Outcomes, i in Matches,j in Bookmakers} (1-      (1-O[k,i,j])*x[k,i,j] );

subject to MinProb:
prod{k in Outcomes, i in Matches,j in Bookmakers} (1-      (1-P[k,i])*x[k,i,j] )>=Pmin;


subject to NoConflictOutcomes {i in Matches, j in Bookmakers}:
sum{k in Outcomes} x[k,i,j] <=1;

subject to NoConflictBookmakers {k in Outcomes, i in Matches}:
sum{j in Bookmakers} x[k,i,j] <=1;



