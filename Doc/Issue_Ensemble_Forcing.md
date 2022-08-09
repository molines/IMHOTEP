# Issues when using ensemble average forcing for the ocean
  
## 1. Context
  We decided to use the ensemble mean  for the forcing at ocean surface. Doing so, we arrived to ice model instability after 2.5 years of run. The instability show up with spurious 20m ice thickness but also temperature of -5 or -6 C associated to very high salinities ( > 60 g/kg) at  many points in the Arctic, mainly.  The model security check set an error flag when salinity exceeds 100 g/kg which happens during Nov. 1977 for member #1O. (Ensemble run started from restart files Jan. 01, 1975).  In order to find a solution we performs some tests and compared the results with October 1977.

## 2. Back to individual forcing:
In this test, each member is forced by its own atmospheric/ice forcing.
Anomaly in SSS dissapeared, giving us a clue that the problem  is related to a forcing issue. However, the constraint of having a common forcing for the ocean for all members was thought to be strong, and this solution is not acceptable.

## 3. Using ensemble mean only on ice free ocean.
Ice model instability therefore seems to be due to forcing incoherency, with probably positive feedback.
Thus, we  tested a modification in order to use the ensemble mean forcing only on the ocean where no member is covered by sea-ice.
In this test, SSS anomalies also disappears and no particular discontinuities were observed in the marginal ice zone.

