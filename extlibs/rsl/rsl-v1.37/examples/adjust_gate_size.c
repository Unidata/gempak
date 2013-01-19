#include <stdio.h>
#include<string.h>
#include <stdlib.h>
#include <unistd.h>
#include "rsl.h"

void adjust_gate_size(Radar *radar, float gate_size_adjustment)
{

    int    i,j,k,nvolumes;
    Volume *volume;
    Sweep  *sweep;
    Ray    *ray;
  
	if(gate_size_adjustment != 1.0) {
		nvolumes = radar->h.nvolumes;
		for(i=0; i<nvolumes; i++) {
			if(radar->v[i] != NULL) {
				volume=radar->v[i];
				for(j=0; j<volume->h.nsweeps; j++) {
					if(volume->sweep[j] != NULL) {
						sweep = volume->sweep[j];
						for(k=0; k<sweep->h.nrays; k++) {
							if(sweep->ray[k] != NULL) {
								ray = sweep->ray[k];
								ray->h.gate_size = ray->h.gate_size*gate_size_adjustment;
							}
						}
					}
				}
			}
		}
	}

/*
  If Gate Size Adjustment is not unity, then we must change the
  following:
      old_gs = radar->v[i]->sweep[sweepIndex]->ray[rayIndex=]->h.gate_size
	  radar->v[i]->sweep[sweepIndex]->ray[rayIndex=]->h.gate_size = 
	      old_gs*gate_size_adjustment
   Here are some comments from Sandra Yuter on the necessity of this fix.
   > I dug into the revelant code and it looks like we can do a relatively
	> simple workaround for the SIGMET raw product file range bin size
	> errors for the RHB data pulses widths of 0.5 usec and 2.0 usec as follows.
	> 
	> Since we are all converting from sigmet to UF I suggest we resize 
	> the range bin size values in the ray headers in the qlook step
	> where the sigmet to UF conversion occurs.
	> 
	> The resize requires only 1 additional line of code (I have included
	> a few others for context) in qlook.c
	> 
	> rangeToFirstGate = 0.001 *
	> 	         radar->v[i]->sweep[sweepIndex]->ray[rayIndex]->h.range_bin1;
	> 	      gateSize = 0.001 *
	> 	         radar->v[i]->sweep[sweepIndex]->ray[rayIndex]->h.gate_size;
	> 	      radar->v[i]->sweep[sweepIndex]->ray[rayIndex]->h.gate_size=
	> 		gateSize*0.6*1000;
	> 
	> I have used 0.6 adjustment factor since that is 75/125 which corresponds
	> to the error in my 0.5 usec data, for the SUR scans, this adjustment
	> factor is 133.33/125 or 1.067.
	
	The following is from Joe Holmes from SIGMET
	
	> 
	> I think you have experienced a problem with the RVP7 range resolution
	> configuration.  Both in IRIS and in the RVP7 you manually type in
	> the range resolution.  The RVP7 allows a separate resolution for
	> each pulsewidth, while IRIS only allows 1 value.  There is no feedback
	> if these values are not typed in the same.  Based on setup information
	> we have here from the RH Brown from Oct 23, 1998, you had the following
	> configuration:
	> 
	> RVP7:
	> 0   0.50 usec   75.0m
	> 1   0.80 usec  125.0m
	> 2   1.39 usec  125.0m
	> 3   2.00 usec  133.3m
	> 
	> IRIS: 125.0 meters
	> 
	> I think the error at PW#0 was corrected a while back, but
	> the error  in PW#3 was never corrected.  Next time someone is
	> at the ship, they should check this, fix the long pulse, and remake
	> the bandpass filter for the long pulse.
	> 
	> In the short term, you can correct the error by taking all your
	> long pulse data and changing the header to correctly document the
	> range resolution.  We have a program to do this, it is called "change_raw".
	> The source is on any IRIS system, which was installed with the
	> source, headers, and objects turned on.  It is in the
	> ${IRIS_ROOT}utils/examples directory.  We can supply you with
	> a compiled version of this program, if you want.  Available platforms
	> are Linux, HP-UX, and IRIX.

*/

	return;
}
