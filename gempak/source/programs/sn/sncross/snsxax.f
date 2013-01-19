	SUBROUTINE SNSXAX  ( nstn, stns, sltln, ipsdat, nlvls, sloc, 
     +			     xmin, xmax, iret )
C************************************************************************
C* SNSXAX								*
C*									*
C* This subroutine defines the x axis for the cross section program.	*
C*									*
C* SNSXAX  ( NSTN, STNS, SLTLN, IPSDAT, NLVLS, SLOC, XMIN, XMAX, IRET )	*
C*									*
C* Input parameters:							*
C*	NSTN		INTEGER		Number of stations		*
C*	STNS (NSTN)	CHAR*		Station ids			*
C*	SLTLN (2,NSTN)	REAL		Station lat/lons		*
C*	IPSDAT (NSTN)	INTEGER		Pointers to station data	*
C*	NLVLS (NSTN)	INTEGER		Number of levels		*
C*									*
C* Output parameters:							*
C*	SLOC (NSTN)	REAL		Station location on x axis	*
C*	XMIN		REAL		Minimum value on x axis		*
C*	XMAX		REAL		Maximum value on x axis		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Woytek/GSFC	 9/82	Distance calculation in CSGTDS		*
C* M. desJardins/GSFC	11/85						*
C* M. desJardins/GSFC	 9/90	Remove STNLN; was in wrong place	*
C* M. desJardins/GSFC	 3/91	Added LLTMCX				*
C* S. Jacobs/EAI	10/92	Fixed typo dely->delx in IF block	*
C* S. Jacobs/EAI	 9/93	Changed real exponent to integer;	*
C*				   changed 0.5 exponent to SQRT		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stns(*)
	REAL		sltln (2,*), sloc (*)
	INTEGER		ipsdat (*), nlvls (*)
C*
	REAL		alat (LLTMCX), alon (LLTMCX)
	CHARACTER	stnst*8
C------------------------------------------------------------------------
C*	Convert degrees to radians.
C
	DO  i = 1, nstn
	    alat (i) = sltln (1,i) * DTR
	    alon (i) = sltln (2,i) * DTR
	END DO
C
C*	Map all the stations to the line from station 1 to station n.
C
	iret = 0
	rlat1 = alat (1)
	rlatn = alat (nstn)
	rlon1 = alon (1)
	rlonn = alon (nstn)
C*
C*	Get x and y differences of upper and lower stations.
C
	delx = (rlon1 - rlonn) * COS (( rlat1 + rlatn ) / 2.0)
	dely =  rlat1 - rlatn
	IF ( (delx .ne. 0.) .and. (dely .ne. 0.) ) THEN
C*
C*	   Non-zero, non-infinte slope - calculate slope and get the angle.
C
	   slope = (dely / delx)
	   angle = ATAN ( slope )
	   IF (angle .lt. 0.) angle = angle + PI 	
	   costh = COS (angle)
	   sinth = SIN (angle)
	END IF
C
C*	The distance to the top station is zero. 
C
	sloc (1) = 0.
C
C*	Loop through the other stations, calculating the distance.
C
	DO  i = 2, nstn
	  IF ( ( delx .ne. 0. ) .and. ( dely .ne. 0. ) ) THEN
C
C*		First get longitude on line for station latitude.
C*		  Then get difference between longitude of station and 
C*		  longitude on the line for the given station latitude.
C
		rlnlin = rlon1 + (alat(i) - rlat1) / ( slope * 
     +			 COS (( alat(i) + rlat1 )/2.0) )
		diflon = ( alon(i) - rlnlin ) * COS ( alat(i) ) 
C
C*		Now get distance between station and intersection of 
C*		   perpendicular bisector and the sloped line.
C
		hyp = diflon * sinth
C
C*		Get x-y components of perpendicular bisector(line 
C*		  segment connecting stn to line intersection). 
C*		  Add to stn lat/lon to get intersection.
C
		ych = hyp * costh
		xch = hyp * sinth
		stlat = alat(i) + ych
		stlon = alon(i) - ( xch / cos( (stlat + alat(i)) /
     +				2.0) )
C*
	      ELSE IF ( dely .eq. 0. ) THEN
C
C*		For end stations with equal latitudes, the 
C*		  distance is given by the difference in longitudes.
C
		stlon = alon (i)
		stlat = rlat1
C*
	      ELSE IF ( delx .eq. 0. ) THEN
C
C*		For equal longitudes, the distance is given by the difference
C*		   in latitudes.
C
		stlon = rlon1 
		stlat = alat(i)
	   END IF
C
C*	  Calculate the distance from bottom station to intersection of
C*	     perpendicular line.
C
	  sloc (i) = SQRT ( ( stlat - rlat1 ) ** 2 + 
     +		( ( stlon - rlon1 ) * 
     +			COS ( ( rlat1 + stlat ) / 2.0 )) ** 2 )
 	END DO
C
C*	Sort the distance array from left to right.  All the station
C*	arrays need to be kept in the same order.
C
	DO  i = 1, nstn-1
	    DO  j = i+1, nstn
		IF  ( sloc (j) .lt. sloc (i) ) THEN
		    stnst = stns (i)
		    slatt = sltln (1,i)
		    slont = sltln (2,i)
		    ipst  = ipsdat (i)
		    nlvlst= nlvls (i)
		    sloct = sloc (i)
		    stns (i) = stns (j)
		    sltln (1,i) = sltln (1,j)
		    sltln (2,i) = sltln (2,j)
		    ipsdat (i) = ipsdat (j)
		    nlvls (i)  = nlvls (j)
		    sloc  (i)  = sloc  (j)
		    stns  (j)  = stnst
		    sltln (1,j) = slatt
		    sltln (2,j) = slont
		    ipsdat (j) = ipst
		    nlvls (j) = nlvlst
		    sloc (j) = sloct
		END IF
	    END DO
	END DO
C
C*	Get the maximum and minimum x values from the distance array.
C
	xmin  = sloc (1)
	xmax  = sloc (nstn) 
C*
	RETURN
	END
