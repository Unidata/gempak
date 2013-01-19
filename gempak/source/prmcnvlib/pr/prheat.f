	FUNCTION PR_HEAT ( tmpf, relh )
C************************************************************************
C* PR_HEAT								*
C*									*
C* This function computes HEAT, the Southern Region/CPC Rothfusz heat	*
C* index, from TMPF and RELH.  The output will be calculated in degrees *
C* Fahrenheit.								*
C*									*
C*	Source:  NWS Southern Region SSD Technical Attachment SR 90-23	*
C* 		 7/1/90.  Heat Index was originally known as the	*
C*		 apparent temperature index (Steadman, JAM, July, 1979).*
C*									*
C* The Rothfusz regression is optimal for TMPF > ~80 and RELH > ~40%.	*
C* This code applies a simple heat index formula and then resorts to	*
C* the Rothfusz regression only if the simple heat index exceeds 80,	*
C* implying temperatures near, but slightly below 80.  To make the	*
C* simple calculation continuous with the values obtained from the	*
C* Rothfusz regression, the simple result is averaged with TMPF in	*
C* computing the simple heat index value.				*
C*									*
C* This code includes adjustments made by the CPC for low RELH at high	*
C* TMPF and high RELH for TMPF in the mid 80's.				*
C*									*
C* REAL PR_HEAT  ( TMPF, RELH )  					*
C*									*
C* Input parameters:							*
C*	TMPF		REAL    	Air temperature 		*
C*	RELH		REAL		Relative Humidity		*
C*									*
C* Output parameters:							*
C*	PR_HEAT		REAL		Heat Index			*
C**									*
C* Log:									*
C* M. Nelson		 7/92						*
C* D. Vietor		 7/92	Temps less than 77			*
C* S. Jacobs/EAI	 3/93	Cleaned up				*
C* K. Brill		 3/02	Updated from 1990 reference		*
C* K. Brill/HPC		 1/03	Fix discontinuity around 77 F		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	If either the Temperature or Relative Humidity are missing,
C*	then set the result to missing.
C
	IF  ( ERMISS (tmpf) .or. ERMISS (relh) )  THEN
	    PR_HEAT = RMISSD
C
	ELSE
C
C*	    If the temperature is less than 40 degrees, then set the
C*	    heat index to the temperature.
C
	    IF  ( tmpf .le. 40.0 )  THEN
	        PR_HEAT = tmpf
	    ELSE
C
C*		Compute a simple heat index. If the value is less
C*		than 80 degrees, use it.
C
		PR_HEAT = 61. + ( tmpf - 68. ) * 1.2 + relh * .094
		PR_HEAT = .5 * ( tmpf + PR_HEAT )
		IF  ( PR_HEAT .lt. 80.0 )  THEN
		    RETURN
		ELSE
C
C*		    Otherwise, compute the full regression value
C*		    of the heat index.
C
		    t2 = tmpf * tmpf
		    r2 = relh * relh
		    PR_HEAT =  -42.379
     +				+  2.04901523 * tmpf
     +				+ 10.14333127 * relh 
     +				-  0.22475541 * tmpf * relh
     +				-  0.00683783 * t2 
     +				-  0.05481717 * r2
     +				+  0.00122874 * t2 * relh
     +				+  0.00085282 * tmpf *r2
     +				-  0.00000199 * t2 * r2
C
C*		    Adjust for high regression at low RH for temps
C*		    above 80 degrees F.
C
		    IF ( ( relh .le. 13.0 ) .and.
     +			 ( ( tmpf .ge.  80.0 ) .and.
     +			   ( tmpf .le. 112.0 ) ) )  THEN
C
			adj1 = ( 13. - relh ) / 4.
			adj2 = SQRT ( ( 17. - ABS (tmpf - 95.) ) / 17. )
			adj  = adj1 * adj2
			PR_HEAT = PR_HEAT - adj
C
C*		    Adjust for low regression at high RH and temps
C*		    in the mid 80s.
C
		    ELSE IF ( ( relh .gt. 85.0 ) .and.
     +			      ( ( tmpf .ge. 80.0 ) .and.
     +				( tmpf .le. 87.0 ) ) )  THEN
C
			adj1 = ( ( relh - 85. ) / 10. )
			adj2 = ( ( 87. - tmpf ) / 5. )
			adj  = adj1 * adj2
			PR_HEAT = PR_HEAT + adj
C
		    END IF
		END IF
	    END IF
	END IF
C*
	RETURN
	END
