	SUBROUTINE WP_LTLN ( ltlns, alat, alon, nmpt, iret )
C************************************************************************
C* WP_LTLN 								*
C*									*
C* This subroutine breaks apart the lat/lon string and converts the   	*
C* points from degrees-minutes to degrees-decimal.			*
C*                                                                      *
C* WP_LTLN ( LTLNST, ALAT, ALON, NMPT, IRET )				*
C*									*
C* Input parameters:	                                                *
C*      LTLNS           CHAR*           Lat/lon string for a watch area *
C*									*
C* Output parameters:							*
C*	ALAT (*)  	REAL  	  	Array of lat points (degree,dec)*
C*	ALON (*)  	REAL  	  	Array of lon points (degree,dec)*
C*	NMPT  	  	INTEGER	  	Number of decoded lat/lon points*
C*	IRET  	  	INTEGER	  	Return code			*
C*				  	  0 = normal return		*
C*					 -6 = format error		*
C*					 -7 = string too long		*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 4/05						*
C* F. J. Yen/NCEP	 5/05	Multiply longitude by -1 to put in U.S. *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	ltlns
	REAL		alat (*), alon (*)
C
	PARAMETER       ( MAXPTS = 50 )
	CHARACTER	ltln (MAXPTS)*11, cltln (2)*5, ltlnst*800
C------------------------------------------------------------------------
	iret  = 0
C
C*	Break up composite lat/lon string (ttttt.nnnnn ttttt.nnnnn ...)
C*	into an array of a single lat/lon string.  (ttttt is the latitude
C*	and nnnnn is the longitude in degrees-minutes
C
	lens = LEN ( ltlns )
	IF ( lens .gt. 800 ) THEN
	    iret = -7
	    RETURN
	END IF
	CALL ST_UNPR ( ltlns, lens, ltlnst, lenw, ier )
	CALL ST_CLST ( ltlnst, ' ', ' ', MAXPTS, ltln, nmpt, ier )
	IF ( nmpt .gt. 0 ) THEN
	    DO ii = 1, nmpt
	        CALL ST_CLST ( ltln (ii), '.', ' ', 2, cltln, num, ier )
		IF ( ier .eq. 0 .and. num .eq. 2 ) THEN
		    CALL ST_NUMB ( cltln (1)(1:3), latdg, ier1 )
		    CALL ST_NUMB ( cltln (1)(4:5), latmn, ier2 )
		    CALL ST_NUMB ( cltln (2)(1:3), londg, ier3 )
		    CALL ST_NUMB ( cltln (2)(4:5), lonmn, ier4 )
		    ier = ier1 + ier2 + ier3 + ier4
		    IF ( ier .eq. 0 ) THEN
		        alat (ii) = latdg + latmn/60.
		        alon (ii) = (-1) * ( londg + lonmn/60.)
		      ELSE
		 	iret = -6
			RETURN
		    END IF
		  ELSE
		    iret = -6
		    RETURN
		END IF
	    END DO
	  ELSE
	    iret = -6
	END IF
C*
	RETURN
	END
