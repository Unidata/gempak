	SUBROUTINE IS_OBFC ( report, lenr, iobfc, iptr, iret )
C************************************************************************
C* IS_OBFC 								*
C*									*
C* This subroutine decodes an observed / forecast indicator from an     *
C* international sigmet report.                                         *
C*                                                                      *
C* IS_OBFC ( REPORT, LENR, IOBFC, IPTR, IRET )                          *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*									*
C* Output parameters:							*
C*	IOBFC 		INTEGER 	Obs / fcst indicator            *
C*	IPTR		INTEGER		Pointer to location after indic.*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = obs/fcst indic. not found *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* F. J. Yen/NCEP	 1/00	Provided value for iloc for iobfc=2	*
C* F. J. Yen/NCEP	12/03	Added key words "FORECAST" & "OBSERVED".*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
C------------------------------------------------------------------------
	iret  = 0
	iobfc = IMISSD
	iptr  = 0
C
C*	Look for the key word OBS, FCST, FORECAST, OBSERVED or OBS/FCST.
C
	len = MIN ( 10, lenr ) 
	iloc1 = INDEX ( report ( :len ), ' OBS/FCST' )
	iloc2 = INDEX ( report ( :len ), ' FCST/OBS' )
	IF ( ( iloc1 .gt. 0 ) .or. ( iloc2 .gt. 0 ) ) THEN
	    iobfc = 2
	    iloc = iloc1 + iloc2
	    iadd  = 9
	  ELSE
	    iloc = INDEX ( report ( :len ), ' OBS' )
	    ilocob = iloc
	    IF ( iloc .gt. 0 ) THEN
		iobfc = 0
		IF ( report (iloc:iloc+8) .eq. ' OBSERVED' ) THEN
		    iadd  = 9
		  ELSE
		    iadd  = 4
		END IF
	      ELSE
		iloc = INDEX ( report ( :len ), ' FCST' )
		IF ( iloc .gt. 0 ) THEN
		    iobfc = 1
		    iadd  = 5
		  ELSE
		    iloc = INDEX ( report ( :len ), ' FORECAST' )
		    IF ( iloc .gt. 0 ) THEN
			IF ( ilocob .gt. 0 .and. iadd .eq. 9 ) THEN
			    idif = iabs ( ilocob - iloc )
			    IF ( idif .le. 16 ) THEN
			        iobfc = 2
			        iloc = MAX ( iloc, ilocob )
			      ELSE IF ( ilocob .lt. iloc ) THEN
				iloc = ilocob
			      ELSE
				iobfc = 1
			    END IF
			  ELSE
			    iobfc = 1
			END IF
			iadd  = 9
		    END IF
		END IF
	
	    END IF
	END IF
C
	IF ( iloc .gt. 0 ) THEN
	    iptr = iloc + iadd
	  ELSE
	    iret = -9
	END IF
C*
	RETURN
	END
