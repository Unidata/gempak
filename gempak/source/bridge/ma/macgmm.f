	SUBROUTINE MA_CGMM ( string, mxormn, ier )
C************************************************************************
C* MA_CGMM                                                              *
C*                                                                      *
C* This subroutine gets the maximum or minimum temperature and stores	*
C* it into the interface array in common /rintfv/.			*
C*                                                                      *
C* MA_CGMM  ( STRING, MXORMN, IER )					*
C*                                                                      *
C* Input parameters:                                                    *
C*	RIVALS(IRMNTH)  REAL		Month
C*      STRING          CHAR*		Character string with min/max.	*
C*	MXORMN		INTEGER		Flag to get max or min temp.	*
C*                                                                      *
C* Output parameters:                                                   *
C*	RIVALS(IRMXTM)	REAL		Maximum temperature (degrees C)	*
C*	RIVALS(IRMITM)  REAL		Minimum temperature (degrees C)	*
C*      IER             INTEGER         Return code                     *
C*                                        0 = Normal return             *
C**                                                                     *
C* Log:                                                                 *
C* F. J. Yen/NCEP	 4/01	Created from CG_PRTM.			*
C* D. Kidwell/NCEP	 3/02	Converted temps to degrees C            *
C************************************************************************
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	string
C------------------------------------------------------------------------
	IF ( mxormn .eq. 1 ) THEN
C
C*	    Get maximum temperature.         
C
	    CALL ST_INTG ( string, ist1, ier )
	    rivals(irmxtm) = FLOAT( ist1 )
	    IF ( rivals(irmnth) .gt. 3 .and. 
     +		    rivals(irmnth) .lt. 11 ) THEN
C
C*		Check for 100+ temperatures.
C
		IF ( ist1 .lt. 20 ) THEN
		    rivals(irmxtm) = FLOAT( ist1 ) + 100.
		END IF
	      ELSE
C
C*		Check for sub-zero temperatures.
C
		IF ( ist1 .ge. 90 ) THEN
		    rivals(irmxtm) = FLOAT( ist1 ) - 100.
		END IF
	    END IF
	    rivals(irmxtm) = PR_TMFC ( rivals(irmxtm) )
	  ELSE
C
C*	    Get minimum temperature.         
C
	    CALL ST_INTG ( string, ist1, ier )
	    rivals(irmitm) = FLOAT( ist1 )
	    IF ( rivals(irmnth) .lt. 5 .or. 
     +		        rivals(irmnth) .gt. 10 ) THEN
C
C*		Check for sub-zero temperatures.
C
		IF ( ist1 .gt. 70 .and. rivals(irtmpf) .lt. ist1 ) THEN
		    rivals(irmitm) = FLOAT( ist1 ) - 100.
		END IF
	    END IF
	    rivals(irmitm) = PR_TMFC ( rivals(irmitm) )
	END IF
C*
	RETURN
	END
