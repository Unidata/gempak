	SUBROUTINE IS_CHNG ( report, lenr, ichng, iptr, iret )
C************************************************************************
C* IS_CHNG 								*
C*									*
C* This subroutine decodes a change in intensity from an international  *
C* sigmet report.                                                       *
C*                                                                      *
C* IS_CHNG ( REPORT, LENR, ICHNG, IPTR, IRET )                          *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*									*
C* Output parameters:							*
C*	ICHNG 		INTEGER 	Change in intensity indicator   *
C*	IPTR		INTEGER		Pointer to location after change*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = intensity change not found*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* F. J. Yen/NCEP	10/03	Added additional keywords for Canada.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
C------------------------------------------------------------------------
	iret  = 0
	ichng = IMISSD
	iptr  = 0
C
C*	Look for the key word INTSF(YG), WKN(G), NC, NO CHG, NO CHANGE,
C*	or LTL CHANGE.
C
	len = MIN ( 40, lenr ) 
	iloc = INDEX ( report ( :len ), ' INTSF' )
	IF ( iloc .gt. 0 ) THEN
	    ichng = 1
	    iadd  = 6
	    IF ( report (iloc+1:iloc+2) .eq. 'YG' ) iadd = 8
	  ELSE
	    iloc = INDEX ( report ( :len ), ' WKN' )
	    IF ( iloc .gt. 0 ) THEN
		ichng = -1
		iadd  = 4
	    	IF ( report (iloc+1:iloc+1) .eq. 'G' ) iadd = 5
	      ELSE
		iloc = INDEX ( report ( :len ), ' NC' )
		IF ( iloc .gt. 0 ) THEN
		    ichng = 0
		    iadd  = 3
		  ELSE
		    iloc = INDEX ( report ( :len ), ' NO CHG' )
		    IF ( iloc .gt. 0 ) THEN
			ichng = 0
			iadd = 7
		      ELSE
			iloc = INDEX ( report ( :len ), ' NO CHANGE' )
			IF ( iloc .gt. 0 ) THEN
			    ichng = 0
			    iadd = 10
			  ELSE
			    iloc = INDEX ( report ( :len ), ' LTL CHG')
			    IF ( iloc .gt. 0 ) THEN
			        ichng = -2
				iadd = 8
			    END IF
			END IF
		    END IF
		END IF
	      
	    END IF
	END IF
C
	IF ( iloc .gt. 0 ) THEN
	    iptr = iloc + iadd
	  ELSE
	    iret = -8
	END IF
C*
	RETURN
	END
