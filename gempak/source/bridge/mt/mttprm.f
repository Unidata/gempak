	SUBROUTINE MT_TPRM ( sttprm, temprm, iret )
C************************************************************************
C* MT_TPRM							        *
C*								        *
C* This subroutine will decode temperatures of the form STTT, where	*
C* S = sign ( 0 = positive; 1 = negative ), and TTT is the temperature	*
C* or dewpoint in tenths of degrees Celsius.				*
C*								        *
C* MT_TPRM ( STTPRM, TEMPRM, IRET )					*
C*								        *
C* Input parameters:						        *
C*	STTPRM		CHAR*		Temperature string		*
C*								        *
C* Output parameters:						        *
C*	TEMPRM		REAL		Signed temperature value        *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return 	        *
C*					 -2 = decode error 	        *
C**								        *
C* Log:									*
C* D. Kidwell/NCEP	11/95	                                        *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* D. Kidwell/NCEP       6/97	Replaced ST_CRNM with ST_INTG           *
C* D. Kidwell/NCEP       4/98	Changed error handling                  *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	sttprm
C------------------------------------------------------------------------
	iret = 0
C
	CALL ST_INTG ( sttprm ( 2:4 ), itmprm, iret )
	IF ( iret .eq. 0 ) THEN
	    temprm =  FLOAT ( itmprm ) * .1
	    IF ( sttprm ( 1:1 ) .eq. '1' ) temprm = -temprm
	  ELSE
	    temprm = RMISSD
	END IF
C*
	RETURN
	END
