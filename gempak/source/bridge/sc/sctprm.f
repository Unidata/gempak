	SUBROUTINE SC_TPRM  ( sttprm, temprm, iret )
C************************************************************************
C* SC_TPRM							        *
C*								        *
C* This subroutine decodes temperatures of the form STTT, where 	*
C* S = sign ( 0 = positive; 1 = negative ), and TTT is the temperature	*
C* or dewpoint in tenths of degrees.					*
C*								        *
C* SC_TPRM  ( STTPRM, TEMPRM, IRET )					*
C*								        *
C* Input parameters:						        *
C*	STTPRM		CHAR*		Temperature string		*
C*								        *
C* Output parameters:						        *
C*	TEMPRM		REAL		Signed temperature value        *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return 	        *
C*					 31 = decode error 	        *
C**								        *
C* Log:									*
C* D. Kidwell/NCEP	 3/97	Based on MT_TPRM                        *
C* A. Hardy/GSC         12/97   Added sccmn.cmn for interface           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE         'sccmn.cmn'
C*
	CHARACTER*(*) 	sttprm
C*
C------------------------------------------------------------------------
	iret = 0
C
	CALL ST_CRNM ( sttprm ( 2:4 ), temprm, iret )
	IF ( iret .eq. 0 ) THEN
	    temprm = temprm * .1
	    IF ( sttprm ( 1:1 ) .eq. '1' ) temprm = -temprm
	  ELSE
	    iret = 31
	END IF
C*
	RETURN
	END
