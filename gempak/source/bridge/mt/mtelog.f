	SUBROUTINE MT_ELOG ( string, ierrno, iret )
C************************************************************************
C* MT_ELOG                                                              *
C*                                                                      *
C* This subroutine writes an error message to the decoder log if a      *
C* miscoded field is found.                                             *
C*									*
C* MT_ELOG ( STRING, IERRNO, IRET )					*
C*								        *
C* Input parameters:						        *
C*	STRING		CHAR*	    Miscoded field                      *
C*	IERRNO		INTEGER	    Error message number		*
C*									*
C* Output parameters:	 					        *
C*	IRET		INTEGER	    Return code                         *
C*				      0 = normal return		        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 4/98                                           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
C------------------------------------------------------------------------
	iret = 0
C
	lens = INDEX ( string, ' ' ) - 1
	IF ( lens .lt. 0 ) lens = 40
	CALL DC_WLOG ( 2, 'MT', ierrno, string ( :lens ) , ier )
C*
	RETURN
	END
