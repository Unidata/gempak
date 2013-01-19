	SUBROUTINE IM_BTOT  ( nvals, brit, tmpk, iret )
C************************************************************************
C* IM_BTOT								*
C*									*
C* This image processing function converts an 8-bit brightness pixel	*
C* temperature count (in a 4-byte integer word) to a real temperature	*
C* (degrees K).	Brightness counts are also called mode-A counts or Ca.	*
C*									*
C* The following equation is used:					*
C*									*
C*	IF  (BRIT >= 176)  TMPK = 418 - BRIT				*
C*	IF  (BRIT <  176)  TMPK = 330 - (BRIT/2)			*
C*									*
C* SUBROUTINE IM_BTOT  ( NVALS, BRIT, TMPK, IRET )			*
C*									*
C* Input parameters:							*
C*	NVALS		INTEGER		Number of values to convert	*
C*	BRIT		INTEGER		Brightness (0-255)		*
C*									*
C* Output parameters:							*
C*	TMPK		REAL		Temperature in Kelvin		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 4/96						*
C* T. Lee/GSC		 9/99	Changed input to integer		*
C* D.W.Plummer/NCEP	 2/03	Change from function to subroutine	*
C* T. Piper/SAIC	07/06	Set iret to zero			*
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
	INTEGER		brit(*)
	REAL		tmpk(*)
C------------------------------------------------------------------------
	iret = 0
	DO  ii = 1, nvals
C
	    IF  ( ( brit(ii) .lt. 0 ) .or. ( brit(ii) .gt. 255 ) )  THEN
	        tmpk(ii) = RMISSD
	    ELSE IF ( brit(ii) .ge. 176 ) THEN
	        tmpk(ii) = 418.0 - brit(ii)
	    ELSE
	        tmpk(ii) = 330.0 - ( brit(ii) /2.0 )
	    END IF
C
	END DO
C*
	RETURN
	END
