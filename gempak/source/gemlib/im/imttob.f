	SUBROUTINE IM_TTOB  ( nvals, tmpk, brit, iret )
C************************************************************************
C* IM_TTOB								*
C*									*
C* This image processing function converts a real temperature (degrees	*
C* Kelvin) to an 8-bit brightness pixel temperature count (in a 	*
C* 4-byte real value).  Brightness counts are also called mode-A 	*
C* counts or Ca.  							*
C*									*
C* The following equation is used:					*
C*									*
C*	If  (TMPK <= 242)  BRIT = 418 - TMPK				*
C*	If  (TMPK >  242)  BRIT = 2 * (330 - TMPK)			*
C*									*
C* SUBROUTINE IM_TTOB  ( NVALS, TMPK, BRIT, IRET )			*
C*									*
C* Input parameters:							*
C*	NVALS		INTEGER		Number of values to convert	*
C*	TMPK		REAL		Temperature in Kelvin		*
C*									*
C* Output parameters:							*
C*	BRIT		REAL		Brightness (0-255)		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 4/96						*
C* D.W.Plummer/NCEP	 2/03	Convert from function to subroutine	*
C* T. Piper/SAIC	07/06	Set iret to zero			*
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
	REAL		tmpk(*), brit(*)
C------------------------------------------------------------------------
	iret = 0
	DO  ii = 1, nvals
C
	    IF ( (tmpk(ii) .lt. 163.0) .or. (tmpk(ii) .gt. 330.0)) THEN
	        brit(ii) = RMISSD
	    ELSE IF ( tmpk(ii) .le. 242.0 ) THEN
	        brit(ii) = 418.0 - tmpk(ii)
	    ELSE
	        brit(ii) = 2 * (330.0 - tmpk(ii))
	    END IF
C
	END DO
C*
	RETURN
	END
