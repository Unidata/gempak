	SUBROUTINE TI_ITOJ  ( idtarr, jyear, jday, iret )
C************************************************************************
C* TI_ITOJ								*
C*									*
C* This subroutine converts from a date array to the day of the year.	*
C*									*
C* TI_ITOJ  ( IDTARR, JYEAR, JDAY, IRET )				*
C*									*
C* Input parameters:							*
C*	IDTARR (3)	INTEGER		Date array (YYYY,MM,DD)		*
C*									*
C* Output parameters:							*
C*	JYEAR		INTEGER		Year				*
C*	JDAY		INTEGER		Day of the year			*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Durham/GSC	 9/98						*
C* D. Kidwell/NCEP	 4/99	Corrected prologue                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	INTEGER		idtarr (*)
C------------------------------------------------------------------------
	iret = 0
C
	iyear  = idtarr (1)
	imonth = idtarr (2)
	iday   = idtarr (3)
C
	jyear  = iyear
	jday   = iday
C
	IF ( imonth .gt. 1 ) THEN
	   DO i = 1, imonth-1
	      CALL TI_DAYM ( iyear, i, ndays, ier )
	      jday = jday + ndays
	   END DO
	END IF
C
	RETURN
	END
