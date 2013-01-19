	SUBROUTINE TI_YY24  ( iyy, iyyyy, iret )
C************************************************************************
C* TI_YY24								*
C*									*
C* This subroutine converts a 2-digit year to a 4-digit year.           *
C* Any 2-digit year less than or equal to 20 will be assumed to be in   *
C* the 21st century; years greater than 20 will be assumed to be in the *
C* 20th century.  If the year is greater than 999, it is not changed.   *
C*									*
C* TI_YY24  ( IYY, IYYYY, IRET )					*
C*									*
C* Input parameters:							*
C*	IYY		INTEGER		2-digit year                    *
C*									*
C* Output parameters:							*
C*	IYYYY    	INTEGER  	4-digit year                    *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid year		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/99               				*
C* D. Kidwell/NCEP	 4/99 	Do not allow 3-digit years     		*
C************************************************************************
	iret = 0
	iyyyy = iyy
C
	IF ( iyy .gt. 999 ) THEN
	  ELSE IF ( iyy .lt. 0 ) THEN
	    iret  = -7
	  ELSE IF ( iyy .le. 20 ) THEN
	    iyyyy = 2000 + iyy
	  ELSE IF ( iyy .le. 99 ) THEN
	    iyyyy = 1900 + iyy
	  ELSE
	    iret = -7
	END IF
C*
	RETURN
	END
