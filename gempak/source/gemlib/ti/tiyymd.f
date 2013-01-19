	SUBROUTINE TI_YYMD  ( iyymd, iyyymd, iret )
C************************************************************************
C* TI_YYMD								*
C*									*
C* This subroutine converts an integer 2-digit year, month and day to   *
C* an integer 4-digit year, month and day.  Any 2-digit year less than  *
C* or equal to 20 will be assumed to be in the 21st century; years      *
C* greater than 20 will be assumed to be in the 20th century.  If the   *
C* year is greater than 99, it is assumed to be a 4-digit year already. *
C*									*
C* TI_YYMD  ( IYYMD, IYYYMD, IRET )					*
C*									*
C* Input parameters:							*
C*	IYYMD		INTEGER	    2-digit year, month, day (YYMMDD)   *
C*									*
C* Output parameters:							*
C*	IYYYMD    	INTEGER     4-digit year, month, day (YYYYMMDD) *
C*	IRET		INTEGER     Return code				*
C*				      0 = normal return			*
C*				     -7 = invalid year			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/99               				*
C************************************************************************
	iret = 0
C
	iyy = iyymd / 10000
	CALL TI_YY24 ( iyy, iyyyy, iret ) 
	IF ( iret .eq. 0 ) THEN
	    iyyymd = iyyyy * 10000 + MOD ( iyymd, 10000 )
	  ELSE
	    iyyymd = iyymd
	END IF
C*
	RETURN
	END
