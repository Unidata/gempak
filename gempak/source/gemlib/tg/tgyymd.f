	FUNCTION TG_YYMD  ( iyymd )
C************************************************************************
C* TG_YYMD								*
C*									*
C* This function converts an integer 2-digit year, month and day to     *
C* an integer 4-digit year, month and day.  Any 2-digit year less than  *
C* or equal to 20 will be assumed to be in the 21st century; years      *
C* greater than 20 will be assumed to be in the 20th century.  If the   *
C* year is greater than 99, it is assumed to be a 4-digit year already. *
C*									*
C* INTEGER TG_YYMD  ( IYYMD )						*
C*									*
C* Input parameters:							*
C*	IYYMD		INTEGER	    2-digit year, month, day (YYMMDD)   *
C*									*
C* Output parameters:							*
C*	TG_YYMD    	INTEGER     4-digit year, month, day (YYYYMMDD) *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/99               				*
C************************************************************************
	INTEGER		TG_YYMD
C------------------------------------------------------------------------
	CALL TI_YYMD ( iyymd, iyyymd, iret ) 
	TG_YYMD = iyyymd
C*
	RETURN
	END
