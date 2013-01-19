	SUBROUTINE TI_DAYW  ( idtarr, idayw, iret )
C************************************************************************
C* TI_DAYW								*
C*									*
C* This subroutine returns the day of the week, IDAYW, given an integer	*
C* time.  IDAYW is set to 1 for Sunday, 2 for Monday, 3 for Tuesday,	*
C* 4 for Wednesday, 5 for Thursday, 6 for Friday, and 7 for Saturday.	*
C*									*
C* This subroutine does not check that a valid time was entered.  The 	*
C* year must be a full four-digit year.					*
C*									*
C* TI_DAYW  ( IDTARR, IDAYW, IRET )					*
C*									*
C* Input parameters:							*
C*	IDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C*									*
C* Output parameters:							*
C*	IDAYW		INTEGER		Day of week (1 - Sun,...)	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/87	Adapted from DAYOFWEEK in TRAFIC	*
C* M. desJardins/GSFC	 9/88	Add 1900 to YY				*
C* I. Durham/GSC	 9/98   Added March leap year correction	*
C* I. Durham/GSC	 9/98	Rewrote using new computation		*
C* S. Jacobs/NCEP	11/99	Simplified calculation			*
C************************************************************************
	INTEGER		idtarr (*)
C*
	INTEGER		jdtarr (5)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the number of days for the first of the given month.
C
	DO  i = 1, 5
	    jdtarr (i) = idtarr (i)
	END DO
	jdtarr (3) = 1
	CALL TI_ITOJ ( jdtarr, jyear, jday, ier )
C
C*	Compute the offset for this month.
C
	ioff  = MOD ( jday-1, 7 )
C
C*	Compute the day of the week.
C
	iy    = idtarr (1)
	id    = idtarr (3)
	idayw = MOD ( ( iy + (iy-1)/4 - (iy-1)/100 + (iy-1)/400 +
     +		        ioff + (id-1) ), 7 ) + 1
C
	RETURN
	END
