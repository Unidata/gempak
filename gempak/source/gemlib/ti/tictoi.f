	SUBROUTINE TI_CTOI  ( dattim, idtarr, iret )
C************************************************************************
C* TI_CTOI								*
C*									*
C* This subroutine converts a standard GEMPAK time into an integer	*
C* array containing year, month, day, hour, and minute.  The input	*
C* string must be a complete GEMPAK date/time.  The integers are	*
C* checked for validity.  If YY is less than or equal to 20, 2000 is    *
C* added for the year; otherwise, 1900 is added.			*
C*									*
C* TI_CTOI  ( DATTIM, IDTARR, IRET )					*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		GEMPAK time			*
C*									*
C* Output parameters:							*
C*	IDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid time		*
C*					 -7 = invalid year		*
C*					 -8 = invalid month		*
C*					 -9 = invalid day		*
C*					-10 = invalid hour		*
C*					-11 = invalid minute		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/87	GEMPAK 4				*
C* M. desJardins/GSFC	 9/88	Fixed length of Feb in leap year	*
C* I. Durham/GSC	 9/98   Cleaned up				*
C* D. Kidwell/NCEP	 4/99   Changed to call TI_YY24; fixed prologue *
C* A. Hardy/GSC		 7/01   Added else check for dattim string	*
C************************************************************************
	CHARACTER*(*)	dattim
	INTEGER		idtarr (*)
C------------------------------------------------------------------------
	iret = 0
	DO  i = 1, 5
	    idtarr ( i ) = 0
	END DO
C
C*	Get length of input string.
C
	CALL ST_LSTR  ( dattim, length, ier )
C
	islash = INDEX ( dattim, '/' )
C
C*	Check for a valid dattim string.
C
	IF ( ( length .ge. 11 ) .and. ( islash .eq. 7 ) ) THEN
	   CALL ST_NUMB ( dattim (1:6),  idate, ierd )
	   CALL ST_NUMB ( dattim (8:11), itime, iert )
	ELSE IF ( ( length .ge. 13 ) .and. ( islash .eq. 9 ) ) THEN
	   CALL ST_NUMB ( dattim (1:8),   idate, ierd )
	   CALL ST_NUMB ( dattim (10:13), itime, iert )
	ELSE 
           ierd = -1
           iert = -1
	END IF
C
C*	Check for error in decode.
C
	IF  ( ( ierd .ne. 0 ) .or. ( iert .ne. 0 ) )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Check for negative values of date or time.
C
	IF  ( idate .lt. 0 )  THEN
	    iret = -7
	    RETURN
	END IF
	IF  ( itime .lt. 0 )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Break date and time into year, month, day, hour and minute.
C
	iyear  = idate / 10000
	imonth = idate - iyear * 10000
	imonth = imonth / 100
	iday   = MOD ( idate, 100 )
	ihour  = itime / 100
	iminut = MOD ( itime, 100 )
C
C*	Change February in leap year.
C
	CALL TI_DAYM ( iyear, imonth, ndays, ier )
C
C*	Check that each of these values is valid.
C
	CALL TI_YY24 ( iyear, iyear, iret )
C 
	IF  ( ( imonth .lt. 1 ) .or. ( imonth .gt. 12 ) ) iret = -8
	IF  ( ( iday   .lt. 1 ) .or. ( iday   .gt. ndays ) )
     +                                                    iret = -9
	IF  ( ( ihour  .lt. 0 ) .or. ( ihour  .gt. 24 ) ) iret = -10
	IF  ( ( iminut .lt. 0 ) .or. ( iminut .gt. 60 ) ) iret = -11
C
C*	Return values in array.
C
	IF  ( iret .eq. 0 )  THEN
	    idtarr (1) = iyear
	    idtarr (2) = imonth
	    idtarr (3) = iday
	    idtarr (4) = ihour
	    idtarr (5) = iminut
	END IF
C*
	RETURN
	END
