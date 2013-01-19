	SUBROUTINE TI_ITOC  ( idtarr, dattim, iret )
C************************************************************************
C* TI_ITOC								*
C*									*
C* This subroutine converts an integer time array into a standard	*
C* GEMPAK time.  The integers are checked for validity.			*
C*									*
C* TI_ITOC  ( IDTARR, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	IDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK time			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid year		*
C*					 -8 = invalid month		*
C*					 -9 = invalid day		*
C*					-10 = invalid hour		*
C*					-11 = invalid minute		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/87	GEMPAK 4				*
C* T. Lee/GSC		 2/98	Fixed leap year problem			*
C* I. Durham/GSC	 9/98   Cleaned up				*
C************************************************************************
	CHARACTER*(*)	dattim
	INTEGER		idtarr (*)
C*
	CHARACTER	date*6, time*4
C------------------------------------------------------------------------
	iret   = 0
	dattim = ' '
C
C*	Put array values into variables.
C
	iyear  = idtarr (1)
	imonth = idtarr (2)
	iday   = idtarr (3)
	ihour  = idtarr (4)
	iminut = idtarr (5)
C
C*	Check for leap year.
C
	CALL TI_DAYM ( iyear, imonth, ndays, ier )
	iyear = MOD ( iyear, 100 )
C
C*	Check that each of these values is valid.
C
	IF  ( iyear .lt. 0 )  iret = -7
	IF  ( ( imonth .lt. 1 ) .or. ( imonth .gt. 12 ) ) iret = -8
	IF  ( ( iday   .lt. 1 ) .or. ( iday   .gt. ndays ) ) 
     +							  iret = -9
	IF  ( ( ihour  .lt. 0 ) .or. ( ihour  .gt. 24 ) ) iret = -10
	IF  ( ( iminut .lt. 0 ) .or. ( iminut .gt. 60 ) ) iret = -11
	IF  ( iret .ne. 0 )  RETURN
C
C*	Get the date and time.
C
	idate = iyear * 10000 + imonth * 100 + iday
	itime = ihour * 100   + iminut
C
C*	Convert date and time to character strings.
C
	WRITE  ( date, 50, IOSTAT=ier )  idate
 50	FORMAT ( I6 )
	WRITE  ( time, 60, IOSTAT=ierr)  itime
 60	FORMAT ( I4 )
C
C*	Fill in blanks with zeroes.
C
	DO  i = 1, 3
	    IF  ( time (i:i) .eq. ' ' )  time (i:i) = '0'
	    IF  ( date (i:i) .eq. ' ' )  date (i:i) = '0'
	END DO
C
C*	Check for errors and concatenate date and time.
C
	IF  ( ( ier .ne. 0 ) .or. ( ierr .ne. 0 ) )  THEN
	    iret = -1
	  ELSE
	    dattim = date // '/' // time
	END IF
C*
	RETURN
	END
