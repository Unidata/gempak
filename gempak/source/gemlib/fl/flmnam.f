	SUBROUTINE FL_MNAM ( dattim, templt, filnam, iret )
C************************************************************************
C* FL_MNAM								*
C*									*
C* This routine constructs a file name from the template and a GEMPAK	*
C* date/time string.							*
C*									*
C* Valid substrings for the template include:				*
C*									*
C*	YYYY		Year with the century				*
C*	YY		Year without the century			*
C*	MMM		Month 3 letter abbreviation			*
C*	NNN		Month 3 letter abbreviation, all caps		*
C*	MM		Month number					*
C*	DD		Day						*
C*	HH		Hour						*
C*	NN		Minute						*
C*	DWK		Day of the week 3 letter abbreviation		*
C*	DWU		Day of the week 3 letter abbreviation, all caps	*
C*	fFFFFF		5-character representation of forecast hour/min	*
C*	fFFF		3-character representation of forecast hour	*
C*	fFF		2-character representation of forecast hour	*
C*									*
C* FL_MNAM ( DATTIM, TEMPLT, FILNAM, IRET )				*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		Full GEMPAK Date/time string	*
C*	TEMPLT		CHAR*		File name template		*
C*									*
C* Output parameters:							*
C*	FILNAM		CHAR*		File name			*
C*	IRET		INTEGER		Return code			*
C*					-1 - Invalid dattim or templt	*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 5/95						*
C* S. Jacobs/NCEP	 6/96	Changed to mixed case for month and day	*
C*				names; changed minutes to NN		*
C* G. Krueger/EAI	 8/96	Changed comments			*
C* D.W.Plummer/NCEP	 1/98	Added forecast hour processing		*
C* S. Jacobs/NCEP	 4/99	Added UC ver of month and day-of-week	*
C* M. Li/SAIC		02/02	Added check for the length of date/time	*
C* M. Li/SAIC		03/02	Modified the forecast time processing	*
C* S. Jacobs/NCEP	 8/14	Added support for FFFFF template	*
C************************************************************************
	CHARACTER*(*)	dattim, templt, filnam
C*
	INTEGER		idtarr ( 5 )
	CHARACTER	tstr*24, months(12)*3, dayw(7)*3,
     +			nonths(12)*3, wday(7)*3, fcst*8, tfc*8
C*
	DATA		months / 'Jan', 'Feb', 'Mar', 'Apr', 'May',
     +				 'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
     +				 'Nov', 'Dec' /
C*
	DATA		nonths / 'JAN', 'FEB', 'MAR', 'APR', 'MAY',
     +				 'JUN', 'JUL', 'AUG', 'SEP', 'OCT',
     +				 'NOV', 'DEC' /
C*
	DATA		dayw / 'Sun', 'Mon', 'Tue', 'Wed', 'Thu',
     +			       'Fri', 'Sat' /
C*
	DATA		wday / 'SUN', 'MON', 'TUE', 'WED', 'THU',
     +			       'FRI', 'SAT' /
C------------------------------------------------------------------------
	iret = 0
C
	filnam = templt
C
C*	Check dattim and templt strings for length
C
	CALL ST_LSTR ( dattim, ld, ier )
	CALL ST_LSTR ( templt, lt, ier )
	IF ( ld .eq. 0 .or. lt .eq. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Substitute the 4 digit year for YYYY.
C
	ipyy = INDEX ( filnam, 'YYYY' )
	IF  ( ipyy .ne. 0 )  THEN
	    tstr = dattim (1:2)
	    CALL ST_NUMB ( tstr, iyear, ier )
	    IF  ( iyear .le. 20 )  THEN
		filnam (ipyy:ipyy+1) = '20'
	      ELSE
		filnam (ipyy:ipyy+1) = '19'
	    END IF
	    filnam (ipyy+2:ipyy+3) = dattim (1:2)
	END IF
C
C*	Substitute the 2 digit year for YY.
C
	ipyy = INDEX ( filnam, 'YY' )
	IF  ( ipyy .ne. 0 )  filnam (ipyy:ipyy+1) = dattim (1:2)
C
C*	Substitute the 3 character month for MMM.
C
	ipmm = INDEX ( filnam, 'MMM' )
	IF  ( ipmm .ne. 0 )  THEN
	    tstr = dattim (3:4)
	    CALL ST_NUMB ( tstr, imonth, ier )
	    IF ( imonth .ge. 1 .and. imonth .le. 12 )
     +		filnam (ipmm:ipmm+2) = months ( imonth )
	END IF
C
C*	Substitute the 3 character month in caps for NNN.
C
	ipmm = INDEX ( filnam, 'NNN' )
	IF  ( ipmm .ne. 0 )  THEN
	    tstr = dattim (3:4)
	    CALL ST_NUMB ( tstr, imonth, ier )
	    IF ( imonth .ge. 1 .and. imonth .le. 12 )
     +		filnam (ipmm:ipmm+2) = nonths ( imonth )
	END IF
C
C*	Substitute the 2 digit month for MM.
C
	ipmm = INDEX ( filnam, 'MM' )
	IF  ( ipmm .ne. 0 )  filnam (ipmm:ipmm+1) = dattim (3:4)
C
C*	Substitute the 2 digit day for DD.
C
	ipdd = INDEX ( filnam, 'DD' )
	IF  ( ipdd .ne. 0 )  filnam (ipdd:ipdd+1) = dattim (5:6)
C
C*	Substitute the 2 digit hour for HH.
C
	iphh = INDEX ( filnam, 'HH' )
	IF  ( iphh .ne. 0 )  filnam (iphh:iphh+1) = dattim (8:9)
C
C*	Substitute the 2 digit minute for NN.
C
	ipmn = INDEX ( filnam, 'NN' )
	IF  ( ipmn .ne. 0 )  filnam (ipmn:ipmn+1) = dattim (10:11)
C
C*	Substitute the 3 character day of the week for DWK.
C
	ipdw = INDEX ( filnam, 'DWK' )
	IF  ( ipdw .ne. 0 )  THEN
	    CALL TI_CTOI ( dattim, idtarr, ier )
	    CALL TI_DAYW ( idtarr, idaywk, ier )
	    filnam (ipdw:ipdw+2) = dayw ( idaywk )
	END IF
C
C*	Substitute the 3 character day of the week in caps for DWU.
C
	ipdw = INDEX ( filnam, 'DWU' )
	IF  ( ipdw .ne. 0 )  THEN
	    CALL TI_CTOI ( dattim, idtarr, ier )
	    CALL TI_DAYW ( idtarr, idaywk, ier )
	    filnam (ipdw:ipdw+2) = wday ( idaywk )
	END IF

	IF (ld. gt. 11) THEN
C
C*	    Add '0' if not 3 or 5 letters
C 
	    fcst = dattim(13:)
	    CALL ST_LSTR(fcst, lf, ier)
	    IF ((lf .eq. 4) .or. (lf .eq. 2)) THEN
		tfc = '0' // fcst
	 	fcst = tfc
	    ELSE IF (lf .eq. 1) THEN
	    	tfc = '00' // fcst
	    	fcst = tfc
	    END IF
C
C*	    Substitute the 5 character forecast hour/min for fFFFFF.
C
	    ipfffff = INDEX ( filnam, 'FFFFF' )
	    IF  ( ipfffff .ne. 0 )  THEN
		CALL ST_LSTR(fcst, lf, ier)
		IF  ( lf .lt. 5 ) THEN
		    tfc = fcst(1:lf) // '00'
		    fcst = tfc
		END IF
     	        filnam (ipfffff:ipfffff+4) = fcst (1:5)
	    END IF
C
C*	    Substitute the 3 character forecast hour for fFFF.
C
	    ipfff = INDEX ( filnam, 'FFF' )
	    IF  ( ipfff .ne. 0 )  THEN
     	        filnam (ipfff:ipfff+2) = fcst (1:3)
	    END IF
C
C*	    Substitute the 2 character forecast hour for fFF.
C
	    ipff = INDEX ( filnam, 'FF' )
	    IF  ( ipff .ne. 0 )  THEN
     	        filnam (ipff:ipff+1) = fcst (2:3)
	    END IF
	END IF
C*
	RETURN
	END
