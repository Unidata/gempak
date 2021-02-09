	SUBROUTINE TI_DST  ( idtarr, tzone, dst, iret )
C************************************************************************
C* TI_DST								*
C*									*
C* This subroutine determines whether Daylight Saving Time is in effect *
C* at the given time (assumed to be in UTC) in the specified CONUS time *
C* zone.						                *
C*                                                                      *
C* (Note that this does not take into account local DST exemptions; for *
C* example, this routine may indicate that DST is in effect for the 	*
C* Mountain Time zone, but Arizona remains on Mountain Standard Time	*
C* [MST] all year round.)						*
C*									*
C* Currently, DST starts on the second Sunday in March and ends on the	*
C* first Sunday in November.						*
C*									*
C* TI_DST  ( IDTARR, DST, IRET )					*
C*									*
C* Input parameters:							*
C*	IDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C*      TZONE           CHARACTER       Time zone ("E","C","M", or "P") *
C*									*
C* Output parameters:							*
C*	DST		LOGICAL		Daylight Saving Time flag	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-13 = input year before 1986	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	11/99	Created					*
C* T. Piper/SAIC	08/06	Added support for new DST beginning 2007*
C* L. Oolman/Univ of WY	 3/09	Fixed start of DST when 1 Mar is a Sun	*
C* S. Guan		07/20	Added tzone as an input			*
C************************************************************************
        INTEGER         idtarr(*)
        LOGICAL         dst
        PARAMETER       ( NZONES = 4 )
        CHARACTER       tzones (NZONES)*1, tzone*4
        INTEGER         ihrdif (NZONES)
C*
	INTEGER		jdtarr(5)
	CHARACTER	curdt*20, strdt*20, enddt*20, syszon*4
C
C*      The list of time zones should be ordered such that the later
C*      items are earlier than GMT.
C
        DATA            tzones / 'E', 'C', 'M', 'P' /
        DATA            ihrdif /  0 ,  1 ,  2 ,  3  /

C------------------------------------------------------------------------

C
C*      Use the local time zone given as input to this subroutine
C*      ("E", "C", "M", or "P").
C
        CALL ST_LCUC ( tzone, tzone, ier )
        CALL ST_LSTR ( tzone, len, ier )
        CALL ST_FIND ( tzone(:len), tzones, NZONES, ipos1, ier1 )
C
C*      In case of errors or invalid input time zones, assume
C*      Eastern Time Zone.  This preserves legacy behavior and
C*      limits to a few hours per year those times when an
C*      incorrect DST answer would be given.
C
        IF  ( ( ier1 .ne. 0 ) .or. ( ipos1 .eq. 0 ) ) THEN
            ipos1 = 1
        END IF
C
C*  Convert the given date array to a date/time string.
C
	CALL TI_ITOC ( idtarr, curdt, ier )
	CALL TI_DTM4 ( curdt, enddt, ier )
C
C*  Compute the start date for Daylight Saving Time.
C
	IF ( enddt(1:4) .lt. "1986" )  THEN
	    iret = -13
	    RETURN
	ELSE IF ( enddt(1:4) .ge. "1986" .and.
     +		  enddt(1:4) .le. "2006" )  THEN
C*  Get the day of the week for 4/1 of the given year.
	    jdtarr(1) = idtarr(1)
	    jdtarr(2) = 4
	    jdtarr(3) = 1
	    jdtarr(4) = 7 + ihrdif(ipos1)
	    jdtarr(5) = 0
	    CALL TI_DAYW ( jdtarr, iapr1, ier )
C*  Compute the date of the first Sunday in April.
	    jdtarr(3) = MOD ( ( 1 - iapr1 + 7 ), 7 ) + 1
	ELSE
C*  Get the day of the week for 3/1 of the given year.
	    jdtarr(1) = idtarr(1)
	    jdtarr(2) = 3
	    jdtarr(3) = 1
	    jdtarr(4) = 7 + ihrdif(ipos1)
	    jdtarr(5) = 0
	    CALL TI_DAYW ( jdtarr, imar1, ier )
C*  Compute the date of the second Sunday in March.
	    jdtarr(3) = MOD ( ( 1 - imar1 + 7 ), 7 ) + 1 + 7
	END IF
C
C*  Convert the date array of the start to a date/time string.
C
	CALL TI_ITOC ( jdtarr, strdt, ier )
C
C*  Compute the end date for Daylight Saving Time.
C
	IF ( enddt(1:4) .ge. "1986"  .and.
     +	     enddt(1:4) .le. "2006" )  THEN
C*  Get the day of the week for 10/1 of the given year.
	    jdtarr (1) = idtarr (1)
	    jdtarr (2) = 10
	    jdtarr (3) = 1
	    jdtarr (4) = 6 + ihrdif(ipos1)
	    jdtarr (5) = 0
	    CALL TI_DAYW ( jdtarr, ioct1, ier )
	    CALL TI_DAYM ( jdtarr(1), jdtarr(2), nday, ier )
C*  Compute the date of the last Sunday in October.
	    ifrst = MOD ( ( 1 - ioct1 + 7 ), 7 ) + 1
	    IF  ( ifrst+28 .le. nday )  THEN
		jdtarr(3) = ifrst + 28
	    ELSE
		jdtarr(3) = ifrst + 21
	    END IF
	ELSE
C*      Get the day of the week for 11/1 of the given year.
	    jdtarr (1) = idtarr (1)
	    jdtarr (2) = 11
	    jdtarr (3) = 1
	    jdtarr (4) = 6 + ihrdif(ipos1)
	    jdtarr (5) = 0
	    CALL TI_DAYW ( jdtarr, inov1, ier )
	    CALL TI_DAYM ( jdtarr(1), jdtarr(2), nday, ier )
C*	Compute the date of the first Sunday in November.
	    jdtarr(3) = MOD ( ( 1 - inov1 + 7 ), 7 ) + 1
	END IF
C
C*	Convert the date array of the first Sunday in November to a
C*	date/time string.
C
	CALL TI_ITOC ( jdtarr, enddt, ier )
C
C*	Check if the given date is between the start and end dates
C*	for Daylight Saving Time.
C
	dst = ( ( strdt .le. curdt ) .and. ( curdt .lt. enddt ) )
	iret = 0
C*
	RETURN
	END
