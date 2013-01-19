	SUBROUTINE TI_FORM  ( timein, dattim, iret )
C************************************************************************
C* TI_FORM								*
C*									*
C* This subroutine moves the characters in a partial input time to	*
C* the correct positions for a standard GEMPAK time which is in the	*
C* form:  YYMMDD/HHMM.  If TIMEIN is "LAST", then DATTIM will also	*
C* be set to "LAST".							*
C*									*
C* TI_FORM  ( TIMEIN, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	TIMEIN		CHAR*		Input time			*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		Time in GEMPAK format		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid input time	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/84						*
C* M. desJardins/GSFC	11/87	Cleaned up				*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* K. Tyle/GSC		12/95	Added temp; fixed date manipulation	*
C* S. Jacobs/NCEP	 2/01	Allow timein to have the century	*
C* S. Jacobs/NCEP	 2/01	Fixed check on string length, lenb	*
C************************************************************************
	CHARACTER*(*)	timein, dattim
C*
	CHARACTER	date*8, time*8, newtim*20, ttt*8, temp*6
	LOGICAL  	badstr
C------------------------------------------------------------------------
	iret   = 0
	dattim = ' '
C
C*	Check for 'LAST'.
C
	CALL ST_LCUC  ( timein, newtim, iret )
	IF  ( newtim .eq. 'LAST' )  THEN
	    dattim = newtim
	    RETURN
	ENDIF
C
C*	Break string into two parts.  Return error if length is 0.
C
	CALL ST_LSTR  ( newtim, length, ier )
	IF  ( length .eq. 0 )  THEN
	    iret = -1
	    RETURN
	ENDIF
C
C*	Initialize counters for parts of string
C
	date   = ' '
	time   = ' '
C
C*	Search for / to find two parts of string.
C
	idash = INDEX  ( timein, '/' )
	IF  ( idash .eq. 0 )  THEN
	    time  = timein
	    lena  = length
	    date  = ' '
	    lenb  = 0
	  ELSE IF  ( idash .eq. 1 )  THEN
	    time  = timein ( 2 : )
	    lena  = length - 1
	    date  = ' '
	    lenb  = 0
	  ELSE IF  ( idash .eq. length )  THEN
	    time  = ' '
	    lena  = 0
	    date  = timein ( 1: idash - 1 )
	    lenb  = length - 1
	  ELSE
	    time  = timein ( idash + 1 : length )
	    lena  = length - idash
	    date  = timein ( 1 : idash - 1 )
	    lenb  = idash - 1
	END IF
C
C*	Check for valid numbers.
C
	badstr = .false.
	DO  i = 1, lena
	    IF  ( ( time (i:i) .lt. '0' ) .or. 
     +		  ( time (i:i) .gt. '9' ) )  badstr = .true.
	END DO
	DO  i = 1, lenb
	    IF  ( ( date (i:i) .lt. '0' ) .or. 
     +		  ( date (i:i) .gt. '9' ) )  badstr = .true.
	END DO
	IF  ( badstr )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Check for valid lengths.
C
	IF  ( ( ( lenb .gt. 6 ) .and. ( lenb .ne. 8 ) ) .or.
     +	      ( lena .gt. 8 ) )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*      Get the actual length of the string
C
	CALL ST_LSTR  ( date, lenb, ier )
C
C*	Move before time into the correct position.
C
	IF  ( lenb .eq. 8 )  THEN
	    temp = ' '
	    temp = date (3:8)
	    date = temp
            lenb = 6
	ENDIF
C
C*	Move before time into the correct position.
C
	IF  ( ( lenb .ne. 0 ) .and. ( lenb .ne. 6 ) )  THEN
	    temp = ' '
	    temp ( ( 6 - lenb + 1 ) : 6 ) = date ( 1 : lenb )
	    date = temp
	ENDIF
C
C*	Make sure numbers are paired.
C
	IF  ( ( lenb .eq. 1 ) .or. ( lenb .eq. 3 ) .or. 
     +	      ( lenb .eq. 5 ) )  THEN
	    date ( 6 - lenb : 6 - lenb ) = '0'
	END IF
C*
	IF ( ( lena .eq. 1 ) .or. ( lena .eq. 3 ) .or.
     +	     ( lena .eq. 5 ) .or. ( lena .eq. 7 ) )  THEN
	    ttt  = time
	    time = '0' // ttt
	END IF
C
C*	Put the date and time together.
C
	dattim  =  date(1:6)  //  '/'  //  time
C*	
	RETURN
	END
