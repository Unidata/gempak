	SUBROUTINE RA_GFLD  ( report, lenr, iret )
C************************************************************************
C* RA_GFLD								*
C*									*
C* This subroutine divides a surface airways report into individual	*
C* fields for decoding.  Fields must be separated by blanks or		*
C* slashes.  Numbers and non-numeric strings are stored in separate	*
C* fields.  A slash is considered a separate field.  Unprintable	*
C* characters must be replaced by blanks before this subroutine is	*
C* called.  The fields are stored in / RACMN /.				*
C*									*
C* RA_GFLD  ( REPORT, LENR, IRET )					*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		AIRWAYS report			*
C*	LENR		INTEGER		Length of report		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid report		*
C**									*
C* Log:									*
C* B. Doty		11/87						*
C* M. desJardins/GSFC	 8/89						*
C************************************************************************
	INCLUDE		'racmn.cmn'
C*
	CHARACTER*(*)	report
C*
	CHARACTER	cc*1
C------------------------------------------------------------------------
	iret   = 0
C
C*	The first field is the station id.
C
	is = 0
	ie = 0
	i  = 1
	DO WHILE  ( ( ie .eq. 0 ) .and. ( i .le. lenr ) )
	    IF  ( ( report (i:i) .eq. ' ' ) .and. ( is .ne. 0 ) )  THEN
		ie = i - 1
	      ELSE IF  ( ( report (i:i) .ne. ' ' ) .and. ( is .eq. 0 ) )
     +								THEN
		is = i
	    END IF
	    i = i + 1
	END DO
C*
	IF  ( ie .eq. 0 )  THEN
	    iret   = -1
	    nfield = 0
	    RETURN
	  ELSE
	    nfield = 1
	    ifstrt ( nfield ) = is
	    ifendp ( nfield ) = ie
	    ifsize ( nfield ) = ie - is + 1
	    iftype ( nfield ) = 1
	END IF
C
C*	Loop through report checking type of character. 
C*	    ITYPE = 0    no type
C*	    ITYPE = 1    non-numeric
C*	    ITYPE = 2    numeric
C
	itype = 0
	DO  i = ie + 1, lenr
	    cc = report (i:i)
C
C*	    Check for a number.
C
	    IF  ( ( cc .ge. '0' ) .and. ( cc .le. '9' ) )  THEN
C
C*		Check against current type.
C
		IF  ( itype .eq. 1 )  THEN
		    ifendp ( nfield ) = i - 1
		    ifsize ( nfield ) = i - ifstrt ( nfield )
		  ELSE IF  ( itype .eq. 4 )  THEN
		    itype = 2
		    iftype ( nfield ) = 2
		END IF
		IF  ( itype .ne. 2 )  THEN
		    IF  ( nfield .lt. IFLDMX )  THEN
			nfield = nfield + 1
			ifstrt ( nfield ) = i
			iftype ( nfield ) = 2
			itype = 2
		    END IF
		END IF
C
C*		Check for slash ( / ) .
C
	      ELSE IF  ( cc .eq. '/' )  THEN
C
C*		End other type and make new type for slash alone.
C
		IF  ( itype .ne. 0 )  THEN
		    ifendp ( nfield ) = i - 1
		    ifsize ( nfield ) = i - ifstrt ( nfield )
		END IF
		IF  ( nfield .lt. IFLDMX )  THEN
		    nfield = nfield + 1
		    ifstrt ( nfield ) = i
		    ifendp ( nfield ) = i
		    ifsize ( nfield ) = 1
		    iftype ( nfield ) = 3
		    itype = 0
		END IF
C
C*		Check for a blank.  Finish current field.
C
	      ELSE IF  ( cc .eq. ' ' )  THEN
		IF  ( ( itype .ne. 0 ) .and. ( itype .ne. 4 ) )  THEN
		    ifendp ( nfield ) = i - 1
		    ifsize ( nfield ) = i - ifstrt ( nfield )
		END IF
		IF  ( itype .ne. 4 )  itype = 0
C
C*		Check for a dash.  Leave unless it is the start of
C*		a number.
C
	      ELSE IF  ( cc .eq. '-' )  THEN
C
C*		Set type to 4 until we find out if this is followed by
C*		a number or a letter.
C
		IF  ( itype .eq. 2 )  THEN
		    ifendp ( nfield ) = i - 1
		    ifsize ( nfield ) = i - ifstrt ( nfield )
		    itype = 0
		  ELSE IF  ( itype .eq. 4 )  THEN
		    nfield = nfield - 1
		    itype  = 0
		END IF
		IF  ( ( itype .eq. 0 ) .and. ( nfield .lt. IFLDMX ) )
     +								THEN
		    nfield = nfield + 1
		    ifstrt ( nfield ) = i
		    itype = 4
		END IF
C
C*		Otherwise, save report in character type.
C
	      ELSE IF  ( itype .ne. 1 )  THEN
		IF  ( itype .eq. 2 )  THEN
		    ifendp ( nfield ) = i - 1
		    ifsize ( nfield ) = i - ifstrt ( nfield )
		  ELSE IF  ( itype .eq. 4 )  THEN
		    itype = 1
		    iftype ( nfield ) = 1
		END IF
		IF  ( ( nfield .lt. IFLDMX ) .and. 
     +		      ( itype .ne. 1 ) )  THEN
		    nfield = nfield + 1
		    ifstrt ( nfield ) = i
		    iftype ( nfield ) = 1
		    itype = 1
		END IF
	    END IF
	END DO
C
C*	Check whether last field is finished.
C
	IF  ( itype .ne. 0 )  THEN
	    ifendp ( nfield ) = lenr
	    ifsize ( nfield ) = lenr - ifstrt ( nfield ) + 1
	END IF
C
C*	After loop, convert all numbers to integers and get
C*	character fields.
C
	DO  i = 1, nfield
	    CALL ST_RMBL  ( report ( ifstrt (i) : ifendp (i) ), 
     +			    cfield (i), ifsize (i), ier )
	    IF  ( iftype ( i ) .eq. 2 )  THEN
		CALL ST_NUMB ( cfield (i), ifintg (i), ier )
	      ELSE
		ifintg (i) = 0
	    END IF
	END DO
C*
	RETURN
	END
