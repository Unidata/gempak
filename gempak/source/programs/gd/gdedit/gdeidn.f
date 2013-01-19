	SUBROUTINE GDEIDN  ( rec, time, level, ivcord, parm, iret )
C************************************************************************
C* GDEIDN								*
C*									*
C* This subroutine extracts the grid identifier.			*
C*									*
C* GDEIDN  ( REC, TIME, LEVEL, IVCORD, PARM, IRET )			*
C*									*
C* Input parameters:							*
C*	REC		CHAR*		Data record			*
C*									*
C* Output parameters:							*
C*	TIME  (2)	CHAR*		Grid date/time			*
C*	LEVEL (2)	INTEGER		Grid level			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	PARM		CHAR*		Grid parameter name		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -3 = invalid identifier	*
C*									*
C* Log:									*
C* M. Goodman/RDS	11/85						*
C* I. Graffman/RDS	 8/86	Restructure				*
C* M. desJardins/GSFC	 9/88	Rewrote for GEMPAK4			*
C* M. desJardins/GSFC	 4/89	Replaced TI subs with TG subs		*
C* M. desJardins/GSFC	 7/89	Corrected call to TG_CTOI		*
C************************************************************************
	CHARACTER*(*)	rec, time (2), parm
	INTEGER		level (2)
C*
	CHARACTER	string (8)*20, vparm*4
	INTEGER		itime (3)
C------------------------------------------------------------------------
	iret = 0
	istr = 1
C
C*	Remove extra blanks and break the input record into substrings.
C
	CALL ST_RXBL  ( rec, rec, length, ier )
	CALL ST_CLST  ( rec, ' ', ' ', 6, string, nstr, ier )
	IF  ( nstr .lt. 4 )  THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Check that the first string is a valid time.
C
	islash = INDEX  ( string (1), '/' )
	IF  ( islash .eq. 0 )  THEN
	    iret = -3
	    RETURN
	  ELSE
	    CALL TG_CTOI  ( string (1), itime, iret )
	    IF  ( iret .ne. 0 )  THEN
		iret = -3
		RETURN
	      ELSE
		time (1) = string (1)
	    END IF
	END IF
C
C*	Check for a second time.
C
	islash = INDEX  ( string (2), '/' )
	IF  ( islash .eq. 0 )  THEN
	    time (2) = ' '
	    next     = 2
	  ELSE
	    CALL TG_CTOI  ( string (2), itime, iret )
	    IF  ( iret .ne. 0 )  THEN
		iret = -3
		RETURN
	      ELSE
		time (2) = string (2)
		next = 3
	    END IF
	END IF
C
C*      Get the first grid level.
C
	CALL ST_NUMB  ( string (next), level (1), ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -3
	    RETURN
	  ELSE 
	    next = next + 1
	END IF
C
C*      Check for the second grid level.
C
	CALL ST_NUMB  ( string (next), level (2), ier )
	IF  ( ier .ne. 0 )  THEN
	    level (2) = -1
	  ELSE
	    next = next + 1
	END IF
C
C*	Get the vertical coordinate.
C
	CALL LV_CORD  ( string (next), vparm, ivcord, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -3
	    RETURN
	  ELSE
	    next = next + 1
	END IF
C
C*	Get parameter name from last string.
C
	parm = string (next)
	IF  ( parm .eq. ' ' )  iret = -3
C*
	RETURN
	END
