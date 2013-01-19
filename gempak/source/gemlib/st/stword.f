	SUBROUTINE ST_WORD  ( chrstr, ityp, iret )
C************************************************************************
C* ST_WORD                                    				*
C*									*
C* This subroutine determines whether a character string is a word, a   *
C* latitude or longitude value, or neither.  The character string       *
C* should not contain any blanks.  A latitude or longitude consists of  *
C* a numeric value with an optional decimal point, either starting or   *
C* ending with E, W, N or S.                                            *
C* 									*
C* ST_WORD  ( CHRSTR, ITYP, IRET )					*
C*									*
C* Input parameters: 							*
C*	CHRSTR		CHAR*		Character string to analyze	*
C*									*
C* Output parameters:							*
C*	ITYP		INTEGER		Character string type           *
C*				   	  0 = word (all alphabetic)     *
C*				   	  1 = latitude                  *
C*				   	  2 = longitude                 *
C*				   	 99 = other                     *
C*	IRET		INTEGER 	Return code 			*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/01						*
C************************************************************************
	CHARACTER*(*) 	chrstr
C*
	CHARACTER	hem (4)*1
	LOGICAL 	done
C*
	DATA    	hem / 'N', 'S', 'E', 'W' /
C*-----------------------------------------------------------------------
	iret = 0
	ityp = 0
C
	CALL ST_LSTR ( chrstr, lens, ier )
C
C*	Check for all alphabetic characters.
C
	ii   = 1
	done = .false.
	DO WHILE ( .not. done )
	    CALL ST_ALNM ( chrstr ( ii:ii ), jtyp, ier )
	    IF ( jtyp .ne. 2 ) THEN
		ityp = 99
		done = .true.
	      ELSE
		ii = ii + 1
		IF ( ii .gt. lens ) done = .true.
	    END IF
	END DO
C
	IF ( ityp .eq. 0 ) THEN
	    RETURN
	  ELSE	
C
C*	    Check for a hemisphere designator.
C
	    ii   = 1
	    done = .false.
	    DO WHILE ( .not. done )
		ipos = INDEX  ( chrstr ( :lens ), hem ( ii ) )
		IF ( ( ipos .eq. 1 ) .or. ( ipos .eq. lens ) ) THEN
		    done = .true.
		    ityp = ( ii + 1 ) / 2
		  ELSE
		    ii = ii + 1
		    IF ( ii .gt. 4 ) done = .true.
		END IF
	    END DO
	END IF
C
	IF ( ityp .eq. 99 ) THEN
	    RETURN
	  ELSE
C
C*	    The string begins or ends with a hemisphere designator.
C
	    is = 1
	    ie = lens
	    IF ( ipos .eq. 1 ) is = 2
	    IF ( ipos .eq. lens ) ie = lens - 1
	    idec = 0
	    DO ii = is, ie
		IF ( chrstr ( ii:ii ) .eq. '.' ) THEN
		    idec = idec + 1
		  ELSE
		    CALL ST_ALNM ( chrstr ( ii:ii ), jtyp, ier )
		    IF ( jtyp .ne. 1 ) ityp = 99
		END IF
	    END DO
	    IF ( idec .gt. 1 ) ityp = 99
	END IF
C*
	RETURN
	END
