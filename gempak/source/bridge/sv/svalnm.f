	SUBROUTINE SV_ALNM  ( chrstr, lenstr, ityp, iret )
C************************************************************************
C* SV_ALNM                                    				*
C*									*
C* This subroutine determines whether a character string is all numbers,*
C* all letters, or neither.  It ignores leading and trailing blanks.	*
C* 									*
C* SV_ALNM  ( CHRSTR, LENSTR, ITYP, IRET )				*
C*									*
C* Input parameters: 							*
C*	CHRSTR		CHAR*		Character string to analyze	*
C*	LENSTR	        INTEGER		Length of character string	*
C*									*
C* Output parameters:							*
C*	ITYP		INTEGER		Character type			*
C*				   	  0 = mixed / neither		*
C*				   	  1 = all numbers		*
C*				   	  2 = all letters		*
C*	IRET		INTEGER 	Return code 			*
C*				   	  0 = normal return		*
C*					 -1 = invalid length		*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	12/00						*
C* F. J. Yen/NCEP	 1/01	Ignored leading and trailing blanks.	*
C************************************************************************
	CHARACTER*(*) 	chrstr
C*
	LOGICAL		done
C------------------------------------------------------------------------
	iret = 0
	ityp = 0
	IF ( lenstr .le. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
	ib = 1
	done = .false.
	DO WHILE ( ib .lt. lenstr .and. .not. done )
	    IF ( chrstr(ib:ib) .eq. ' ' ) THEN
		ib = ib + 1
	      ELSE
		done = .true.
	    END IF
	END DO
	ie = lenstr
	done = .false.
	DO WHILE ( ie .gt. ib .and. .not. done )
            IF ( chrstr(ie:ie) .eq. ' ' ) THEN
		ie = ie - 1
	      ELSE
		done = .true.
	    END IF
	END DO
	CALL ST_ALNM ( chrstr(ib:ib), ityp, ier )
	IF ( ityp .eq. 0 ) RETURN
	ib = ib + 1
	DO WHILE ( ib .le. ie ) 
	    CALL ST_ALNM ( chrstr(ib:ib), ictyp, ier )
	    IF ( ictyp .ne. ityp ) THEN
		ityp = 0
		RETURN
	    END IF
	    ib = ib + 1
	END DO
C*
	RETURN
	END
