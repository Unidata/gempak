	SUBROUTINE ST_NARG ( strng, ipos, argsep, nargs, iret )
C************************************************************************
C* ST_NARG								*
C*									*
C* This subroutine counts the number of arguments enclosed by a		*
C* delimiter pair for a function whose argument list begins with	*
C* the opening delimiter at position IPOS in the input string, STRNG.	*
C* The arguments are separated by the single character stored in	*
C* ARGSEP.  The number of arguments is returned as an integer value	* 
C* in NARGS.								*
C*									*
C* ST_NARG ( STRNG, IPOS, ARGSEP, NARGS, IRET)				*
C*									*
C* Input parameters:							*
C*	STRNG		CHAR*		Input string of arguments	*
C*	IPOS		INTEGER		Location of opening delimiter	*
C*	ARGSEP		CHAR*1		Arguments separator		*
C*									*
C* Output parameters:							*
C*	NARGS		INTEGER		Number of arguments in STRNG	*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return 		*
C**									*
C* Log:									*
C* m.gamazaychikov/SAIC	09/05						*
C************************************************************************
	CHARACTER*(*)	strng, argsep
C*
	CHARACTER*1	od
	LOGICAL		done
C------------------------------------------------------------------------
	iret = 0
	nargs = 0
	done = .false.
	od = strng ( ipos:ipos )
C
C*	Find the location of closing delimeter in the input string.
C
	CALL ST_OPCL ( strng, ipos, strng, kstop, ier )
C
C*	Start character-by-character forward search looking for ARGSEP,
C*	the opening delimeter, or stoping point.
C
	ii = ipos + 1
	DO WHILE  ( .not. done )
	   IF ( strng ( ii:ii ) .eq. od ) THEN
	      CALL ST_OPCL ( strng, ii, strng, kpos, ier )
	      ii = kpos+1
	    ELSE IF ( strng ( ii:ii ) .eq. argsep ) THEN
	      nargs = nargs + 1
	      ii = ii + 1
	    ELSE IF ( ii .eq. kstop ) THEN
	      nargs = nargs + 1
	      done =  .true.
	    ELSE
	      ii = ii + 1
	   END IF
	END DO
C*
	RETURN
	END
