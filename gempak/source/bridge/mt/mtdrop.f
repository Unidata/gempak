	SUBROUTINE MT_DROP ( strarr, num, strout, numout, iret )
C************************************************************************
C* MT_DROP                                                              *
C*                                                                      *
C* This subroutine checks for and drops fields "METAR", "RTD", "SPECI",	*
C* and/or "/////" from a METAR report.					*
C* 								        *
C* MT_DROP  ( STRARR, NUM, STROUT, NUMOUT, IRET)		        *
C*								        *
C* Input parameters:						        *
C*	STRARR (*)	CHAR*		Array of fields			*
C*	NUM		INTEGER		Number of fields		*
C*								        *
C* Output parameters:						        *
C*	STROUT (*)	CHAR*		Array of fields 		*
C*	NUMOUT		INTEGER		Number of fields 		*
C*	IRET		INTEGER		Return code                     *
C*					  0 = normal return             *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 6/95				                *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 2/97	Change parameter declarations		*
C* D. Kidwell/NCEP	 4/98   Cleaned up prologue                     *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	strarr(*), strout(*)
C*
	CHARACTER*5 	cdrop ( 4 ) 
	LOGICAL 	found
C-----------------------------------------------------------------------
	iret = 0
	ibrk = 0
	cdrop ( 1 ) = 'METAR'
	cdrop ( 2 ) = 'RTD'
	cdrop ( 3 ) = 'SPECI'
	cdrop ( 4 ) = '/////'
	numout = num
	DO i = 1, numout
	    strout ( i ) = strarr ( i ) 
	END DO
C
	DO idrop = 1, 4
	    i = 1
	    found = .false.
	    DO WHILE ( .not. found )
		IF ( strout ( i ) ( 1:5 ) .eq. cdrop ( idrop ) ) THEN
C
C*		    Drop this field.
C
		    DO j = i + 1, numout
			strout ( j - 1 ) = strout ( j )
		    END DO
		    strout ( numout ) = ' '
		    numout = numout - 1
		    found = .true.
		    IF ( idrop .le. 3 ) ibrk = i - 1
		  ELSE
		    i = i + 1
		    IF ( i .gt. num ) found = .true.
		END IF
	    END DO
	END DO
C
C*	See if there are redundant time fields and drop the second one.
C
	IF ( ibrk .ne. 0 ) THEN
	    IF ( ( strout ( ibrk ) ( 1:4 ) .eq. strout ( ibrk+1 ) (1:4))
     +	       .and. ( ( strout ( ibrk ) ( 5:5 ) .eq. 'Z' ) .or.
     +	 	       ( strout ( ibrk ) ( 5:5 ) .eq. ' ' ) ) ) THEN
		DO j = ibrk + 2, numout
		    strout ( j - 1 ) = strout ( j )
		END DO
	        numout = numout - 1
	    END IF
	END IF
C*
	RETURN
	END
