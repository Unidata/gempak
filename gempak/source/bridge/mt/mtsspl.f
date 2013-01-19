	SUBROUTINE MT_SSPL ( iptr, strarr, num, strout, numout, iret )
C************************************************************************
C* MT_SSPL                                                              *
C*                                                                      *
C* This subroutine will split a list of concatenated sky condition	*
C* groups into an array of strings.					*
C* 								        *
C* MT_SSPL ( IPTR, STRARR, NUM, STROUT, NUMOUT, IRET )		        *
C*								        *
C* Input parameters: 						        *
C*	IPTR		INTEGER		Pointer in strarr		*
C*      STRARR (*)	CHAR*		Array of fields			*
C*	NUM		INTEGER		Number of fields		*
C*								        *
C* Output parameters:						        *
C*      STROUT (*)	CHAR*		Array of fields			*
C*	NUMOUT		INTEGER		Number of fields		*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = no split needed		*
C*	      		    		  1 = groups split		*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP 	 4/95                                           *
C* K. Tyle/GSC		 1/97	Change jret to iret; reorganize 	*
C*				header and comments			*
C* K. Tyle/GSC		 2/97	Remove unneeded parameter declarations	*
C* D. Kidwell/NCEP 	 6/97   Replaced ST_LSTR with INDEX             *
C* D. Kidwell/NCEP 	 6/97   Added check for string of length .ge. 40*
C* D. Kidwell/NCEP 	 4/98   Cleaned up prologue                     *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	strarr (*), strout (*)
C*
	CHARACTER	strsky*40, string*3
	LOGICAL 	endsky
C------------------------------------------------------------------------
	iret   = 0 
	endsky = .false.
C
	numout = num
C
C*	Move input strings to output array.
C
	DO i = 1, num
	    strout ( i ) = strarr ( i )
	END DO
	ip = iptr
C
	DO WHILE ( .not. endsky )
	    strsky = strarr ( ip )
	    string = strsky ( 1:3 )
	    IF ( ( string .eq. 'FEW' ) .or.
     +  	 ( string .eq. 'SCT' ) .or.
     +  	 ( string .eq. 'BKN' ) .or.
     +  	 ( string .eq. 'OVC' ) ) THEN
		lens = INDEX ( strsky, ' ' ) - 1
		IF ( lens .lt. 0 ) lens = 40
		IF ( lens .ge. 12 ) THEN
C
C*		    We may have concatenated sky groups, so
C*		    search for the start of the second one.
C
		    infew = INDEX ( strsky ( 4:lens ), 'FEW' )
		    insct = INDEX ( strsky ( 4:lens ), 'SCT' )
		    inbkn = INDEX ( strsky ( 4:lens ), 'BKN' ) 
		    inovc = INDEX ( strsky ( 4:lens ), 'OVC' )	
		    iloc = MAX ( infew, insct, inbkn, inovc ) + 3
		    IF ( iloc .gt. 3 ) THEN
C
C*			Concatenated groups exist, so make room
C*			in output array for a new string.
C
			numout = num + 1
			DO i = numout, ip + 2, -1
			    strout ( i ) = strarr ( i - 1 )
			END DO
C
C*			Split the concatenation.
C
			strout ( ip ) = strsky ( 1:iloc-1 )
			strout ( ip + 1 ) = strsky ( iloc:lens )
			iret = 1
			endsky = .true.
		    END IF
		END IF
	      ELSE
		endsky = .true.
	    END IF
	    IF ( .not. endsky ) THEN
		ip = ip + 1
		IF ( ip .gt. numout ) endsky = .true.
	    END IF
	END DO
C*
	RETURN
	END 
