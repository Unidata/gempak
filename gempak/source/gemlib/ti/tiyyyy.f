	SUBROUTINE TI_YYYY ( ntime, timin, outime, iret )
C************************************************************************
C* TI_YYYY								*
C*									*
C* This subroutine reorders a list of GEMPAK times so that times in the *
C* 20th century (YY greater than 20) precede those in the 21st century  *
C* (YY less than or equal to 20).  The input and output arrays may be   *
C* the same.  The input times must be sorted smallest to largest.  The  *
C* output times will be sorted earliest to latest.                      *
C*									*
C* TI_YYYY  ( NTIME, TIMIN, OUTIME, IRET )				*
C*									*
C* Input parameters:							*
C*	NTIME		INTEGER		Number of times 		*
C*	TIMIN (NTIME)	CHAR*		GEMPAK times 			*
C*									*
C* Output parameters:							*
C*	OUTIME (NTIME)	CHAR*		Sorted times			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 2/99	                                        *
C* D. Kidwell/NCEP	 4/99	Stored to outime; added check for YYYY  *
C* T. Piper/SAIC	 4/02	Fixed UMR; checked for ntime < 1	*
C************************************************************************
	CHARACTER*(*) 	timin (*), outime (*)
C*
	CHARACTER 	savstr*30, cent1*2, cent2*2
	LOGICAL 	found, down
C------------------------------------------------------------------------
	iret  = 0
	IF ( ntime .lt. 1 ) RETURN
C
	DO i = 1, ntime
	    outime ( i ) = timin ( i )
	END DO
C
C*	If input year is four digits, data is already ordered correctly.
C
	CALL ST_LSTR ( timin ( 1 ), length, ier )
	islash = INDEX ( timin ( 1 ), '/' )
	IF ( ( length .ge. 13 ) .and. ( islash .eq. 9 ) )  RETURN
C
C*	Reorder the 2-digit years if they span two centuries.
C
	IF ( ntime .gt. 1 ) THEN
	    CALL TI_CCNT ( timin ( 1 ), cent1, iret )
	    CALL TI_CCNT ( timin ( ntime ), cent2, iret )
C
C*	    Check for new century.
C
	    IF ( cent1 .ne. cent2 ) THEN
		i = 1
		found = .false.
	        DO WHILE ( .not. found )
		    IF ( timin ( i ) ( 1:2 ) .gt. '20' ) THEN
			found = .true.
			IF ( i .gt. ( ntime / 2 ) ) THEN
			    down  = .true.
			    imove = ntime - i + 1
			  ELSE 
			    down  = .false.
			    imove = i - 1
			END IF
		      ELSE
			i = i + 1
			IF ( i .gt. ntime ) RETURN
		    END IF
		END DO
C
C*		Reorder GEMPAK times.
C
		DO i = 1, imove
		    IF ( down ) THEN
			savstr = outime ( ntime )
			DO j = ntime, 2, -1
			    outime ( j ) = outime ( j - 1 )
			END DO
			outime ( 1 ) = savstr
		      ELSE
			savstr = outime ( 1 )
			DO j = 1, ntime - 1
			    outime ( j ) = outime ( j + 1 )
			END DO
			outime ( ntime ) = savstr
		    END IF
		END DO
	    END IF
	END IF
C*
	RETURN
	END
