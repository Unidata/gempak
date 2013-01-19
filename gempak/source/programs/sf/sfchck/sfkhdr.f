	SUBROUTINE SFKHDR ( times, ntime, header, headr2, iret )
C************************************************************************
C* SFKHDR								*
C*									*
C* This subroutine sets up the header for the SFCHCK program.  The      *
C* header will either display one line consisting of hour information   *
C* or two lines consisting of either day and hour information or hour   *
C* and minute information, depending on the times in the file.          *
C*									*
C* SFKHDR  ( TIMES, NTIME, HEADER, HEADR2, IRET )			*
C*									*
C* Input parameters:							*
C*	TIMES (NTIME)	CHAR*		Times in the data file		*
C*	NTIME		INTEGER		Number of times			*
C*									*
C*  Output parameters:							*
C*	HEADER		CHAR*		Heading				*
C*	HEADR2		CHAR*		Second line heading             *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Tyle/GSC		 4/97						*
C* D. Kidwell/NCEP	10/00	Added 2nd header line, cleaned up	*
C************************************************************************
	CHARACTER*(*) 	times (*), header, headr2
C*
	LOGICAL 	minute
C------------------------------------------------------------------------
	iret   = 0
	minute = .false.
C
C*	Set up heading for table.
C
	header = ' '
	ipos   = 11
	header (4:7) = 'STID'
C
C*	First get the hour information.
C
	DO i = 1, ntime
	    header ( ipos:ipos + 1 ) = times ( i ) ( 8:9 )
	    ipos = ipos + 3
	END DO
	header ( ipos:ipos + 2 ) = 'TOT'
C
	headr2 = ' '
	ipos   = 11
	IF ( times ( 1 ) ( 5:6 ) .ne. times ( ntime ) ( 5:6 ) ) THEN
C
C*	    The first line of the header will be the day.
C
	    DO i = 1, ntime
		headr2 ( ipos:ipos + 1 ) = header ( ipos: ipos + 1 )
	        header ( ipos:ipos + 1 ) = times ( i ) ( 5:6 )
		ipos = ipos + 3
	    END DO
	  ELSE 
	    i = 1
	    DO WHILE ( .not. minute .and. ( i .le. ntime ) )
		IF ( times ( i ) ( 10:11 ) .ne. '00' ) minute = .true.
		i = i + 1
	    END DO
	    IF ( minute ) THEN
C
C*		The second line of the header will be the minutes.
C
 		DO i = 1, ntime
		    headr2 ( ipos:ipos + 1 ) = times ( i ) ( 10:11 )
		    ipos = ipos + 3
		END DO
	    END IF
	END IF
C*
	RETURN
	END
