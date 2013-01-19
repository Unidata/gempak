	SUBROUTINE IS_EGCA ( leng, report, iret )
C************************************************************************
C* IS_EGCA 								*
C*									*
C* This subroutine cleans up the point definition string of the area	*
C* covered by a MUHA international sigmet phenomenon:  Commas that	*
C* are used as decimal points in lat/lon values are replaced by		*
C* periods.  The remaining commas are replaced by blanks.  Slashes are  *
C* removed.								*
C*                                                                      *
C* IS_EGCA ( LENG, REPORT, IRET )					*
C*									*
C* Input parameters:							*
C*	LENG		INTEGER		Length of string                *
C*									*
C* Input and output parameters:						*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	10/01	Created					*
C************************************************************************
	CHARACTER*(*)	report
C------------------------------------------------------------------------
	iret = 0
C
C*	Replace commas used as decimal points with periods.
C*	Replace rest of commas with spaces.
C
	DO i = 1, leng - 3
	    IF ( report ( i:i ) .eq. ',' ) THEN
		IF ( report ( i + 2:i + 2 ) .eq. 'N' .or.
     +		     report ( i + 2:i + 2 ) .eq. 'W' .or.
     +		     report ( i + 2:i + 2 ) .eq. 'S' .or.
     +		     report ( i + 2:i + 2 ) .eq. 'E' )    THEN
		    CALL ST_ALNM ( report ( i - 1:i - 1 ), ityp, ier )
		    IF ( ityp .eq. 1) THEN
			report ( i:i ) = '.'
		      ELSE
			report ( i:i ) = ' '
		    END IF
		  ELSE IF ( report ( i + 3:i + 3 ) .eq. 'N' .or.
     +               	    report ( i + 3:i + 3 ) .eq. 'W' .or.
     +               	    report ( i + 3:i + 3 ) .eq. 'S' .or.
     +               	    report ( i + 3:i + 3 ) .eq. 'E' )    THEN
                    CALL ST_ALNM ( report ( i + 1:i + 1 ), ityp, ier )
                    IF ( ityp .eq. 1) THEN
                        CALL ST_ALNM ( report ( i + 2:i + 2 ),
     +				       ityp, ier )
                        IF ( ityp .eq. 1) THEN
			    report ( i:i ) = '.'
			  ELSE
			    report ( i:i ) = ' '
			END IF
                      ELSE
                        report ( i:i ) = ' '
		    END IF
		  ELSE
		    report ( i:i ) = ' '
		END IF
	    END IF
	END DO
C
C*	Remove slashes.
C
	ipos = - 1
	ib = 3
	ie = leng - 3
	DO WHILE ( ipos .ne. 0 .and. ie .gt. ib )
	    ipos = INDEX ( report ( ib:ie ), '/' )
	    IF ( ipos .ne. 0 ) THEN
		report = report ( 1:ib + ipos - 2 ) //
     +			    report ( ib + ipos : ie ) // ' '
		ie = ie - 1
		ib = ib + ipos - 1
	    END IF
	END DO
C*
	RETURN
	END
