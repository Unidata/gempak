	SUBROUTINE DC_TEST  ( bull, lenb, itest, iret )
C************************************************************************
C* DC_TEST								*
C*									*
C* This routine checks to see if a bulletin should be flagged as a      *
C* test bulletin.  The word 'TEST' must occur at least twice in the text*
C* for the bulletin to be recognized as a test.  The special case of    *
C* tornado/severe tstm watch bulletins, which may only have one instance*
C* of the word 'TEST', is flagged with a non-zero return code.          *
C*									*
C* DC_TEST  ( BULL, LENB, ITEST, IRET )                                 *
C*									*
C* Input parameters:							*
C*	BULL		CHAR*		Bulletin                        *
C*	LENB		INTEGER		Length of bulletin              *
C*									*
C* Output parameters:							*
C*	ITEST		INTEGER		Flag for test report            *
C*	IRET		INTEGER		Return code 			*
C*					  1 = one occurrence of 'TEST'  *
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/00	                                        *
C* D. Kidwell/NCEP	 9/04	Added check for one occurrence of 'TEST'*
C* F. J. Yen/NCEP	10/06	Checked for false test ') TEST MESSAGE' *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bull
C*
	CHARACTER 	strnot*20, carr (3)*16
	LOGICAL 	done, test
C-----------------------------------------------------------------------
	iret  = 0
	itest = 0
	done  = .false.
	iptr  = 1
C
	DO WHILE ( .not. done )
	    indx = INDEX ( bull ( iptr:lenb ), 'TEST' )
	    IF ( indx .eq. 0 ) THEN
		done = .true.
	      ELSE
C
		test  = .true.
		iloc  = iptr + indx -1
		ipre  = iloc - 1
		ipost = iloc + 4
		inot  = iloc - 12
C
		IF ( ipre .gt. 0 ) THEN
     		    CALL ST_ALNM ( bull ( ipre:ipre ), ityp, ier )
		    IF ( ityp .eq. 2 ) test = .false.
		END IF
		IF ( ipost .le. lenb ) THEN
		    CALL ST_ALNM ( bull ( ipost:ipost ), ityp, ier )
		    IF ( ityp .eq. 2 ) test = .false.
		END IF
C
C*		Eliminate false test (') TEST MESSAGE') from possible
C*		garbled data.
C
		IF (indx .ge. 3) THEN
		    indf = iptr + indx - 3
		  ELSE
		    indf = iptr
		END IF
		lenf = MIN ( indf + 14, lenb )
		indxf = INDEX ( bull ( indf:lenf ), ') TEST MESSAGE' ) 
		IF ( indxf .gt. 0 ) test = .false.
C
		IF ( test ) THEN
		    itest  = itest + 1
		    ilocsv = iloc
		END IF
C
		IF ( inot .gt. 0 ) THEN
		    lenstr = ipost - inot + 1
		    CALL ST_UNPR ( bull ( inot:ipost ), lenstr, strnot,
     +				   ln, ier )
		    CALL ST_RMBL ( strnot ( :ln ), strnot, lennot, ier )
		    idxnot = INDEX ( strnot ( :lennot ), 'NOTATEST' ) 
		    IF ( idxnot .gt. 0 ) THEN
			itest = 0
			done  = .true.
		    END IF
		END IF
		iptr = ipost
	    END IF
	END DO
C
	IF ( itest .lt. 2 ) THEN
C
C*	    Check for one occurrence of 'TEST' in a watch message.
C
	    IF ( itest .eq. 1 ) THEN
		lent = MIN ( ilocsv + 15, lenb )
		CALL ST_CLST ( bull ( ilocsv:lent ), ' ', ' ', 3,
     +			       carr, num, ier )
		CALL ST_LSTR ( carr ( 2 ), lens, ier )
C
C*		Check for text specific to a watch test.
C
		IF ( ( carr ( 2 ) ( :lens ) .eq. 'TORNADO' ) .or.
     +		     ( carr ( 2 ) ( :lens ) .eq. 'SEVERE'  ) ) THEN
		    iret = 1
		END IF
	    END IF
C
	    itest = 0
	  ELSE
	    itest = 2
	END IF
C*
	RETURN
	END
