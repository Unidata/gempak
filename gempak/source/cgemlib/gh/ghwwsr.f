	SUBROUTINE GH_WWSR ( ityp, iact, ibkact, iret )
C************************************************************************
C* GH_WWSR                                                              *
C*									*
C* This subroutine sorts the watch/warning breakpoint pairs for a type  *
C* in ascending order.  Each pair is assumed to have the smaller value  *
C* first.                                                               *
C*                                                                      *
C* GH_WWSR ( ITYP, IACT, IBKACT, IRET )                                 *
C*                                                                      *
C* Input parameters:                                                    *
C*	ITYP		INTEGER		Watch/warning type code         *
C*	IACT		INTEGER		Number of bkpts for this type   *
C*                                                                      *
C* Input and output parameters:                                         *
C*	IBKACT(4,*)	INTEGER		Breakpoint pairs for this type  *
C*									*
C* Output parameters:                                                   *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	11/03						*
C************************************************************************
	INTEGER		ibkact (4,*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Bubble sort the watch/warning bands (segments) in ascending
C*	order.
C
	istop  = iact - 2
	iswflg = 1
	DO WHILE ( ( iswflg .ne. 0 ) .and. ( istop .ge. 1 ) )
	    iswflg = 0
	    DO jj = 1, istop, 2
		IF ( ibkact (ityp, jj) .gt. ibkact (ityp, jj+2) ) THEN
		    iswflg = 1
		    iswpb1 = ibkact ( ityp, jj ) 
		    iswpb2 = ibkact ( ityp, jj + 1 )
		    ibkact ( ityp, jj )     = ibkact ( ityp, jj + 2 )
		    ibkact ( ityp, jj + 1 ) = ibkact ( ityp, jj + 3 )
		    ibkact ( ityp, jj + 2 ) = iswpb1
		    ibkact ( ityp, jj + 3 ) = iswpb2
		END IF
	    END DO
	    istop = istop - 2
	END DO
C*
	RETURN
	END
