	SUBROUTINE GH_WWLR ( ityp, iact, ibkact, iret )
C************************************************************************
C* GH_WWLR                                                              *
C*									*
C* This subroutine sorts the watch/warning breakpoint list for a type   *
C* in ascending order and ensures that the points are unique.           *
C*                                                                      *
C* GH_WWLR ( ITYP, IACT, IBKACT, IRET )                                 *
C*                                                                      *
C* Input parameters:                                                    *
C*	ITYP		INTEGER		Watch/warning type code         *
C*                                                                      *
C* Input and output parameters:                                         *
C*	IACT		INTEGER		Number of bkpts for this type   *
C*	IBKACT(4,*)	INTEGER		Breakpoint list for this type   *
C*									*
C* Output parameters:                                                   *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	 2/05	From GH_WWSR				*
C************************************************************************
	INTEGER		ibkact (4,*)
C*
	INTEGER		ibk (100)
C------------------------------------------------------------------------
	iret = 0
	IF ( iact .le. 1 ) RETURN
C
C*	Bubble sort the watch/warning points in ascending order.
C
	istop  = iact - 1
	iswflg = 1
	DO WHILE ( ( iswflg .ne. 0 ) .and. ( istop .ge. 1 ) )
	    iswflg = 0
	    DO jj = 1, istop
		IF ( ibkact (ityp, jj) .gt. ibkact (ityp, jj+1) ) THEN
		    iswflg = 1
		    iswpb  = ibkact ( ityp, jj ) 
		    ibkact ( ityp, jj )     = ibkact ( ityp, jj + 1 )
		    ibkact ( ityp, jj + 1 ) = iswpb
		END IF
	    END DO
	    istop = istop - 1
	END DO
C
C*	Make sure points are unique.
C
	DO jj = 1, iact
	    ibk ( jj ) = ibkact ( ityp, jj )
	END DO
	npts = 0
	DO jj = 1, iact - 1
	    IF ( ibk ( jj ) .ne. ibk ( jj + 1 ) ) THEN
		npts = npts + 1
		ibkact ( ityp, npts ) = ibk ( jj )
	    END IF
	END DO
	npts = npts + 1
	ibkact ( ityp, npts ) = ibk ( iact )
	DO jj = npts + 1, iact
	    ibkact ( ityp, jj ) = 0
	END DO
	iact = npts
C*
	RETURN
	END
