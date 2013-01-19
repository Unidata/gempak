	SUBROUTINE GH_WWDP ( ityp, iact, ibkact, istact, iret )
C************************************************************************
C* GH_WWDP                                                              *
C*									*
C* This subroutine eliminates duplicate subsets of breakpoint pairs     *
C* within a VTEC action class ('NEW', 'CON', or 'CAN') and sorts the    *
C* pairs in the class in ascending order.                               *
C*                                                                      *
C*                                                                      *
C* GH_WWDP ( ITYP, IACT, IBKACT, ISTACT, IRET )                         *
C*                                                                      *
C* Input parameters:                                                    *
C* 	ITYP		INTEGER		Watch/warning type code         *
C*	IACT		INTEGER         Number of bkpts for this type   *
C*                                                                      *
C* Input and output parameters:                                         *
C*	IBKACT(4,*)	INTEGER		Breakpoint pairs for this type  *
C*									*
C* Output parameters:                                                   *
C*      ISTACT		INTEGER		Index of first good bkpt value  *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	11/03						*
C************************************************************************
	INTEGER		ibkact (4,*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Eliminate duplicate subsets within an action class (NEW/CON/CAN)
C*	for this watch/warning type, and sort in ascending order.
C
	DO jj = 1, iact - 2, 2
	    DO kk = jj + 2, iact, 2 
		IF ( ( ibkact (ityp,kk) .ge. ibkact (ityp,jj) ) .and.
     +		     ( ibkact (ityp,kk+1) .le. ibkact (ityp,jj+1) ) ) 
     +		       THEN
		    ibkact ( ityp, kk )     = 0
		    ibkact ( ityp, kk + 1 ) = 0
		  ELSE IF ( ( ibkact (ityp,jj) .ge. ibkact (ityp,kk) ) 
     +			    .and.
     +		        ( ibkact (ityp,jj+1) .le. ibkact (ityp,kk+1) ) )
     +                    THEN
		    ibkact ( ityp, jj )     = 0
		    ibkact ( ityp, jj + 1 ) = 0
		END IF
	    END DO
	END DO
C
	CALL GH_WWSR ( ityp, iact, ibkact, ier )
C
	jj = 1
	IF ( iact .gt. 0 ) THEN
	    DO WHILE ( ibkact ( ityp, jj ) .eq. 0 )
	        jj = jj + 2
	    END DO
	END IF
	istact = jj
C*
	RETURN
	END
