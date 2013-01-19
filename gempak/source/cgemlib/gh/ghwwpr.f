	SUBROUTINE GH_WWPR ( ibk, num, ibkout, numout, iret )
C************************************************************************
C* GH_WWPR                                                              *
C*									*
C* This subroutine converts clockwise pairs of breakpoints for Puerto   *
C* Rico to lists of breakpoints.  NOTE - Any changes in the number or   *
C* ordering of the points for Puerto Rico in tcabkpt.tbl will require   *
C* changes to this routine.  Only even-numbered breakpoints for Puerto  *
C* Rico have UGCs assigned.                                             *
C*                                                                      *
C* GH_WWPR ( IBK, NUM, IBKOUT, NUMOUT, IRET )                           *
C*                                                                      *
C* Input parameters:                                                    *
C* 	IBK(4,*)	INTEGER		Breakpoint pairs for Puerto Rico*
C*	NUM(*)		INTEGER		Count of current bkpt pairs     *
C*                                                                      *
C* Output parameters:                                                   *
C* 	IBKOUT(4,*)	INTEGER		Breakpoint lists for Puerto Rico*
C*	NUMOUT(*)	INTEGER		Count of current bkpts          *
C*	IRET		INTEGER		Return code                     *
C*					  0 = normal return             *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	 2/05	                                        *
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	INTEGER		ibk (4,*), num (*), ibkout (4,*), numout (*) 
C*
	INTEGER		iugcpt (4), notpt (4)
	LOGICAL		out
C*
	DATA		iugcpt / 2, 4, 6, 8 /
C------------------------------------------------------------------------
	iret = 0
C
	DO ii = 1, 4
	    numout ( ii ) = 0
	    DO jj = 1, num ( ii ) * 2, 2
		IF ( ibk ( ii, jj ) .eq. ibk ( ii, jj + 1 ) ) THEN
C
C*		    First point = last point means entire island.
C
		    DO kk = 2, nbkpts ( IPRICO ), 2
		        numout ( ii ) = numout ( ii ) + 1
		        ibkout ( ii, numout ( ii) ) = kk
		    END DO

		  ELSE IF ( ibk ( ii, jj ) .lt. ibk ( ii, jj+1 ) ) THEN
C
C*		    First point < last point - clockwise movement.
C
		    DO kk = ibk ( ii, jj ), ibk ( ii, jj + 1 )
			IF ( MOD ( kk, 2 ) .eq. 0 ) THEN
		            numout ( ii ) = numout ( ii ) + 1
		            ibkout ( ii, numout ( ii) ) = kk
			END IF
		    END DO

		  ELSE
C
C*		    First point > last point - clockwise movement.
C
		    k2 = ibk ( ii, jj )
		    IF ( MOD ( k2, 2 ) .eq. 0 ) k2 = k2 - 1
		    k1 = ibk ( ii, jj + 1 )
		    IF ( MOD ( k1, 2 ) .eq. 0 ) k1 = k1 + 1
		    ipt = 0
		    DO kk = k1, k2
			IF ( MOD ( kk, 2 ) .eq. 0 ) THEN
			    ipt = ipt + 1
			    notpt ( ipt ) = kk
			END IF
		    END DO
		    DO iu = 1, 4
			out = .false.
			DO ip = 1, ipt
			    IF ( iugcpt ( iu ) .eq. notpt ( ip ) )
     +				 out = .true.
			END DO
			IF ( .not. out ) THEN
		            numout ( ii ) = numout ( ii ) + 1
		            ibkout ( ii, numout ( ii) ) = iugcpt ( iu )
			END IF
		    END DO
		END IF
       	    END DO
	END DO
C*
	RETURN
	END
