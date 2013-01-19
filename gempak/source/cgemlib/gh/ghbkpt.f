	SUBROUTINE GH_BKPT ( ibkpt, numbkp, iarea, iret )
C************************************************************************
C* GH_BKPT								*
C*									*
C* This subroutine draws breakpoints which are given as a list.         *
C*									*
C* GH_BKPT ( IBKPT, NUMBKP, IAREA, IRET )				*
C*									*
C* Input parameters:							*
C*	IBKPT(*)	INTEGER		Array of breakpoint numbers	*
C*	NUMBKP 		INTEGER		Count of breakpoints            *
C*      IAREA           INTEGER         Geographic area designator      *
C*                                        IWATER = Bodies of water	*
C*                                        IOTHER = Other islands        *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/03	Replaces GH_BKCU and GH_BKEL            *
C* D. Kidwell/NCEP	 1/04	Bug fix for ll .eq. iend (USVI problem) *
C* m.gamazaychikov/SAIC	04/04	From GH_BKLI with modifications		*
C* m.gamazaychikov/SAIC	08/04	Added call to GFILL for bodies of water	*
C* D. Kidwell/NCEP	 4/05	More robust bug fix for ll .eq. iend    *
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	INTEGER		ibkpt (*)
C*
	LOGICAL		done
C------------------------------------------------------------------------
	iret = 0
C
	ibeg = indxbk ( iarea )
	iend = indxbk ( iarea + 1 ) - 1
C
C*	Loop over the breakpoint list for the current type.
C
	DO ii = 1, numbkp
	    larea = ibkpt ( ii )
C
C*	    Find the beginning and ending indices in the 
C*	    breakpoint sequence array.  Breakpoints in the list
C*	    may have more than one line segment associated with
C*	    them.
C
	    DO kk = ibeg, iend
	        IF ( ibkseq ( kk ) .eq. larea ) THEN
		    indxb = kk + 1
	    	    indxe = 0
		    ll    = indxb + 1
		    done  = .false.
		    DO WHILE ( .not. done )
			ipen = MOD ( ibkpri ( ll ), 10 )
			IF ( ( ipen .ne. 5 ) .or.
     +			     ( ll .eq. iend ) ) THEN
			    IF ( ( indxe .ne. 0 ) .or.
     +				 ( ll .eq. iend ) ) THEN
				IF ( indxe .ne. 0 ) THEN
                       		    np = indxe - indxb + 1
				ELSE
				    np = iend - indxb
				END IF
				IF ( ( ll .eq. iend ) .and. 
     +				     ( ipen .eq. 5 ) )  np = np + 1
C
C*                      	Draw the coastline for this list
C*			        item.
C
                        	CALL GLINE ( 'M', np, 
     +				          bklat ( indxb ),
     +                           	  bklon ( indxb ), ier )
C
C*                              Fill the area if it is a body of water
C
                                IF ( ( iarea .eq. IWATER )      .and.
     +                               ( bklat (indxb) .eq. 
     +                                 bklat (indxb + np - 1) ) .and.
     +                               ( bklon (indxb) .eq. 
     +                                 bklon (indxb + np - 1) ) )
     +                               CALL GFILL ( 'M', np,
     +                                            bklat ( indxb ),
     +                                            bklon ( indxb ), ier )
				IF ( ipen .eq. 1 ) THEN
				    indxb = ll
				ELSE
				    done = .true.
				END IF
			    END IF
			ELSE
			    indxe = ll
                  	END IF
			ll = ll + 1
			IF ( ll .gt. iend ) done = .true.
		    END DO
	        END IF
	    END DO
	END DO
C*
	RETURN
	END
