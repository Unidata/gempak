	SUBROUTINE GH_WWBK ( ibkus, numbus, ibkuso, numbuo, iarea, 
     +			     clistc, zlistc, ivtecc, bkpstc, nsegc, 
     +			     wfostc, iret )
C************************************************************************
C* GH_WWBK                                                              *
C*									*
C* This subroutine transforms the breakpoint numbers into values to be  *
C* used to create the segmented breakpoint watch/warning message - VTEC *
C* actions and event types, lists of UGCs, breakpoint text strings and  *
C* a WFO ATTN list.  This routine usese breakpoint pairs as input,      *
C* while routine GH_WWBL uses breakpoint lists.  The input/output       *
C* parameters are cumulative, with values being appended by each call   *
C* to this routine.                                                     *
C*                                                                      *
C* GH_WWBK ( IBKUS, NUMBUS, IBKUSO, NUMBUO, IAREA, CLISTC, ZLISTC,      *
C*	     IVTECC, BKPSTC, NSEGC, WFOSTC, IRET )                      *
C*                                                                      *
C* Input parameters:                                                    *
C* 	IBKUS(4,*)	INTEGER		Current array of bkpt numbers   *
C*	NUMBUS(*)	INTEGER		Count of current bkpt pairs     *
C*	IBKUSO(4,*)	INTEGER		Prev. array of bkpt numbers     *
C*	NUMBUO(*)	INTEGER		Count of prev. bkpt pairs       *
C*	IAREA		INTEGER		Geographic area designator      *
C*					  IUSGEC = US Gulf Mx & E coast *
C*					  IKEYS  = Florida Keys         *
C*                                                                      *
C* Input and output parameters:                                         *
C*	CLISTC(*)	CHAR*		County UGCs by segment          *
C*	ZLISTC(*)	CHAR*		Marine zone UGCs by segment     *
C*	IVTECC(3,*)	INTEGER		VTEC action and event codes     *
C*	BKPSTC(*)	CHAR*		Bkpt text strings by segment    *
C*      NSEGC		INTEGER		Number of output segments       *
C*	WFOSTC		CHAR*		List of WFOs for ATTN string    *
C*                                                                      *
C* Output parameters:                                                   *
C*	IRET		INTEGER		Return code                     *
C*					  4 = no breakpoints for storm  *
C*					  0 = normal return             *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	11/03						*
C* B. Yin/SAIC		 8/04	Increased bkpt array size; added init. 	*
C* D. Kidwell/NCEP	 2/05	Fixed GH_WWSR calls: ibkus -> jbkus	*
C* D. Kidwell/NCEP	 4/05	CSC for iarea; used cumulative I/O      *
C* D. Kidwell/NCEP	 9/05	Added lenr check for wfostc             *
C* S. Jacobs/NCEP	 9/10	Increased array sizes for temp		*
C*				breakpoint lists for each type		*
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	CHARACTER*(*)	clistc (*), zlistc (*), bkpstc (*), wfostc
	INTEGER		ibkus (4,*), numbus (*), ibkuso (4,*), 
     +			numbuo (*), ivtecc (3,*)
C*
	CHARACTER	clist(50)*1000, zlist(50)*1000, bkpstr(50)*110,
     +			wfostr*200
        INTEGER         ibkcan (4,300), ibkcon (4,300), ibknew (4,300),
     + 	       		jbkus (4,300), jbkuso (4,300), nbkold (4),
     +			nbknew (4), istcan (4), istcon (4), istnew (4),
     +			ncan (4), ncon (4), nnew (4), ibkseg (2,50),
     +			ivtec (3,50)
C------------------------------------------------------------------------
	iret = 0
C
C*      Initialization
C
        DO ii = 1, 4
            nbkold ( ii ) = 0
            nbknew ( ii ) = 0
            istcan ( ii ) = 0
            istcon ( ii ) = 0
            istnew ( ii ) = 0
            ncan   ( ii ) = 0
            ncon   ( ii ) = 0
            nnew   ( ii ) = 0
            DO jj = 1, 300
                ibkcan ( ii, jj ) = 0
                ibkcon ( ii, jj ) = 0
                ibknew ( ii, jj ) = 0
                jbkus  ( ii, jj ) = 0
                jbkuso ( ii, jj ) = 0
            END DO
        END DO
C
        DO ii = 1, 2
            DO jj = 1, 50
                ibkseg ( ii, jj ) = 0
            END DO
        END DO
	IF ( iarea .eq. IUSGEC ) wfostc = ' ' 
C 
C*	Make sure all breakpoints are in increasing order, within each
C*	pair and for the bands (segments) within a type.
C
	DO ii = 1, 4
	    nbknew ( ii ) = numbus ( ii) * 2
	    DO jj = 1, nbknew ( ii ), 2
		jbkus ( ii, jj ) = MIN0 ( ibkus ( ii, jj ), 
     +					  ibkus ( ii, jj + 1 ) )
		jbkus ( ii, jj + 1 ) = MAX0 ( ibkus ( ii, jj ),
     +					      ibkus ( ii, jj + 1 ) ) 
	    END DO
C
	    CALL GH_WWSR ( ii, nbknew ( ii ), jbkus, ier )
C
	    nbkold ( ii ) = numbuo ( ii ) * 2
	    DO jj = 1, nbkold ( ii ), 2
		jbkuso ( ii, jj ) = MIN0 ( ibkuso ( ii, jj ), 
     +					  ibkuso ( ii, jj + 1 ) )
		jbkuso ( ii, jj + 1 ) = MAX0 ( ibkuso ( ii, jj ),
     +					      ibkuso ( ii, jj + 1 ) ) 
	    END DO
C
	    CALL GH_WWSR ( ii, nbkold ( ii ), jbkuso, ier )
	END DO
C
C*	Loop over watch/warning types.
C
	DO ii = 1, 4
	    ican = 0
	    icon = 0
	    inew = 0
	    nbold = nbkold ( ii )
	    nbnew = nbknew ( ii )
	    IF ( nbold .eq. 0 ) THEN
C
C*		There are only NEW band(s) (segment(s)), if any, for
C*		this type.  Store them.
C
		DO kk = 1, nbnew, 2
		    CALL GH_WWST ( ii, jbkus ( ii, kk ),
     +			      jbkus ( ii, kk + 1 ), inew, ibknew, ier )
		END DO
	    END IF
C
C*	    Loop over old bands (segments) for a type.
C
	    DO jj = 1, nbold, 2
	      ibold = jbkuso ( ii, jj )
	      ieold = jbkuso ( ii, jj + 1 )
	      IF ( nbnew .eq. 0 ) THEN
C
C*		There are only CAN band(s) (segment(s)) for this type.
C*		Store them.
C
	 	CALL GH_WWST ( ii, ibold, ieold, ican, ibkcan, ier )
	      END IF
C
C*	      Loop over new bands (segments) for a type.
C
	      DO kk = 1, nbnew, 2
		ibnew = jbkus ( ii, kk )
		ienew = jbkus ( ii, kk + 1 )
		IF ( (ieold .le. ibnew) .or. (ienew .le. ibold) ) THEN 
C
C*		    These segments are disjoint.  Store them.
C
		    CALL GH_WWST ( ii, ibold, ieold, ican, ibkcan, ier )
		    CALL GH_WWST ( ii, ibnew, ienew, inew, ibknew, ier )
		  ELSE
C
C*		    These segments overlap.  Store them based on 
C*		    endpoint sequencing.
C
		    iemin = MIN0 ( ieold, ienew )
		    IF ( ibold .lt. ibnew ) THEN
			CALL GH_WWST ( ii, ibold, ibnew, ican, ibkcan,
     +				       ier )
			IF ( ibnew .lt. iemin ) THEN
			    CALL GH_WWST ( ii, ibnew, iemin, icon, 
     +					   ibkcon, ier )
			END IF
		      ELSE IF ( ibnew .lt. ibold ) THEN
			CALL GH_WWST ( ii, ibnew, ibold, inew, ibknew, 
     +				       ier )
			IF ( ibold .lt. iemin ) THEN
			    CALL GH_WWST ( ii, ibold, iemin, icon,
     +					   ibkcon, ier )
			END IF
		      ELSE
			CALL GH_WWST ( ii, ibold, iemin, icon, ibkcon,
     +				       ier )
		    END IF
C
		    IF ( ieold .lt. ienew ) THEN
			CALL GH_WWST ( ii, ieold, ienew, inew, ibknew,
     +				       ier )
		      ELSE IF ( ienew .lt. ieold ) THEN
			CALL GH_WWST ( ii, ienew, ieold, ican, ibkcan,
     +				       ier )
		    END IF
		END IF
	      END DO
	    END DO
C
C*	    Eliminate duplicate subsets within an action class
C*	    (NEW/CON/CAN) for this watch/warning type, and sort in
C*	    ascending order.
C
	    CALL GH_WWDP ( ii, inew, ibknew, istnew ( ii ), ier )
C
	    CALL GH_WWDP ( ii, icon, ibkcon, istcon ( ii ), ier )
C
	    CALL GH_WWDP ( ii, ican, ibkcan, istcan ( ii ), ier )
C
C*	    Eliminate NEW and CAN segments which are already included
C*	    as CON.
C
	    CALL GH_WWRM ( ii, istcon ( ii ), icon, ibkcon,
     +			   istnew ( ii ), inew, ibknew, ier )
	    CALL GH_WWRM ( ii, istcon ( ii ), icon, ibkcon,
     +			   istcan ( ii ), ican, ibkcan, ier )
	    nnew ( ii ) = inew
	    ncon ( ii ) = icon
	    ncan ( ii ) = ican
	END DO
C
C*	Reorganize the endpoints by VTEC type, and check for overlapping
C*	segments.
C
	CALL GH_WWSG ( ibknew, nnew, istnew, ibkcon, ncon, istcon,
     +		       ibkcan, ncan, istcan, nseg, ivtec, ibkseg, ier )
C
C*	Get the inputs for the breakpoint watch/warning text message -
C*	the county and zone lists, the breakpoint text string, the
C*	number of segments and the WFO string.
C
  	nsegin = nseg
	CALL GH_WWIN ( nsegin, ibkseg, iarea, ivtec, clist, zlist, 
     +		       bkpstr, nseg, wfostr, iret )
C
C*      Add these values to the existing arrays.
C
        DO ii = 1, nseg
            nsegc = nsegc + 1
            clistc ( nsegc ) = clist ( ii )
            zlistc ( nsegc ) = zlist ( ii )
            bkpstc ( nsegc ) = bkpstr ( ii )
            DO jj = 1, 3
                ivtecc ( jj, nsegc ) = ivtec ( jj, ii )
            END DO
        END DO
        IF ( nseg .gt. 0 ) THEN
            CALL ST_LSTR ( wfostc, lenc, ier )
            CALL ST_LSTR ( wfostr, lenr, ier )
	    IF ( lenr .ge. 3 ) THEN
                IF ( lenc .ge. 3 ) THEN
                    wfostc = wfostc ( :lenc ) // ';' // wfostr ( :lenr )
                  ELSE
                    wfostc = wfostr ( :lenr )
                END IF
	    END IF
	    CALL ST_NULL ( wfostc, wfostc, lenc, ier )
        END IF
C*
	RETURN
	END
