	SUBROUTINE GH_WWBL ( ibkc, numc, ibkp, nump, iarea, clistc, 
     +			     zlistc, ivtecc, bkpstc, nsegc, wfostc, 
     +			     iret )
C************************************************************************
C* GH_WWBL                                                              *
C*									*
C* This subroutine transforms a list of breakpoint numbers into values  *
C* to be used to create the segmented breakpoint watch/warning message: *
C* VTEC actions and event types, lists of UGCs, breakpoint text strings *
C* and a WFO ATTN list.  This routine uses breakpoint lists as input,   *
C* while routine GH_WWBK uses breakpoint pairs.  The input/output       *
C* parameters are cumulative, with values being appended by each call   *
C* to this routine.                                                     *
C*                                                                      *
C* GH_WWBL ( IBKC, NUMC, IBKP, NUMP, IAREA, CLISTC, ZLISTC, IVTECC,     *
C*	     BKPSTC, NSEGC, WFOSTC, IRET )                              *
C*                                                                      *
C* Input parameters:                                                    *
C* 	IBKC(4,*)	INTEGER		Current array of bkpt numbers   *
C*	NUMC(*)		INTEGER		Count of current bkpts          *
C*	IBKP(4,*)	INTEGER		Previous array of bkpt numbers  *
C*	NUMP(*)		INTEGER		Count of previous bkpts         *
C*	IAREA		INTEGER		Geographic area designator      *
C*                                        IOTHER = Other islands        *
C*                                        IPRICO = Puerto Rico          *
C*                                        IWATER = Bodies of water      *
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
C*					  0 = normal return             *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	 2/05	From GH_WWBK                            *
C* D. Kidwell/NCEP	 4/05	Added ST_NULL for wfostc                *
C* D. Kidwell/NCEP	 9/05	Added lenr check for wfostc             *
C* S. Jacobs/NCEP	 9/10	Increased array sizes for temp		*
C*				breakpoint lists for each type		*
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	CHARACTER*(*)	clistc (*), zlistc (*), bkpstc (*), wfostc
	INTEGER		ibkc (4,*), numc (*), ibkp (4,*), nump (*), 
     +			ivtecc (3,*)
C*
	CHARACTER	clist(50)*1000, zlist(50)*1000, bkpstr(50)*110,
     +			wfostr*200
        INTEGER         ibkcan (4,300), ibkcon (4,300), ibknew (4,300),
     +			ncan (4), ncon (4), nnew (4), ibkseg (50),
     +  		ibkcur (4,300), numcur (4), ibkprv (4,300), 
     +			numprv (4), ivtec (3,50)
	LOGICAL		done
C------------------------------------------------------------------------
	iret = 0
C
C*      Initialize arrays.
C
        DO ii = 1, 4
            numprv ( ii ) = 0
            numcur ( ii ) = 0
            ncan   ( ii ) = 0
            ncon   ( ii ) = 0
            nnew   ( ii ) = 0
            DO jj = 1, 300
                ibkcan ( ii, jj ) = 0
                ibkcon ( ii, jj ) = 0
                ibknew ( ii, jj ) = 0
                ibkcur ( ii, jj ) = 0
                ibkprv ( ii, jj ) = 0
            END DO
        END DO
C
	DO ii = 1, 50
	    ibkseg ( ii ) = 0
	END DO
C
C*	For Puerto Rico, convert ordered input pairs to lists.
C
	IF ( iarea .eq. IPRICO ) THEN
	    CALL GH_WWPR ( ibkc, numc, ibkcur, numcur, ier )
	    CALL GH_WWPR ( ibkp, nump, ibkprv, numprv, ier )
	  ELSE
C
C*	    Just move list data for other areas to the work arrays.
C
	    DO ii = 1, 4
		numcur ( ii ) = numc ( ii ) 
		numprv ( ii ) = nump ( ii )
		DO jj = 1, numc ( ii ) 
		    ibkcur ( ii, jj ) = ibkc ( ii, jj )
		END DO
		DO jj = 1, nump ( ii ) 
		    ibkprv ( ii, jj ) = ibkp ( ii, jj )
		END DO
	    END DO
	END IF
C 
C*	Make sure breakpoints are in increasing order within a type,
C*      and unique.
C
	DO ii = 1, 4
            CALL GH_WWLR ( ii, numcur ( ii ), ibkcur, ier )
            CALL GH_WWLR ( ii, numprv ( ii ), ibkprv, ier )
	END DO
C
C*	Loop over watch/warning types.
C
	DO ii = 1, 4
	    ican = 0
	    icon = 0
	    inew = 0
	    nbprv = numprv ( ii )
	    nbcur = numcur ( ii )
	    IF ( nbprv .eq. 0 ) THEN
C
C*		There are only NEW points, if any, for this type.
C
		DO kk = 1, nbcur
		    inew = inew + 1
		    ibknew ( ii, inew ) = ibkcur ( ii, kk )
		END DO
	    END IF
C
	    IF ( nbcur .eq. 0 ) THEN
C
C*	        There are only CAN points, if any, for this type.
C
	        DO kk = 1, nbprv
		    ican = ican + 1
		    ibkcan ( ii, ican ) = ibkprv ( ii, kk ) 
		END DO
	    END IF
C
C*	    Compare previous and current points.
C
	    IF ( ( nbprv .gt. 0 ) .and. ( nbcur .gt. 0 ) ) THEN
		ip   = 1
		ic   = 1
		done = .false.
		DO WHILE ( .not. done )
		    ibprv = ibkprv ( ii, ip )
		    ibcur = ibkcur ( ii, ic )
C
		    IF ( ibprv .eq. ibcur ) THEN
			icon = icon + 1
			ibkcon ( ii, icon ) = ibcur
			ip = ip + 1
			ic = ic + 1
		      ELSE IF ( ibprv .lt. ibcur ) THEN
			ican = ican + 1
			ibkcan ( ii, ican ) = ibprv
			ip = ip + 1
		      ELSE
			inew = inew + 1
			ibknew ( ii, inew ) = ibcur
			ic = ic + 1
		    END IF
C
		    IF ( ip .gt. nbprv ) THEN
			done = .true.
			DO kk = ic, nbcur
		            inew = inew + 1
		            ibknew ( ii, inew ) = ibkcur ( ii, kk )
		        END DO
		    END IF
		    IF ( ic .gt. nbcur ) THEN 
			done = .true.
	                DO kk = ip, nbprv
		            ican = ican + 1
		            ibkcan ( ii, ican ) = ibkprv ( ii, kk ) 
		        END DO
		    END IF
		END DO
	    END IF
	    nnew ( ii ) = inew
	    ncon ( ii ) = icon
	    ncan ( ii ) = ican
	END DO
C
C*	Reorganize the endpoints by VTEC type, and check for overlapping
C*	segments.
C
        CALL GH_WWLG ( ibknew, nnew, ibkcon, ncon, ibkcan, ncan, nseg, 
     +		       ivtec, ibkseg, ier )
C
C*	Get the inputs for the breakpoint watch/warning text message -
C*      the county and zone lists, the breakpoint text string, the
C*      number of segments and the WFO string.
C
	nsegin = nseg
	CALL GH_WWLN ( nsegin, ibkseg, iarea, ivtec, clist, zlist, 
     +		       bkpstr, nseg, wfostr, ier )
C
C*	Add these values to the existing arrays.
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
