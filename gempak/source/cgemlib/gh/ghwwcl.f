	SUBROUTINE GH_WWCL ( nseg, ivtec, ncnty, nzone, action, itype, 
     +			     cpart, zpart, bkpstr, clist, zlist, iret )
C************************************************************************
C* GH_WWCL                                                              *
C*									*
C* This subroutine performs a final check of the segments, eliminating  *
C* redundant segments and adding new segments if needed.                *
C*                                                                      *
C* GH_WWCL ( NSEG, IVTEC, NCNTY, NZONE, ACTION, ITYPE, CPART, ZPART,    *
C*	     BKPSTR, CLIST, ZLIST, IRET )                               *
C*                                                                      *
C* Input and output parameters:                                         *
C*	NSEG		INTEGER		Number of segments              *
C*	IVTEC(3,*)	INTEGER		VTEC action and event codes     *
C*	NCNTY(*)	INTEGER		Number of counties in segment   *
C*	NZONE(*)	INTEGER		Number of zones in segment      *
C*	ACTION(3,*)	CHAR*		VTEC action codes               *
C*	ITYPE(3,*)	INTEGER		VTEC event codes                *
C*	CPART(300,*)	CHAR*		County UGCs by segment          *
C*	ZPART(300,*)	CHAR*		Marine zone UGCs by segment     *
C*	BKPSTR(*)	CHAR*		Bkpt text strings by segment    *
C*									*
C* Output parameters:                                                   *
C*	CLIST(*)	CHAR*		County UGCs by segment          *
C*	ZLIST(*)	CHAR*		Marine zone UGCs by segment     *
C*	IRET		INTEGER		Return code                     *
C*					  0 = normal return             *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	 2/05	From GH_WWIN                            *
C* D. Kidwell/NCEP	 9/05	Added calls to GH_WWHI                  *
C* m.gamazaychikov/SAIC	04/08	Add checks on ncnty and nzone b4 GH_WWMV*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C*
	PARAMETER	( NSEGMX = 50, NCZMX = 300 )
C*
        CHARACTER*(*)   action (3,*), cpart (NCZMX,*), zpart (NCZMX,*),
     +			bkpstr (*), clist (*), zlist (*)
	INTEGER		ivtec (3,*), ncnty(*), nzone (*), itype (3,*)
C*
	INTEGER		isgnum (NSEGMX), ivtcnm (NSEGMX)
	LOGICAL		drop (NSEGMX), done, add
C-----------------------------------------------------------------------
	iret = 0
C
C*	Look for UGCs which appear in more than one segment and
C*	eliminate from the less critical segment.
C*	First create temporary segments for any second and third VTEC
C*	lines which exist.
C
	newseg = nseg
	DO nn = 1, nseg
	    isgnum ( nn ) = nn
	    ivtcnm ( nn ) = ivtec ( 1, nn )
	    done  = .false.
	    nvtec = 2
	    DO WHILE ( .not. done .and. nvtec .le. 3 )
	        IF ( ivtec ( nvtec, nn ) .ne. 0 ) THEN
C
C*		    There is another VTEC line for this segment.
C
		    newseg = newseg + 1
		    isgnum ( newseg ) = nn
		    ivtcnm ( newseg ) = ivtec ( nvtec, nn )
		    ncnty ( newseg )  = ncnty ( nn )
		    nzone ( newseg )  = nzone ( nn )
		    action ( 1, newseg ) = action ( nvtec, nn )
		    itype ( 1, newseg )  = itype ( nvtec, nn )
		    DO ii = 1, ncnty ( nn )
			cpart ( ii, newseg ) = cpart ( ii, nn )
		    END DO
		    DO ii = 1, nzone ( nn )
			zpart ( ii, newseg ) = zpart ( ii, nn )
		    END DO
		    bkpstr ( newseg ) = bkpstr ( nn )
		    nvtec = nvtec + 1
		  ELSE
		    done = .true.
		END IF
	    END DO
	END DO
C
C*	Loop over all segments (now with only one VTEC line per segment)
C*	and eliminate any UGCs when one event type or action takes 
C*	priority over another.
C
	CALL GH_WWHI ( ncnty, newseg, action, itype, NCZMX, cpart, ier )
	CALL GH_WWHI ( nzone, newseg, action, itype, NCZMX, zpart, ier )
C
	DO ii = 1, newseg - 1
	    CALL GH_WWCZ ( ii, ncnty, newseg, action, itype, NCZMX,
     +			   cpart, ier )
C
	    CALL GH_WWCZ ( ii, nzone, newseg, action, itype, NCZMX,
     +			   zpart, ier )
	END DO
C
	IF ( newseg .gt. nseg ) THEN
C
C*	    Check for segments which were created temporarily and need
C*	    to be added as new segments.  This will only be done if the 
C* 	    UGCs have changed between the segments.
C
	    nsg = nseg
	    DO ii = nsg + 1, newseg
		DO nn = 1, nsg
		    IF ( isgnum ( ii ) .eq. nn ) THEN
C
C*			The temporary segment 'ii' was created from
C*			original segment 'nn'.  This will be the case 
C*			for at most one pass through the 'DO nn' loop.
C
			add = .false.
C
C*			Look for a change in the county or marine zone
C*			list.
C
			DO jj = 1, ncnty ( nn )
			    IF ( cpart ( jj, ii ) .ne. 
     +				 cpart ( jj, nn ) )  add = .true.
			END DO
C
			IF ( .not. add ) THEN
			    DO jj = 1, nzone ( nn )
			        IF ( zpart ( jj, ii ) .ne. 
     +				     zpart ( jj, nn ) )  add = .true.
			    END DO
			END IF
C
			IF ( add ) THEN
C
C*			    A change in the county or zone list was
C*			    found, so add a new segment.  (Note that 
C*			    nseg will always be less than or equal to 
C*			    ii, so no needed UGC data is being 
C*			    overwritten.)
C
			    nseg = nseg + 1
			    ivtec ( 1, nseg ) = ivtcnm ( ii )
			    ivtec ( 2, nseg ) = 0
			    ivtec ( 3, nseg ) = 0
			    ncnty ( nseg ) = ncnty ( ii )
			    nzone ( nseg ) = nzone ( ii )
			    DO jj = 1, ncnty ( nseg )
				cpart ( jj, nseg ) = cpart ( jj, ii )
			    END DO
			    DO jj = 1, nzone ( nseg )
				zpart ( jj, nseg ) = zpart ( jj, ii )
			    END DO
			    bkpstr ( nseg ) = bkpstr ( ii )
C
C*			    Drop the VTEC line from original segment.
C
			    IF ( ivtec ( 2, nn ) .eq. 
     +				 ivtec ( 1, nseg ) ) THEN
				ivtec ( 2, nn ) = 0
			      ELSE IF ( ivtec ( 3, nn ) .eq.
     +					ivtec ( 1, nseg ) ) THEN
				ivtec ( 3, nn ) = 0
			    END IF
			END IF
		    END IF
		END DO
	    END DO
C
C*	    Adjust ivtec to eliminate intervening zero values which
C*	    could have been introduced when a VTEC line was dropped from
C*	    the original segment.
C	
	    DO nn = 1, nsg
		IF ( ( ivtec ( 2, nn ) .eq. 0 ) .and.
     +		     ( ivtec ( 3, nn ) .ne. 0 ) ) THEN
		    ivtec ( 2, nn ) = ivtec ( 3, nn ) 
		    ivtec ( 3, nn ) = 0
		END IF
	    END DO
	END IF
C
C*	Move the UGC lists to the output strings, eliminating blank
C*	substrings.
C 
	DO ii = 1, nseg
            IF ( ncnty ( ii ) .gt. 0 ) 
     +      CALL GH_WWMV ( cpart ( 1, ii ), ncnty ( ii ), clist ( ii ),
     +			   lenc, ier )
C
            IF ( nzone ( ii ) .gt. 0 ) 
     +      CALL GH_WWMV ( zpart ( 1, ii ), nzone ( ii ), zlist ( ii ),
     +			   lenz, ier )
C
C*	    Check that there is at least one county or marine zone.
C
	    IF ( ( lenc .ge. 6 ) .or. ( lenz .ge. 6 ) ) THEN
		drop ( ii ) = .false.
	      ELSE
		drop ( ii ) = .true.
	    END IF
	END DO
C
C*	Check for segments which have been completely eliminated.
C
	ii = 1
	DO WHILE ( ii .le. nseg )
	    IF ( drop ( ii ) ) THEN
		nseg = nseg - 1
		DO jj = ii, nseg
		    drop ( jj )   = drop ( jj + 1 )
		    clist ( jj )  = clist ( jj + 1 )
		    zlist ( jj )  = zlist ( jj + 1 )
		    bkpstr ( jj ) = bkpstr ( jj + 1 )
		    DO kk = 1, 3
		        ivtec ( kk, jj ) = ivtec ( kk, jj + 1 )
		    END DO
		END DO
	      ELSE
	        ii = ii + 1
	    END IF
	END DO
C*
	RETURN
	END
