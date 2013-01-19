	SUBROUTINE GH_WWIN ( nsegin, ibkseg, iarea, ivtec, clist, zlist,
     +			     bkpstr, nsegot, wfostr, iret )
C************************************************************************
C* GH_WWIN                                                              *
C*									*
C* This subroutine uses the breakpoint numbers and initial VTEC action  *
C* and event codes to get the inputs for the breakpoint watch/warning   *
C* text message - the updated VTEC codes, UGC lists, breakpoint text    *
C* strings and WFO list.  This code is used for breakpoint pairs.       *
C*                                                                      *
C* GH_WWIN ( NSEGIN, IBKSEG, IAREA, IVTEC, CLIST, ZLIST, BKPSTR,        *
C*	     NSEGOT, WFOSTR, IRET )                                     *
C*                                                                      *
C* Input parameters:                                                    *
C* 	NSEGIN		INTEGER		Number of input segments        *
C*	IBKSEG(2,*)	INTEGER         Breakpoint number pairs         *
C*	IAREA		INTEGER		Geographic area designator      *
C*                                                                      *
C* Input and output parameters:                                         *
C*	IVTEC(3,*)	INTEGER		VTEC action and event codes     *
C*									*
C* Output parameters:                                                   *
C*	CLIST(*)	CHAR*		County UGCs by segment          *
C*	ZLIST(*)	CHAR*		Marine zone UGCs by segment     *
C*	BKPSTR(*)	CHAR*		Bkpt text strings by segment    *
C*      NSEGOT		INTEGER		Number of output segments       *
C*	WFOSTR		CHAR*		List of WFOs for ATTN string    *
C*	IRET		INTEGER		Return code                     *
C*					  4 = no breakpoints for storm  *
C*					  0 = normal return             *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	11/03						*
C* D. Kidwell/NCEP	 1/04	Replaced '--' for state with '  '       *
C* D. Kidwell/NCEP	 3/04	Checked for dup WFOs before storing     *
C* D. Kidwell/NCEP	 4/04	Allowed mult. instances of = UGC border *
C* D. Kidwell/NCEP	 8/04	Used closest official bkpt to get UGCs  *
C* m.gamazaychikov/SAIC	01/05	Made changes for county FLC087		*
C*				replaced '_' with '-' in bkpoint names  *
C*				changed the breakpoints table name	*
C* D. Kidwell/NCEP	 2/05	Added call to GH_WWCL, updated iarea def*
C* D. Kidwell/NCEP	 2/05	Replaced INDEX chk on CHNULL w/ ST_RNUL *
C* S. Jacobs/NCEP	 2/05	Replaced CLO_FINDSTN w/ GH_FSTN		*
C* D. Kidwell/NCEP	 4/05	Added arg iarea; maxlen, info 128->256  *
C* D. Kidwell/NCEP	 9/05	Modified fmt of IUSGEC, IKEYS bkpt strng*
C* M. Li/SAIC            1/06   Changed county codes to zone            *
C* m.gamazaychikov/SAIC	03/06	Made changes to account for WFO Key West*
C* m.gamazaychikov/SAIC	09/08	Replaced GH_FSTN w/ CLO_FINDSTNW	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C*
	PARAMETER	( NSEGMX = 50, NCZMX = 300, NWFOMX = 150 )
C*
        CHARACTER*(*)   clist (*), zlist (*), bkpstr (*), wfostr
	INTEGER		ivtec (3,*), ibkseg (2,*)
C*
	CHARACTER	cn (2)*5, mz (2)*4, coun (2)*6, zncode, znnum*3,
     +			mzon (2)*6, locnam*12, xid*9, xstate, info*256,
     +			bname*40, blat*20, blon*20, bktemp*53,locnm2*11,
     +			cpart (NCZMX,NSEGMX)*6, zpart (NCZMX,NSEGMX)*6,
     +			tmpl*53, wfo (NWFOMX)*3, wfonam*9, bst*10,
     +			action (3,NSEGMX)*3, cnbeg*10, cnend*10, 
     +			mzbeg*8, mzend*8, isep
	CHARACTER       tag*25, dirsym*160, tblnam*72, value*12,
     +                  keywest*3
	INTEGER		itype (3,NSEGMX), ncnty (NSEGMX), 
     +			nzone (NSEGMX), jndx (2)
	LOGICAL		found, done, add, c2beg, c2end, z2beg, z2end, 
     +			std (2), eyw, mfl
	REAL		obklat (2), obklon (2), dist (2)
C-----------------------------------------------------------------------
	iret = 0
	ischtp = 1
	maxlen = 256
	value  = ' '
	keywest= ' '
	tmpl = '<STID><STNM><NAME><ST><CO><LAT><LON><ELV><PRI><COL10>'
C
	DO ii = 1, NSEGMX
	    clist ( ii )  = ' '
	    zlist ( ii )  = ' '
	    bkpstr ( ii ) = ' '
	END DO
	wfostr = ' '
	found  = .false.
	eyw    = .false.
	mfl    = .false.
	nseg   = MIN0 ( nsegin, NSEGMX )
C
C*      Read the value of tag TCV_ZONE in prefs.tbl.
C
        tag     = 'TCV_ZONE'
        tblnam  = 'prefs.tbl'
        dirsym  = 'config'
C
        CALL ST_NULL ( tblnam, tblnam, lens, ier )
        CALL ST_NULL ( dirsym, dirsym, lens, ier )
        CALL ST_NULL ( tag, tag, lens, ier)
        CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier1 )
        CALL ST_RNUL ( value, value, lens, ier )
C
C*      Read the value of tag TCV_KEYWEST in prefs.tbl.
C
        tag     = 'TCV_KEYWEST'
        CALL CTB_RDPRF ( tblnam, dirsym, tag, keywest, ier1 )
        IF ( ier1 .ne. 0 ) keywest = 'KEY'
        CALL ST_RNUL ( keywest, keywest, lens, ier )
C
C*	Translate the ivtec values to storm types and actions.
C
	DO ii = 1, nseg
	    DO jj = 1, 3
		IF ( ivtec ( jj, ii ) .gt. 0 ) THEN
		    itype ( jj, ii ) = MOD ( ivtec ( jj, ii ), 4 )
		    IF ( itype ( jj, ii ) .eq. 0 ) itype ( jj, ii ) = 4
		    iact = ( ivtec ( jj, ii ) + 3 ) / 4
		    IF ( iact .eq. 1 ) THEN
			action ( jj, ii ) = 'NEW'
		      ELSE IF ( iact .eq. 2 ) THEN
			action ( jj, ii ) = 'CON'
		      ELSE
			action ( jj, ii ) = 'CAN'
		    END IF
	  	  ELSE
		    itype ( jj, ii ) = IMISSD
		    action ( jj, ii ) = ' '
		END IF
	    END DO
	END DO
C
C*	Initialize the clo library values.
C
	CALL CLO_INIT ( ier ) 
	locnam = 'TCA_BKPTS'
	CALL ST_NULL ( locnam, locnam, lenloc, ier )
	xstate = CHNULL
	IF ( value .eq. 'COUNTY' ) THEN
            locnm2 = 'COUNTY'
          ELSE
            locnm2 = 'LAND_ZONES'
        END IF
	CALL ST_NULL ( locnm2, locnm2, lenloc, ier )
	ifo = 0
C
C*	Loop through the segments.
C
	DO ii = 1, nseg
	    ibkbeg = ibkseg ( 1, ii )
	    ibkend = ibkseg ( 2, ii ) 
	    ifound = 0
	    indx   = indxbk ( iarea )
C
C*	    Find the beginning and ending indices in the breakpoint
C*	    table.
C
	    DO WHILE ( ( ifound .lt. 2 ) .and.
     +		       ( indx .lt. indxbk ( iarea + 1 ) ) )
		
		IF ( ibkbeg .eq. ibkseq ( indx ) ) THEN
		    indxb  = indx
		    ifound = ifound + 1
		  ELSE IF ( ibkend .eq. ibkseq ( indx ) ) THEN
		    indxe  = indx
		    ifound = ifound + 1
		END IF
		indx = indx + 1
	    END DO 
C
	    IF ( ifound .eq. 2 ) THEN
C
C*		Check for standard/supplemental breakpoints, and if
C*		found, use the UGCs for the closest official breakpoint.
C*		Only areas 1 (US Gulf of Mexico and East coast) and 9 
C*		(Florida Keys) have the supplemental breakpoints.
C
		IF ( MOD ( ibkpri ( indxb ), 10 ) .eq. 0 ) THEN
		    jndxb = indxb
		    std ( 1 ) = .false.
		  ELSE
		    jndx ( 1 ) = indxb
		    std ( 1 )  = .true.
		END IF
C
		IF ( MOD ( ibkpri ( indxe ), 10 ) .eq. 0 ) THEN
		    jndxe = indxe
		    std ( 2 ) = .false.
		  ELSE
		    jndx ( 2 ) = indxe
		    std ( 2 )  = .true.
		END IF
C
		DO jj = 1, 2
		    IF ( std ( jj ) ) THEN
C
C*			This is a supplemental breakpoint.  Find the
C*			official breakpoint which comes just before it
C*			in the table.
C
			done = .false.
			kk = jndx ( jj )
			DO WHILE ( .not. done ) 
			    kk = kk - 1
 			    IF ( kk .lt. indxbk ( iarea ) ) THEN
				done  = .true.
				joff1 = 0
			      ELSE
			        IF ( MOD ( ibkpri ( kk ), 10 ) .eq. 0 )
     +				     THEN 
			            done  = .true.
				    joff1 = kk
				END IF
			    END IF
			END DO
C
C*			Find the official breakpoint which comes just 
C*			after it in the table.
C
			done = .false.
			kk = jndx ( jj )
			DO WHILE ( .not. done ) 
			    kk = kk + 1
 			    IF ( kk .ge. indxbk ( iarea + 1 ) ) THEN
				done  = .true.
				joff2 = 0
			      ELSE
			        IF ( MOD ( ibkpri ( kk ), 10 ) .eq. 0 )
     +				     THEN 
			            done  = .true.
				    joff2 = kk
				END IF
			    END IF
			END DO
C
C*			Find the closer of the two official breakpoints.
C
			IF ( joff1 .eq. 0 ) THEN
			    jclos = joff2
			  ELSE IF ( joff2 .eq. 0 ) THEN
			    jclos = joff1
			  ELSE
			    obklat ( 1 ) = bklat ( joff1 )		
			    obklon ( 1 ) = bklon ( joff1 )
			    obklat ( 2 ) = bklat ( joff2 )
			    obklon ( 2 ) = bklon ( joff2 ) 
			    CALL CLO_DIST ( bklat ( jndx ( jj ) ),
     +					    bklon ( jndx ( jj ) ), 2,
     +					    obklat, obklon, dist, ier )
			    IF ( dist ( 1 ) .le. dist ( 2 ) ) THEN
				jclos = joff1
			      ELSE
				jclos = joff2
			    END IF
			END IF
C
C*			Use the closer official breakpoint to get the
C*			counties and zones.
C
		    	IF ( jj .eq. 1 ) THEN
			    jndxb = jclos
			  ELSE
			    jndxe = jclos
			END IF
		    END IF
		END DO
C
C*		Get the counties and marine zones from the last field of
C*		the breakpoint plotting table.
C
	        icnty = 0
	        izone = 0
		found = .true.
C
C*		Check for a segment which begins or ends on a UGC border
C*		(county or marine zone).
C
		c2beg = .false.
		c2end = .false.
		z2beg = .false.
		z2end = .false.
		cnbeg = bktbch ( jndxb ) ( 3:12 )
		cnend = bktbch ( jndxe ) ( 3:12 )
		mzbeg = bktbch ( jndxb ) ( 13:20 )
		mzend = bktbch ( jndxe ) ( 13:20 )
		IF ( INDEX ( cnbeg, ' ' ) .eq. 0 ) c2beg = .true.
		IF ( INDEX ( cnend, ' ' ) .eq. 0 ) c2end = .true.
		IF ( INDEX ( mzbeg, ' ' ) .eq. 0 ) z2beg = .true.
		IF ( INDEX ( mzend, ' ' ) .eq. 0 ) z2end = .true.
C
C*		Loop over the breakpoints in the segment.
C	
		DO jj = jndxb, jndxe
		    cn ( 1 ) = bktbch ( jj ) ( 3:7 )
		    cn ( 2 ) = bktbch ( jj ) ( 8:12 )
		    mz ( 1 ) = bktbch ( jj ) ( 13:16 )
		    mz ( 2 ) = bktbch ( jj ) ( 17:20 )
C
C*		    Check if FLC087 - Key West WFO or Miami WFO
C
                    IF ( bktbch(jj)(:2) .eq. 'KW' ) THEN
                        eyw = .true.
                     ELSE IF ( bktbch(jj)(:2) .eq. 'MI' ) THEN
                        mfl = .true.
                    END IF
C
C*		    Construct full 6 character UGC names.
C
		    DO kk = 1, 2
			IF ( cn ( kk ) .ne. ' ' ) THEN
			  IF ( value .eq. 'COUNTY' ) THEN
			    coun ( kk ) = cn ( kk ) ( 1:2 ) // 'C' //
     +			 	          cn ( kk ) ( 3:5 )	
			  ELSE
			    coun ( kk ) = cn ( kk ) ( 1:2 ) // 'Z' //
     +                                    cn ( kk ) ( 3:5 )     
			  END IF
			ELSE
			    coun ( kk ) = ' '
			END IF
			IF ( mz ( kk ) .ne. ' ' ) THEN
			    zncode = mz ( kk ) ( 1:1 )
			    znnum  = mz ( kk ) ( 2:4 )
			    IF ( zncode .eq. 'G' ) THEN
				mzon ( kk ) = 'GMZ' // znnum
			      ELSE IF ( zncode .eq. 'M' ) THEN
				mzon ( kk ) = 'AMZ' // znnum
			      ELSE IF ( zncode .eq. 'N' ) THEN
				mzon ( kk ) = 'ANZ' // znnum
			    END IF
			  ELSE
			    mzon ( kk ) = ' '
			END IF
		    END DO
C
C*		    Add the UGCs to the county and zone lists.
C
		    IF ( jj .eq. jndxb ) THEN
			IF ( coun ( 2 ) .ne. ' ' ) THEN
			    icnty = icnty + 1
			    cpart ( icnty, ii ) = coun ( 2 )
			  ELSE IF ( coun ( 1 ) .ne. ' ' ) THEN
			    icnty = icnty + 1
			    cpart ( icnty, ii ) = coun ( 1 )
			END IF
			IF ( mzon ( 2 ) .ne. ' ' ) THEN
			    izone = izone + 1
			    zpart ( izone, ii ) = mzon ( 2 )
			  ELSE IF ( mzon ( 1 ) .ne. ' ' ) THEN
			    izone = izone + 1
			    zpart ( izone, ii ) = mzon ( 1 ) 
			END IF
C
		      ELSE IF ( jj .eq. jndxe ) THEN
			IF ( coun ( 1 ) .ne. ' ' ) THEN
			    icnty = icnty + 1
			    cpart ( icnty, ii ) = coun ( 1 )
			END IF
			IF ( mzon ( 1 ) .ne. ' ' ) THEN
			    izone = izone + 1
			    zpart ( izone, ii ) = mzon ( 1 )
			END IF
C
		      ELSE
			ll = 1
			mm = 2
			IF ( c2beg .and. ( cnbeg .eq. 
     +				   bktbch ( jj ) ( 3:12 ) ) ) ll = 2
			IF ( c2end .and. ( cnend .eq. 
     +				   bktbch ( jj ) ( 3:12 ) ) ) mm = 1
			DO kk = ll, mm
			    IF ( coun ( kk ) .ne. ' ' ) THEN
			        icnty = icnty + 1
			        cpart ( icnty, ii ) = coun ( kk )
			    END IF
			END DO
C
			ll = 1
			mm = 2
			IF ( z2beg .and. ( mzbeg .eq. 
     +				   bktbch ( jj ) ( 13:20 ) ) ) ll = 2
			IF ( z2end .and. ( mzend .eq. 
     +				   bktbch ( jj ) ( 13:20 ) ) ) mm = 1
			DO kk = ll, mm
			    IF ( mzon ( kk ) .ne. ' ' ) THEN
			        izone = izone + 1
			        zpart ( izone, ii ) = mzon ( kk )
			    END IF
			END DO
		    END IF
		END DO
C
C*	        Sort the UGCs, eliminating duplicates.
C
		IF ( icnty .gt. 1 ) THEN
	            CALL ST_SORT ( 2, icnty, cpart ( 1, ii ), 
     +				   ncnty ( ii ), cpart ( 1, ii ), ier )
		  ELSE
		    ncnty ( ii ) = icnty
		END IF
		IF ( izone .gt. 1 ) THEN
	            CALL ST_SORT ( 2, izone, zpart ( 1, ii ), 
     +				   nzone ( ii ), zpart ( 1, ii ), ier )
		  ELSE
		    nzone ( ii ) = izone
		END IF
C
C*		Get the breakpoint name and location for the two
C*		endpoints.  Preserve the exact format of the breakpoint
C*		name for later use, for areas IUSGEC and IKEYS only.
C
		xid = bkstid ( indxb ) 
		IF ( ( iarea .eq. IUSGEC ) .or. ( iarea .eq. IKEYS ) )
     +		       THEN
		    isep = '+'
		  ELSE
		    isep = '-'
		END IF
		DO ipt = 1, 2
		    IF (ipt .eq. 2 ) xid = bkstid ( indxe )
	    	    CALL ST_NULL ( xid, xid, lenxid, ier )
	    	    CALL CLO_FINDSTNW ( locnam, xid, xstate, ischtp,
     +				        maxlen, nstns, info, ier )
		    CALL ST_RNUL ( info, info, leni, ier )
		    CALL ST_RMBL ( info ( :leni ), info, lens, ier )
		    CALL ST_GTST ( tmpl, 'NAME>','<', info ( :lens ),
     +				   bname, len1, ier )
		    CALL ST_GTST ( tmpl, 'ST>','<',  info ( :lens ),
     +				   bst, len4, ier )
		    CALL ST_GTST ( tmpl, 'LAT>','<',  info ( :lens ),
     +				   blat, len2, ier )
		    CALL ST_GTST ( tmpl, 'LON>','<',  info ( :lens ),
     +				   blon, len3, ier )
		    IF ( bst ( 4:5 ) .ne. '--' ) THEN
                        IF ( bst(4:len4) .eq. bname(len1-1:len1) ) THEN
		            bname  = bname (6:len1) 
                          ELSE
                            bname  = bname(6:len1) // isep //bst(4:len4)
                        END IF
		      ELSE
		        bname  = bname ( 6:len1 )
		    END IF
		    IF ( ( iarea .ne. IUSGEC ) .and.
     +			 ( iarea .ne. IKEYS  ) ) THEN
C
C*                      Replace the '_' with '-' in breakpoint name 
C*			string.
C
		        CALL ST_LSTR ( bname, lenbn, ier )
                        ipos = 100
                        DO WHILE ( ipos .ne. 0 )
                            CALL ST_RPST ( bname(:lenbn), '_','-', 
     +                                     ipos, bname, ier)
                        END DO
		    END IF
		    bktemp = bname ( :36 ) // blat ( 5:len2 ) //
     +			     'N ' // blon ( 6:len3 ) // 'W'
		    CALL ST_LSTR ( bktemp, lenk, ier )
		    IF ( ipt .eq. 1 ) THEN
		        bkpstr ( ii ) = bktemp ( :lenk )
		      ELSE
			CALL ST_LSTR ( bkpstr ( ii ), lenb, ier )
			bkpstr ( ii ) = bkpstr ( ii ) ( :lenb ) //
     +				        ';' // bktemp ( :lenk )
		    END IF
		END DO
C
C*	    	Get the WFOs for the ATTN string.
C
		DO jj = 1, ncnty ( ii )
		    xid = cpart ( jj, ii ) 
	    	    CALL ST_NULL ( xid, xid, lenxid, ier )
	    	    CALL CLO_FINDSTNW ( locnm2, xid, xstate, ischtp,
     +				        maxlen, nstns, info, ier )
		    IF ( ier .ge. 0 ) THEN
		        CALL ST_RNUL ( info, info, leni, ier )
		        CALL ST_GTST ( tmpl, 'COL10>','<', 
     +				     info ( :leni ), wfonam, len3, ier )
C
C*                      If needed, replace MFL with KEY/EYW in ATTN string
C
                        IF ( ( xid(:6) .eq. 'FLC087'   .or. 
     +                         xid(:6) .eq. 'FLZ075' ) .and. eyw ) THEN
                           IF ( .not. mfl ) THEN
                              wfonam (7:9) = keywest(1:3)
                             ELSE
                              IF ( ifo .lt. NWFOMX ) ifo = ifo + 1
                              wfo (ifo ) =  keywest(1:3)
                           END IF 
                        END IF 
C
C*			Check to see if this WFO is already present.
C
			add = .true.
			DO kk = 1, ifo
			    IF ( wfo ( kk ) .eq. wfonam ( 7:9 ) ) 
     +				 add = .false.
			END DO
			IF ( add ) THEN
			    IF ( ifo .lt. NWFOMX ) ifo = ifo + 1
			    wfo ( ifo ) = wfonam ( 7:9 )
			END IF
		    END IF
		END DO
C
	    END IF
	END DO
C
C*	Check for no breakpoints found.
C
	IF ( .not. found ) THEN
	    iret = 4
	    RETURN
	END IF
C
C*	Clean up the UGCs by eliminating redundant segments and adding
C*	new segments as necessary.
C		    
	CALL GH_WWCL ( nseg, ivtec, ncnty, nzone, action, itype, cpart,
     +		       zpart, bkpstr, clist, zlist, ier )
	nsegot = nseg
	IF ( nsegot .le. 0 ) iret = 4
C
C*	Create the WFO string.
C
	IF ( ifo .gt. 1 ) THEN
	    CALL ST_SORT ( 2, ifo, wfo, nwfo, wfo, ier )
	  ELSE
	    nwfo = ifo
	END IF
	CALL ST_LSTC ( wfo, nwfo, ';', wfostr, ier )
C*
	RETURN
	END
