	SUBROUTINE GH_WWLN ( nsegin, ibkseg, iarea, ivtec, clist, zlist,
     +			     bkpstr, nsegot, wfostr, iret )
C************************************************************************
C* GH_WWLN                                                              *
C*									*
C* This subroutine uses the breakpoint numbers and initial VTEC action  *
C* and event codes to get the inputs for the breakpoint watch/warning   *
C* text message - the updated VTEC codes, UGC lists, breakpoint text    *
C* strings and WFO list.  This code is used for lists of breakpoints.   *
C*                                                                      *
C* GH_WWLN ( NSEGIN, IBKSEG, IAREA, IVTEC, CLIST, ZLIST, BKPSTR, NSEGOT,*
C*           WFOSTR, IRET )                                             *
C*                                                                      *
C* Input parameters:                                                    *
C* 	NSEGIN		INTEGER		Number of input segments        *
C*	IBKSEG(*)	INTEGER         Breakpoint numbers              *
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
C*					  4 = no breakpoints found      *
C*					  0 = normal return             *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	 2/05	From GH_WWIN                            *
C* D. Kidwell/NCEP	 2/05	Replaced INDEX chk on CHNULL w/ ST_RNUL *
C* S. Jacobs/NCEP	 2/05	Replaced CLO_FINDSTN w/ GH_FSTN		*
C* D. Kidwell/NCEP	 4/05	Added chk on KW; maxlen, info 128->256  *
C* D. Kidwell/NCEP	10/05	Added check for Dry Tortugas Island     *
C* M. Li/SAIC            1/06   Changed county codes to zone            *
C* m.gamazaychikov/SAIC 03/06   Made changes to account for WFO Key West*
C* m.gamazaychikov/SAIC 12/07   Add code to account for Hawaiian islnds	*
C* S. Jacobs/NCEP	 8/08	Added islands other than HI and VI	*
C* m.gamazaychikov/SAIC	09/08	Replaced GH_FSTN w/ CLO_FINDSTNW	*
C* s. gilbert           08/14   Fix bug with HI zone codes              *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C*
	PARAMETER	( NSEGMX = 50, NCZMX = 300, NWFOMX = 150 )
C*
        CHARACTER*(*)   clist (*), zlist (*), bkpstr (*), wfostr
	INTEGER		ivtec (3,*), ibkseg (*)
C*
	CHARACTER	cn (6)*5, mz (2)*4, coun (12)*6, zncode, znnum*3,
     +			mzon (12)*6, locnam*12, xid*9, xstate, info*256,
     +			bname*40, blat*20, blon*20, bktemp*53,locnm2*11,
     +			cpart (NCZMX,NSEGMX)*6, zpart (NCZMX,NSEGMX)*6,
     +			tmpl*53, wfo (NWFOMX)*3, wfonam*9, bst*10,
     +			action (3,NSEGMX)*3, cpref*3, bstat*10,
     +			jjid*9, infojj*256, bcntr*10, cnhib*5, cnhie*5,
     +                  cncurr*3
 	CHARACTER       tag*25, dirsym*160, tblnam*72, value*12,
     +                  keywest*3
	INTEGER		itype (3,NSEGMX), ncnty (NSEGMX), nzone (NSEGMX)
	LOGICAL		found, done, add, eyw, state
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
	IF ( iarea .eq. IOTHER ) THEN
	    cpref = 'VIC'
	  ELSE IF ( iarea .eq. IPRICO ) THEN
	      IF ( value .eq. 'COUNTY' ) THEN
	    	  cpref = 'PRC'
	        ELSE
		  cpref = 'PRZ'
	      END IF
	  ELSE
	    cpref = ' '
	END IF
C
C*	Loop through the segments.
C
	DO ii = 1, nseg
	    ifound = 0
	    indx   = indxbk ( iarea )
C
C*	    Find the index of this point in the breakpoint table.
C
	    DO WHILE ( ( ifound .lt. 1 ) .and.
     +		       ( indx .lt. indxbk ( iarea + 1 ) ) )
		IF ( ibkseg ( ii ) .eq. ibkseq ( indx ) ) THEN
		    indxb  = indx
		    ifound = ifound + 1
		END IF
		indx = indx + 1
	    END DO 
C
	    IF ( ifound .eq. 1 ) THEN
C
C*		Get the counties and marine zones from the last field of
C*		the breakpoint plotting table.
C
		jj    = indxb
	        icnty = 0
	        izone = 0
		found = .true.
		done  = .false.
		state = ( iarea .eq. IWATER ) .or.
     +                  ( ( iarea .eq. IOTHER ) .and.
     +                    ( bkstid ( indxb ) .eq. 'DRY_ISLE' ) ) 
C
C*		Get the UGCs for the segment.
C	
		DO WHILE ( .not. done )
		    IF ( state ) THEN
		        cn ( 1 ) = bktbch ( jj ) ( 3:7 )
		        cn ( 2 ) = bktbch ( jj ) ( 8:12 )
		        mz ( 1 ) = bktbch ( jj ) ( 13:16 )
		        mz ( 2 ) = bktbch ( jj ) ( 17:20 )
C
C*			Check for WFO Key West.
C
			IF ( bktbch (jj) ( :2 ) .eq. 'KW' ) eyw = .true.
			numpl = 2
C
C*		        Construct full 6 character UGC names for bodies 
C*			of water.
C
		        DO kk = 1, numpl
			    IF ( cn ( kk ) .ne. ' ' ) THEN
			      IF ( value .eq. 'COUNTY' ) THEN
			        coun ( kk ) = cn ( kk ) ( 1:2 ) // 'C'
     +			 	              // cn ( kk ) ( 3:5 )	
			      ELSE
				coun ( kk ) = cn ( kk ) ( 1:2 ) // 'Z'
     +                                        // cn ( kk ) ( 3:5 )
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
		      ELSE IF ( ( iarea .eq. IOTHER ) .or.
     +				( iarea .eq. IPRICO ) ) THEN
		        jjid = bkstid ( jj ) 
	    	        CALL ST_NULL ( jjid, jjid, lenjjid, ier )
	    	        CALL CLO_FINDSTNW (locnam, jjid, xstate,ischtp,
     +				           maxlen, nstns, infojj, ier )
		        CALL ST_RNUL ( infojj, infojj, leni, ier )
		        CALL ST_RMBL ( infojj(:leni),infojj,lenjj,ier)
		        CALL ST_GTST ( tmpl, 'CO>','<', infojj(:lenjj),
     +			       	bcntr, lenbcr, ier )
			CALL ST_GTST ( tmpl, 'ST>','<', infojj(:lenjj),
     +				bstat, lenst, ier )
                        IF ( bcntr (4:lenbcr) .eq. 'HI' ) THEN
                          cncurr = ' '
                          cnhib = bktbch ( jj ) (3:5)
                          cnhie = bktbch ( jj ) (6:8)
                          CALL ST_NUMB ( cnhib, icnhib, ier)
                          CALL ST_NUMB ( cnhie, icnhie, ier)
                          ncnhi = icnhie - icnhib + 1
                          DO kk = 1, ncnhi
                            icurr = icnhib + ( kk -1 )
                            IF (icurr .gt. 0  .and. icurr .le. 9) THEN
                                cpref = 'Z00'
                                CALL ST_LSTR (cpref, lencp, ier)
                              ELSE IF ( icurr .ge. 10 ) THEN
                                cpref = 'Z0'
                                CALL ST_RMBL (cpref, cpref, lencp, ier)
                            ENDIF 
                            CALL ST_INCH (icurr, cncurr, ier)
                            CALL ST_RMBL (cncurr, cncurr, lencn, ier)
                            coun(kk)='HI'//cpref(:lencp)//cncurr(:lencn)
                            mzon ( kk )= ' '
                          END DO
			  numpl = ncnhi
                        ELSE
			  IF ( bcntr(4:lenbcr) .ne. 'VI' ) THEN
			      IF ( value .eq. 'COUNTY' ) THEN
				  cpref = bstat(4:lenst) // 'C'
			      ELSE
				  cpref = bstat(4:lenst) // 'Z'
			      ENDIF
			  END IF
			  numpl = 6
			  ibeg  = 3
			  DO kk = 1, numpl
			    iend = ibeg + 2
			    cn ( kk ) = bktbch ( jj ) ( ibeg:iend )
			    IF ( cn ( kk ) .ne. ' ' ) THEN
				coun ( kk ) = cpref // cn ( kk ) ( 1:3 )
			      ELSE
				coun ( kk ) = ' '
			    END IF
			    mzon ( kk )= ' '
			    ibeg = iend + 1
			  END DO
                        END IF
		    END IF
C
C*		    Add the UGCs to the county and zone lists.
C
		    DO kk = 1, numpl
			IF ( coun ( kk ) .ne. ' ' ) THEN
			    icnty = icnty + 1
			    cpart ( icnty, ii ) = coun ( kk )
			END IF
C
			IF ( mzon ( kk ) .ne. ' ' ) THEN
			    izone = izone + 1
			    zpart ( izone, ii ) = mzon ( kk )
			END IF
		    END DO
		    jj = jj + 1
		    IF ( ( MOD ( ibkpri ( jj ), 10 ) .eq. 0 ) .or.
     +			 ( bktbch ( jj ) ( 3:20 ) .eq. ' ' )  .or.
     +			 ( jj .eq. indxbk ( iarea + 1 ) ) )   THEN 
		        done = .true.
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
C*		Get the breakpoint name and location for the point.
C
		xid = bkstid ( indxb ) 
	    	CALL ST_NULL ( xid, xid, lenxid, ier )
	    	CALL CLO_FINDSTNW ( locnam, xid, xstate, ischtp,
     +				    maxlen, nstns, info, ier )
		CALL ST_RNUL ( info, info, leni, ier )
		CALL ST_RMBL ( info ( :leni ), info, lens, ier )
		CALL ST_GTST ( tmpl, 'NAME>','<', info ( :lens ),
     +			       bname, len1, ier )
		CALL ST_GTST ( tmpl, 'ST>','<',  info ( :lens ),
     +			       bst, len4, ier )
		CALL ST_GTST ( tmpl, 'LAT>','<',  info ( :lens ),
     +			       blat, len2, ier )
		CALL ST_GTST ( tmpl, 'LON>','<',  info ( :lens ),
     +			       blon, len3, ier )
		IF ( state ) THEN
                        bname  = bname (6:len1) // '-' // bst(4:len4)
		  ELSE
		        bname  = bname ( 6:len1 )
		END IF
C
C*              Replace the '_' with '-' in breakpoint name string
C
		CALL ST_LSTR ( bname, lenbn, ier )
                ipos = 100
                DO WHILE ( ipos .ne. 0 )
                       CALL ST_RPST ( bname(:lenbn), '_','-', 
     +                                ipos, bname, ier)
                END DO
		bktemp = bname ( :36 ) // blat ( 5:len2 ) //
     +			     'N ' // blon ( 6:len3 ) // 'W'
		CALL ST_LSTR ( bktemp, lenk, ier )
		bkpstr ( ii ) = bktemp ( :lenk )
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
C*	Check whether EYW is to be added to the WFO string.
C
	IF ( eyw ) THEN
	    IF ( ifo .lt. NWFOMX ) ifo = ifo + 1
            wfo ( ifo ) = keywest(1:3)
        END IF
      
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
