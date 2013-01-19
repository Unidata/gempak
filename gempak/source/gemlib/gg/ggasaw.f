        SUBROUTINE GG_ASAW ( inumb, dattim, stptim, iret) 
C************************************************************************
C* GG_ASAW        							*
C*        								*
C* This subroutine searches the raw SAW for watch inumb and retrieves	*
C* the record and amends it to reflect the extension in time if it has	*
C* not already been amended with that extension time.		 	* 
C*        								*
C* GG_ASAW ( INUMB, DATTIM, STPTIM, IRET )				* 
C* Input parameters:        						*
C*        INUMB		INTEGER		Current watch number 		*
C*        DATTIM 	CHAR*		User specified time(YYMMDD/HHMM)*
C*        STPTIM 	CHAR*		Watch end time - GEMPAK format  *
C*        								*
C* Output parameters:        						*
C*        IRET		INTEGER		Return code			*
C*        								*
C**        								*
C* Log:        								*
C* F. J. Yen/NCEP	 1/07	Created					*
C* F. J. Yen/NCEP	 1/07	Added corrections. Enhanced error msg.	*
C*				Cleaned up.				*
C* F. J. Yen/NCEP	 4/08	Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
        INCLUDE		'BRIDGE.PRM'
C*
        CHARACTER*(*)	dattim, stptim
C* ---------------------------------------------------------------
	CHARACTER*(*)	sawbuf*(DCMXBF), rcntsw*(DCMXBF)
        CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +        		rec*125, tfile*128, dattm4*20,
     +                  stime*20, flstrt*160, flend*160 
	CHARACTER	isstm*6, amdcde*4, amdrcn*4, wtchnm*5, cnumb*5
	CHARACTER	amdlet*26, corcde*4, sawstp*6, tswstp*20
        CHARACTER	gisstm*20, gisrcn*20, gissti*20
	CHARACTER*1	chunpr(3), rcnlet, amdlr
        CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
        INTEGER		itarr(5), jtarr(5), itype
        LOGICAL		done, found, unpr, eob
	DATA		amdlet /' ABCDEFGHIJKLMNOPQRSTUVWXX'/
	DATA		chunpr / CHCTLA, CHCR, CHCTLC /
C
C-----------------------------------------------------------------------
	iret   = 0
C
C*      Begin searching the raw SAW files for watch inumb.
C
C*      Search raw SAW directory for latest reports for watch number
C*	based on similar criteria that GG_WACT used.
C
C
	filnam = 'SAW_RAW'
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii,
     +			 ion, ihb, mnb, iha, mna, mstrct, idtmch, ier )
        CALL ST_RNUL ( path, path, lens, ier )
        CALL ST_RNUL ( templ, templ, lens, ier )
	CALL ST_LSTR ( path, lenp, ier )
        nexp   = MXNMFL
	iorder = -1
	CALL FL_SCND (path, templ, iorder, nexp, files, nfile, ier)
C
C*	Check for the last file requested by the user.
C
	CALL ST_LCUC ( dattim, dattim, ier )
	itype = 1 
	CALL CSS_GTIM ( itype, cdttm, ier )
	CALL TI_STAN ( dattim, cdttm, dattm2, ier )
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'TI', ier, dattim, ierr )
	    iret = ier
	    RETURN
	END IF	    
	CALL FL_MNAM ( dattm2, templ, filnam, ier )
C
C*	Find the earliest file to start searching. 
C*	Subtract 12 hours from the time given.
C
	minuts = 720
C
C*	Find end file name
C
	CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_ITOC ( itarr, stime, ier )
	CALL FL_MNAM ( stime, templ, flend, ier )
        CALL ST_LSTR ( flend, len, ier )
C
C*	Find start file name
C
	CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
	CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*	Get 4-digit year to compare dates.
C
	CALL TI_DTM4 ( dattm2, dattm4, ier )
C
C*	Decode each file until the end time is reached.
C
	rcnlet = ' '
	amdlr  = ' '
	nwtch = 0
        jj = 1
	done  = .false.
	ifl = 1
	gisrcn = ' '
	gisstm = ' '
	lenrcn = 0
	CALL ST_INCH (inumb, cnumb, ier)
	DO WHILE ( ( ifl .le. nfile) .and. ( .not. done ) )
	    IF ( ifl .gt. nfile ) THEN
 		done = .true.
	      ELSE
	        IF ( ( files(ifl) .ge. flstrt ) .and.
     +                ( files(ifl) .le. flend ) ) THEN
		    tfile = path(:lenp) // '/' // files(ifl)
		    CALL FL_SOPN ( tfile, lunf, ier )
		    iostat = 0
		    ib = -1
		    corcde = ' '
		    DO WHILE  ( iostat .eq. 0 )
		      READ ( lunf, 2, IOSTAT = iostat ) rec
   2		      FORMAT (A)
		      IF ( iostat .eq. 0 ) THEN
			CALL ST_LSTR (rec,lnr, ierb)
C
C*			Find WMO Header (Start of SAW)
C
			nxb = INDEX ( rec, 'WWUS30 KWNS ' )
			IF ( nxb .ne. 0 .and. ib .eq. -1 ) THEN
     			  ib = 0
			  isstm = rec (13:18)
C
C*			  Update issue time in header
C
			  rec (13:18) = dattim(5:6) // 
     +					dattim(8:11)
			  iwtnm = -1 
		          IF ( lnr .eq. 20 .or. lnr .eq. 21 ) THEN
C
C*			      No BBB field, so initial SAW.
C*			      Set amendment code to AAA
C
		              amdcde = 'AAA'
C
C*			      Save the code letter to use when
C*			      there are corrections
C
			      amdlr = ' '
			      rec (19:24) = ' ' // amdcde (1:3)
     +					            // CHCR // CHCR
			      lnr = lnr + 4
		           ELSE IF ( rec (20:21) .eq. 'AA' .or.
     +				    rec (20:21) .eq. 'CC' ) THEN
C
C*			      Get amendment code & change to the next code;
C
			      ib = 0
			      IF ( rec (20:21) .eq. 'AA' ) THEN
C
C*				Amendment SAW
C
		                amdcde = rec (20:22)
C
C*			        Save the code letter to use when
C*			        there are corrections
C
				amdlr = amdcde (3:3)
			       ELSE
C
C*				Correction SAW
C
				corcde = rec (20:22)
				amdcde = 'AA' // rcnlet
			      END IF
		              nn = 1
		              found = .false.
		              DO WHILE ( .not. found .and.
     +				         nn .le. 25 )
		                IF ( amdcde(3:3) .eq.
     +					amdlet (nn:nn) ) THEN
			          amdcde(3:3) = amdlet(nn+1:nn+1)
			          rec (20:22) = amdcde(1:3)
				  found = .true.
			         ELSE
				  nn = nn + 1
			        END IF
		              END DO
			      IF ( nn .gt. 25 ) THEN
C
C*			        Amendment letter is invalid.
C*				(SAWs should not have Y or Z. Use X.)
C
			        numerr = -26
			        CALL ER_WMSG ( 'GG', numerr, amdcde(3:3),
     +					     ier)
			        amdcde(3:3) = amdlet(26:26)
                                rec (20:22) = amdcde (1:3)
			      END IF
			    ELSE
C
C*			      Not initial, amendment, or correction SAW.
C*			      SAWs should not have other BBBs
C
		              ib = -1
		          END IF
		          CAll ST_NULL ( amdcde, amdcde, ln, ier )
			END IF
			IF ( ib .ge. 0 ) THEN
			    ndxspc = INDEX ( rec, 'SPC AWW' )
			    IF ( ndxspc .gt. 0 ) THEN
			      rec (8+ndxspc:13+ndxspc) = dattim (5:6) //
     +					                 dattim (8:11)
C
C*			      If 'CCC' is in the 'SPC AWW' line,
C*			      then remove 'CCC'.
C
			      ndxc = INDEX ( rec, 'CC' )
			      IF ( ndxc .gt. 8 ) THEN
			        jj = ndxc - 1
				je = jj + 4
				DO WHILE ( je .le. lnr )
				  rec (jj:jj) = rec (je:je)
				  jj = jj + 1
				  je = je + 1
				END DO
				lnr = lnr - 4
			      END IF
			     ELSE IF ( rec (1:3) .eq. 'WW ' ) THEN
C
C*			      Testing rec(1:3) assures that
C*			      'REPLACES WW ' and the 'SPC AWW ' lines
C*			      are not taken to be the 'WW ' line
C
			      iendnm = INDEX (rec(4:8), ' ')
			      IF ( iendnm .le. 1 ) THEN
C
C*				Error in finding watch number
C
  				numerr = -28
				CALL ER_WMSG ( 'GG', numerr, rec(:25),
     +					       ier )
			        ib = -1
				corcde = ' '
			      END IF
			      wtchnm = rec (4:2+iendnm)

			      CALL ST_NUMB (wtchnm, iwtnm, ier)
			      IF (iwtnm .eq. inumb) THEN
C
C*				Get the stop time of the SAW
C*				and check if we want it.
C
				IF ( rec (lnr-2:lnr-2) .eq.'Z' ) THEN
				  sawstp = rec(lnr-8:lnr-3)
C
C*				  Use the system time in array itarr
C*				  and the SAW stop time sawstp to make
C*				  a GEMPAK time tswstp.
C
				  CALL ST_INTG (sawstp, iswstp, ier)
				  IF ( ier .eq. 0 ) THEN
				    idy = iswstp / 10000	
				    ihr = mod(iswstp,10000) / 100
				    imn = mod(iswstp,100)
				    CALL DC_ITIM (itarr, idy, ihr, imn,
     +						  jtarr, ier)
			            CALL TI_ITOC (jtarr, tswstp, ier )
C
C*				    Modify SAW with new stop time, if
C*				    it is later.  
C
				    CALL TI_DIFF ( stptim, tswstp,
     +						   mdif, ier )
C
C*				    If SAW stop time is less than 
C*				    stop time, then skip (not EXT)
C
				    IF ( mdif .gt. 0 ) THEN
C
C*				      Convert issue time isstm to
C*				      GEMPAK gissti to compare issue
C*				      time with most recent.
C
				      CALL ST_INTG (isstm, iisstm, ier)
				      IF (ier.eq.0) THEN
					idy = iisstm / 10000	
					ihr = mod(iisstm,10000) / 100
					imn = mod(iisstm,100)
					CALL DC_ITIM (itarr, idy, ihr,
     +					      imn, jtarr, ier)
				      END IF
				      CALL TI_ITOC (jtarr,
     +						          gissti, ier )
C
C*				      Check for issue time before dattim
C
				      CALL TI_DIFF ( dattim, gissti,
     +						     mdif, ier)
				      IF ( mdif .lt. 0 ) THEN
					ib = -1
					corcde = ' '
				       ELSE
					If (gisrcn .ne. ' ') THEN
					  CALL TI_DIFF ( gisrcn,
     +					         gissti, mdif, ier )
					END IF
C
C*					Sometimes the issue time
C*					is the same, but is a correction
C*					So, test is for mdif .le. 0 and
C*					not mdif .lt. 0
C
					If (gisrcn .eq. ' ' .or.
     +					       mdif .le. 0 ) THEN
					  gisstm = gissti
					  rec (lnr-8:lnr-3) =
     +					       stptim(5:6) //
     +						        stptim(8:11)
					 ELSE
C
C*					  Skip this SAW
C
					  ib = -1
					  corcde = ' '
					END IF
				      END IF
				     ELSE
C
C*				      Skip this SAW
C
				      ib = -1 
				      corcde = ' '
				    END IF
				  END IF
				 ELSE
			          jxb = INDEX ( rec, 'CANCELLED' )
				  IF ( jxb .gt. 0 ) THEN
C
C*				    Skip over cancellations
C
				    ib = -1
				    corcde = ' '
				   ELSE
C
C*				    Error in finding stop time
C
				    numerr = -29
				    CALL ER_WMSG ('GG', numerr, rec(:25),
     +						  ier)
				  END IF
				END IF
			      END IF
			      IF (iwtnm .ne. inumb) THEN
				ib = -1
				corcde = ' '
			      END IF
			    END IF
			    IF ( ib .ge. 0 ) THEN
C
C*			      Eliminate control char: CHCTLA, CHCTLC, and
C*			      carriage return CHCR and store in sawbuf.
C
			      iend = lnr
			      ic = ib + 1
			      eob = .false.
			      DO ij = 1, iend
			        ik = 1
			        unpr = .false.
			        DO WHILE ( ik .le. 3 .and. .not. unpr)
				  IF ( rec(ij:ij) .eq. chunpr (ik) ) THEN
				    IF ( ic .eq. 1 ) THEN
   				      sawbuf(1:1) = ' ' 
				      ic = ic + 1
				     ELSE IF ( ik .eq. 3) THEN
				      eob = .true.
   				      ic = ic - 1
				    END IF
				    unpr = .true.
				  END IF
				  ik = ik + 1
			        END DO
			        IF ( .not. unpr ) THEN
				  sawbuf(ic:ic) = rec(ij:ij)
				  ic = ic + 1
			        END IF
			      END DO
   			      IF ( .not. eob ) THEN
   			        sawbuf (ic:ic) = CHLF
   			       ELSE
   			        ic = ic - 1
   			      END IF
			      ib = ic
			    END IF
			    IF ( iwtnm .eq. inumb .and.
     +			         rec(1:1) .eq. CHCTLC .and.
     +			         .not. done ) THEN
C
C*			      Completed storing a bulletin.
C*			      Save it for possible most recent.
C   
			      lenrcn = ib - 1
C
C*			      If it is a correction, do not save the
C*			      most recent amendment letter, in case
C*			      there are other corrections.
C
			      IF ( corcde .eq. ' ' ) THEN
			        rcnlet = amdlr
			      END IF
			      amdrcn = amdcde
			      gisrcn = gisstm
			      rcntsw (:lenrcn) = sawbuf (:lenrcn)
			      ib = -1
			      corcde = ' '
			    END IF
		        END IF
		      END IF
		    END DO 
	        END IF
	    END IF
	    ifl = ifl + 1
	END DO
	    IF ( lenrcn .gt. 0 ) THEN
		rcntsw ( lenrcn+1:lenrcn+1 ) = CHNULL 
		CALL VFASAW (rcntsw, inumb, amdrcn, ier)
	      ELSE
C
C*		No prior SAW found for watch
C
		numerr = -27
		CALL ER_WMSG ( 'GG', numerr, cnumb, ier )
	    END IF
C*        
        RETURN
        END
