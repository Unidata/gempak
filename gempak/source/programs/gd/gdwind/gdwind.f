	PROGRAM  GDWIND
C************************************************************************
C* GDWIND								*
C*									*
C* This program draws wind barbs or arrows for a gridded wind field.	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/85						*
C* M. desJardins/GSFC	 8/88	Cleaned up				*
C* M. desJardins/GSFC	 5/89	Write vector name in title & display	*
C* M. desJardins/GSFC	11/89	Change GR_FILE to DG_OFIL		*
C* K. Brill/GSC		12/89	Added call to DG_AREA			*
C* J. Shadid/GSFC	 7/90	Added LATLON				*
C* K. Brill/NMC		 9/90	Change PFUN to PFUNC in ER_WMSG		*
C* J. Whistler/SSAI	 4/91	Added SCALE				*
C* J. Whistler/SSAI	 4/91	Changed GDWTIT to GR_TITL		*
C* J. Whistler/SSAI	 4/91	Changed title and ttlstr to *72 from 48	*
C* J. Whistler/SSAI	 5/91	Added reference arrow			*
C* K. Brill/NMC		07/91	Convert to KNOTS for K in WIND		*
C* K. Brill/NMC		07/91	Remove leftover debug write		*
C* M. desJardins/NMC	10/91	Changed panel to *48			*
C* K. Brill/NMC		01/92	Changed POINTS to SKIP			*
C* K. Brill/NMC		02/92	Changed determination of dmax and dmin	*
C* S. Jacobs/EAI	11/92	Added call to GMESG and 'shrttl'	*
C* K. Brill/NMC		01/93	Get 'N' (no ref arrow) from WINUNI	*
C* S. Jacobs/EAI	 3/93	Added buffering of wind symbols;	*
C*				  Changed calc of knots to use PR_MSKN	*
C* L. Sager/NMC 	 6/93	Added wind-plot staggering		*
C* L. Sager/NMC		 7/93	Added REFVEC parameter			*
C* L. Sager/NMC		 8/93	Replaced GR_SCAL with IN_SCAL & GR_VSCL	*
C* S. Jacobs/EAI	 9/93	Modified short title			*
C* S. Jacobs/EAI	10/93	Changed call to IN_RVEC and plotting of	*
C*				   reference arrow			*
C* S. Jacobs/EAI	 2/94	Added COLADD flag to DG_OFIL		*
C* S. Jacobs/NMC	 3/94	Added satellite display routines	*
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC	 6/94	DEVICE*12 --> *72			*
C* L. Williams/EAI	 7/94	Removed GDWUPD and added shrttl		*
C* P. Bruehl/Unidata	 8/94	Added animation				*
C* J. Cowie/COMET	 8/94	Added sat image looping			*
C* S. Jacobs/NMC	 8/94	Changed IN_RVEC to GG_RVEC		*
C* M. desJardins/NMC	 8/94	Added ST_FLST, GR_TLST			*
C* M. desJardins/NMC	 8/94	Removed GDWPNT; Ignore non-existent	*
C*				   vector error				*
C* S. Jacobs/NMC	 9/94	Clean up; Reorganized plotting,		*
C*				   skipping and scaling			*
C* S. Jacobs/NMC	10/94	Fixed calling sequence for DG_AREA	*
C* S. Jacobs/NMC	10/94	Added check for only one time in list	*
C* J. Cowie/COMET	 1/95	Added SATFIL & RADFIL			*
C* S. Jacobs/NMC	 2/95	Moved IN_TEXT to before setting proj	*
C* S. Jacobs/NMC	 3/95	Removed check for only one time in list	*
C* S. Jacobs/NMC	 5/95	Changed scaling to use sub area		*
C* J. Cowie/COMET	 8/95	Changed GSATIM to IM_DROP, add IM_LUTF,	*
C*				use idrpfl				*
C* D.W.Plummer/NCEP	11/95	Added LUTFIL as a parameter		*
C* D. Keiser/GSC	12/95	Added STNPLT as a parameter		*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to GDWDSP		*
C* K. Tyle/GSC		 8/96	Eliminated refs. to TAE in comments	*
C* D.W.Plummer/NCEP	10/96	Added latitudinal dependant skip	*
C* S. Jacobs/NCEP	11/96	Added check for MXLOOP number of times	*
C* S. Jacobs/NCEP	 1/97	Changed the order of IM_DROP & IM_LUTF	*
C* S. Maxwell/GSC	 7/97	Increased input character length	*
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* T. Lee/GSC		 8/99	Fixed latitudinal thinning indices	*
C* T. Lee/GSC		11/00	Changed calling sequence of GR_FIXA	*
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL		*
C* T. Lee/GSC		 7/01	Processed multiple files		*
C* K. Brill/HPC		 8/02	CALL DG_SUBG not GR_GALM & DG_AREA	*
C* m.gamazaychikov/SAIC 10/02	Put the call to IM_LUTF after GCLEAR	*
C* K. Brill/HPC		11/02	Eliminate use of the subset flag	*
C* K. Brill/HPC		12/02	Added IJSKIP; COLADD=false in DG_MFIL	*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* M. Li/SAIC		11/03	Added color bar for images		*
C* R. Tian/SAIC		11/03	Added nuflg to DG_INTL call		*
C* R. Tian/SAIC		2/04	Removed nuflg from DG_INTL call		*
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* T. Piper/SAIC	10/04	Moved GG_PANL after GCLEAR		*
C* R. Tian/SAIC		10/04	Changes for time/file mngmnt		*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), map*(LLMXLN), mscale*(LLMXLN),
     +			title*(LLMXLN),	wind*(LLMXLN), refvec*(LLMXLN)
	CHARACTER	gdatim*(LLMXLN), gvect*(LLMXLN),
     +			glevel*(LLMXLN), gvcord*(LLMXLN)
	CHARACTER	device*(LLMXLN), proj*(LLMXLN), panel*(LLMXLN),
     +			garea*(LLMXLN), skip*(LLMXLN)
	CHARACTER	text*(LLMXLN), scale*(LLMXLN), latlon*(LLMXLN),
     +			shrttl*(LLMXLN)
	CHARACTER	satfil*(LLMXLN), radfil*(LLMXLN),
     +			lutfil*(LLMXLN), stnplt*(LLMXLN),
     +			ijskip*(LLMXLN), imcbar*(LLMXLN)
	LOGICAL		clear
C*
	REAL		grid1 (LLMXGD), grid2 (LLMXGD)
	REAL		sped  (LLMXGD), drct  (LLMXGD)
	REAL		fi (100), fj (100), s (100), d (100)
  
	INTEGER		level (2)
	CHARACTER	parmu*12, parmv*12, pfunc*72
	CHARACTER	gv*72, time(2)*20, winuni*1, wintyp*1
	CHARACTER	ttlstr*72,  garout*72, defstr*12, uprj*72
	CHARACTER	imgfls (MXLOOP)*132, timfnd*36
	CHARACTER	prjout*72
	LOGICAL		respnd, done, proces, gottm
	LOGICAL         first, novect, latt
	INTEGER		iskplt (2)
	REAL		alatsk (181)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDWIND', ier )
C
C*  Initialize GEMPLT.
C
	    CALL GG_INIT ( 1, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*  Initialize grid library common area grdcmn.cmn
C 
		CALL GD_INIT  ( ier )
C
C*  Initialize the DG library.
C
		CALL DG_INTL ( ier )
		done = .false.
	    ELSE
		done = .true.
	    END IF
	ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in GEMPAK parameters and draw winds.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the interface.
C
	  CALL GDWINP  ( gdfile, gdatim, glevel, gvcord, gvect, map,
     +			 mscale, title, device, proj, garea, clear,
     +			 wind, refvec, skip, panel, text, imcbar,
     +			 scale, latlon, satfil, radfil, lutfil,
     +			 stnplt, ijskip, iperr )
C
C*	  Exit if there is an error.
C
	  IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	   ELSE
C
C*	  Process the GDFILE input.
C
	    CALL DG_NFIL ( gdfile, ' ', ier )
	    IF ( ier .ne. 0 ) THEN
		CALL ER_WMSG ( 'DG', ier, ' ', irr )
		proces = .false.
	    END IF
C
C*	  Process the GDATTIM input; setup the time server.
C
	    CALL DG_NDTM ( gdatim, ier )
	    IF ( ier .ne. 0 ) THEN
		CALL ER_WMSG ( 'DG', ier, gdatim, irr )
		proces = .false.
	    END IF
C
C*	    Set up the graphics device.
C
	    CALL ST_LCUC  ( gvect, gvect, ier )
	    CALL GG_SDEV  ( device, iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
C*	    Set the attributes that do not vary within the time loop.
C
	    IF  ( proces )  THEN
C
C*		Set text and scaling attributes.
C
		CALL IN_TEXT  ( text,  iret )
		CALL IN_SCAL ( scale, iscale, iscalv, iret )
C
C*		Set wind attributes and arrow head size.
C
		CALL IN_WIND  ( wind, wintyp, winuni, icolor, ier )
C
C*		Check for points to skip.
C
		CALL IN_SKIP  ( skip, iskpcn, iskplt, ier )
		ixinc = iskplt (1)
		iyinc = iskplt (2)
		IF  ( iyinc .ge. 0 ) THEN
C
C*		    Normal skip processing.
C
		    latt = .false.
C
C*		    Check for stagger ( ixinc .lt. 0 ).
C
		    IF ( ixinc .ge. 0 )  THEN
			ixstep = ixinc + 1
			istag  = 0
		    ELSE
			ixstep = - ixinc + 1
			istag  = ixstep / 2
		    END IF    
		    iystep = iyinc + 1
C
		  ELSE IF ( iyinc .lt. 0 )  THEN
C
C*		    Latitudinal thinning processing ( iyinc .lt. 0 ).
C*
C*		    For each latitude degree, figure out the number of
C*		    wind barbs that would fit around a latitude circle,
C*		    given the currect size of a wind barb (rszwb) and 
C*		    the graphical distance along the latitude line (dd).
C*		    The variable ixinc is not used.
C
		    latt = .true.
		    istag = 0
		    ixstep = 1
		    iystep = - iyinc 
C
		CALL GQSYSZ  ( rxszmk, ryxzmk, rxsztx, rysztx,
     +				   rxszwb, ryszwb, iret )
		    rszwb = SQRT ( rxszwb*rxszwb + ryszwb*ryszwb )
		    CALL GQBND   ( 'M', rlatmn, rlonmn, dlatmx, dlonmx, 
     +				   iret )
C
C*		    Loop through each latitude.
C
		    DO  ilt = -90, 90
			ilt1 = ilt + 91.
			CALL GTRANS ( 'M', 'N', 1, float(ilt), rlonmn, 
     +				      xout1, yout1, iret)
			CALL GTRANS ( 'M', 'N', 1, float(ilt), rlonmn+1, 
     +				      xout2, yout2, iret)
			dd = SQRT ( (xout1-xout2)**2+(yout1-yout2)**2 )
			dd = dd * 360.
			alatsk( ilt1 ) = dd / rszwb
		    END DO
C
C*		    Set north and south pole values such that only
C*		    one wind barb gets plotted there.
C
		    alatsk(  1) = 0.
		    alatsk(181) = 0.
C
		END IF    
	    END IF
C
C*	    Loop over times.
C
	    itime = 1
	    gottm = proces
	    first = .true.
	    DO WHILE  ( gottm )
C
C*	      Get the next time to process from time server.
C
		CALL DG_NTIM ( .true., .true., time, gottm, ier )
		proces = ( ier .eq. 0 .and. gottm )
		IF ( ier .ne. 0 ) THEN
		    ier = 2
		    CALL ER_WMSG ( 'GDWIND', ier, time(1), irr )
		END IF
		CALL TG_DUAL ( time, timfnd, ier )
C
C*		Set the map projection and graphics area.
C
		IF  ( proces )  THEN
		    CALL ST_LCUC ( proj, uprj, ier )
		    IF  ( ( uprj (1:3) .ne. 'SAT' ) .and.
     +		          ( uprj (1:3) .ne. 'RAD' ) ) THEN
			CALL DG_FIXA  ( garea, proj, garout, 
     +					prjout, ier )
		      ELSE
			prjout = proj
			garout = garea
		    END IF
C
C*		    If projection=SAT or RAD, check for multiple image 
C*		    files.
C
		    IF ( uprj (1:3) .eq. 'SAT' ) THEN
			CALL ST_FLST ( satfil, ';', ' ', MXLOOP, imgfls,
     +				       numimg, iret )
		      ELSE IF ( uprj (1:3) .eq. 'RAD' ) THEN
			CALL ST_FLST ( radfil, ';', ' ', MXLOOP, imgfls,
     +				       numimg, iret )
		    END IF
C
C*		    Set map projection
C
		    CALL GG_MAPS ( prjout, garout, imgfls (1), idrpfl, 
     +				   iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*		Set the projection, garea for SAT or RAD (for each plot)
C
		IF ( proces ) THEN
		    IF  ( first )  THEN
			CALL GSTANM ( iret )
		      ELSE 
			CALL GSPLOT ( iret )
C
C*			Set map proj for all images after 1st.
C
			IF ( uprj (1:3) .eq. 'SAT' .or. 
     +			     uprj (1:3) .eq. 'RAD' )
     +			  CALL GG_MAPS ( prjout, garout, imgfls (itime),
     +					 idrpfl, iret )
		    END IF
		END IF
C
C*	 	Set up subset grid that covers graphics area.
C
		IF  ( proces )  THEN
		    CALL DG_SUBG  ( ijskip, ix1, iy1, ix2, iy2, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'DG', iret, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*		Compute the requested vector.
C
		IF  ( proces )  THEN
C
		    CALL DG_VECT ( timfnd, glevel, gvcord,
     +				   gvect, pfunc, grid1, grid2, kx, ky,
     +				   time, level, ivcord, parmu, parmv, 
     +				   iret )
C
C*		    Check for error message.
C
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'DG', iret, pfunc, ier )
			novect = .true.
			proces = .false.
			time (1) = timfnd
			time (2) = ' '
		    ELSE
			novect = .false.
C
C*			Set the use flag to calculate the magnitude,
C*			now that we know kx, ky.
C
			npt = kx *  ky
C
C*			Calculate the wind speed and direction.
C
			CALL PD_SPED  ( grid1, grid2, npt,
     +					sped, iret )
			CALL PD_DRCT  ( grid1, grid2, npt,
     +					drct, iret )
			CALL ST_LCUC  ( gvect, gv, ier )
C
C*			Convert sped to knots, if necessary.
C
			iposk = INDEX ( gv, 'KNTV' )
			IF  ( winuni .eq. 'K' .and. iposk .eq. 0 )
     +			    CALL PD_MSKN ( sped, npt,
     +					   sped, iret )
		    END IF
		 END IF
C*
		 IF  ( proces )  THEN
C
C*		    Scale the data.
C
		    IF  ( first )  THEN
			IF  ( wintyp .ne. 'B' )  THEN
			    CALL GR_SSCL  ( iscalv, kx, ky, ix1, iy1,
     +					    ix2, iy2, sped, dmin, dmax,
     +					    ier )
			  ELSE 
C
C*			    Don't scale wind barbs
C
		    iscalv = 0
			    CALL GR_STAT ( sped, kx, ky, ix1, iy1,
     +					   ix2, iy2, dmin, dmax,
     +					   davg, ddev, ier )
			END IF
		      ELSE IF  ( iscalv .ne. 0 )  THEN
			CALL GR_SSCL  ( iscalv, kx, ky, ix1, iy1,
     +					ix2, iy2, sped, dmin, dmax,
     +					ier )
		    END IF
		 END IF
C
C*		 Give user a chance to exit.
C
		 IF  ( proces )  THEN
		    CALL GDWDSP  ( gdfile, time, level, ivcord, parmu, 
     +				   wind, garea, iscalv, dmin, dmax,
     +				   first, iret )
C
C*		  Stop looping if user requests exist.
C
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
			gottm = .false.
		    END IF
C
C*		  Set first to false upon first successful plot.
C
		    first = .false.
		 END IF
C
C*		Draw wind symbols.
C
		IF  ( proces ) THEN
C
C*		    Clear screen if requested, and set the panel.
C
		    IF  ( clear )  CALL GCLEAR ( ier )
		    CALL GG_PANL ( panel, ier )
C
C*		    Apply LUT file.
C
		    CALL IM_LUTF ( lutfil, ier )
C
C*		    Display satellite image, if desired.
C
		    IF ( ( idrpfl .eq. 1 ) .or.
     +			 ( idrpfl .eq. 0 .and. clear ) ) THEN
     			CALL IM_DROP ( iret)
			CALL IM_CBAR ( imcbar, iret )
		    END IF
C
C*		    Draw map, lat/lon lines, and station ID/marker.
C
		    CALL GG_MAP  ( map, ier )
		    CALL GG_LTLN ( latlon, ier )
		    CALL GG_SPLT ( stnplt, ier )
		    CALL GG_SCAL ( mscale, ier )
C
C*		    Draw winds.
C
		    IF  ( ( icolor .ne. 0 ) .and.
     +			  ( .not. novect ) ) THEN
			CALL GSCOLR ( icolor, ier )
			npts   = 0
			ixstrt = ix1
			DO  j = iy1, iy2, iystep
			  iy = ( j - 1 ) * kx
C*
			  IF ( latt )  THEN
C
C*			      Processing for latitudinal thinning.
C*
C*			      Figure out what the closest latitude is and
C*			      use that value to index into the alatsk array
C*			      to get skip increment (ixstep).  Also compute
C*			      istag which is the indent stagger for the
C*			      next row.
C
			      CALL GTRANS('G', 'M', 1, float(ix1), 
     +				  FLOAT(j), alt, aln, iret)
			      indexx = NINT(alt) + 91
			      IF ( alatsk(indexx) .ne. 0. )  THEN
			   	  ixstep = INT(kx / alatsk(indexx)) + 1
			        ELSE
				  ixstep = ix2
			      END IF
			      istag = ixstep / 2
			  END IF
C
			  DO  i = ixstrt, ix2, ixstep
			    ixy = iy + i
			    IF ( (.not.ERMISS(sped(ixy)) ) .and.
     +				 (.not.ERMISS(drct(ixy)) ) ) THEN
				npts = npts + 1
				fi ( npts ) = FLOAT (i)
				fj ( npts ) = FLOAT (j)
				s ( npts )  = sped ( ixy )
				d ( npts )  = drct ( ixy )
			    END IF
			    IF  ( ( npts .ge. 100 ) .or.
     +				  ( ( i + ixstep .gt. ix2 ) .and.
     +				    ( j + iystep .gt. iy2 ) ) )  THEN
				IF  ( wintyp .eq. 'B' ) THEN
				    CALL GBARB ( 'G', npts, fi, fj,
     +						 s, d, ier )
				  ELSE
				    CALL GARRW ( 'G', npts, fi, fj,
     +						 s, d, ier )
				END IF
				npts = 0
			    END IF
			  END DO
			  IF  ( ixstrt .eq. ix1 )  THEN
				ixstrt = ixstrt + istag
			    ELSE
				ixstrt = ix1
			  END IF
			END DO
C
C*			Plot reference arrow if arrows were requested.
C*			Parse the parameter REFVEC and draw the arrow.
C
			IF  ( ( wintyp .eq. 'A' ) .and.
     +			      ( winuni .ne. 'N' ) )  THEN
			    IF  ( winuni .eq. 'K' )  defstr = 'kts'
			    IF  ( winuni .eq. 'M' )  defstr = 'm/s'
			    CALL GG_RVEC  ( refvec, defstr, ier )
			END IF
C
C*			Write title.
C
			CALL IN_TITL  ( title, 0, ititl, linttl,
     +					ttlstr, ier )
			CALL GR_TITL  ( ttlstr, time, .true., level,
     +					ivcord, parmu(2:), iscalv, ' ',
     +					ttlstr, shrttl, iret )
			IF  ( clear )  CALL GMESG  ( shrttl, ier )
			IF  ( ititl .ne. 0 )  THEN
			    CALL GSCOLR   ( ititl, ier )
			    CALL GG_WSTR  ( ttlstr, linttl, ier )
			END IF
		    END IF
C
C*		    Flush the graphics buffers.
C
		    CALL GEPLOT  ( ier )
C
C*		  Increment itime only if plot was successful.
C
		    itime = itime + 1
		END IF
	      END DO
C
	      CALL GENANM ( iret )
C
C*	      Prompt for next wind plot to be done.
C
	      CALL IP_DYNM  ( done, ier )
	    END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDWIND', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the interface.
C
	CALL GENDP  ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
