	PROGRAM  GDFRZL
C************************************************************************
C* PROGRAM GDFRZL							*
C*									*
C* This program generates GFA FZLVL elements from a grid.		*
C**									*
C* Log:									*
C* B. Yin/SAIC           4/06	Modified from gdcntr.f			*
C* B. Yin/SAIC           4/06	Convert tag and stat to upper case	*
C* B. Yin/SAIC           5/06	Fix the seg fault during second run	*
C* C. Bailey/HPC	 6/06	Added contour text labeling		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C* D.W.Plummer/NCEP     12/06   Added GCLOSP to close temp VGF file     *
C* B. Yin/SAIC		 1/07	Added lvlincr ang gvcord for vg2frzl	*
C* B. Yin/SAIC		 2/07	Read the settings table                 *
C* S. Gilbert/NCEP	 5/07	Removed call to GCNTLN			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* L. Hinson/AWC        06/08   Add Cycle                               *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER	gdfile*(LLMXLN), title*(LLMXLN), clrbar*(LLMXLN),
     +			hlsym*(LLMXLN), gdatim*(LLMXLN), gfunc*(LLMXLN),
     +			glevel*(LLMXLN), gvcord*(LLMXLN), map*(LLMXLN),
     +			cint*(LLMXLN), line*(LLMXLN), device*(LLMXLN),
     +			proj*(LLMXLN), garea*(LLMXLN), panel*(LLMXLN),
     +			text*(LLMXLN), scale*(LLMXLN), latlon*(LLMXLN),
     +			contur*(LLMXLN), skip*(LLMXLN), fint*(LLMXLN),
     +			fline*(LLMXLN), ctype*(LLMXLN), hilo*(LLMXLN),
     +			shrttl*(LLMXLN), satfil*(LLMXLN),imcbar*(LLMXLN),
     +			radfil*(LLMXLN), lutfil*(LLMXLN),
     +			ijskip*(LLMXLN), stnplt*(LLMXLN),
     +			mscale*(LLMXLN), cycle*(LLMXLN), fhr*(LLMXLN), 
     +                  tag*(LLMXLN), stat*(LLMXLN), lvlincr*(LLMXLN)
C
	CHARACTER	time (2)*20, ttlstr*72, garout*72, parm*12,
     +			pfunc*72, prjout*72, timfnd*36, uprj*72,
     +			imgfls(MXLOOP)*132, carr (4)*72, filnam*72,
     +			dev*72, tmpfil*72, inputf*72, clbl(LLCLEV)*24
C
	REAL		grid (LLMXGD), subgrd (LLMXGD), clvl (LLCLEV),
     +			flvl (LLCLEV)
C
	INTEGER		level (2), icolor (LLCLEV), lintyp (LLCLEV),
     +			linwid (LLCLEV), linlbl (LLCLEV),
     +			ifcolr (LLCLEV), iflabl (LLCLEV),
     +			ifltyp (LLCLEV), iswind (2), nchar
C
	LOGICAL		misflg, cflag, lflag, sflag, bflag, fflag, 
     +			nflag, gottm, first, newlev, newgrd, clear,
     +			respnd, done, proces, scflag
C----------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT ('GDFRZL', ier)
C
C*      Initialize GEMPLT.
C
	    CALL GG_INIT  ( 1, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
		CALL GD_INIT  ( ier )
C
C*      Initialize the DG library.
C
		CALL DG_INTL ( ier )
C
C*      Read the settings table.           
C
		CALL CES_RTBL ( iret )
		done = .false.
	    ELSE
		done = .true.
	    END IF
	ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in TAE parameters and draw contours.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL GDNINPFZ ( gdfile, gdatim, glevel, gvcord, gfunc, cint,
     +			line, map, mscale, title, device, proj, garea,
     +			clear, panel, text, imcbar, scale, latlon,
     +			contur, skip, fint, fline, ctype, hilo, hlsym,
     +			clrbar, satfil, radfil, lutfil, stnplt, ijskip,
     +                  cycle, fhr, tag, stat, lvlincr,
     +			iperr )
C
C*	  Exit if there is an error.
C
	  IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	   ELSE
C
C*	    Process the GDFILE input.
C
	    CALL DG_NFIL ( gdfile, ' ', ier )
	    IF ( ier .ne. 0 ) THEN
		CALL ER_WMSG ( 'DG', ier, ' ', irr )
		proces = .false.
	    END IF
C
C*	    Process the GDATTIM input; setup the time server.
C
	    CALL DG_NDTM ( gdatim, ier )
	    IF ( ier .ne. 0 ) THEN
		CALL ER_WMSG ( 'DG', ier, gdatim, irr )
		proces = .false.
	    END IF
C
C*	    Get the vg file name
C
	    CALL ST_CLST ( device, '|', ' ', 4, carr, num, ier )
C
	    filnam = carr (2)
	    IF  ( filnam .eq. ' ' )  THEN
		filnam = 'vgf.vgf'
	    END IF
C
	    CALL ST_LCUC ( carr (1), dev, ier )
	    IF  ( dev .ne. 'VG'  )  THEN
		ier = -16
		CALL ER_WMSG ( 'GDFRZL', ier, ' ', irr )
		proces = .false.
	    END IF
C
C*	    Set up the graphics device.
C*	    Change the vg file name to make sure reset the device.
C
	    IF ( tmpfil .eq. 'gdfrzl_tmp.vgf' ) THEN
	       tmpfil = 'gdfrzl_tmp0.vgf'
	     ELSE
	       tmpfil = 'gdfrzl_tmp.vgf'
	    END IF
C
	    device = 'vg|' // tmpfil
	    CALL GG_SDEV  ( device, iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
C*	    Set the attributes that do not vary within the time loop.
C
	    IF  ( proces )  THEN
C
C*	    	Set the text attributes, especially the size,
C*	    	before setting the margins.
C
		CALL IN_TEXT  ( text, ier )
C
C*		Set contour attributes; get contour skip factor.
C
		CALL IN_CONT  ( contur, ier )
		CALL IN_SKIP  ( skip, iskpxy, iswind, ier )
C
C*		Get contouring type.
C
		CALL IN_CTYP  ( ctype, nflag, lflag, sflag, bflag, 
     +				fflag, ier )
		IF  ( lflag .or. sflag .or. bflag .or. nflag )  THEN
		    cflag = .true.
		  ELSE
		    cflag = .false.
		END IF
C
C*		Set scaling.
C
     	   	CALL IN_SCAL ( scale, iscale, iscalv, iret )
C
	    END IF
C
C*	    Loop over times.
C
	    itime = 1
	    gottm = proces
	    first = .true.
	    DO WHILE  ( gottm )
C
C*		Get the next time to process from time server.
C
		CALL DG_NTIM ( .true., .true., time, gottm, ier )
		proces = ( ier .eq. 0 .and. gottm )
		IF ( ier .ne. 0 ) THEN
		    ier = 2
		    CALL ER_WMSG ( 'GDFRZL', ier, time(1), irr )
		END IF
		CALL TG_DUAL ( time, timfnd, ier )
C
C*		Set the map projection and graphics area.
C
		IF  ( proces )  THEN
		    CALL ST_LCUC ( proj, uprj, ier )
		    IF  ( ( uprj (1:3) .ne. 'SAT' ) .and. 
     +			  ( uprj (1:3) .ne. 'RAD' ) )  THEN
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
		    IF ( uprj (1:3) .eq. 'SAT' )  THEN
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
		ifound = 0
		newgrd = .false.
C
C*		Set the projection, garea for SAT or RAD (for each plot)
C
		IF ( proces ) THEN
		    IF ( first ) THEN
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
C*		Setup the grid subset that covers the graphics area.
C
		IF  ( proces )  THEN
		    CALL DG_SUBG ( ijskip, ix1, iy1, ix2, iy2, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG ( 'DG', iret, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*		Compute the requested grid.  If the requested grid
C*		cannot be computed, continue.
C
		IF  ( proces )  THEN
		    CALL DG_GRID ( timfnd, glevel, gvcord, gfunc,
     +				   pfunc, grid, kx, ky, time, level, 
     +				   ivcord, parm, igderr )
		    IF  ( igderr .ne. 0 )  THEN
			CALL ER_WMSG  ( 'DG', igderr, pfunc, ier )
			time (1) = timfnd
			time (2) = ' '
			proces = .false.
		      ELSE
			ifound = ifound + 1
			newgrd = ( ifound .eq. 1 )
		    END IF
		END IF
C
		IF  ( proces )  THEN
C
C*		  Define scaling.
C
		  CALL GR_SSCL ( iscale, kx, ky, ix1, iy1, ix2, iy2, 
     +				 grid, dmin, dmax, iret )
C
C*		  Define contour levels and characteristics.
C*		  The contour level is only set for the first time.
C
		  IF ( cflag .and. ( first .or. newgrd ) ) THEN
		      CALL IN_INTC ( cint, dmin, dmax, clvl, nclvl,
     +				     clbl, rint, cmin, cmax, iret )
		      IF  ( iret .ne. 0 )  THEN
			nclvl = 0
			rint  = 0.
		      END IF
		      CALL IN_LINE ( line, clvl, nclvl, icolor, 
     +				     lintyp, linwid, linlbl,
     +				     smooth, filter, scflag, iret )
C
C*		      Check for duplicate contours & sort contours
C
		      CALL GR_NLEV ( nclvl, clvl, clbl, icolor, lintyp,
     +				     linwid, linlbl, iret ) 
C
C*		      If there is not a list in LINE and the contour
C*		      interval is a constant, different contour levels
C*		      may be selected for each time.
C
		      isemic = INDEX ( line, ';' )

		      IF ( (rint .eq. 0) .or. (isemic .gt. 0) ) THEN
			  newlev = .false.
			ELSE
			  newlev = .true.
		      END IF
		    ELSE IF ( newlev ) THEN
C
C*		      Compute new levels for this time.
C
		      CALL GR_CLVL ( LLCLEV, cmin, cmax, rint, dmin,
     +				     dmax, nclvl, clvl, rcint, iret )
		      IF ( iret .ne. 0 )  nclvl = 0
C*
		      CALL IN_LINE ( line, clvl, nclvl, icolor, 
     +				     lintyp, linwid, linlbl,
     +				     smooth, filter, scflag, iret )
		  END IF
C
C*		  Convert contour levels to labels so GFA FZLVL tags are 
C*		  generated correctly
C
		  DO i = 1, nclvl
		    CALL GR_LABL(clvl(i), 0, 0, clbl(i), nchar, ier)
		  END DO
C
C*		  Process color fill contours.
C
		  IF ( fflag .and.  ( first .or. newgrd ) ) THEN
		      CALL IN_FILL  ( fint, fline, dmin, dmax, flvl,
     +				      nflvl, rfint, fmin, fmax, 
     +				      ifcolr, ifltyp, iflabl, iret )
		      IF  ( iret .ne. 0 )  THEN
			  CALL ER_WMSG ( 'IN', iret, ' ', ier )
		      END IF   
		  END IF
		END IF
C
C*		Give user a chance to exit.
C
		IF  ( proces )  THEN
	  	  CALL GDNDSP ( map, gdfile, time, level, ivcord,
     +		 		parm, garea, iscale, dmin, dmax, 
     +				cflag, nclvl, clvl, icolor, lintyp, 
     +				linwid, linlbl, fflag, nflvl, flvl, 
     +				ifcolr, ifltyp, first, iret )
C
C*  		  Stop looping if user requests exist.
C
		  IF  ( iret .ne. 0 )  THEN
		    proces = .false.
		    gottm = .false.
		  END IF
C
C*  		  Set first to false upon first successful plot.
C
		  first = .false.
	        END IF
C
C*	        Draw contours.
C
	        IF  ( proces )  THEN
C
C*		  Clear the screen if requested, and set the panel.
C
	 	  IF  ( clear )  CALL GCLEAR ( ier )
		  CALL GG_PANL  ( panel, iret )
C
C*	          Apply LUT file
C
		  CALL IM_LUTF ( lutfil, ier )
C
C*		  Display satellite image, if desired.
C
		  IF ( ( idrpfl .eq. 1 ) .or.
     +		       ( idrpfl .eq. 0 .and. clear ) ) THEN
     		       CALL IM_DROP ( iret)
		       CALL IM_CBAR ( imcbar, iret )
		  END IF
C
C*		  Draw contours.
C
		  IF  ( cflag .or. fflag )  THEN
		    misflg = .false.
		    CALL GR_SUBX ( kx, ky, grid, ix1, iy1, ix2, iy2, 
     +				   iskpxy, misflg, kxsub, kysub, subgrd,
     +				   ioffx, ioffy, iskip, ier )
C
		    IF  ( fflag )  THEN
		        CALL GCFILL  ( kxsub, kysub, subgrd, ioffx, 
     +				       ioffy, iskip, nflvl, flvl, 
     +				       ifcolr, iflabl, ifltyp, iret )
		    IF  ( iret .ne. 0 )  CALL ER_WMSG ( 'GEMPLT',
     +						iret, ' ', ier )
		    END IF
C
		    IF  ( cflag )  THEN
		      IF  ( lflag )  THEN
C
			IF  ( smooth .ne. 0.0 )  THEN
			    CALL GSSMTH ( 2, smooth, ier )
			END IF
			CALL GSRDUC ( filter, ier )
C
                        CALL GCLGRN (kxsub, kysub, subgrd, ioffx, ioffy,
     +                               iskip, nclvl, clvl, clbl, icolor, 
     +                               lintyp, linwid, linlbl, scflag, 
     +                               iret )
		        IF (iret .ne. 0) CALL ER_WMSG ( 'GEMPLT', iret,
     +							' ', ier )
C
			IF  ( smooth .ne. 0.0 )  THEN
			    CALL GSSMTH ( 0, 0.0, ier )
			END IF
			CALL GSRDUC ( 0.0, ier )
C
		      END IF
C
		      IF  ( bflag )  THEN
		        CALL GCBOXX (kxsub, kysub, subgrd, ioffx, ioffy,
     +				     iskip, nclvl, clvl, icolor, lintyp,
     +				     linwid, linlbl, iret )
		        IF (iret .ne. 0) CALL ER_WMSG ( 'GEMPLT', iret,
     +							' ', ier )
		      END IF
		    END IF
		  END IF
C		    
C*		  Mark highs and lows.
C
		  CALL GDNHLO ( hilo, hlsym, grid, kx, ky, ier )
C
C*		  Draw map, lat/lon lines, and station ID/marker.
C
		  CALL GG_MAP  ( map, ier )
		  CALL GG_LTLN ( latlon, ier )
		  CALL GG_SPLT ( stnplt, ier )
		  CALL GG_SCAL ( mscale, ier )
C
C*		  Plot the color bar.
C
		  IF  ( fflag )  CALL GG_CBAR ( clrbar, nflvl, flvl, 
     +						ifcolr, ier )
C
C*		  Write title.
C
		  CALL IN_TITL ( title, 0, ititl, linttl, ttlstr, ier )
		  CALL GR_TITL ( ttlstr, time, .true., level, ivcord,
     +		 		 parm, iscale, ' ', ttlstr, shrttl,
     +				 iret )
		  IF ( clear ) CALL GMESG ( shrttl, ier )
		  IF ( ititl .ne. 0 ) THEN
		    CALL GSCOLR   ( ititl, ier )
		    CALL GG_WSTR  ( ttlstr, linttl, ier )
		  END IF
C
C*		  Flush the graphics buffers.
C
		  CALL GEPLOT  ( ier )
C
C*   		  Increment itime only if plot was successful.
C
		  itime = itime + 1
	        END IF
C*
	    END DO
C
	    CALL GENANM ( iret )
C
C*	    Prepare input parms for the C routine call
C
	    CALL ST_LSTR ( filnam, lendev, ier )
	    filnam( lendev+1:lendev+1 ) = CHAR(0)
C
	    CALL ST_LSTR ( tmpfil, lendev, ier )
	    inputf = tmpfil
	    inputf( lendev+1:lendev+1 ) = CHAR(0)
C
            CALL ST_LSTR ( cycle, lendev, ier )
            cycle( lendev+1:lendev+1 ) = CHAR(0)
C
	    CALL ST_LSTR ( fhr, lendev, ier )
	    fhr( lendev+1:lendev+1 ) = CHAR(0)
C
	    CALL ST_LCUC ( tag, tag , ier )
	    CALL ST_LSTR ( tag, lendev, ier )
	    tag( lendev+1:lendev+1 ) = CHAR(0)
C
	    CALL ST_LCUC ( stat, stat, ier )
	    CALL ST_LSTR ( stat, lendev, ier )
	    stat( lendev+1:lendev+1 ) = CHAR(0)
C
	    CALL ST_LCUC ( lvlincr, lvlincr, ier )
	    CALL ST_LSTR ( lvlincr, lendev, ier )
	    lvlincr( lendev+1:lendev+1 ) = CHAR(0)
C
	    CALL ST_LCUC ( gvcord, gvcord, ier )
	    CALL ST_LSTR ( gvcord, lendev, ier )
	    gvcord( lendev+1:lendev+1 ) = CHAR(0)
C
C*	    Generate a vg file that contains GFA FZLVLs.
C
            CALL GCLOSP ( ier )
C
	    CALL VG2FRZL ( filnam, inputf, kx, ky, grid,
     +			   cycle, fhr, tag, stat, gvcord, lvlincr,
     +			   ier )
C
C*	    Prompt for next contour to be done.
C
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDFRZL', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the interface.
C
	CALL GENDP  ( 1, iret )
	CALL IP_EXIT  ( iret )
C*
	END
