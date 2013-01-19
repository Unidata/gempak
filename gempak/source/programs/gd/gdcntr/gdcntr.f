	PROGRAM  GDCNTR
C************************************************************************
C* PROGRAM GDCNTR							*
C*									*
C* This program draws contours through a grid.				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* I. Graffman/RDS	 3/86	Added display of max and min values	*
C* M. desJardins/GSFC	 4/86	Eliminated GR_DSPL			*
C* I. Graffman/RDS	 6/86	Added line width, fixed default lines	*
C* I. Graffman/RDS	 5/88	Device size and new GG call seq.	*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	 6/89	Changed calling sequence in GCONTR	*
C* M. desJardins/GSFC	11/89	Changed GR_FILE to DG_OFIL		*
C* K. Brill/GSC		12/89   Added call to DG_AREA			*
C* K. Brill/GSC		 5/90   Changes for IN_CINT			*
C* J. Shadid/GSFC	 7/90	Added LATLON				*
C* K. Brill/NMC		 8/90   Mod DG_OFIL calling sequence		*
C* J. Whistler/SSAI	 4/91	Changed GDNTIT to GR_TITL		*
C* M. desJardins/NMC	10/91	Change panel to *48			*
C* M. desJardins/NMC	11/91	Changes for new contouring capabilities	*
C* K. Brill/NMC		01/92   Replace GERROR with ER_WMSG		*
C* J. Whistler/SSAI	05/92	Added a color bar			*
C* J. Whistler/SSAI	08/92	Changed order of plotting		*
C* S. Jacobs/EAI	11/92	Added call to GMESG and 'shrttl'	*
C* K. Brill/NMC		 6/93	Added hilo and hlsym			*
C* S. Jacobs/EAI	 7/93   Changed iswind in IN_SKIP to iswind(2)  *
C* S. Jacobs/EAI	 8/93	Added call to GR_RARG			*
C* G. Krueger/EAI	 8/93	Changed CLRBAR to view coordinates	*
C* S. Jacobs/EAI	 9/93	Changed IN_CBAR & GR_CBAR to GG_CBAR	*
C* S. Jacbos/EAI	 9/93	Modified short title			*
C* S. Jacobs/EAI	11/93	Removed call to GR_RARG			*
C* S. Jacobs/EAI	 2/94	Added COLADD flag to DG_OFIL		*
C* S. Jacobs/NMC	 3/94	Added satellite display routines	*
C* L. Williams/EAI	 3/94   Clean up declarations of input vars	*
C* S. Jacobs/NMC	 6/94   DEVICE*12 --> *72			*
C* L. Williams/EAI	 7/94	Remove GDNUPD and added	shrttl		*
C* P. Bruehl/Unidata	 8/94	Added animation				*
C* P. Bruehl/Unidata	 8/94	Added sat image from J. Cowie/COMET	*
C* M. desJardins/NMC	 8/94	Added ST_FLST, GR_TLST			*
C* M. desJardins/NMC	 8/94	Ignore non-existent function error	*
C* S. Jacobs/NMC	10/94	Added check for only one time in list	*
C* J. Cowie/COMET	 1/95	Added SATFIL & RADFIL			*
C* S. Jacobs/NMC	 2/95	Moved IN_TEXT to before setting proj	*
C* S. Jacobs/NMC	 3/95	Removed check for only one time in list	*
C* J. Cowie/COMET	 8/95	Changed GSATIM to IM_DROP, add IM_LUTF,	*
C*				use idrpfl				*
C* D. Plummer/NCEP	11/95	Added LUTFIL as an input parameter	*
C* D. Keiser/GSC	12/95	Added STNPLT as an input parameter	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in GDNDSP			*
C* S. Jacobs/NCEP	11/96	Added check for MXLOOP number of times	*
C* S. Jacobs/NCEP	 1/97	Changed the order of IM_DROP & IM_LUTF	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	 8/98	Fixed for when 1st grid is missing	*
C* S. Jacobs/NCEP	 8/98	Changed FL_MFIL to FL_FFIL		*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE; Added smoothing*
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* T. Lee/GSC		 4/99	Changed FL_FFIL to FL_MFIL		*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE; Added filter	*
C* M. Li/GSC		 1/00	Added GCNTLN and nflag; removed GCSPLN	*
C* T. Lee/GSC		 8/00	Added grid shifting for any map display	*
C* T. Lee/GSC		11/00	Changed calling sequence of GR_FIXA	*
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL		*
C* T. Lee/GSC		 6/01	Processed single and multiple files	*
C* T. Lee/SAIC		11/01	Added contour fill types		*
C* K. Brill/HPC		 8/02	Remove calls to GR_GALM & GR_RARG; call	*
C*				DG_SUBG instead of DG_AREA		* 
C* m.gamazaychikov/SAIC 10/02	Put the call to IM_LUTF after GCLEAR    *
C* K. Brill/HPC		12/02	Added IJSKIP input parameter		*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* M. Li/SAIC		11/03	Added color bar for satellite images	*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC		02/04	Removed nuflg from DG_INTL call		*
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* T. Piper/SAIC	10/04	Moved GG_PANL after GCLEAR		*
C* R. Tian/SAIC		10/04	Changes for time/file mngmnt		*
C* C. Bailey/HPC	 6/06	Added contour label array 		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag to	*
C*				IN_LINE, GCLGRN, and GCNTLN		*
C* S. Gilbert/NCEP	 5/07	Removed call to GCNTLN			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
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
     +			mscale*(LLMXLN)
C*
	LOGICAL		clear
C*
	CHARACTER	time (2)*20, ttlstr*72, garout*72, parm*12,
     +			pfunc*72, prjout*72, clbl(LLCLEV)*24
	LOGICAL		respnd, done, proces
C
	REAL		grid (LLMXGD), subgrd (LLMXGD), clvl (LLCLEV),
     +			flvl (LLCLEV)
	INTEGER		level (2), icolor (LLCLEV), lintyp (LLCLEV),
     +			linwid (LLCLEV), linlbl (LLCLEV),
     +			ifcolr (LLCLEV), iflabl (LLCLEV),
     +			ifltyp (LLCLEV), iswind (2)
	LOGICAL		misflg, cflag, lflag, sflag, bflag, fflag, 
     +			nflag, gottm, scflag
	CHARACTER	timfnd*36
	LOGICAL		first, newlev, newgrd
	CHARACTER	imgfls(MXLOOP)*132, uprj*72
C----------------------------------------------------------------------
C*	Initialize TAE
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT ('GDCNTR', ier)
C
C*	Initialize GEMPLT
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
	  CALL GDNINP ( gdfile, gdatim, glevel, gvcord, gfunc, cint,
     +			line, map, mscale, title, device, proj, garea,
     +			clear, panel, text, imcbar, scale, latlon,
     +			contur, skip, fint, fline, ctype, hilo, hlsym,
     +			clrbar, satfil, radfil, lutfil, stnplt, ijskip,
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
C*	    Set up the graphics device.
C
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
		    CALL ER_WMSG ( 'GDCNTR', ier, time(1), irr )
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
C
C*                    Convert new levels to labels
C
                      DO i = 1, nclvl
                        CALL GR_LABL(clvl(i), 0, 0, clbl(i), nchar, ier)
                      END DO
C*
		      CALL IN_LINE ( line, clvl, nclvl, icolor, 
     +				     lintyp, linwid, linlbl,
     +				     smooth, filter, scflag, iret )
		  END IF
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
C*	    Prompt for next contour to be done.
C
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDCNTR', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the interface.
C
	CALL GENDP  ( 0, iret )
	CALL IP_EXIT  ( iret )
C*
	END
