	PROGRAM  GDSTREAM
C************************************************************************
C* PROGRAM GDSTREAM							*
C*									*
C* This program draws streamlines for a gridded wind field.		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85						*
C* I. Graffman/RDS	 5/88	New GG calls and DEVICE declaration	*
C* I. Graffman/RDS	 5/88	Changed GFLUSH to GEPLOT		*
C* I. Graffman/RDS	 6/88	Changed Proj size to *20		*
C* M. desJardins/GSFC	 5/89	Write vector name in title & display	*
C* M. desJardins/GSFC	11/89	Change GR_FILE to DG_OFIL		*
C* K. Brill/GSC         12/89   Added call to DG_AREA			*
C* J. Shadid/GSFC	 7/90	Added LATLON				*
C* S. Schotz/GSC	 7/90	Update for call to IN_LINE		*
C* S. Schotz/GSC	 8/90	Added arrow head size to GQARRW,GSARRW	*
C* K. Brill/NMC          9/90   Change PFUN to PFUNC in ER_WMSG		*
C* S. Schotz/GSC	10/90	Set hw flags to 0 in call to GSLINE	*
C* K. Brill/NMC         10/90   Fixed GAREA problems			*
C* J. Whistler/SSAI	 4/91	Changed GDSTTL to GR_TITL		*
C* J. Whistler/SSAI	 5/91	Added WIND				*
C* M. desJardins/NMC	10/91	Changed panel to *48			*
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* S. Jacobs/EAI	 9/93	Modified short title			*
C* S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
C* S. Jacobs/NMC         3/94   Added satellite display routines        *
C* L. Williams/EAI       3/94   Clean up declarations of user input	*
C* 				variables				*
C* S. Jacobs/NMC         4/94   Removed unused variables        	*
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* L. Williams/EAI	 7/94	Removed call to GDSUPD and added shrttl *
C*				to the user input variables		*
C* P. Bruehl/Unidata	 8/94	Added calls (GSTANM,GSPLOT,GENANM) to   *
C*				create multiple pixmaps; added logical  *
C*				first to prompt once; added calls to    *
C*				process LIST as input for date & time;	*
C*				added functionality to loop over list of*
C*				grids w/2 times; added Jim Cowie's 	*
C*				(COMET) multiple sat image looping; 	*
C*				added extra GG_MAP call	to initialize 	*
C*				area for GR_GALM call			*
C* P. Bruehl/Unidata	 8/94	Added ".and.clear" to IF to drop sat img*
C* L. Williams		 9/94	Added call to GRTLST                    *
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* S. Jacobs/NMC        10/94   Added check for only one time in list   *
C* J. Cowie/COMET	 1/95	Added SATFIL, RADFIL			*
C* S. Jacobs/NMC	 2/95	Moved IN_TEXT to before setting proj	*
C* S. Jacobs/NMC	 3/95	Removed check for only one time in list	*
C* J. Cowie/COMET	 8/95	Changed GSATIM to IM_DROP, add IM_LUTF	*
C*				use idrpfl				*
C* D. Plummer/NCEP	11/95	Added LUTFIL as a parameter		*
C* D. Keiser/GSC	12/95	Added STNPLT as a parameter		*
C* D.W.Plummer/NCEP	 5/96	Added STREAM capability to thin strmlns	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to GDSDSP		*
C* S. Jacobs/NCEP	11/96	Added check for MXLOOP number of times	*
C* S. Jacobs/NCEP	 1/97	Changed the order of IM_DROP & IM_LUTF	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT; Removed	*
C*				unused variables			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* T. Lee/GSC		 8/00	Added grid shifting for any map display	*
C* T. Lee/GSC		11/00	Changed calling sequence of GR_FIXA	*
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL 		*
C* T. Lee/GSC		 7/01	Processed multiple files		*	
C* K. Brill/HPC		 8/02	Remove calls to GR_GALM & GR_RARG; call *
C*				DG_SUBG instead of DG_AREA		*
C* m.gamazaychikov/SAIC 10/02   Put the call to IM_LUTF after GCLEAR    *
C* K. Brill/HPC		12/02	Added IJSKIP				*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* M. Li/SAIC		11/03	Added color bar for images		*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC          2/04   Removed nuflg from DG_INTL call         *
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* T. Piper/SAIC	10/04	Moved GG_PANL after GCLEAR		*
C* R. Tian/SAIC         10/04   Changes for time/file mngmnt            *
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), device*(LLMXLN), proj*(LLMXLN),
     +			gdatim*(LLMXLN), gvect*(LLMXLN), line*(LLMXLN),
     +			gvcord*(LLMXLN), map*(LLMXLN), glevel*(LLMXLN),
     +			panel*(LLMXLN), garea*(LLMXLN), text*(LLMXLN), 
     +			latlon*(LLMXLN), wind*(LLMXLN), title*(LLMXLN),
     +			shrttl*(LLMXLN), satfil*(LLMXLN),
     +			radfil*(LLMXLN), lutfil*(LLMXLN),
     +			stnplt*(LLMXLN), stream*(LLMXLN),
     +			ijskip*(LLMXLN), imcbar*(LLMXLN),
     +			mscale*(LLMXLN)
	LOGICAL		clear
C*
	REAL		grid1 (LLMXGD), grid2 (LLMXGD), values(2)
	INTEGER		level (2), linwid(2), iline(2), ilabel(2)
	CHARACTER	time(2)*20, ttlstr*72, garout*72, wintyp*1,
     +			winuni*1, pfunc*72, uprj*72,
     +			parmu*12, parmv*12, prjout*72
	LOGICAL		respnd, done, proces, scflag

	CHARACTER       timfnd*36
	LOGICAL         first, gottm
	CHARACTER 	imgfls(MXLOOP)*132
C	
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDSTREAM', ier )
C
C*  Initialize GEMPLT.
C
	    mode = 1
	    CALL GG_INIT  ( mode, ier )
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
C*	Main loop to read in TAE parameters and streamlines.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL GDSINP  ( gdfile, gdatim, glevel, gvcord, gvect, map,
     +			 mscale, title, device, proj, garea, clear,
     +			 wind, line, panel, text, imcbar, latlon,
     +			 satfil, radfil, lutfil, stnplt, stream, 
     +			 ijskip, iperr )
C
C*	  Exit if there is an error.
C
	  IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	   ELSE
C
C*          Process the GDFILE input.
C
            CALL DG_NFIL ( gdfile, ' ', ier )
            IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, ' ', irr )
                proces = .false.
            END IF
C
C*          Process the GDATTIM input; setup the time server.
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
C*		Set text and line attributes.
C
		CALL IN_TEXT  ( text, ier )
		CALL IN_LINE  ( line, values, 1, icolor, iline, 
     +				linwid, ilabel, smth, fltr, scflag, 
     +				ier )
C
C*		Set arrow head size.
C
		CALL IN_WIND ( wind, wintyp, winuni, iicolr, ier )
C
C*		Process STREAM parameter.
C
		CALL GDSTIN   ( stream, filtst, filtar, ststop, 
     +				dispc, displ, ier )
	    END IF
C
C*	    Loop over times.
C
	    itime = 1
            gottm = proces
            first = .true.
            DO WHILE  ( gottm )
C
C*              Get the next time to process from time server.
C
                CALL DG_NTIM ( .true., .true., time, gottm, ier )
                proces = ( ier .eq. 0 .and. gottm )
                IF ( ier .ne. 0 ) THEN
                    ier = 2
                    CALL ER_WMSG ( 'GDSTREAM', ier, time(1), irr )
                END IF
                CALL TG_DUAL ( time, timfnd, ier )
C
C*		Set the map projection and graphics area.
C
		IF  ( proces )  THEN
		    CALL ST_LCUC ( proj, uprj, ier )
		    IF  ( ( uprj (1:3) .ne. 'SAT' ) .and.
     +		          ( uprj (1:3) .ne. 'RAD' )) THEN 
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
		    CALL GG_MAPS ( prjout, garout, imgfls(1), idrpfl,
     +				   iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
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
C*              Set up a subset grid that covers the graphics area.
C
		IF  ( proces )  THEN
		    CALL DG_SUBG ( ijskip, ix1, iy1, ix2, iy2, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'DG', iret, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*		Find the wind only if the grid file was successfully 
C*		opened.
C
		IF  ( proces )  THEN
C
C*		    Compute the requested vector and rotate to 
C*		    grid-relative orientation.
C
		    CALL DG_VECR ( timfnd, glevel, gvcord, gvect,
     +				   pfunc, grid1, grid2, kx, ky, time, 
     +				   level, ivcord, parmu, parmv, iret )
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
			CALL ER_WMSG  ( 'DG', iret, pfunc, ier )
		    END IF
		END IF
C
C*		Give user a chance to exit.
C
		IF  ( proces )  THEN
		    CALL GDSDSP  ( gdfile, time, level, ivcord, parmu,
     +				   wind, icolor, garea, first, iret )
C
C*                  Stop looping if user requests exist.
C
                    IF  ( iret .ne. 0 )  THEN
                        proces = .false.
                        gottm = .false.
                    END IF
C
C*                  Set first to false upon first successful plot.
C
                    first = .false.
		END IF
C
C*		Draw streamlines.
C
		IF  ( proces )  THEN
C
C*		  Clear the screen if requested, and set the panel.
C
		  IF  ( clear )  CALL GCLEAR  ( ier )
		  CALL GG_PANL  ( panel, ier )
C
C*		  Apply LUT file.
C
		  CALL IM_LUTF ( lutfil, ier )
C
C*		  Display satellite image, if desired.
C
		  IF ( ( idrpfl .eq. 1 ) .or.
     +		       ( idrpfl .eq. 0 .and. clear ) ) THEN
     			CALL IM_DROP ( iret )
			CALL IM_CBAR ( imcbar, iret )
		  END IF
C
C*		  Draw map, lat/lon lines, and station ID/marker.
C
		  CALL GG_MAP  ( map, ier )
		  CALL GG_LTLN ( latlon, ier )
		  CALL GG_SPLT ( stnplt, ier )
		  CALL GG_SCAL ( mscale, ier )
C
C*		  Query arrow line attributes.
C
		  CALL GQARRW ( szarrw, szarrh, iarwid, iartyp, ier )
		  CALL GQLINE ( lintyp, ilhw, iwidth, ilwhw, ier )
C
C*                Set color, line type and width for streamlines and 
C*                arrows
C
		  IF ( iartyp .eq. 2 )  THEN
		      jartyp = 1
		    ELSE
		      jartyp = iartyp
		  END IF
C*
		  CALL GSCOLR ( icolor, ier )
		  CALL GSLINE ( iline(1), 0, linwid(1), 0, ier )
                  CALL GSARRW ( szarrw, szarrh, linwid(1), jartyp, ier )
C
C*		  Draw streamlines.
C
		  CALL GSTRML ( kx, ky, grid1, grid2, ix1, iy1, 
     +				ix2, iy2, .true., filtst, filtar, 
     +				ststop, dispc, displ, ier )
C
C*		  Reset arrow and line attributes to previous
C
		  CALL GSARRW ( szarrw, szarrh, iarwid, iartyp, iret )
		  CALL GSLINE ( lintyp, 0, iwidth, 0, ier )
C
C*		  Write title.
C
		  CALL IN_TITL ( title, 0, ititl, linttl, ttlstr, ier )
		  CALL GR_TITL ( ttlstr, time, .true., level, ivcord,
     +				parmu(2:), iscale, ' ', ttlstr, shrttl,
     +				iret )
		  IF  ( clear )  CALL GMESG ( shrttl, ier )
		  IF  ( ititl .ne. 0 )  THEN
		      CALL GSCOLR   ( ititl, ier )
		      CALL GG_WSTR  ( ttlstr, linttl, ier )
		  END IF
C
C*		  Flush the graphics buffers.
C
		  CALL GEPLOT  ( ier )
C
C*                Increment itime only if plot was successful.
C
		  itime = itime + 1
		END IF
	      END DO
C
	      CALL GENANM ( iret )
C
C*	      Prompt for next streamline to be done.
C
	      CALL IP_DYNM  ( done, ier )
	   END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDSTREAM', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the interface.
C
	CALL GENDP ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
