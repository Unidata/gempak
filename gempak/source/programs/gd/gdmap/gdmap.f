	PROGRAM  GDMAP
C************************************************************************
C* GDMAP								*
C*									*
C* This program plots grid data on a map.				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85						*
C* M. desJardins/GSFC	 8/88	Cleaned up				*
C* G. Huffman/GSC	 1/89	Don't do title if color .lt. 1		*
C* M. desJardins/GSFC	11/89	Change GR_FILE to DG_OFIL		*
C* K. Brill/GSC         12/89   Added call to DG_AREA			*
C* J. Shadid/GSFC	 7/90	Added LATLON				*
C* M. desJardins/GSFC	 2/91	Added CINT				*
C* J. Whistler/SSAI	 4/91	Changed GDMTTL to GR_TITL		*
C* J. Whistler/SSAI	 6/91	Added SCALE to GDMDSP call		*
C* S. Jacobs/SSAI	 9/91	Added call to GDMLBL			*
C* M. desJardins/NMC	10/91	Change panel to *48			*
C* K. Brill/NMC		01/92	Changed POINTS to SKIP			*
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* J. Whistler/SSAI	 8/92	Added ability to plot markers when no	*
C*				data is in file (Applied STJ 3/93)	*
C* L. Sager/NMC		 8/93   Changed GR_SCAL to IN_SCAL & GR_SSCL	*
C* S. Jacobs/EAI	 9/93	Modified short title			*
C* S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
C* S. Jacobs/NMC         3/94   Added satellite display routines        *
C* L. Williams/EAI       3/94   Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* S. Jacobs/NMC         6/94   COLOR*24 --> *72                        *
C* L. Williams/EAI	 7/94	Removed call to GDMUPD and added shrttl	*
C*				to user input variables			*
C* P. Bruehl/Unidata	 9/94	Added .and. clear to IF for image drop	*
C* L. Williams/EAI	 9/94	Removed GDMPNT and ignore non-existent  *
C*				function error				*
C* S. Jacobs/NMC	 9/94	Cleaned up entry for SKIP; Added 	*
C*				   staggered plotting			*
C* S. Jacobs/NMC        10/94   Added check for only one time in list   *
C* J. Cowie/COMET	 1/95	Added SATFIL & RADFIL			*
C* S. Jacobs/NMC	 2/95	Moved IN_TEXT to before setting proj	*
C* S. Jacobs/NMC	 3/95	Removed check for only one time in list	*
C* J. Cowie/COMET	 8/95	Changed GSATIM to IM_DROP, add IM_LUTF, *
C*				use idrpfl				*
C* D. Plummer/NCEP	11/95	Added LUTFIL as a parameter		*
C* D. Keiser/GSC	12/95	Added STNPLT as a parameter		*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to GDMDSP		*
C* S. Jacobs/NCEP	11/96	Added check for MXLOOP number of times	*
C* S. Jacobs/NCEP	 1/97	Changed the order of IM_DROP & IM_LUTF	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* T. Lee/GSC		11/00	Changed calling sequence of GR_FIXA	*
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL 		*
C* T. Lee/GSC		 7/01	Processed multiple files		*
C* R. Tian/SAIC         10/02   Remove call to GR_GALM; call DG_SUBG	*
C*				instead of DG_AREA 		        *
C* m.gamazaychikov/SAIC 10/02   Put the call to IM_LUTF after GCLEAR    *
C* K. Brill/HPC		12/02	Added IJSKIP parameter			*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* M. Li/SAIC		11/03	Added color bar for images		*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC          2/04   Removed nuflg from DG_INTL call         *
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* T. Piper/SAIC	10/04	Moved GG_PANL after GCLEAR		*
C* R. Tian/SAIC         10/04   Changes for time/file mngmnt            *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), device*(LLMXLN), proj*(LLMXLN),
     +			gdatim*(LLMXLN), gfunc*(LLMXLN), glevel*(LLMXLN),
     +			gvcord*(LLMXLN), color*(LLMXLN), map*(LLMXLN),
     +			title*(LLMXLN), marker*(LLMXLN), positn*(LLMXLN),
     +			garea*(LLMXLN),  scale*(LLMXLN), skip*(LLMXLN),
     +			panel*(LLMXLN),	text*(LLMXLN), latlon*(LLMXLN),
     +			cint*(LLMXLN), grdlbl*(LLMXLN),	shrttl*(LLMXLN),
     +			radfil*(LLMXLN), satfil*(LLMXLN), lutfil*(LLMXLN),
     +			stnplt*(LLMXLN), ijskip*(LLMXLN), imcbar*(LLMXLN),
     +			mscale*(LLMXLN)
	LOGICAL		clear
C*
	REAL		grid (LLMXGD), rarr (3)
	INTEGER		level (2), iskplt(2)
	CHARACTER	time (2)*20, ttlstr*72, garout*72,
     +			parm*12, pfunc*72, uprj*72, prjout*72,
     +			imgfls (MXLOOP)*132, timfnd*36
	LOGICAL		respnd, done, proces, first, gottm
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDMAP', ier )
C
C*  Initialize GEMPLT.
C
	    mode = 1
	    CALL GG_INIT  ( mode, ier )
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
	  CALL GDMINP ( gdfile, gdatim, glevel, gvcord, gfunc, color,
     +			map, mscale, title, device, proj, garea,
     +			clear, skip, scale, marker, positn, panel,
     +			text, imcbar, latlon, cint, grdlbl, satfil,
     +			radfil, lutfil, stnplt, ijskip, iperr )
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
C*		Set the text attributes, especially the size,
C*		before setting the margins.
C
		CALL IN_TEXT  ( text, ier )
C
C*		Check for points to skip.
C
		CALL IN_SKIP  ( skip, iskpcn, iskplt, ier )
		ixinc = iskplt (1)
		iyinc = iskplt (2)
C
C*		Check for stagger.
C
		IF  ( ixinc .ge. 0 ) THEN
		    ixstep = ixinc + 1
		    istag  = 0
		  ELSE
		    ixstep = - ixinc + 1
		    istag  = ixstep / 2
		END IF
		iystep = iyinc + 1
C
		CALL IN_SCAL  ( scale, iscale, iscalv, iret )
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
                    CALL ER_WMSG ( 'GDMAP', ier, time(1), irr )
                END IF
                CALL TG_DUAL ( time, timfnd, ier )
C
C*		Set the map projection and graphics area.
C
		IF  ( proces )  THEN
		    CALL ST_LCUC ( proj, uprj, ier )
		    IF  ( ( uprj (1:3) .ne. 'SAT' ) .and.
     +		 	  ( uprj (1:3) .ne. 'RAD' )) THEN 
			CALL DG_FIXA  ( garea, proj, garout, 
     +					prjout, ier )
		      ELSE
			prjout = proj
			garout = garea
		    END IF
C
C*                  If projection=SAT or RAD, check for multiple image
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
                    CALL GG_MAPS  ( prjout, garout, imgfls (1), idrpfl,
     +				    iret )
                    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*		Set the projection, garea for SAT, RAD (for each plot)
C
		IF  ( proces )  THEN
		    IF ( first ) THEN
			CALL GSTANM ( iret )
		      ELSE
			CALL GSPLOT ( iret )
C
C*			Set map proj for all images after 1st.
C
			IF ( uprj (1:3) .eq. 'SAT' .or. 
     +			     uprj (1:3).eq.'RAD')
     +			  CALL GG_MAPS ( prjout, garout, imgfls (itime),
     +					 idrpfl, iret)
		    END IF
		END IF
C
C*              Setup the grid subset that covers the graphics area.
C
                IF  ( proces )  THEN
                    CALL DG_SUBG ( ijskip, ix1, iy1, ix2, iy2, iret )
                    IF  ( iret .ne. 0 )  THEN
                        CALL ER_WMSG ( 'DG', iret, ' ', ier )
                        proces = .false.
                    END IF
                END IF
C
C*		Compute the requested grid.
C
		IF  ( proces )  THEN
		    CALL DG_GRID  ( timfnd, glevel, gvcord,
     +				    gfunc, pfunc, grid, kx, ky, time,
     +				    level, ivcord, parm, iret )
		    IF  ( iret .ne. 0 )  THEN
		        CALL ER_WMSG ( 'DG', iret, pfunc, ier )
		        time(1) = timfnd
		        time(2) = ' '
		        proces = .false.
		    END IF
		END IF
C
C*	        Get scaling factor.
C
		IF  ( proces )  THEN
	            CALL GR_SSCL  ( iscale, kx, ky, ix1, iy1, ix2, 
     +				    iy2, grid, rmin, rmax, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*	        Give user a chance to exit.
C
	        IF  ( proces )  THEN
		    CALL GDMDSP ( gdfile, time, level, ivcord,
     +				  parm, garea, iscale, first, iret )
C
C*                  Stop looping if user requests exist.
C
                    IF  ( iret .ne. 0 )  THEN
                        proces = .false.
                        gottm = .false.
                    END IF
C
C*                Set first to false upon first successful plot.
C
                  first = .false.
	        END IF
C
C*	        Plot data.
C
		IF  ( proces ) THEN
C
C*		    Clear the screen if requested, and set the panel.
C
		    IF  ( clear )  CALL GCLEAR ( ier )
		    CALL GG_PANL  ( panel, ier )
C
C*		    Apply LUT file
C
        	    CALL IM_LUTF ( lutfil, ier )
C
C*		    Display satellite image, if desired.
C
		    IF  ( ( idrpfl .eq. 1 ) .or.
     +			  ( idrpfl .eq. 0 .and. clear ) ) THEN
      			  CALL IM_DROP ( iret )
			  CALL IM_CBAR ( imcbar, iret )
		    END IF
C
C*		    Draw map, lat/lon lines, station ID/marker.
C
		    CALL GG_MAP  ( map, ier )
		    CALL GG_LTLN ( latlon, ier )
		    CALL GG_SPLT ( stnplt, ier )
		    CALL GG_SCAL ( mscale, ier )
C
C*		    Draw markers.
C
		    CALL GDMMRK ( marker, kx, ky, ix1, iy1, ix2, iy2,
     +				  ixstep, istag, iystep, ier )
C
C*		    Label grid axes.
C
		    CALL GDMLBL ( grdlbl, kx, ky, ix1, iy1, ix2, iy2,
     +				  ixstep, iystep, ier )
C
C*		    Get range for data.
C
		    CALL ST_RLST ( cint, '/', RMISSD, 3,
     +				   rarr, n, ier )
		    IF  ( ERMISS ( rarr (2) ) )  THEN
			rmind = -1. E10
		      ELSE
		    	rmind = rarr (2)
		    END IF
		    IF  ( ERMISS ( rarr (3) ) )  THEN
			rmaxd = +1. E10
		      ELSE
			rmaxd = rarr (3)
		    END IF
C
C*                  Plot data.
C
		    CALL GDMPLT ( grid, kx, ky, ix1, iy1,
     +				  ix2, iy2, ixstep, istag,
     +				  iystep, color, positn,
     +				  rmind, rmaxd, iret )
C
C*		    Write title.
C
		    CALL IN_TITL ( title, 0, ititl, linttl,
     +				   ttlstr, ier )
		    CALL GR_TITL ( ttlstr, time, .true., level,
     +				   ivcord, parm, iscale, ' ',
     +				   ttlstr, shrttl, iret )
		    IF  ( clear )  CALL GMESG ( shrttl, ier )
		    IF ( ititl .ne. 0 ) THEN
			CALL GSCOLR   ( ititl, ier )
			CALL GG_WSTR  ( ttlstr, linttl, ier )
		    END IF
C
C*		    Flush the graphics buffers.
C
		    CALL GEPLOT  ( ier )
C
C*                  Increment itime only if plot was successful.
C
                    itime = itime + 1
		END IF
	    END DO
C
	    CALL GENANM ( iret )
C
C*	    Prompt for next map to be done.
C
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDMAP', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the interface.
C
	CALL GENDP ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
