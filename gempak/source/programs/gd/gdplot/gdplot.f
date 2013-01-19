	PROGRAM GDPLOT
C************************************************************************
C* PROGRAM GDPLOT							*
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
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* S. Jacobs/EAI	11/92	Added call to GMESG and 'shrttl'	*
C* G. Krueger/EAI	 5/93	Added multiple overlay/plot ability	*
C* G. Krueger/EAI	 8/93	Added REFVEC, HILO, HLSYM, & CLRBAR	*
C* S. Jacobs/EAI	 9/93	Changed IN_CBAR & GR_CBAR to GG_CBAR	*
C* G. Krueger/EAI	10/93	Changed GFUNC & GVECT,fixed CLRBAR parse*
C* S. Jacobs/EAI	10/93	Changed call to IN_RVEC and plotting of *
C*				   reference arrow			*
C* S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
C* S. Jacobs/NMC         3/94   Added satellite display routines        *
C* L. Williams/EAI       3/94	Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC         4/94   Removed unused variables        	*
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* L. Williams/EAI	 7/94	Removed call to GDBUPD and added shrttl	*
C*				to the user input variables		*
C* P. Bruehl/Unidata	 8/94	Increased size of character string tmfnd*
C*				from 20 to 36 for grids w/2 times	*
C*				Added .and.clear to IF for drop sat img	*
C* P. Bruehl/Unidata	 9/94	Added more checks for drop sat img	*
C* S. Jacobs/NMC	10/94	Check for proces=.true. before GDBTIM	*
C* P. Bruehl/Unidata	12/94	Fixed call to GR_VSCL			*
C* J. Cowie/COMEt	 8/95	Changed GSATIM to IM_DROP, add IM_LUTF,	*
C*				use idrpfl				*
C* P. Bruehl/NWS	12/95	Added code to allow looping		*
C* D. Keiser/GSC	12/95	Added STNPLT as a parameter		*
C* S. Jacobs/NCEP	 5/96	Fixed vector staggering			*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* G. Krueger/EAI	 8/96	Increased user variables to 72 chars.	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to GDBDSP		*
C* S. Jacobs/NCEP	 1/97	Changed the order of IM_DROP & IM_LUTF	*
C* S. Maxwell/GSC	 4/97	Changed call to  GR_VSCL		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* T. Lee/GSC		 9/97	Fixed typo for ref arrow plot, x1 -> xl	*
C* S. Jacobs/NCEP	 9/97	Changed call to GQTEXT and GSTEXT	*
C* S. Jacobs/NCEP	 1/99	Changed call to GDBLEV			*
C* S. Jacobs/NCEP	 1/99	Added smoothing				*
C* T. Lee/GSC		 1/99	Changed timfnd(200) to timfnd(LLMXGT)	*
C* S. Jacobs/NCEP	 5/99	Changed call to GDBLEV			*
C* A. Hardy/GSC	         6/99   Added call to INSKIP			*
C* T. Lee/GSC		 8/99	Added latitudinal dependant skip	*
C* M. Li/GSC             1/00   Added GCNTLN and nflag; removed GCSPLN  *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* T. Lee/GSC		 8/00	Added grid shifting for any map display	*
C* T. Lee/GSC		11/00	Changed calling sequence of GR_FIXA	*
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL		*
C* T. Lee/GSC		 7/01	Replaced GDBTIM with GR_FTIM		*
C* T. Lee/SAIC		10/01	Called GR_FTIM before DG_MFIL call	*
C* T. Lee/SAIC		10/01	Added contour fill types		*
C* K. Brill/HPC		 8/02	Remove calls to GR_GALM & GR_RARG; call *
C*				DG_SUBG instead of DG_AREA		*
C* K. Brill/HPC		12/02	Added IJSKIP				*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* M. Li/SAIC		11/03	Added IM_CBAR				*
C* R. Tian/SAIC		11/03	Added nuflg to DG_INTL call		*
C* R. Tian/SAIC		 2/04	Removed nuflg from DG_INTL call		*
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* R. Tian/SAIC         11/04   Changes for time/file mngmn             *
C* C. Bailey/HPC	 6/06	Added contour label array		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C* S. Gilbert/NCEP	05/07	Removed call to GCNTLN			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), gdatim*(LLMXLN),
     +			glevel*(LLMXLN), gvcord*(LLMXLN), gfunc*(LLMXLN),
     +			gvect*(LLMXLN), cint*(LLMXLN), line*(LLMXLN),
     +			map*(LLMXLN), title*(LLMXLN), device*(LLMXLN),
     +			proj*(LLMXLN), garea*(LLMXLN), panel*(LLMXLN),
     +			text*(LLMXLN), scale*(LLMXLN), latlon*(LLMXLN),
     +		        contur*(LLMXLN), wind*(LLMXLN), refvec*(LLMXLN),
     +			skip*(LLMXLN), fint*(LLMXLN), fline*(LLMXLN),
     +			ctype*(LLMXLN), hilo*(LLMXLN), hlsym*(LLMXLN),
     +			clrbar*(LLMXLN), shrttl*(LLMXLN),
     +			lutfil*(LLMXLN), stnplt*(LLMXLN),
     +			ijskip*(LLMXLN), mscale*(LLMXLN)
	CHARACTER	timlst (LLMXGT)*36, timfnd*36, trange*36
	LOGICAL		clear, gottm, gotol, scflag
C*
	CHARACTER	time (2)*20, ttlstr*72, garout*72, prjout*72,
     +                  parm*12, pfunc*72, etitle*72, egfunc*72,
     +			parmu*12, parmv*12, egvect*72, eglev*72,
     +			egvcrd*72, ocint (50)*72, eline*72, ecint*72,
     +			epanel*72, oscale (50)*72, escale*72,
     +			econtu*72, erfvec*72, ewind*72, eskip*72,
     +			ofint (50)*72, efint*72, efline*72, ectype*72,
     +			opanel*72, ehilo*72, ehlsym*72, eclrbr*72,
     +			arolab*12, gv*72, wintyp*1, winuni*1, 
     +			cints (3)*72, fints (3)*72, rstr*72, 
     +			rtext*72, satfil*132, clbl(LLCLEV)*24
	LOGICAL		respnd, done, proces, funvld, vctvld,
     +			ttlvld
C*
	REAL		gridu (LLMXGD), gridv (LLMXGD), grid (LLMXGD),
     +			subgrd (LLMXGD), clvl (LLCLEV), flvl (LLCLEV)
	REAL		fi ( 100 ), fj ( 100 ), s ( 100 ), d ( 100 ),
     +			alatsk (182)
	INTEGER		level (2), icolrs (LLCLEV), lintyp (LLCLEV),
     +			linwid (LLCLEV), linlbl (LLCLEV),
     +			ifcolr (LLCLEV), iflabl (LLCLEV), 
     +			ifltyp (LLCLEV), iswind (2)
	LOGICAL		misflg, cflag, lflag, sflag, bflag, fflag,
     +			aroref, knots, fndit, latt, nflag
C
 	INCLUDE		'ERMISS.FNC'
C
C*	This can be removed when lutfil is an input parameter
C
	DATA	lutfil / 'DEFAULT' /
C*
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT ('GDPLOT', ier)
C
C*  Initialize GEMPLT.
C
	    CALL GG_INIT  ( 1, ier )
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
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Read in the variables from the TAE.
C
	    CALL GDBINP ( gdfile, gdatim, glevel, gvcord, gfunc, gvect,
     +			  cint, line, map, mscale, title, device, proj,
     +                    garea, clear, panel, text, scale, latlon,
     +                    contur, wind, refvec, skip, fint, fline,
     +                    ctype, hilo, hlsym, clrbar, stnplt, ijskip,
     +                    iperr )
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
	    CALL DG_QTMS ( LLMXGT, .true., timlst, ltime, trange, ier )
C
C*	    Set up the graphics device.
C
	    CALL GG_SDEV  ( device, iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
C*	    Process the text size.
C
	    CALL IN_TEXT  ( text, ier )
C
C*	    Process overlay.
C
	    IF  ( proces )  THEN
		CALL ST_PELT ( gfunc, -1, ' ', lfunct, egfunc, iret )
		CALL ST_PELT ( gvect, -1, ' ', lvectr, egvect, iret )
		CALL ST_PELT ( title, -1, ' ', ltitle, etitle, iret )
		lovrly = MAX ( lfunct, lvectr, ltitle )
	    END IF
C
C*          Loop over times.
C
	    itime  = 1
	    gottm = proces
	    DO WHILE ( gottm )
C
C*              Get the next time to process from time server.
C
                CALL DG_NTIM ( .true., .true., time, gottm, ier )
                proces = ( ier .eq. 0 .and. gottm )
                IF ( ier .ne. 0 ) THEN
                    ier = 3
                    CALL ER_WMSG ( 'GDPLOT', ier, time(1), irr )
                END IF
                CALL TG_DUAL ( time, timfnd, ier )
C
C*	        Set the map projection and graphics area.
C
	        IF  ( proces )  THEN
		    CALL ST_LCUC ( proj, proj, ier )
		    IF  ( ( proj (1:3) .ne. 'SAT' ) .and.
     +                    ( proj (1:3) .ne. 'MCI' ) )  THEN
		        CALL DG_FIXA  ( garea, proj, 
     +			   	        garout, prjout, ier )
		      ELSE
		        prjout = proj
		        garout = garea
		    END IF
		    CALL GG_MAPS  ( prjout, garout, satfil, idrpfl,
     +		                    iret )
		    IF  ( iret .ne. 0 )  proces = .false.
	        END IF
C
C*		Setup the grid subset that covers the graphics area.	
C
		IF  ( proces )  THEN
		    CALL DG_SUBG ( ijskip, iminx, iminy, imaxx, imaxy,
     +				   iret )
		    IF  ( iret .ne. 0 )  THEN
		        CALL ER_WMSG  ( 'DG', iret, ' ', ier )
			proces = .false.
		    END IF
		END IF
		IF  ( proces )  THEN
                    IF ( itime .eq. 1 ) THEN
                        CALL GSTANM ( iret )
		      ELSE
		        CALL GSPLOT ( iret )
                    END IF
		END IF
C
C*		Loop over overlays.
C
	        ifunct = 1
	        gotol = proces
                DO WHILE ( gotol )
C
C*		    Process the grid identifier or find the wind only
C*		    if the grid file was successfully opened:
C
		    IF  ( proces )  THEN
C
C*		        Compute the requested grid/vector:
C
		        iret = 0
		        CALL ST_PELT ( ctype, ifunct, ' ', nscal,
     +			               ectype, iret )
		        CALL IN_CTYP  ( ectype, nflag, lflag, sflag,
     +			                bflag, fflag, ier )
		        funvld = .FALSE.
		        vctvld = .FALSE.
		        IF ( ifunct .le. lovrly ) THEN
C
C*			    Parse the reference arrow parameter refvec
C
			    CALL ST_PELT ( refvec, ifunct, ' ', nscal,
     +				           erfvec, iret )
			    CALL IN_RVEC ( erfvec, rmag, rx, ry, rtext,
     +				           rstr, iret )
			    CALL ST_PELT ( gfunc, ifunct, ' ', nscal,
     +				           egfunc, iret )
			    IF ( egfunc .ne. ' ' .and.
     +			         ifunct .le. lfunct ) THEN
			        funvld = .TRUE.
			    END IF
			    IF ( funvld ) THEN
			        CALL ST_PELT  ( glevel, ifunct, ' ',
     +				                nscal, eglev, iret )
			        CALL ST_PELT  ( gvcord, ifunct, ' ',
     +				                nscal, egvcrd, iret )
			        CALL DG_GRID ( timfnd, eglev,
     +					       egvcrd, egfunc, pfunc,
     +					       grid, kx, ky, time,
     +                                         level, ivcord, parm,
     +                                         iret )
			        IF  ( iret .ne. 0 )  THEN
				    CALL ER_WMSG  ( 'DG', iret, pfunc,
     +				                    ier )
				    funvld = .FALSE.
				    CALL ER_WMSG ( 'GDPLOT', +2, ' ',
     +				                   ier )
			        END IF
			    ENDIF
C
C*			    Set contour attributes.  Get contour skip
C*			    factor.
C
			    IF  ( proces )  THEN
			        CALL ST_PELT ( contur, ifunct, ' ',
     +					       nscal, econtu, iret )
			        CALL IN_CONT ( econtu, ier )
			        CALL ST_PELT ( skip, ifunct, ' ',
     +					       nscal, eskip, iret )
                           CALL IN_SKIP ( eskip, iskpxy, 
     +		              iswind, ier )
			        CALL GDBPNT  ( eskip, kx, ky, ix1, iy1,
     +					       ix2, iy2, ixstep, iystep,
     +					       istag, latt, alatsk,
     +                                         iret )
			    END IF
			    IF ( funvld ) THEN
C
C*			        Get contouring type.
C
			        IF  ( lflag .or. sflag .or. 
     +				      bflag .or. nflag ) THEN
				    cflag = .true.
			          ELSE
				    cflag = .false.
			        END IF
C
C*			        Get grid subset area, i.e. area covering
C*			        graphics display.
C
			        IF  ( proces )  THEN
C
C*				    Define countour levels and
C*				    characteristics.
C*				    Write warning if there are no
C*                                  contour levels.
C
				    CALL ST_PELT ( line, ifunct, ' ',
     +					           nscal, eline, iret )
				    CALL ST_PELT ( cint, ifunct, ' ',
     +					           nscal, ecint, iret )
				    CALL ST_PELT ( fline, ifunct, ' ',
     +					           nscal, efline, iret )
				    CALL ST_PELT ( fint, ifunct, ' ',
     +					           nscal, efint, iret )
				    CALL ST_PELT ( scale, ifunct, ' ',
     +					           nscal, escale, iret )
				    CALL ST_CLST ( ecint, '/', ' ', 3,
     +					           cints, ncints, iret )
				    CALL ST_CLST ( efint, '/', ' ', 3,
     +					           fints, nfints, iret )
				    IF ( itime .ne. 1 ) THEN
				        IF ( ecint .eq. ' ' .or.
     +					     cints (1) .eq. ' ' .or.
     +					     cints (1) .eq. '0' ) THEN
					    ecint = ocint (ifunct) //
     +						    '/' // cints (2) //
     +						    '/' // cints (3)
				        END IF
				        IF ( efint .eq. ' ' .or.
     +					     fints (1) .eq. ' ' .or.
     +					     fints (1) .eq. '0' ) THEN
					    efint = ofint (ifunct) //
     +						    '/' // fints (2) //
     +						    '/' // fints (3)
				        END IF
				        IF ( escale .eq. ' ' .or.
     +					     escale .eq. '999' ) THEN
     					    escale = oscale (ifunct)
				        END IF
				    END IF
				    CALL GDBLEV ( cflag, eline, ecint,
     +					          fflag, efline, efint,
     +					          escale, kx, ky, iminx,
     +					          iminy, imaxx, imaxy,
     +					          grid, nclvl, clvl,
     +					          clbl, icolrs, lintyp,
     +                                            linwid, linlbl,
     +                                            smooth, filter,
     +					          nflvl, flvl, ifcolr,
     +					          iflabl, ifltyp,
     +                                            iscale, dmin, dmax,
     +                                            rcint, rfint, scflag, 
     +						  ier )
				    IF ( itime .eq. 1 ) THEN
C
C*				        Save contour interval, fill
C*				        interval, and scale parameters
C*                                      for the current overlay (ifunct)
C*                                      to use as the default for
C*                                      subsequent time periods.
C
				        CALL ST_RLCH ( rcint, 9,
     +						       ocint (ifunct),
     +						       iret )
				        CALL ST_RLCH ( rfint, 9,
     +						       ofint (ifunct),
     +						       iret )
				        WRITE ( oscale (ifunct),
     +					        '(i9)' ) iscale
				    END IF
				    IF ( nclvl .eq. 0) cflag = .false.
				    IF ( nflvl .eq. 0) fflag = .false.
				    IF ( .not. ( cflag .or. fflag ) )
     +				    THEN
				        CALL ER_WMSG  ( 'GDPLOT', +1,
     +						        ' ', ier )
				    END IF
			        END IF
			    END IF
			    CALL ST_PELT ( gvect, ifunct, ' ', nscal,
     +				           egvect, iret )
			    IF ( egvect .ne. ' ' .and.
     +			         ifunct .le. lvectr ) THEN
			        vctvld = .TRUE.
			    END IF
			    IF ( vctvld ) THEN
			        CALL ST_PELT ( glevel, ifunct, ' ',
     +					       nscal, eglev, iret )
			        CALL ST_PELT ( gvcord, ifunct, ' ',
     +				               nscal, egvcrd, iret )
			        CALL DG_VECT ( timfnd, eglev,
     +					       egvcrd, egvect, pfunc,
     +					       gridu, gridv, kx, ky,
     +                                         time, level, ivcord,
     +                                         parmu, parmv, iret )
			        IF  ( iret .ne. 0 )  THEN
				    CALL ER_WMSG  ( 'DG', iret, pfunc,
     +						    ier )
				    vctvld = .FALSE.
				    CALL ER_WMSG ( 'GDPLOT', +2, ' ',
     +				                   ier )
			        ENDIF
			    ENDIF
			    IF ( vctvld ) THEN
C
C*			        Get type of wind to plot:
C
			        CALL ST_PELT ( wind, ifunct, ' ', nscal,
     +					       ewind, iret )
			        CALL IN_WIND  ( ewind, wintyp, winuni,
     +					        icolor, ier )
C
C*			        Scale the data.
C
			        IF ( wintyp .ne. 'B' ) THEN
				    CALL ST_PELT ( scale, ifunct, ' ',
     +					           nscal, escale, iret )
				    CALL IN_SCAL ( escale, iscale,
     +				                   iscalv, iret )
				    CALL GR_VSCL ( iscalv, kx, ky, 1, 1,
     +					           kx, ky, gridu, gridv,
     +					           vmin, vmax, 
     +					           vmin2, vmax2, ier )
			        ENDIF
C
C*			        Find the largest and smallest wind.
C
			        fndit  = .false.
			        ixstrt = ix1
			        DO  j = iy1, iy2, iystep
				    iy = ( j - 1 ) * kx
				    DO  i = ixstrt, ix2, ixstep
				        ixy = iy + i
				        sped = PR_SPED ( gridu (ixy),
     +						         gridv (ixy) )
				        IF  ( .not. ERMISS ( sped ) )
     +					THEN
					    IF ( .not. fndit ) THEN
					        vmin = sped
					        vmax = sped
					        fndit = .true.
					      ELSE
					        IF ( sped .gt. vmax )
     +						    vmax = sped
					        IF ( sped .lt. vmin )
     +						    vmin = sped
					    END IF
				        END IF
				    END DO
				    IF  ( ixstrt .eq. ix1 )  THEN
				        ixstrt = ixstrt + istag
				      ELSE
				        ixstrt = ix1
				    END IF
			        END DO
			    END IF
		        END IF
		    END IF
C
C*		    Give user a chance to exit.
C
		    IF  ( proces .and. itime .eq. 1 .and.
     +		          ifunct .eq. 1 ) THEN
		        CALL GDBDSP ( gdfile, timlst, ltime, glevel,
     +				      level, gvcord, ivcord, gfunc,
     +                                gvect, lovrly, lfunct, lvectr,
     +                                garea, iret )
C
C*                      Stop looping if user requests exist.
C
		        IF  ( iret .ne. 0 )  THEN
			    proces = .false.
			    gotol = .false.
			    gottm = .false.
			END IF
		    END IF
C
C*		    Draw contours and vector symbols:
C
		    IF  ( proces )  THEN
C
C*		        Clear screen if requested.
C
		        IF (( clear .or. itime .gt. 1 ) .and.
     +			    ( ifunct .le. 1 )) THEN
			    CALL GCLEAR ( ier )
			    opanel = 'NO PANEL'
		        ENDIF
C
C*		        Define the text and panel (view region).
C
		        CALL ST_PELT ( panel, ifunct, ' ', lpanel,
     +			               epanel, iret )
		        IF ( opanel .ne. epanel ) lindef = 0
		        opanel = epanel
		        CALL GG_PANL  ( epanel, iret )
		        CALL IN_TEXT  ( text, iret )
C
C*		        Apply LUT file.
C
		        IF ( itime .eq. 1 ) CALL IM_LUTF ( lutfil, ier )
C
C*		        Display image.
C
		        IF  ( ( idrpfl .eq. 1 .or.
     +			      ( idrpfl .eq. 0 .and. clear ) ) .and.
     +			      ( ifunct .le. 1 ) ) THEN
     			    CALL IM_DROP ( iret )
			    CALL IM_CBAR ( '1/V/LL/0;.05/.90', ier )
		        END IF
C
C*		        Draw contours.
C
		        IF  ( ( cflag .or. fflag ) .and. funvld )  THEN
			    misflg = .false.
			    CALL GR_SUBX ( kx, ky, grid, iminx, iminy,
     +				           imaxx, imaxy, iskpxy, misflg,
     +				           kxsub, kysub, subgrd, ioffx,
     +				           ioffy, iskip, ier )
			    IF  ( fflag )  THEN
			        CALL GCFILL ( kxsub, kysub, subgrd,
     +				              ioffx,ioffy, iskip,
     +                                        nflvl, flvl, ifcolr,
     +                                        iflabl, ifltyp, iret )
			        IF  ( iret .ne. 0 )
     +				    CALL ER_WMSG ( 'GEMPLT', iret, ' ',
     +					           ier )
			    END IF
			    IF  ( cflag )  THEN
			        IF  ( lflag )  THEN
C
				    IF  ( smooth .ne. 0.0 )  THEN
				        CALL GSSMTH ( 2, smooth, ier )
				    END IF
				    CALL GSRDUC ( filter, ier )
C
				    CALL GCLGRN  ( kxsub, kysub, subgrd,
     +					           ioffx, ioffy, iskip,
     +					           nclvl, clvl, clbl, 
     +					           icolrs, lintyp, 
     +					           linwid, linlbl, 
     +					           scflag, iret )
				    IF  ( iret .ne. 0 )
     +				        CALL ER_WMSG ( 'GEMPLT', iret,
     +					 	       ' ', ier )
C
				    IF  ( smooth .ne. 0.0 )  THEN
				        CALL GSSMTH ( 0, 0.0, ier )
				    END IF
				    CALL GSRDUC ( 0.0, ier )
C
			        END IF
			        IF  ( bflag )  THEN
				    CALL GCBOXX  ( kxsub, kysub, subgrd,
     +					           ioffx, ioffy, iskip,
     +					           nclvl, clvl, icolrs,
     +					           lintyp, linwid,
     +                                             linlbl, iret )
				    IF  ( iret .ne. 0 )
     +				        CALL ER_WMSG ( 'GEMPLT', iret,
     +						       ' ', ier )
			        END IF
			    END IF
		        END IF
		        IF ( funvld ) THEN
C
C*			    Mark highs and lows.
C
			    CALL ST_PELT ( hilo, ifunct, ' ', nscal,
     +			                   ehilo, iret )
			    CALL ST_PELT ( hlsym, ifunct, ' ', nscal,
     +				           ehlsym, iret )
			    CALL GDBHLO ( ehilo, ehlsym, grid, kx, ky,
     +			                  ier )
C
C*			    Draw color bar.
C
			    IF ( fflag ) THEN
			        CALL ST_PELT ( clrbar, ifunct, ' ',
     +					       nscal, eclrbr, iret )
			        CALL GG_CBAR ( eclrbr, nflvl, flvl,
     +				               ifcolr, ier )
			    ENDIF
		        ENDIF
C
C*		        Get type of wind to plot:
C
		        IF ( vctvld ) THEN
			    knots = .false.
			    CALL ST_LCUC ( gvect, gv, ier )
			    iuck = index ( gv, 'KNTV')
			    IF ( winuni .eq. 'K' .and. iuck .eq. 0 )
     +			        knots = .true.
C
C*			    Draw winds.
C
			    IF  ( icolor .ne. 0 )  THEN
			        CALL GSCOLR ( icolor, ier )
			        npts   = 0
			        ixstrt = ix1
			        DO  j = iy1, iy2, iystep
				    iy = ( j - 1 ) * kx
			            IF  ( latt )  THEN
C
C*				        Processing for latitudinal
C*                                      thinning.
C*
C*				        Figure out what the closest
C*                                      latitude is and use that value
C*                                      to index into the alatsk array
C*                                      to get skip increment (ixstep).
C*                                      Also compute istag which is the
C*                                      indent stagger for the next row.
C
				        CALL GTRANS ( 'G', 'M', 1,
     +					             FLOAT (i),
     +					             FLOAT(j), alt,
     +                                               aln, iret)
				        indexx = NINT (alt) + 91
				        IF ( alatsk (indexx) .ne. 0. )
     +					THEN
					    ixstep = INT ( kx/alatsk
     +					                   (indexx) ) +1
				          ELSE
					    ixstep = ix2
				        END IF
				        istag = ixstep / 2
				    END IF
C
				    DO  i = ixstrt, ix2, ixstep
				        npts = npts + 1
				        fi ( npts ) = float (i)
				        fj ( npts ) = float (j)
				        ixy = iy + i
				        s ( npts ) =
     +					    PR_SPED ( gridu (ixy),
     +		    				      gridv (ixy) )
				        d ( npts ) =
     +					    PR_DRCT ( gridu (ixy),
     +		    				      gridv (ixy) )
				        IF ( knots )
     +					    s(npts) = PR_MSKN (s(npts))
				        IF ( ( ERMISS ( s( npts ) ) )
     +					     .and.
     +					     ( ERMISS ( d( npts ) ) ) )
     +					THEN
					    fi ( npts ) = RMISSD
					    fj ( npts ) = RMISSD
				        END IF
				        IF  ( npts .ge. 50 )  THEN
					    IF  ( wintyp .eq. 'B' )
     +					    THEN
					        CALL GBARB ( 'G', npts,
     +							     fi, fj, s,
     +                                                       d, ier )
					      ELSE
					        CALL GARRW  ( 'G', npts,
     +							      fi, fj, s,
     +							      d, ier )
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
			        IF  ( npts .gt. 0 )  THEN
				    IF  ( wintyp .eq. 'B' )  THEN
				        CALL GBARB  ( 'G', npts, fi, fj,
     +						      s, d, ier )
				      ELSE
				        CALL GARRW  ( 'G', npts, fi, fj,
     +						      s, d, ier )
				    END IF
			        END IF
C
C*			        Plot reference arrow if arrows were
C*			        requested.
C
			        IF ( wintyp .eq. 'A' ) THEN
				    IF ( winuni .eq. 'K' ) THEN
				        aroref = .true.
				        CALL ST_RLCH ( rmag, 0, arolab,
     +						       iret )
				        IF ( rstr .eq. ' ' ) THEN
					    rstr = 'kts'
					END IF
				        arolab = arolab(1:2) // ' ' //
     +					         rstr
				      ELSE IF ( winuni .eq. 'M' ) THEN
				        aroref = .true.
				        CALL ST_RLCH ( rmag, 0, arolab,
     +						       iret )
				        IF ( rstr .eq. ' ' ) THEN
					    rstr = 'm/s'
					END IF
				        arolab = arolab(1:2) // ' ' //
     +					         rstr
				      ELSE IF ( winuni .eq. 'N' ) THEN
				        aroref = .false.
				    END IF
				    IF ( rmag .eq. RMISSD ) THEN
				        aroref = .false.
				    END IF
				    IF ( rmag .eq. 0.0 ) THEN
				        aroref = .false.
				    END IF
				    IF ( aroref ) THEN
				        CALL GQTEXT ( mtxfn, mtxhw,
     +					              szmtxt, mtxwid,
     +                                                mbrdr, mrrotn,
     +                                                mjust, ier )
				        CALL IN_TEXT ( rtext, ier )
				        CALL GQSYSZ ( rwm, rhm, rwc,
     +					              rhc, rxlb, rylb,
     +                                                ier )
				        CALL GQBND ( 'N', xl, yb, xr,
     +					             yt, ier )
				        xrp = xl + rx * ( xr - xl )
				        yrp = yb + ry * ( yt - yb )
				        x01 = xrp
				        yyy = yrp
				        CALL GTEXT ( 'N', x01, yyy,
     +						     arolab, 0., 0, 0,
     +						     ier )
				        CALL GSTEXT ( mtxfn, mtxhw,
     +					              szmtxt, mtxwid,
     +                                                mbrdr, mrrotn,
     +                                                mjust, ier )
				        spd = rmag
				        dir = 270.0
				        CALL ST_LSTR (arolab, lenaro,
     +					              ier)
				        offset = FLOAT ( lenaro + 1 )
				        x01 = x01 + offset * rwc
				        CALL GARRW ( 'N', 1, x01, yyy,
     +						     spd, dir, iret )
				    END IF
			        END IF
			    END IF
		        END IF
		        CALL ST_PELT ( title, ifunct, ' ', nscal,
     +			               etitle, iret )
		        CALL IN_TITL  ( etitle, lindef, ititl, linttl,
     +				        ttlstr, ier )
		        lindef = linttl + 1
		        IF ( lindef .eq. 0 ) lindef = lindef + 1
		        ttlvld = .true.
		        IF ( funvld ) THEN
			    CALL GR_TITL  ( ttlstr, time, .true., level,
     +					    ivcord, parm, iscale, ' ',
     +					    ttlstr, shrttl, iret )
		          ELSE IF ( vctvld ) THEN
			    CALL ST_LCUC ( egvect, egvect, ier )
			    CALL GR_TITL  ( ttlstr, time, .true., level,
     +					    ivcord, egvect, iscale, ' ',
     +					    ttlstr, shrttl, iret )
		          ELSE IF ( ttlstr .ne. ' ' ) THEN
			    CALL ST_NUMB ( eglev, level, iret )
			    CALL LV_CORD ( egvcrd, egvcrd, ivcord, iret)
			    time (1) = timfnd
			    time (2) = " "
			    CALL GR_TITL ( ttlstr, time, .true., level,
     +				           ivcord, '...', iscale, ' ',
     +				           ttlstr, shrttl, iret )
		          ELSE
			    ttlvld = .false.
		        ENDIF
		        IF ( ttlvld ) THEN
			    CALL ST_LSTR ( shrttl, lensh, ier )
			    IF (( clear .or. itime .gt. 1 ) .and.
     +			        ( ifunct .le. 1 )) THEN
				CALL GMESG ( shrttl, ier )
			    END IF
			    IF ( ititl .ne. 0 ) THEN
			        CALL GSCOLR   ( ititl, ier )
			        CALL GG_WSTR  ( ttlstr, linttl, ier )
			    END IF
		        ENDIF
		        IF ( ifunct .ge. lovrly ) THEN
C
C*			    Draw map, lat/lon lines, and station 
C*                          ID/marker.
C
			    opanel = 'NO PANEL'
			    DO jfunct = 1, lpanel
			        CALL ST_PELT ( panel, jfunct, ' ',
     +				               nscal, epanel, iret )
			        IF ( opanel .ne. epanel ) THEN
				    CALL GG_PANL  ( epanel, iret )
				    CALL GG_MAP  ( map, iret )
				    CALL GG_LTLN ( latlon, ier )
				    CALL GG_SPLT ( stnplt, ier )
				    CALL GG_SCAL ( mscale, ier )
			        END IF
			        opanel = epanel
			    END DO
		        END IF
		    END IF
		    ifunct = ifunct + 1
		    IF ( ifunct .gt. lovrly ) THEN
		       gotol = .FALSE.
		    END IF
	        END DO
C
C*	        Flush the graphics buffers.
C
	        CALL GEPLOT  ( ier )
	        itime = itime + 1
	    END DO
	    CALL GENANM ( iret )
C
C*	    Prompt for next contour/wind plot to be done.
C
	    CALL IP_DYNM  ( done, ier )
C
C*	    Print general error messages if necessary.
C
	    IF (iperr .ne. 0)
     +		CALL ER_WMSG ( 'GDPLOT', iperr, ' ', ier )
	    iret  = 0
	    ier  = 0
	    proces = .false.
	END DO
C
C*	Exit from GEMPLT and the TAE.
C
	CALL GENDP  ( 0, iret )
	CALL IP_EXIT  ( iret )
C*
	END
