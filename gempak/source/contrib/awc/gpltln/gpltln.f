	PROGRAM GPLTLN
C************************************************************************
C* GPLTLN								*
C* This program sets up the graphics area and optionally draws a map,	*
C* lat/lon lines, and a title.						*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* I. Graffman/RDS	 6/85   GEMPLT Version 3.1			*
C* I. Graffman/RDS	 5/88   DEVICE declaration			*
C* M. desJardins/GSFC	 7/88						*
C* G. Huffman/GSC	10/88	Error messages				*
C* S. Schotz/GSC	 8/90	Added lat/lon line, title, panel, 	*
C*				garea, proj features			*
C* K. Brill/NMC		10/91	PANEL*24  --> *48			*
C* S. Jacobs/EAI	11/92   Added call to GMESG and 'shrttl'	*
C* S. Jacobs/NMC	 3/94	Added satellite display routines	*
C* L. Williams/EAI	 3/94   Clean up declarations of variables	*
C* S. Jacobs/NMC	 6/94   DEVICE*12 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to GPMUPD; added shrttl	*
C* P. Bruehl/Unidata	 8/94	Added animation				*
C* M. desJardins/NMC	 8/94	Added ST_FLST				*
C* S. Jacobs/NMC	 9/94	Added text; Added GG_STTL;		*
C*				Reorganized title plotting		*
C* J. Cowie/COMET	 1/95	Added SATFIL, RADFIL, image subsetting	*
C* S. Jacobs/NMC	 2/95	Moved IN_TEXT to before setting proj	*
C* J. Cowie/COMET	 8/95	Change GSATIM to IM_DROP, add IM_LUTF,	*
C*				use drop flag		 		*
C* S. Jacobs/NMC	10/95	Changed check for when to use GG_STTL	*
C* D. Plummer/NCEP	11/95	Added LUTFIL as a parameter             *
C* D. Keiser/GSC	12/95	Added STNPLT as a parameter		*
C* J. Lewis/AWC		 1/02	Added attribute tables to VG file parse	*
C* M. Li/SAIC		11/03	Added color bar for images		*
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* T. Piper/SAIC	07/05	Added cplus variable			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* L. Hinson/AWC        11/11   Added CTB_PFREAD  and CES_GTRTBL to load*
C*                              the prefs table and group table         *
C*                              Done to enable placement, if requested  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	map*72, device*72, title*72, panel*72, text*72,
     +			lat*72, lon*72, garea*72, proj*72, shrttl*72,
     +			satfil*72, radfil*72, lutfil*72, stnplt*72,
     +			marker*72, vgfile*72, plus*72, varr(3)*72,
     +			imcbar*72, mscale*(LLMXLN)
C*
	CHARACTER	ttlstr*72, ttlinp*72, label*20 , vgfl*72,
     +                  vgf2*72, vgf3*72
	REAL		rarr(2)
	LOGICAL		respnd, done
C*	
	CHARACTER	imgfls(MXLOOP)*132, cplus*4, ucproj*72
	LOGICAL		first, proces
	DATA		cplus / '+' /
C-----------------------------------------------------------------------
C*  Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPLTLN', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize preferences table.
C
	CALL CTB_PFREAD  ( iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPLTLN', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize group id table
C
	CALL CES_GTRTBL  ( iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPLTLN', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*  Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
C
C*  Initialize grid library common area grdcmn.cmn
C 
	    CALL GD_INIT  ( iret )
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GPLTLN', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPLTLN', ier )
C
	DO WHILE  ( .not. done )
C	
	    proces = .true.
	    first = .true.
	    numimg = 1
C
C*	    Get input parameters.
C
	    CALL GPTINP  ( device, map, mscale, garea, proj, satfil,
     +			   radfil, panel, title, text, imcbar, lat,
     +			   lon, clear, marker, lutfil, stnplt, vgfile,
     +			   plus, iperr )	
	    IF  ( iperr .eq. 0 )  THEN
C
C*		Set device and projection.
C
		CALL GG_SDEV  ( device, ier )
		IF  ( ier .ne. 0 )  proces = .false.
		IF  ( proces )  THEN
		  CALL IN_TEXT ( text, ier )
C
C*		  If projection=SAT or RAD, see if multiple image
C*		  files have been specified.
C
		  CALL ST_LCUC ( proj, ucproj, ier )
		  IF  ( ucproj (1:3) .eq. 'SAT' )  THEN
		      CALL ST_FLST  ( satfil, ';', ' ', MXLOOP, imgfls, 
     +				      numimg, ier )
		  ELSE IF  ( ucproj (1:3) .eq. 'RAD' )  THEN
		      CALL ST_FLST  ( radfil, ';', ' ', MXLOOP, imgfls, 
     +				      numimg, ier )
		  END IF
C
C*		  Set map projection
C		  
		  CALL GG_MAPS ( proj, garea, imgfls (1), idrpfl, ier )
		  IF  ( ier .ne. 0 )  proces = .false.
C
C*		  Get the size and width of the '+' sign.
C
		  CALL ST_RLST ( plus, '/', RMISSD, 2, rarr, num, ier )
		  IF ( rarr(1) .eq. RMISSD ) THEN
		    splus = 1
		  ELSE
		    splus = rarr(1)
		  END IF
C*
		  IF ( rarr(2) .eq. RMISSD ) THEN
		    iwplus = 1
		  ELSE
		    iwplus = NINT ( rarr(2) )
		  END IF
C
C*		  Start loop over input image files.
C
		  IF  ( proces )  THEN
	      	     DO  ifile = 1, numimg
C
C*			Reset the projection for each image.
C
			IF ( ucproj (1:3) .eq. 'SAT' .or. 
     +			     ucproj (1:3) .eq. 'RAD' )
     +                       CALL GG_MAPS ( proj, garea, imgfls (ifile),
     +          			    idrpfl, ier )

			IF ( ifile .eq. 1 ) THEN
C
C*			    Go to the first frame.
C
		  	    first = .true.
			    CALL GSTANM ( ier )
			  ELSE 
C
C*			    Advance to the next frame.
C
			    first = .false.
			    CALL GSPLOT ( ier )
			END IF
C
C*			Display user options, allow program exit.
C
C			IF  ( first ) 
C     +			    CALL GPMOPT ( device, proj, garea, map, 
C     +					  title, panel, latlon, clear, 
C     +					  ier )
C
			IF  ( ier .ne. 0 )  proces = .false.
C
			IF  ( proces )  THEN
C
C*			    Clear the screen, if requested, and set
C*			    the panel.
C
			    IF  ( clear ) CALL GCLEAR ( iret )
			    CALL GG_PANL ( panel, iret )
C
C*			    Display satellite image, if desired.
C
			    IF  ( ( idrpfl .eq. 1 ) .or. 
     +			          ( idrpfl .eq. 0 .and. clear ) ) THEN
     				CALL IM_DROP ( iret )
				CALL IM_CBAR ( imcbar, iret )
			    END IF
C
C*			    Apply LUT file
C
			    IF  ( ifile .eq. 1 )
     +				CALL IM_LUTF ( lutfil, ier )
C
C*			    Draw map and station ID/marker.
C
			    CALL GG_MAP  ( map, iret )
			    CALL IN_MARK  ( marker, mkcolr, iret )
			    CALL GSCOLR ( mkcolr, ier )
			    CALL GG_SCAL ( mscale, ier )
C
C*			    Draw the latitude lines.
C
			    CALL GQBND ( 'M', blatmn, blonmn, blatmx,
     +					 blonmx, ier )
C*
			    IF ( ( blatmn .lt. -80. ) .and. 
     +				 ( blatmx .lt. -80. ) ) THEN
				rstrt = -999. 
			    ELSE IF ( ( blatmn .lt. -80. ) .and. 
     +				      ( blatmx .gt. -80. ) ) THEN
				rstrt = -80.
			    ELSE IF ( ( blatmn .gt. -80. ) .and. 
     +				      ( blatmn .lt. 80. ) ) THEN
				rstrt = INT ((blatmn - 5) / 10) * 10.
			    ELSE IF ( blatmn .gt. 80. ) THEN
				rstrt = -999.
			    END IF
C*
			    IF ( blatmx .lt. -80. ) THEN
				rend = -999. 
			    ELSE IF ( ( blatmx .gt. 80. ) .and. 
     +				      ( blatmn .gt. 80. ) ) THEN
				rend = -999.
			    ELSE IF ( ( blatmx .gt. -80. ) .and. 
     +				      ( blatmx .lt. 80. ) ) THEN
				rend = INT ((blatmx + 10) / 10) * 10.
			    ELSE IF ( ( blatmn .lt. 80. ) .and. 
     +				      ( blatmx .gt. 80. ) ) THEN
				rend = 80.
			    END IF

			    blonstr = INT ( blonmn / 30. ) * 30. - 30.
			    blonend = INT ( blonmx / 30. ) * 30. + 30.
			    IF ( blonstr .lt. -180. ) blonstr = -180.
			    IF ( blonend .gt. 180. ) blonend = 180.
C
C Draw markers for the length of each latitude line, only up till -60/60
C latitude, past that we use a different marker spacing
C
                            rmstrt = rstrt
                            rmend  = rend
                            IF ( rmstrt .lt. -60. ) rmstrt = -60.
                            IF ( rmend  .gt.  60. ) rmend  =  60.
			    DO rii = rmstrt, rmend, 10
			      DO rjj = blonmn, blonmx, 1
			          CALL GMARK ( 'M', 1, rii, rjj, ier )
			      END DO
			    END DO
C*
C Draw latitude lines at every 5 past 65, but only put markers every 5
C
			    IF ( blatmn .lt. -65 ) THEN
			      DO rii = -70., -85., -5
			        DO rjj = blonmn, blonmx, 5
			          CALL GMARK ( 'M', 1, rii, rjj, ier )
			        END DO
			      END DO
			    END IF			      
C*
C Draw latitude lines at every 5 past 65, but only put markers every 5
C
			    IF ( blatmx .gt. 65 ) THEN
			      DO rii = 70., 85., 5
			        DO rjj = blonmn, blonmx, 5
			          CALL GMARK ( 'M', 1, rii, rjj, ier )
			        END DO
			      END DO
			    END IF
C
C Draw markers in the center of each square
C
C			    DO rii = rstrt, rend, 10
C			      DO rjj = blonstr, blonend, 10
C			        CALL GMARK ( 'M', 1, rii+5, rjj+5, ier )
C			      END DO
C			    END DO

C
C Draw latitude lables foreach ring
C      
			    DO rii = rstrt, rend, 10
                              ires = MOD(NINT(rii),30)
                              IF ( ires .eq. 0) THEN
			        DO rjj = blonstr, blonend, 30
                                   CALL ST_INCH ( ABS(NINT(rii)), label,
     +                                             ier )
			            CALL ST_LSTR ( label, ilen, ier )
C			            IF ( rii .gt. 0. ) THEN
C			              label = label(:ilen) // 'N'
C			            ELSE IF ( rii .lt. 0. ) THEN
C			              label = label(:ilen) // 'S'
C			            END IF
C			            CALL ST_LSTR ( label, ilen, ier )
                                    rot = 0.
			            CALL GTEXT ( 'M',rii,rjj+5.,label,
     +			          		rot, 0, 0, ier )
			        END DO
                              END IF
			    END DO
C
C*			    Draw the longitude lines.
C
			    rstrt = INT ( ( blonmn - 5 ) / 10 ) * 10 
			    IF ( rstrt .lt. -180 ) rstrt = -180.
			    rend =  INT ( ( blonmx + 10 ) / 10 ) * 10 
			    IF ( rend .ge. 180. ) rend = 170.
			    IF ( ( blatmn .lt. -85 ) .and. 
     +				 ( blatmx .lt. -85 ) ) THEN
				rlstrt = -999. 
			    ELSE IF ( ( blatmn .lt. -85 ) .and. 
     +				      ( blatmx .gt. -85 ) ) THEN
				rlstrt = -85.
			    ELSE IF ( ( blatmn .gt. -85 ) .and. 
     +				      ( blatmn .lt. 85 ) ) THEN
				rlstrt = INT ((blatmn - 5) / 10) * 10.
			    ELSE IF ( blatmn .gt. 85 ) THEN
				rlstrt = -999.
			    END IF
C*
			    IF ( blatmx .lt. -85 ) THEN
				rlend = -999. 
			    ELSE IF ( ( blatmx .gt. 85 ) .and. 
     +				      ( blatmn .gt. 85 ) ) THEN
				rlend = -999.
			    ELSE IF ( ( blatmx .gt. -85 ) .and. 
     +				      ( blatmx .lt. 85 ) ) THEN
				rlend = INT ((blatmx + 10) / 10) * 10.
			    ELSE IF ( ( blatmn .lt. 85 ) .and. 
     +				      ( blatmx .gt. 85 ) ) THEN
				rlend = 85.
			    END IF
C*
C Draw markers for longitude lines and at the same time draw 
C longitude labels, at 25 & 55 north and south
C
			    DO rii = rstrt, rend, 10
			      DO rjj = rlstrt, rlend, 1
			        CALL GMARK ( 'M', 1, rjj, rii, ier )
                                IF (( rjj .eq. -55. ) .or.
     +                              ( rjj .eq. -25. ) .or.
     +                              ( rjj .eq.  25. ) .or.
     +                              ( rjj .eq.  55. )) THEN
                                  ires = MOD(NINT(rii),30)
                                  IF (ires .eq. 0) THEN
				    CALL ST_INCH ( ABS(NINT(rii)), label,
     +				              	 ier )
				    CALL ST_LSTR ( label, ilen, ier )
C				    IF ( rii .gt. 0. ) THEN
C				      label = label(:ilen) // 'E'
C				    ELSE IF ( rii .lt. 0. ) THEN
C				      label = label(:ilen) // 'W'
C				    END IF
C				    CALL ST_LSTR ( label, ilen, ier )
                                    rot = 0.
			            CALL GTEXT ( 'M', rjj, rii, label,
     +						  rot, 0, 0, ier )
				  END IF
				END IF
			      END DO
			    END DO

C
C Draw crosshairs at the pole if it is visible on the map
C
C			    IF ( blatmn .lt. -65 ) THEN
C			      DO rii = -180, 170, 90
C			        DO rjj = -90., -80, 1
C			          CALL GMARK ( 'M', 1, rjj, rii, ier )
C			        END DO
C			      END DO
C			    END IF
C			    IF ( blatmx .gt. 75 ) THEN
C			      DO rii = -180, 170, 90
C			        DO rjj = 80., 90, 1
C			          CALL GMARK ( 'M', 1, rjj, rii, ier )
C			        END DO
C			      END DO
C			    END IF
C
C*			    Draw the pluses.
C
			    CALL GQTEXT ( itxfn, itxhw, sztext, itxwid,
     +                                    ibrdr, irrotn, ijust, iret )
			    CALL GSTEXT ( 0, 0, splus, iwplus, 
     +                                    111, 2, 2, iret )
			    rstrt = INT ( ( blonmn - 5 ) / 10 ) * 10 
			    IF ( rstrt .lt. -180. ) rstrt = -180.
			    rend =  INT ( ( blonmx + 10 ) / 10 ) * 10 
			    IF ( rend .ge. 180. ) rend = 170.
			    rlstrt = INT ( ( blatmn - 5 ) / 10 ) * 10 
			    IF ( rlstrt .lt. -60. ) rlstrt = -60.
			    rlend =  INT ( ( blatmx + 10 ) / 10 ) * 10 
			    IF ( rlend .ge. 60. ) rlend = 60.
			    DO rii = rstrt, rend, 10
			     DO rjj = rlstrt, rlend, 10
			       CALL GTEXT ( 'M', rjj, rii, cplus, 0.,
     +					    0, 0, ier )
			     END DO
			    END DO
			    CALL GSTEXT ( itxfn, itxhw, sztext, itxwid,
     +					  ibrdr, irrotn, ijust, iret )
C
C
C*			    Plot station markers.
C
			    CALL GG_SPLT ( stnplt, iret )
C
C*                          Plot VGF file.
C
                            IF  ( vgfile .ne. ' ' )  THEN
                                CALL ST_CLST ( vgfile, '|', ' ', 3,
     +                                          varr, num, ier )
				CALL ST_NULL ( varr(1), vgfl, lenf, ier )
				CALL ST_NULL ( varr(2), vgf2, lenf, ier )
				CALL ST_NULL ( varr(3), vgf3, lenf, ier )
				icol = 0
				CALL CDS_INIT ( iret )
C
C*                              Set scaling factors.
C
                                IF  ( varr(2) .ne. ' ' )  THEN
                                    CALL CDS_SCAL ( vgf2, ier )
                                END IF
C
C*                              Set user attributes.
C
                                IF  ( varr(3) .ne. ' ' )  THEN
                                    CALL CDS_RTBL ( vgf3, ier )
                                END IF
C
C*                              Plot the contents of the VG file.
C
                                CALL GG_DVGF ( vgfl, icol, iret )
                                IF  ( iret .ne. 0 )  THEN
                                    CALL ER_WMSG  ( 'GG', iret, vgfile,
     +                                              ier )
                                END IF
C
C*                              Reset scaling factors.
C
                                IF  ( varr(2) .ne. ' ' )  THEN
                                    CALL CDS_RESS ( ier )
                                END IF
                            END IF
C
C*                          Reset the text attributes.
C
                            CALL IN_TEXT ( text, ier )
C
C*			    Decode title input and draw the title.
C
			    ipbar = INDEX ( title, '|' )
			    IF  ( ipbar .ne. 0 )  THEN
				shrttl = title (ipbar+1:)
				IF  ( ipbar .eq. 1 )  THEN
				    ttlinp = ' '
				  ELSE
				    ttlinp = title (:ipbar-1)
				END IF
			      ELSE
				CALL ST_LSTR ( garea, len1, ier )
				shrttl = 'MAP OF AREA: ' // garea(:len1)
				ttlinp = title
			    END IF
			    CALL IN_TITL ( ttlinp, -3, ititl, linttl, 
     +					   ttlstr, iret )
			    IF  ( ( ucproj (1:3) .eq. 'SAT' .or. 
     +				    ucproj (1:3) .eq. 'RAD' ) .and.
     +				  ( ttlstr .eq. ' ' ) )  THEN
				CALL GG_STTL ( ttlstr, iret )
			    END IF
			    IF  ( clear ) CALL GMESG ( shrttl, iret )
			    IF  ( ititl .gt. 0 )  THEN
				CALL GSCOLR  ( ititl, iret )
				CALL GG_WSTR ( ttlstr, linttl, iret )
			    END IF
C
C*			    Flush the graphics buffer.
C
			    CALL GEPLOT  ( iret)
			END IF
		      END DO
C
C*		      Mark the end of the animation sequence.
C
		      CALL GENANM ( iret)
		  END IF
		END IF
	      END IF
C
C*	    Call the dynamic tutor.
C
	    CALL IP_DYNM ( done, iret )
	END DO
C*
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'GPLTLN', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
