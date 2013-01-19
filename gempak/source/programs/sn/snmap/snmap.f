	PROGRAM SNMAP
C************************************************************************
C* PROGRAM SNMAP							*
C*									*
C*	This program plots surface data on a map.			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Rewrote					*
C* G. Huffman/GSC	 1/89   Warn on SCALE in [-100,-5],[5,100]	*
C* M. desJardins/GSFC	11/89	Changes for station time		*
C* J. Shadid/GSFC	 7/90	Added LATLON				*
C* S. Schotz/GSC	 8/90	Remove scale, add plot parameter at	*
C*				station like SFMAP			*
C* J. Whistler/SSAI	 7/91	Moved parm cond. filter out of SNMPLT	*
C*				and placed it befor station filter	*
C* J. Whistler/SSAI	 9/91	Corrected typo from previous change	*
C* S. Jacobs/SSAI	10/91	Added capability to plot certain	*
C*				stations before filtering		*
C* K. Brill/NMC		10/91	Restructured code nesting; PANEL-->*48	*
C* M. desJardins/NMC	10/91	Check for @ST; don't filter stn list;	*
C*				pass x,y rather than lat,lon to SFMPLT	*
C* K. Brill/NMC		11/91	Added J. Nielsen's flexible filter	*
C* K. Brill/NMC		12/91	Changes for new wind parms		*
C* S. Jacobs/EAI         6/92   Fixed call to SNMPLT to send lat/lon    *
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* K. Brill/NMC		 8/93	tstn*4 -> tstn*8			*
C* S. Jacobs/NMC         3/94   Added satellite display routines        *
C* S. Jacobs/NMC         6/94   DEVICE*24 --> *72                       *
C* S. Jacobs/NMC         6/94   COLORS*24 --> *72                       *
C* L. Williams/EAI	 7/94	Removed call to SFMUPD and added shrttl *
C*				to the user input variables		*
C* S. Jacobs/NMC	 8/94	Added GSTANM, GSPLOT for animation	*
C* P. Bruehl/Unidata	 8/94	Added logical first, prompt only once	*
C* J. Cowie/COMET	 8/94	Modified for sat image looping		*
C* P. Bruehl/Unidata	 8/94	Fixed typo: process -> proces		*
C* P. Bruehl/Unidata	 8/94	Added .and.clear to IF to drop sat img	*
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* S. Jacobs/NMC	10/94	Added GR_MTTL to create the title	*
C* J. Cowie/COMET	 1/95	Added SATFIL & RADFIL			*
C* S. Jacobs/NMC	 2/95	Moved IN_TEXT to before setting proj	*
C* S. Jacobs/NMC	 3/95	Changed call to SNMLEV to pass file num	*
C* J. Cowie/COMET	 8/95	Changed GSATIM to IM_DROP, add IM_LUTF,	*
C*				use idrpfl				*
C* D. Plummer/NCEP	11/95	Added LUTFIL as a parameter		*
C* D. Keiser/GSC	12/95	Added STNPLT as a parameter		*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call	*
C* S. Jacobs/NCEP	 1/97	Changed the order of IM_DROP & IM_LUTF	*
C* S. Maxwell/GSC	 3/97	Added call to TB_PARM			*
C* S. Maxwell/GSC	 3/97	Removed marker, imark 			*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* A. Hardy/GSC          1/99   Added grouping calls for station models *
C* A. Hardy/GSC          3/99   Added priority parameter to PC_SSTN     *
C* D. Kidwell/NCEP       5/99   Moved level processing before params    *
C* T. Piper/SAIC	 4/02	Initialized arecur, datcur, et al.	*
C* T. Piper/SAIC	 4/02	Fixed core dump; add if (proces)	*
C* M. Li/SAIC		11/03	Added color bar for images		*
C* T. Piper/SAIC	08/04	Added gg_scal and mscale		*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	snfile*(LLMXLN), area*(LLMXLN), garea*(LLMXLN),
     +			prminp*(LLMXLN), dattim*(LLMXLN),
     +			colors*(LLMXLN), map*(LLMXLN), title*(LLMXLN),
     +			device*(LLMXLN), proj*(LLMXLN), panel*(LLMXLN),
     +			text*(LLMXLN), levels*(LLMXLN), vcoord*(LLMXLN),
     +			latlon*(LLMXLN), filter*(LLMXLN),
     +			shrttl*(LLMXLN), satfil*(LLMXLN),
     +			radfil*(LLMXLN), lutfil*(LLMXLN),
     +			stnplt*(LLMXLN), imcbar*(LLMXLN),
     +			mscale*(LLMXLN)
	LOGICAL		clear
C*
	CHARACTER	snfcur*72, arecur*48, datcur*48, voutc*4
	CHARACTER 	pmdset (MMPARM)*4, params*72, colrs*72
	CHARACTER	parms (MMPARM)*4, times (LLMXTM)*20
        CHARACTER	prcons (MMPARM)*16, chd (MMPARM)*8
	CHARACTER	tstn*8, sta*8, ttlstr*80, ttt*72
	CHARACTER	area1*48, area2*48, ttlinp*72, filnam*72
	CHARACTER	imgfls(MXLOOP)*132, uprj*72, shrtin*72
	INTEGER		icolor (MMPARM), iscale (MMPARM)
	LOGICAL		respnd, done, proces, newfil, chrflg (MMPARM)
	LOGICAL		more, morlev, plot, wndflg
	REAL		offset (4), sxplt (LLSTFL), outd (MMPARM)
	REAL		syplt (LLSTFL), data (LLMXDT), rlevel (LLMXLV)

	LOGICAL		first
C------------------------------------------------------------------------
	iflno = 0
	filtfc = 0.
	arecur = ' '
	datcur = ' '
	snfcur = ' '
	plot = .false.

	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
            CALL GD_INIT  ( iperr )
	    CALL GG_INIT  ( 1, iperr )
	END IF 
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
	CALL IP_IDNT  ( 'SNMAP', ier )
C
	DO WHILE  ( .not. done )
	    CALL SNMINP ( snfile, area, garea, prminp, dattim, colors,
     +			  map, mscale, title, clear, device, proj,
     +			  filter, panel, levels, vcoord, text, imcbar,
     +			  latlon, satfil, radfil, lutfil, stnplt, 
     +			  iperr )
	    IF  ( iperr .lt. 0 )  THEN
	        done = .true.
		proces = .false.
	      ELSE
	        proces = .true.
	    END IF
C*
	    IF ( proces ) THEN
C
C*		Set up device and projection.
C
		CALL GG_SDEV  ( device, iret )
		IF  ( iret .ne. 0 ) proces = .false.
C
C*		Set text.
C
	    	CALL IN_TEXT ( text, ier )
C
C*		If projection=SAT or RAD, see if multiple image files
C*		were specified.
C
 		CALL ST_LCUC ( proj, uprj, ier )
                IF ( uprj (1:3) .eq. 'SAT' ) THEN
	            CALL ST_FLST ( satfil, ';', ' ', MXLOOP, imgfls,
     +                             numimg, iret )
                ELSE IF ( uprj (1:3) .eq. 'RAD' ) THEN
	            CALL ST_FLST ( radfil, ';', ' ', MXLOOP, imgfls,
     +                             numimg, iret )
		END IF
C
C*		Set map projection
C
		IF  ( iret .eq. 0 )  THEN
		    CALL GG_MAPS  ( proj, garea, imgfls (1), idrpfl, iret )
		ELSE
		    proces = .false.
		END IF
C
C*	        Process filename, winds, and title.
C
		IF ( iret .eq. 0 ) THEN
		    CALL FL_MFIL ( snfile, ' ', filnam, iret )
		    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret,
     +						      ' ', ier )    
		    CALL SNMFIL ( filnam, snfcur, iflno, newfil, pmdset, 
     +			          npmdst, ivert, iret )
		END IF
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
	    IF  ( proces )  THEN
		CALL IN_FILT ( filter, filtfc, ier )
C
                CALL TB_PARM ( prminp, params, colrs, iret )
                IF ( iret .lt. 0 ) THEN
                   CALL ER_WMSG ( 'TB', iret, ' ', ier )
                   proces = .false.
                ELSE IF ( iret .eq. 2 ) THEN
                   params = prminp
                   colrs  = colors
                ELSE
                   IF ( colors .ne. ' ' ) colrs = colors
                END IF
		print*, 'parms  = ', params
		print*, 'colors = ', colrs
            END IF
C
C*	    Get levels to process.
C
	    IF  ( proces )  THEN
	    	CALL SNMLEV  ( iflno, levels, vcoord, ivert, nlev,
     +			       rlevel, voutc, lvert, iret )
	   	IF  ( ( iret .ne. 0 ) .or. ( nlev .eq. 0 ) )
     +			proces = .false.
	    END IF
C
            IF  ( proces )  THEN
C
C*	   	Process parameter names.
C
	    	CALL SNMPRM ( newfil, params, pmdset, npmdst, 
     +			      parms, chrflg, ncprm,
     +			      prcons, wndflg, iret )
C
C*	  	Determine whether any data will be plotted.
C
		IF ( ncprm .eq. 0 ) THEN
                        plot = .false.
                  ELSE
                        plot = .true.
                END IF
		IF  (  ncprm .gt. 0  )
     +		     CALL SNMCLR (ncprm, parms, colrs, icolor, ier)
	    END IF
C
C*	    Get offsets for filtering
C
	    IF  ( ( filtfc .ne. 0. ) .and. plot .and. proces ) 
     +	         CALL SNMCOF ( ncprm, parms, wndflg,
     +			       filtfc, offset, ier )	
C
C*	    Set area and get times to be processed.
C
	    IF  ( proces )  THEN
	    	ipos2 = INDEX ( area, '/' )
		IF ( area (1:1) .eq. '@' .and. ( ipos2 .gt. 4 ) ) THEN
		    area1 = area ( :ipos2-1 )
		    area2 = area ( ipos2+1: )
		    iloop = 1
		  ELSE
		    area1 = area
		    iloop = 2
		END IF
		CALL LC_UARE  ( area1, newfil, iflno, arecur, tstn, 
     +			        ier )
		IF  ( ier .ne. 0 )  proces = .false.
C*
		CALL SNMDAT  ( dattim, iflno, newfil, datcur, ntime,
     +			       times, ier )
		IF  ( ier .ne. 0 )  proces = .false.
	    END IF
C
C*	    Begin processing if inputs are ok.
C
	    IF  ( proces )  THEN
C
C*	      For projection = SAT or RAD, make sure we only display as
C*	      many times as we have images for.
C 
	      IF ( uprj (1:3) .eq. 'SAT' .or. 
     +		   uprj (1:3) .eq. 'RAD' )
     +             ntime = MIN ( ntime, numimg )
C
C*	      Loop over times.
C
	      ipass = 1
	      itime = 1
	      more  = .true.
C
	      DO WHILE  ( more )
C
C*		Set the projection, garea for SAT (for each plot)
C
		IF ( uprj (1:3) .eq. 'SAT' .or. 
     +		     uprj (1:3) .eq. 'RAD' ) THEN
		    CALL GG_MAPS  ( proj, garea, imgfls (itime),
     +			     	    idrpfl, iret )
		    IF (iret .ne. 0) more = .false.
		END IF
C	             
C*		Set the current pixmap.
C*		If this is the first time, go to the first pixmap.
C*		If it is not the first time, go to the next pixmap.
C
		IF ( more ) THEN
		    IF  ( itime .eq. 1 )  THEN
	 	         first = .true.
		         CALL GSTANM ( iret )
		    ELSE
		         first = .false.
		         CALL GSPLOT ( iret )
		    END IF
		END IF

	        nplot = 0
	        CALL SN_STIM  ( iflno, times (itime), ier )
C
C*	        Loop over levels.
C
	    	ilevl  = 1
	    	morlev = .true.
		DO WHILE  ( morlev )
		  CALL SN_BEGS  ( iflno, ier )
		  vlevel = rlevel ( ilevl )
		  IF ( first )
     +		    CALL SNMOPT ( snfcur, times (itime), device, proj,
     +				area, garea, ncprm, prcons, icolor,
     +				map, title, clear, filtfc, ipass,
     +				panel, vlevel, voutc, iopt )
		  ipass = ipass + 1
		  IF  ( iopt .lt. 0 )  THEN
		      more   = .false.
		      morlev = .false.
		  END IF
C
C*	          Process clear, define panel, set up
C*		  filtering and draw map.
C
		  IF  ( more )  THEN
		    IF  ( clear )  CALL GCLEAR  ( iret )
C
C*		    Set the panel
C
		    CALL GG_PANL  ( panel, ier )
C
C*		    Apply LUT file
C
		    IF ( itime .eq. 1 ) CALL IM_LUTF ( lutfil, ier )
C
C*		    Display satellite image, if desired.
C
		    IF ( idrpfl .eq. 1 .or.
     +		       ( idrpfl .eq. 0 .and. clear )) THEN
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
C*		    Set up filtering.
C
		    IF  ( ( filtfc .ne. 0. ) .and. plot )  THEN
		        DO  m = 1, LLSTFL
		          sxplt (m) = RMISSD
			  syplt (m) = RMISSD
		        END DO
		    END IF
C
C*		    For special plotting, change the area on the 
C*		    second time through.
C
		    DO  lll = iloop, 2
		      IF (( lll .eq. 2 ) .and. ( iloop .eq. 1 )) THEN
			  CALL LC_UARE ( area2, newfil, iflno,
     +					 arecur, tstn, ier )
			  IF  ( ier .ne. 0 ) plot = .false.
		      END IF
C
C*		      Station loop.
C
		      iout = 0
		      DO WHILE  ( plot .and. ( iout .eq. 0 )  )
		        CALL SN_SNXT ( iflno, sta, id, slat, 
     +			               slon, selv, iout )
	                IF  ( iout .eq. 0 )  THEN
C
C*			  Get the data.
C
			  CALL SN_RDAT  ( iflno, numlev, data, 
     +			                  ihhmm, ier )
C
C*			  Filter, first parm filter and 
C*			  second sta filter, if requested.
C
			  IF  ( ier .eq. 0 )  THEN
			      ispri = 0
			      CALL PC_SSTN ( sta, id, slat, slon, selv,
     +				             ispri, ihhmm, numlev, ier )
			      CALL PC_CMVS ( vlevel, lvert, data, 
     +				             outd, chd, ier )
			      IF  ( ier .eq. 0 )  THEN
				CALL GTRANS ( 'M', 'P', 1, slat,
     +					       slon, sx, sy, ier2 )
			      END IF
			  END IF
C*
			  IF  ( ( ier .eq. 0 ) .and. ( filtfc .ne. 0. ) 
     +				.and. ( lll .eq. 2 ) )  THEN
			    CALL SNMOVR  ( sx, sy, sxplt, syplt,
     +				             nplot, offset, ier )
			  END IF
			  IF ( ier .eq. 0 .and. filtfc .ne. 0. )  THEN
C
C*			     Save x/y for no overlap.
C
			     nplot = nplot + 1
			     sxplt (nplot) = sx
			     syplt (nplot) = sy
			  END IF
C
C*    	                  Plot if we are ok to here.
C
	                  IF  ( ier .eq. 0 )  THEN
C
C*                             Grouping the station model as group type 10.
C
			    igroup = 10
                            CALL GSGRP (igroup, iret )
			    CALL SNMPLT ( icolor, parms, sx, sy, slat,
     +					  slon, chrflg, ncprm, outd,
     +					  chd, ier ) 
			    CALL GEGRP ( iret )
	                  END IF
			END IF
		      END DO
C
C*		      Create and draw the title.
C
		      ipbar = INDEX ( title, '|' )
		      IF  ( ipbar .ne. 0 )  THEN
			shrtin = title ( ipbar+1: )
			IF  ( ipbar .eq. 1 )  THEN
			    ttlinp = ' '
			ELSE
			    ttlinp = title ( :ipbar-1 )
			END IF
		      ELSE
			shrtin = ' '
			ttlinp = title
		      END IF
C
C*		      Create and draw the title.
C
		      CALL IN_TITL ( title, -3, ititl, linttl,
     +				     ttlstr, ier )
		      DO ii = 1, ncprm
			iscale (ii) = 0
		      END DO
		      ilvl = NINT ( vlevel )
		      IF  ( ititl .gt. 0 )  THEN
			CALL GR_MTTL  ( ttlstr, '^ @ _', .false.,
     +					times (itime), ' ', .true.,
     +					ilvl, -1, lvert, ncprm,
     +					prcons, iscale, ' ', ttt, ier )
			CALL GSCOLR  ( ititl, ier )
			CALL GG_WSTR ( ttt, linttl, ier )
		      END IF
C
C*		      Create the short title string.
C
		      IF  ( clear )  THEN
			CALL GR_MTTL  ( ttlstr, 'UPPER_AIR ^ @ #',
     +					.true., times (itime), ' ',
     +					.true., ilvl, -1, lvert,
     +					ncprm, prcons, iscale, area,
     +					shrttl, ier )
			CALL GMESG ( shrttl, ier )
		      END IF
C
C*		      Flush the graphics buffer.
C
		      CALL GEPLOT  ( iret )
		    END DO
		  END IF
		  ilevl = ilevl + 1
		  IF  ( ilevl .gt. nlev )  morlev = .false.
		END DO
		itime = itime + 1
		IF  ( itime .gt. ntime )  more = .false.
	      END DO
	    END IF
	    CALL GENANM ( iret )
	    CALL IP_DYNM  ( done, iret )
	END DO
C*
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'SNMAP', iperr, ' ', ier )
	CALL GENDP  ( 0, iret )
	CALL IP_EXIT  ( iret )
C*
	END
