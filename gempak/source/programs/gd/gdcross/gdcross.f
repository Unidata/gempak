	PROGRAM  GDCROSS
C************************************************************************
C* PROGRAM GDCROSS							*
C*									*
C* This program creates cross sections through scalar grids.		*
C*									*
C**									*
C* Log:									*
C* K. F. Brill/GSC    6/89    Created from GDPROF			*
C* K. Brill/GSC      11/89    Added calls to DG_OFIL, DG_FLNO, DG_AREA	*
C* K. Brill/GSC       1/90    Added CALL IN_TEXT 			*
C* K. Brill/GSC       5/90    Changes for IN_AXIS and IN_CINT		*
C* S. Schotz/GSC      7/90    Update for IN_LINE			*
C* S. Schotz/GSC      7/90    Added changes for IN_PTYP			*
C* K. Brill/NMC       8/90    Added call to GDXSDL; remove -9 error	*
C* K. Brill/NMC       8/90    DG_OFIL calling sequence change		*
C* K. Brill/NMC      11/90    Chngd intrpltn rng for wnds in GDXGRD	*
C* K. Brill/NMC	      1/91    Remove GVCORD from CALL GDXGTS		*
C* K. Brill/NMC       3/91    Use scalar field to make the label	*
C* J. Whistler/SSAI   4/91    Changed GDXTTL to GR_TITL			*
C* M. desJardins/NMC 10/91    Changed panel to *48			*
C* K. Brill/NMC	     01/92    Changes for contour filling		*
C* K. Brill/NMC      01/92    Replace GERROR with ER_WMSG	        *
C* S. Jacobs/EAI     11/92    Added call to GMESG and 'shrttl'          *
C* K. Brill/NMC	      4/93    Set origin for MSFC calculation		*
C* L. Sager/NMC	      7/93    Added REFVEC to GDXINP and GDXUPD		*
C* S. Jacobs/EAI      9/93    Added CLRBAR, IN_CBAR and GG_CBAR		*
C* S. Jacobs/EAI      9/93    Changed IN_CBAR and GG_CBAR to GG_CBAR	*
C* S. Jacbos/EAI      9/93    Modified short title			*
C* S. Jacobs/EAI      2/94    Added COLADD flag to DG_OFIL		*
C* S. Jacobs/NMC      3/94    Removed interpolation of vector to 	*
C*			        background grid				*
C* L. Williams/EAI    3/94    Clean up declarations of user input	*
C*			      variables					*
C* S. Jacobs/NMC      6/94    DEVICE*12 --> *72                         *
C* L. Williams/EAI    7/94    Removed call to GDXUPD and added shrttl	*
C*			      to the user input variables		*
C* S. Jacobs/NMC      9/94    Moved the title plotting to the end	*
C* D. Keiser/GSC      8/96    Added FL_MFIL to search for file type	*
C* K. Tyle/GSC	      8/96    Added ER_WMSG call after FL_MFIL call,	*
C*			      use filnam in GDXDSP			*
C* S. Maxwell/GSC     7/97    Increased input character length          *
C* S. Jacobs/NCEP    10/97    Added the border color to GDXSDL for 	*
C*			      side labels for THTA			*
C* M. Li/GSC          1/00    Added GCNTLN and nflag; removed GCSPLN    *
C* R. Curtis 	      8/00    Added calls to GSTANM and GENANM          *
C* S. Jacobs/NCEP     3/01    Replaced DG_OFIL with DG_MFIL		*
C* T. Lee/GSC	      6/01    Processed multiple files; Added time loop	*
C* T. Lee/SAIC	     10/01    Added contour fill types			*
C* K. Brill/HPC	      5/02    Allow cross section path to cross grid	*
C*			      discontinuity.				*
C* R. Tian/SAIC	     10/02    Added call to DG_CXGP			*
C* K. Brill/HPC	     12/02    Added IJSKIP parameter; chk for -4 error	*
C* K. Brill/HPC	      4/03    CALL DG_INTL				*
C* R. Tian/SAIC      11/03    Added nuflg to DG_INTL call               *
C* T. Piper/SAIC	06/04	Added call to GD_CPF 			*
C* T. Piper/SAIC	10/04	Moved GG_PANL after GCLEAR		*
C* K. Brill/HPC      10/04    Changes for time/file management          *
C* C. Bailey/HPC      6/06    Added contour labels			*
C* C. Bailey/HPC     10/06    Added clbl to GDXSDL calling sequence	*
C* C. Bailey/HPC     10/06    Added suppress small contour flag		*
C* S. Gilbert/NCEP   05/07    Removed call to GCNTLN  			*
C* S. Gilbert/NCEP   08/07    Redimensioned many arrays to remove LLMXGD*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* F. J. Yen/NCEP	07/08	Turned off surface plotting if not PRES *	
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	gdfile*(LLMXLN), border*(LLMXLN), ptype*(LLMXLN),
     +			gdatim*(LLMXLN), gfunc*(LLMXLN), gvcord*(LLMXLN),
     +			title*(LLMXLN),	yaxis*(LLMXLN), device*(LLMXLN),
     +			scale*(LLMXLN), panel*(LLMXLN),	cxstns*(LLMXLN),
     +			wind*(LLMXLN), cint*(LLMXLN), line*(LLMXLN),
     +			text*(LLMXLN), contur*(LLMXLN), fint*(LLMXLN),
     +			fline*(LLMXLN), ctype*(LLMXLN),	gvect*(LLMXLN),
     +			skip*(LLMXLN), refvec*(LLMXLN), clrbar*(LLMXLN),
     +			shrttl*(LLMXLN), ijskip*(LLMXLN)
C*
	LOGICAL         lscal, lvert, first, scflag
	LOGICAL		cflag, lflag, sflag, bflag, fflag, nflag, gottm
C*
	REAL 		ugrd (LLMXLV*MXXPTS), vgrd (LLMXLV*MXXPTS)
	REAL	        ponth (LLMXLV*MXXPTS), xgrd (LLMXLV*MXXPTS) 
	REAL            qgrd (LLMXLV*MXXPTS), rlvls (LLMXLV),
     +			qlvls (LLMXLV), vlvls (LLMXLV), ylbl (LLAXIS),
     +                  rgx (MXXPTS), rgy (MXXPTS), rlat (MXXPTS),
     +			rlon (MXXPTS), vclsfc (MXXPTS)
	CHARACTER	time (2)*20, lastim*20, ttlstr*72, parm*12,
     +			timev (2)*20, parmv*12, firstm*20, prmlbl*12,
     +			timfnd*36, times (2)*20, clbl(LLCLEV)*24, 
     +                  flbl(LLCLEV)*24
	LOGICAL		respnd, done, proces, havsfc, havscl, havvec
C*
	REAL		clvl (LLCLEV), flvl (LLCLEV), rmargn (4)
	INTEGER		icolor (LLCLEV), iline (LLCLEV), ilwid (LLCLEV),
     +			labflg (LLCLEV), ifcolr (LLCLEV),
     +			iflabl (LLCLEV), ifltyp (LLCLEV), level(2)
C-----------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDCROSS', ier )
C
C*      Initialize GEMPLT.
C
	    CALL GG_INIT  ( 0, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C
        	CALL GD_INIT  ( iperr )
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
C*	Main loop to read in TAE parameters and draw profile.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL GDXINP  ( gdfile, gdatim, gvcord, cxstns, gfunc, 
     +			 cint, scale, line, ptype, yaxis, border, gvect,
     +                   wind, refvec, skip, title,  clear,
     +			 device, text, panel, contur, fint, fline,
     +			 ctype, clrbar, ijskip, iperr )
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
C*          Set the attributes that do not vary within the time loop.
C
	    IF  ( proces )  THEN
C
C*	      Set the text attributes, especially the size,
C*	      before setting the margins.
C
	      CALL IN_TEXT ( text, ier )
C
C*	      Get contouring type.
C

	      CALL IN_CONT ( contur, ier )
	      CALL IN_CTYP ( ctype, nflag, lflag, sflag, bflag, 
     +		             fflag, ier )
	      IF ( lflag .or. sflag .or. bflag .or. nflag ) THEN
		  cflag = .true.
		ELSE
		  cflag = .false.
	      END IF
	    END IF
C
C*	    Loop over times.
C
	    itime = 1
	    gottm = proces
	    first = .true.
	    DO  WHILE ( gottm )
C
C*		Get the next time to process from the time server.
C
		CALL DG_NTIM ( .true., .true., time, gottm, ier )
		proces = ( ier .eq. 0 .and. gottm )
		IF ( ier .ne. 0 ) THEN
		    ier = 3
		    CALL ER_WMSG ( 'GDCROSS', ier, time(1), irr)
		END IF
		CALL TG_DUAL ( time, timfnd, ier )
		IF ( proces .and. first ) THEN
		    CALL DG_FLNO  ( gfunc, iflnos, ier )
		    CALL DG_QDTM ( iflnos, firstm, lastim, ier )
		    IF ( firstm .eq. ' ' ) THEN
			proces = .false.
		    END IF
		END IF
C
C*		Get time and vertical coordinate to use.
C
		IF  ( proces )  THEN
		    CALL GDXDTV   ( timfnd, gvcord, gfunc, 
     +				    firstm, lastim, times, ivcord, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
			proces = .false.
		    END IF
		END IF
C*
                IF  ( proces .and. first )  THEN
                    CALL DG_FLNO  ( gvect, iflnov, ier1 )
		    CALL DG_QDTM ( iflnov, firstm, lastim, ier )
		    IF ( firstm .eq. ' ' ) THEN
			proces = .false.
		    END IF
                END IF
C
                IF  ( proces )  THEN
                    CALL GDXDTV  ( timfnd, gvcord, gvect,
     +                             firstm, lastim, timev, jvcord, iret )
                    IF  ( iret .ne. 0 )  THEN
                        CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
                        proces = .false.
                    END IF
                END IF
C
C*		Get information about y axis.
C
	        IF  ( proces .and. first )  THEN
		    CALL GDXYAX  ( ptype, yaxis, ivcord, iyaxis, ratio, 
     +				   ystrt, ystop, ylbl, nylbl, rmargn, 
     +                             ilbfrq, iglfrq, itmfrq, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
			proces = .false.
		    END IF
	        END IF
C
C*	GD_CPF added to process possible file name containing lat/lon pairs.
C
		CALL GD_CPF ( cxstns, cxstns, ier )
                IF ( ier .ne. 0 ) THEN
                    iret = -4
                    CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
                    proces = .false.
                END IF
		igt = INDEX ( cxstns, '>' )
		IF ( proces .and. igt .eq. 0 ) THEN
		    iret = -4
		    CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
		    proces = .false.
		END IF
C
C*		Compute subset grid needed for cross section path
C
		IF ( proces ) THEN
		    CALL DG_CXGP ( cxstns, ijskip, MXXPTS,
     +			           nhxs, rgx, rgy, rlat, rlon, iret )
		END IF
		IF  ( iret .ne. 0 ) proces = .false.
C
C*		Compute length of cross section.
C
		IF ( proces ) THEN
		    CALL GDXLEN ( nhxs, rlat, rlon, rlngth, iier )
		END IF
C
C*		Check that there are some points.
C
		IF  ( nhxs .le. 0 )  THEN
		    proces = .false.
		END IF
C
C*		Set the origin of the cross section for MSFC calculation.
C
		IF ( proces .and. first ) THEN
		    CALL DG_ORGN ( rlat (1), rlon (1), ier )
		END IF
C*
		IF ( proces ) THEN
		    IF (ivcord .eq. 1) THEN
C
C*	                The vertical coord. is PRES, so get surface data
C
      		        CALL GDXGTS  ( times, ivcord, rgx, rgy, nhxs,
     +				   vclsfc, havsfc, parm, ier )
		    ELSE
C
C*                      Set havsfc to false since not PRES
C
			havsfc = .false.
		    END IF
C
C*		    Get scalar data to plot.
C
    		    CALL GDXDTA ( iflnos, timfnd, gvcord, ystrt,
     +				  ystop, gfunc, times, ivcord,
     +				  rgx, rgy, nhxs, rlvls, xgrd,
     +				  nvxs, prmlbl, ybeg, yend, iret )
C
C*		    If all is well, create a regularly spaced grid.
C
		    IF ( iret .eq. 0 ) THEN
			havscl = .true.
			CALL GDXGRD ( xgrd, nhxs, nvxs, ivcord, iyaxis,
     +				      rlvls, ystrt, ystop, .false.,
     +				      qgrd, qlvls, nvo, iret )
			IF ( iret .ne. 0 ) THEN
			    iret = - 10
			    CALL ER_WMSG ( 'GDCROSS', iret, ' ', ier )
			  ELSE
C
C*			    Set underground values to missing.
C
			    IF ( havsfc ) THEN
				CALL GDXSFM ( ivcord, qgrd, qlvls, 
     +					      nhxs, nvo, vclsfc, iret )
			    END IF
			END IF
		      ELSE
			havscl = .false.
			IF ( iret .lt. 0 ) proces = .false.
		    END IF
		END IF
C
C*		Get the vector components defined by GVECT.
C
		IF ( proces ) THEN
    		    CALL GDXDVV ( iflnov, timfnd, gvcord, ystrt,
     +				  ystop, gvect, timev, ivcord, rgx, rgy,
     +				  nhxs, rlvls, ugrd, vgrd, ponth, nvv,
     +				  parm, parmv, lvert, lscal, iret )
		    IF ( iret .eq. 0 ) THEN
			havvec = .true.
			IF ( .not. havscl ) prmlbl = parm
C
			DO  ik = 1, nvv
			    vlvls (ik) = rlvls (ik)
			END DO
C
		        IF ( havsfc ) THEN
			    CALL GDXSFM ( ivcord, ugrd, vlvls, nhxs,
     +					  nvv, vclsfc, iret )
			    CALL GDXSFM ( ivcord, vgrd, vlvls, nhxs,
     +					  nvv, vclsfc, iret )
			END IF
		      ELSE
			havvec = .false.
			IF ( iret .lt. 0 ) proces = .false.
		    END IF        
		END IF
C
C*		Define contour levels and characteristics.
C*		Write warning if there are no contour levels.
C
		nlvl = 0
		IF ( proces .and. havscl ) THEN
		    CALL GDXLEV ( cflag, line, cint, fflag, fline, fint,
     +				  scale, nhxs, nvo, 1, 1, nhxs, nvo,
     +				  qgrd, nlvl, clvl, clbl, icolor, iline, 
     +				  ilwid, labflg, nflvl, flvl, ifcolr,
     +				  iflabl, ifltyp, iscale, dmin, dmax, 
     +				  scflag, iret )
		    IF ( ( nlvl .eq. 0 .and. nflvl .eq. 0 ) .or.
     +			 ( iret .ne. 0 ) )  THEN
			CALL ER_WMSG ( 'GDCROSS',1,' ',ier)
		    END IF
C
		    IF ( nlvl .eq. 0 ) cflag = .false.
		    IF ( nflvl .eq. 0 ) fflag = .false.
		END IF
C
C*		Set the current pixmap.
C
		IF  ( proces .and. first )  THEN
		    CALL GSTANM ( iret )
		ELSE IF ( proces ) THEN
		    CALL GSPLOT ( iret )
		END IF
C
C*		Give user a chance to exit.
C
		IF  ( proces )  THEN
		    CALL GDXDSP  ( gdfile, gfunc, cxstns, nhxs, iscale,
     +				   timfnd, gvcord, nlvl, clvl,
     +				   dmin, dmax, icolor, iline, ilwid,
     +				   labflg, nflvl, flvl, ifcolr, iflabl,
     +				   ifltyp, device, panel, gvect, skip, 
     +				   wind, first , iret )
C
C*  		    Stop looping if user requests exist.
C
		    IF ( iret .ne. 0 ) THEN
			proces = .false.
			gottm = .false.
		    END IF
C
C*  		    Set first to false upon first successful plot.
C
		    first = .false.
		END IF
C
C*		Draw the cross section.
C
		IF  ( proces ) THEN
C
C*		    Set plotting mode to graph mode.
C
		    CALL GQMODE  ( mode, ier )
		    CALL GSMODE  ( 2, ier )
C
C*		    Clear the screen if requested, and set the panel.
C
		    IF  ( clear )  CALL GCLEAR  ( ier )
		    CALL GG_PANL  ( panel, ier )
C
C*		    Set up the graph.
C	
		    xstrt = 1.00
	            xstop = FLOAT ( nhxs )
		    CALL GDXSUG ( iyaxis, ystrt, ystop, xstrt, xstop,
     +				  ratio, rmargn, iret )
C
C*	            Draw the contours.
C
	            IF  ( havscl )  THEN
			CALL GSGGRF ( 1, iyaxis, nhxs, nvo, xstrt,
     +				      ystrt, xstop, ystop, iret )
C
C*			Do side labels for THTA.
C
			parmv = ' '
			parmv = gfunc (1:4)
			CALL ST_LCUC ( parmv, parmv, ier )
			IF  ( parmv (1:4) .eq. 'THTA' .and.
     +			      ( iret .eq. 0 ) )  THEN
			    IF ( cflag )  THEN
				CALL GDXSDL ( border, nhxs, nvo, qgrd,
     +					      nlvl, clvl, clbl, labflg, 
     +                                        iret )
			    END IF
			    IF ( fflag )  THEN
				DO i = 1, nflvl
				  CALL GR_LABL(flvl(i), 0, 0, flbl(i), 
     +                                                      nchar, ier)
				END DO
      				CALL GDXSDL ( border, nhxs, nvo, qgrd,
     +					      nflvl, flvl, flbl, iflabl, 
     +                                        iret)
			    END IF
			END IF
C
			IF ( iret .eq. 0 ) THEN
			    IF  ( fflag )  THEN
				CALL GCFILL ( nhxs, nvo, qgrd, 0, 0, 0,
     +					      nflvl, flvl, ifcolr,
     +					      iflabl, ifltyp, iret )
				IF ( iret .ne. 0 )  THEN
				  CALL ER_WMSG('GEMPLT', iret, ' ', ier)
				END IF
			    END IF
C
			    IF  ( cflag )  THEN
			        IF  ( lflag )  THEN
				    CALL GCLGRN ( nhxs, nvo, qgrd, 0, 0,
     +						  0, nlvl, clvl, clbl,
     +						  icolor, iline, ilwid,
     +						  labflg, scflag, iret )
				    IF ( iret .ne. 0 )  
     +				        CALL ER_WMSG  ( 'GEMPLT', iret,
     +							' ', ier )
C
			        END IF
C
			        IF  ( bflag )   THEN
				    CALL GCBOXX ( nhxs, nvo, qgrd, 0, 0,
     +						  0, nlvl, clvl, icolor,
     +						  iline, ilwid, labflg,
     +						  iret )
				    IF ( iret .ne. 0 )  CALL ER_WMSG 
     +						('GEMPLT',iret,' ',ier)
			        END IF
			    END IF
		          ELSE
			    iret = -11
			    CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
		        END IF
		    END IF
C
		    IF ( havvec ) THEN
			IF ( lscal ) THEN
C
C*		            Scale the vertical component.
C
			    asprat=0.0
			    CALL GDXSCV ( vgrd, ponth, vlvls, nhxs, nvv,
     +				          rlngth, ivcord, iyaxis, ystrt,
     +				          ystop, asprat, vgrd, iiir )
			    IF ( iiir .ne. 0 )  THEN
			        CALL ER_WMSG  
     +					( 'GDCROSS', iiir, ' ', ier )
			    END IF
		        END IF
C
C*		        Load the locations of the wind points into
C*		        arrays xgrd and qgrd.
C
		        indx = 1
		        DO k = 1, nvv
		        DO i = 1, nhxs
			    xgrd ( indx ) = FLOAT ( i )
			    qgrd ( indx ) = vlvls ( k )
			    indx = indx + 1
		        END DO
		        END DO
C
C*		        Plot the vector field.
C
		        CALL GDXPUW ( gvect, ugrd, vgrd, xgrd, qgrd,
     +				      nhxs, nvv, wind, skip, refvec,
     +				      ier )
		    END IF
C
C*		    Plot background axes with labels.
C
	            CALL GDXPLT ( border, ystrt, ystop, vclsfc, havsfc,
     +				  ylbl, nylbl, xstrt, xstop, cxstns,
     +				  nhxs, ilbfrq, iglfrq, itmfrq, iret )
C
C*		    Plot the color bar.
C
		    IF  ( fflag )  CALL GG_CBAR ( clrbar, nflvl, flvl,
     +					          ifcolr, ier )
C
C*		    Write title.
C
		    CALL IN_TITL (title, 0, ititl, linttl, ttlstr, ier)
		    level(1) = -1
		    level(2) = -1
		    CALL GR_TITL ( ttlstr, times, .false., level, ivcord,
     +				   prmlbl, iscale, ' ', ttlstr, shrttl,
     +				   iret )
		    IF  ( clear )  CALL GMESG  ( shrttl, ier )
		    IF  ( ititl .ne. 0 )  THEN
		        CALL GSCOLR   ( ititl, ier )
		        CALL GG_WSTR  ( ttlstr, linttl, ier )
		    END IF
C
C*		    Reset the plotting mode and flush buffers.
C
		    CALL GSMODE  ( mode, ier )
		    CALL GEPLOT  ( ier )
C
C*  		    Increment itime only if plot was successful.
C
		    itime = itime + 1
	        END IF
	    END DO
C
            CALL GENANM   ( iret )
C
C*	    Prompt for next cross section to be done.
C
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDCROSS', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the interface.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
