	PROGRAM  NEXR2RHI
C************************************************************************
C* PROGRAM NEXR2RHI							*
C*									*
C* This program creates cross sections through Level II radar volumes.	*
C*									*
C**									*
C* Log:									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear, interp
	CHARACTER	border*(LLMXLN), ptype*(LLMXLN),
     +			gvcord*(LLMXLN),
     +			title*(LLMXLN),	yaxis*(LLMXLN), device*(LLMXLN),
     +			scale*(LLMXLN), panel*(LLMXLN),	cxstns*(LLMXLN),
     +			cint*(LLMXLN), line*(LLMXLN),
     +			text*(LLMXLN), contur*(LLMXLN), fint*(LLMXLN),
     +			fline*(LLMXLN), ctype*(LLMXLN),
     +			clrbar*(LLMXLN),
     +			shrttl*(LLMXLN), radfil*(LLMXLN),
     +			radtim*(LLMXLN), radparm*(LLMXLN)
C*
	LOGICAL         first, scflag
	LOGICAL		cflag, lflag, sflag, bflag, fflag, nflag
C*
	PARAMETER	(NCXP = 1000)
C*
	REAL	        qgrd (LLMXGD),
     +			ylbl (LLAXIS)
	CHARACTER	time (2)*20, ttlstr*72, prmlbl*12,
     +			timfnd*20, parm*48, clbl(LLCLEV)*24,
     +			imgfls(MXLOOP)*132, tplate*(LLMXLN)
	LOGICAL		respnd, done, proces
C*
	REAL		clvl (LLCLEV), flvl (LLCLEV), rmargn (4)
	INTEGER		icolor (LLCLEV), iline (LLCLEV), ilwid (LLCLEV),
     +			labflg (LLCLEV), ifcolr (LLCLEV),
     +			iflabl (LLCLEV), ifltyp (LLCLEV), level(2)
C*
	INTEGER		nhxs, nhys
	REAL		dhorz, dvert
	PARAMETER	(dhorz = 1.00)
	PARAMETER	(dvert = .20)
C-----------------------------------------------------------------------
C*	Initialize TAE and GEMPLT.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL GG_INIT  ( 0, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
	CALL IP_IDNT  ( 'NEXR2RHI', ier )
C
C*      Initialize the DG library.
C
        CALL DG_INTL ( ier )
C
C*	Set the graphics mode to map so that we can get cxstns projection
C
	CALL GSMODE  ( 1, ier )
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
	  CALL GDXINP  ( radtim, gvcord, cxstns, radfil, radparm,
     +			 cint, scale, line, ptype, yaxis, border,
     +                   title,  clear,
     +			 device, text, panel, contur, fint, fline,
     +			 ctype, clrbar, interp, iperr )
C
C*	  Exit if there is an error.
C
	  IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	  ELSE
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
C
C*	      Define the view region.
C
	      CALL GG_PANL  ( panel, ier )
	    END IF
C
C*	    Get radar times.
C
	    IF  ( proces )  THEN
                CALL GTMFLS ( radfil, radtim, MXLOOP, imgfls,
     +              ntime, tplate, iret )
                IF ( tplate .eq. ' ' )
     +              CALL ST_FLST  ( radfil, ';', ' ', MXLOOP,
     +                  imgfls, ntime, iret )

		IF  ( ( iret .ne. 0 ) .or. ( ntime .lt. 1 )  )  THEN
		    CALL ER_WMSG ( 'GTMFLS', iret, ' ', ier )
		    proces = .false.
		END IF
	    END IF
C
	    IF  ( ntime .gt. MXLOOP )  THEN
		CALL ER_WMSG ( 'GTMFLS', 5, ' ', ier )
		ntime = MXLOOP
	    END IF
C
C*	    Loop over times.
C
	    itime = 1
	    DO  WHILE ( proces .and. ( itime .le. ntime ) )
		first = ( itime .eq. 1 )
C
C*		Get information about y axis.
C
	        IF  ( proces .and. first )  THEN
		    ivcord = 3
		    CALL GDXYAX  ( ptype, yaxis, ivcord, iyaxis, ratio, 
     +				   ystrt, ystop, ylbl, nylbl, rmargn, 
     +                             ilbfrq, iglfrq, itmfrq, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'NEXR2RHI', iret, ' ', ier )
			proces = .false.
		    ELSE
		       nhxs = 920
		       nhys = INT ( (ystop - ystrt ) / (dvert * 1000.)
     +			+ 1 )
		    END IF
	        END IF
C
		IF ( first ) THEN
		   CALL SET_CXSTNS ( cxstns )
		END IF
C
		CALL ST_NULL ( imgfls(itime), imgfls(itime),lens, ier )
		CALL ST_NULL ( radparm, parm, lens, ier )
		CALL RNEX2 ( imgfls(itime), parm, qgrd,
     +			nhxs, nhys, dhorz, dvert, clat, clon, interp,
     +			timfnd, iret )
		CALL ST_RNUL ( timfnd, timfnd, lens, iret )
C
C*
C
C*              Define contour levels and characteristics.
C*              Write warning if there are no contour levels.
C
                nlvl = 0
                IF ( proces ) THEN
                    CALL GDXLEV ( cflag, line, cint, fflag, fline, fint,
     +                            scale, nhxs, nhys, 1, 1, nhxs, nhys,
     +                            qgrd, nlvl, clvl, clbl, icolor, iline,
     +                            ilwid, labflg, nflvl, flvl, ifcolr,
     +                            iflabl, ifltyp, iscale, dmin, dmax,
     +                            scflag, iret )
                    IF ( ( nlvl .eq. 0 .and. nflvl .eq. 0 ) .or.
     +                   ( iret .ne. 0 ) )  THEN
                        CALL ER_WMSG ( 'GDCROSS',1,' ',ier)
                    END IF
C
                    IF ( nlvl .eq. 0 ) cflag = .false.
                    IF ( nflvl .eq. 0 ) fflag = .false.
                END IF
C
C*		Set the current pixmap.
C
		IF  ( first )  THEN
		    CALL GSTANM ( iret )
		  ELSE
		    first = .false.
		    CALL GSPLOT ( iret )
		END IF
C
C*		Give user a chance to exit.
C
		IF  ( proces )  THEN
		    CALL GDXDSP  ( imgfls(itime), cxstns, clat, clon, 
     +				   nhxs, nhys, iscale,
     +				   timfnd, gvcord, nlvl, clvl,
     +				   dmin, dmax, icolor, iline, ilwid,
     +				   labflg, nflvl, flvl, ifcolr, iflabl,
     +				   ifltyp, device, panel, 
     +				   first, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
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
C*		    Clear screen if requested.
C
		    IF  ( clear )  CALL GCLEAR  ( ier )
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
		    CALL GSGGRF ( 1, iyaxis, nhxs, nhys, xstrt,
     +				      ystrt, xstop, ystop, iret )

		    IF  ( fflag )  THEN
                        CALL GCFILL ( nhxs, nhys, qgrd, 0, 0, 0,
     +                      nflvl, flvl, ifcolr,
     +                      iflabl, ifltyp, iret )
                        IF ( iret .ne. 0 )  THEN
                            CALL ER_WMSG('GEMPLT', iret, ' ', ier)
                        END IF
                    END IF
C
                    IF  ( cflag )  THEN
                        IF  ( lflag )  THEN
                            CALL GCLGRN ( nhxs, nhys, qgrd, 0,
     +				0, 0, nlvl, clvl, clbl,
     +				icolor, iline, ilwid, 
     +				labflg, scflag, iret )
                            IF ( iret .ne. 0 )
     +                          CALL ER_WMSG  ( 'GEMPLT', iret,
     +					' ', ier )
C
                        END IF
C
                        IF  ( bflag )   THEN
			    ioffx = 0
			    ioffy = 0
				    
                            CALL GCBOXX ( nhxs, nhys, qgrd, 0, 
     +				0, 0, nlvl, clvl, 
     +				icolor, iline, ilwid, 
     +				labflg, iret )
                            IF ( iret .ne. 0 )  CALL ER_WMSG
     +                                          ('GEMPLT',iret,' ',ier)
                        END IF
                    END IF
C
C*		    Plot background axes with labels.
C
	            CALL GDXPLT ( border, ystrt, ystop,
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
		    level(1) = 0
		    level(2) = -1
		    time(1) = timfnd
		    time(2) = ' '
		    prmlbl = parm(1:12)
		    CALL GR_TITL ( ttlstr, time, .false., level, ivcord,
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
		    CALL GEPLOT  ( ier )
		    CALL GSMODE  ( mode, ier )
	        END IF
	        itime = itime + 1
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
C
C*	Print general error messages if necessary.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'NEXR2RHI', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the interface.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
