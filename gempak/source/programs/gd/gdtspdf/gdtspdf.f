	PROGRAM  GDTSPDF
C************************************************************************
C* PROGRAM GDTSPDF							*
C*									*
C* This program plots probability density or cumulative probability	*
C* at selected point as function of grid value and forecast time.	*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		10/07	Created					*
C* M. Li/SAIC		01/08	Removed check for "NONE" in GVCORD	*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	gdfile*(LLMXLN), border*(LLMXLN),
     +			ptype*(LLMXLN), gpoint*(LLMXLN),
     +			taxis*(LLMXLN), gdatim*(LLMXLN),
     +			gfunc*(LLMXLN), gvcord*(LLMXLN),
     +			title*(LLMXLN), yaxis*(LLMXLN),
     +			device*(LLMXLN), scale*(LLMXLN),
     +			panel*(LLMXLN), shrttl*(LLMXLN),
     +			cint*(LLMXLN), line*(LLMXLN),
     +			text*(LLMXLN), prbtyp*(LLMXLN),
     +			hilo*(LLMXLN), hlsym*(LLMXLN), 
     +			clrbar(LLMXLN), fint*(LLMXLN), 
     +			fline*(LLMXLN), ctype*(LLMXLN), 
     +			output*(LLMXLN), glevel*(LLMXLN)
C*
	REAL 		grdout (LLMXLV * LLMXGT) 
	REAL	        x (LLMXGT), flvl (LLCLEV), 
     +			ylbl (LLAXIS), xptsc (LLMXGT),
     +			vclsfc, xtlbl (LLAXIS),
     +			grdt1 (LLMXLV * LLMXGT)
	CHARACTER	ttlstr*72, ctlbl (LLAXIS)*40, 
     +			trange (2)*40, timfnd (LLMXGT)*20,
     +			time (2)*20, clbl(LLCLEV)*24, parm*32
	LOGICAL		respnd, done, proces, havsfc
	LOGICAL         cflag, lflag, sflag, bflag, fflag,
     +                  nflag, scflag
C*
	REAL		clvl (LLCLEV), rmargn (4)
	INTEGER		icolor (LLCLEV), iline (LLCLEV),
     +			ilwid  (LLCLEV), labflg (LLCLEV),
     +			levl (2), ifcolr (LLCLEV), iflabl (LLCLEV),
     +			ifltyp (LLCLEV)	
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDTSPDF', ier )
C
C*  Initialize GEMPLT.
C
	    CALL GG_INIT  ( 1, iperr )
	    IF ( ier .eq. 0 )  THEN
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
C*  Main loop to read in TAE parameters and draw time section.
C
	DO WHILE  ( .not. done )
C
C*  Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*  Read in the variables from the TAE.
C
	    CALL GDTSINP  (  gdfile, gdatim, gvcord, glevel, gfunc,
     +                     gpoint, taxis, yaxis, ptype, prbtyp, cint,
     +                     line, hilo, hlsym, clrbar, fint, fline,
     +                     ctype, border, title,  clear,
     +                     device, scale, panel, text, output, iperr ) 
	    CALL ST_NUMB ( prbtyp, iprob, ier )
C
C*  Exit if there is an error.
C
	    IF ( iperr .ne. 0 )  THEN
		done = .true.
	      ELSE
C
C*  Process the GDFILE input.
C
	        ip = INDEX ( gdfile, '{' )
	        ip2 = INDEX ( gdfile, '}' )
		IF ( ip .ne. 1 .or. ip2 .le. 1 ) THEN
		    iret = -5
                    CALL ER_WMSG  ( 'GDTSPDF', iret, ' ', ier )
                    proces = .false.
                END IF
C*
		CALL DG_NFIL ( gdfile, ' ', ier )
		IF ( ier .ne. 0 ) THEN
		    CALL ER_WMSG ( 'DG', ier, ' ', irr )
		    proces = .false.
		END IF
C
C*  Process the GDATTIM input; setup the time server.
C
		CALL DG_NDTM ( gdatim, ier )
		IF ( ier .ne. 0 ) THEN
		    CALL ER_WMSG ( 'DG', ier, gdatim, irr )
		    proces = .false.
		END IF
C
C*		Get time information 
C
		IF ( proces ) THEN
		    CALL DG_QTMS ( LLMXGT, .false., timfnd, ntime,
     +				   trange (1), iret )
		    IF ( iret .ne. 0 ) proces = .false.
C
C*  		    Check for number of times greater than zero.
C
		    proces = ( ntime .gt. 0 )
		END IF
C*
		IF ( proces ) THEN
		    CALL ST_LCUC ( gvcord, gvcord, iret )
		    CALL LV_CORD ( gvcord, gvcord, ivcord, iret )
		    IF ( iret .ne. 0 ) proces = .false.
		END IF
C
C*		Set up the graphics device.
C
		IF ( proces ) THEN
		    CALL GG_SDEV  ( device, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*		Set text and contour attributes.
C
		IF ( proces ) THEN
		    CALL IN_TEXT ( text, iret )
		    IF ( iret .ne. 0 ) proces = .false.
		END IF
C
		CALL IN_CTYP  ( ctype, nflag, lflag, sflag, bflag,
     +                          fflag, ier )
                IF  ( lflag .or. sflag .or. bflag .or. nflag )  THEN
                    cflag = .true.
                  ELSE
                    cflag = .false.
                END IF

		IF ( proces ) THEN
                    CALL GSTANM ( iret )
                    CALL GSPLOT ( iret )
                END IF

C
C*              GD_CPF checks if a file name was entered for the grid point
C
                CALL GD_CPF(gpoint, gpoint, ier)
                IF ( ier .ne. 0 ) THEN
                    iret = -14
                    CALL ER_WMSG  ( 'GDTSPDF', iret, ' ', ier )
                    proces = .false.
                END IF

C
C*		Get y-axis parameters
C
		IF ( proces ) THEN
		    CALL GDTSYAX( ptype, yaxis, timfnd, ntime, 
     +		                 gvcord, glevel, gfunc, gpoint,
     +				 ymin, ymax, rgx, rgy, plat, plon,
     +				 iyaxis, ratio, ystrt, ystop, ylbl,
     +				 nylbl, rmargn, ilbfrq, iglfrq, 
     +				 itmfrq, iret )
		    IF ( iret .ne. 0 ) THEN
			CALL ER_WMSG( 'GDTSPDF', iret, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*	Check for invalid times
C
		IF ( proces ) THEN
		    DO i = 1, ntime
			CALL TG_VALD ( timfnd(i), timfnd(i), iret )
			IF ( iret .ne. 0 ) proces = .false.
		    END DO
		END IF
C
C*		Get x-axis parameters
C
		IF ( proces ) THEN
		    CALL GDTXTA ( taxis, LLAXIS, ntime, timfnd, x,
     +				  xstrt, xstop, xtlbl, ctlbl, nxlbl,
     +				  xmndst, itlbfr, itglfr, ittmfr,
     +				  iret )
		    IF ( iret .ne. 0 ) proces = .false.
C
C*		    Get evenly spaced levels in x-axis
C
		    CALL GDTXEV ( x, ntime, 1, xptsc, nptsc, iret)
                    IF ( iret .ne. 0 ) proces = .false.
		END IF
C
C*		Open the grid file and set the grid navigation.  This
C*		will set the proper mode for the grid file.  The mode
C*		must be set to graph mode later.
C
C
C*		Get data for this list of times and values 
C
		IF ( proces ) THEN
		    CALL GDTSDAT ( gdfile, gdatim, gpoint, timfnd, 
     +				   ntime, gvcord, glevel,
     +				   gfunc, iprob, ymin, ymax, ylbl,
     +			           nylbl, ystrt, ystop,
     +                             rgx, rgy, grdt1, iret )
		    IF ( iret .lt. 0 ) proces = .false.
		END IF
C
C*		Put contour data in evenly spaced grid
C
		IF ( proces ) THEN
		    CALL GDTXTI( grdt1, ntime, nylbl, x, xptsc, nptsc,
     +				 grdout, iret)
		    IF ( iret .ne. 0) proces = .false.
		END IF
C
C*		Decide contour levels, colors and line types
C
		IF ( proces ) THEN
		    CALL GDTXLV( line, cint, scale, nptsc, nylbl, 1, 1,
     +				 nptsc, nylbl, grdout, icolor, iline,
     +				 ilwid, labflg, iscale, dmin, dmax,
     +				 clvl, nlvl, clbl, scflag, iret )
		    IF ( iret .ne. 0 ) proces = .false.
		ELSE
		    dmin = 0.0
		    dmax = 0.0
		    nlvl = 0
		END IF
C
		IF ( proces ) THEN
		    CALL GDTSSP ( gdfile, gfunc, ntime, iscale,
     +				  gdatim, gvcord, nlvl, clvl, dmin,
     +				  dmax, icolor, iline, ilwid, labflg,
     +				  device, panel, iret )
		    IF ( iret .ne. 0 ) proces = .false.
		END IF

		IF ( proces ) THEN
		    CALL GQMODE ( mode, ier )
		    CALL GSMODE ( 2, ier )
		    IF ( clear ) CALL GCLEAR ( ier )
		    CALL GG_PANL ( panel, ier )
C
		    havsfc = .false.
		    CALL GDTSPL( border, iyaxis, ystrt, ystop, vclsfc,
     +				 havsfc, ylbl, nylbl, xstrt, xstop,
     +				 xtlbl, ctlbl, nxlbl, ntime, itlbfr,
     +				 itglfr, ittmfr, ratio, rmargn, ilbfrq,
     +				 iglfrq, itmfrq, iret )
C
		    CALL GSGGRF ( 1, iyaxis, nptsc, nylbl, xptsc (1),
     +				  ystrt, xptsc (nptsc), ystop, iret )
C
C*                  Draw contours.
C
                    IF ( fflag ) THEN
                      CALL IN_FILL  ( fint, fline, dmin, dmax, flvl,
     +                                nflvl, rfint, fmin, fmax,
     +                                ifcolr, ifltyp, iflabl, iret )
                      IF  ( iret .ne. 0 )  THEN
                          CALL ER_WMSG ( 'IN', iret, ' ', ier )
                      END IF
C
		      CALL GCFILL  ( nptsc, nylbl, grdout, 0, 0, 0, 
     +                               nflvl, flvl,
     +                               ifcolr, iflabl, ifltyp, iret )
                      IF  ( iret .ne. 0 )  CALL ER_WMSG ( 'GEMPLT',
     +                                          iret, ' ', ier )

                    END IF

		    IF ( cflag ) THEN
			CALL GCLGRN ( nptsc, nylbl, grdout, 0, 0, 0,
     +				      nlvl, clvl, clbl, icolor, iline,
     +				      ilwid, labflg, scflag, iret )
		    END IF
C               
C*                  Mark highs and lows.
C
                    CALL GDNHLO ( hilo, hlsym, grdout, nptsc, nylbl,
     +                            ier )
C
C*                  Plot the color bar.
C
                    IF  ( fflag )  CALL GG_CBAR ( clrbar, nflvl, flvl,
     +                                            ifcolr, ier )
C
C*		    Write the title.
C
		    CALL IN_TITL ( title, 0, ititl, linttl, ttlstr,
     +				   ier )
		    levl (1) = -1
		    levl (2) = -1
		    time (1) = trange (1)
		    time (2) = ' '
		    CALL ST_LSTR ( gfunc, len, ier )
        	    CALL ST_LCUC ( gfunc, parm, ier )
        	    parm = 'PROB OF ' // parm(:len)
C
		    CALL GR_TITL  ( ttlstr, time, .false.,
     +				    levl, ivcord, parm, iscale,
     +				    gpoint, ttlstr, shrttl, iret )
		    IF ( clear ) CALL GMESG ( shrttl, ier )
		    IF ( ititl .ne. 0 ) THEN
			CALL GSCOLR ( ititl, ier )
			CALL GG_WSTR ( ttlstr, linttl, ier )
		    END IF
C
		    CALL GSMODE ( mode, ier )
		END IF
		CALL GEPLOT ( ier )
		CALL IP_DYNM ( done, ier )
	    END IF
	END DO
	CALL GENDP ( 0, iret )
	CALL IP_EXIT ( iret )

	END
