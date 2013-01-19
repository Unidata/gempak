	PROGRAM  GDPTPDF
C************************************************************************
C* PROGRAM GDPTPDF							*
C*									*
C* This program draws probability density/cumumulative probbability at	*
C* selected point and time, with values on the he vertical axis and 	*
C* probability along the horizontal axis. 				* 
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC         	08/07                                           *
C* M. Li/SAIC         	01/08	Check for invalid date/time input	*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), border*(LLMXLN), ptype*(LLMXLN),
     +			gdatim*(LLMXLN), gfunc*(LLMXLN),
     +			gvcord*(LLMXLN), title*(LLMXLN),
     +			xaxis*(LLMXLN),  yaxis*(LLMXLN), line*(LLMXLN),
     +			gpoint*(LLMXLN), device*(LLMXLN),
     +			marker*(LLMXLN), scale*(LLMXLN), panel*(LLMXLN),
     +			text*(LLMXLN),
     +			output*(LLMXLN),
     +			shrttl*(LLMXLN), glevel*(LLMXLN), prbtyp(LLMXLN)
	LOGICAL		clear
C*
	REAL		x (LLMXLV), y (LLMXLV), xlbl (LLAXIS), 
     +			ylbl (LLAXIS), rmargn (4)
C*
	INTEGER         level (2)
	CHARACTER	time (2)*20, lastim*20, ttlstr*72, parm*32,
     +			firstm*20
	LOGICAL		respnd, done, proces
	LOGICAL		first, gottm
	CHARACTER       timfnd*36
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDPTPDF', ier )
C
C*  Initialize GEMPLT.
C
	    CALL GG_INIT  ( 0, ier )
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
C*	Main loop to read in TAE parameters and draw probability.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL GDPTINP ( gdfile, gdatim, gvcord, glevel, gfunc, gpoint,
     +			 ptype, xaxis, yaxis, prbtyp, border, line, 
     +			 marker, title, clear,  device, scale, panel, 
     +                   text, output, iperr ) 
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
C*	    Process date/time input
C
	    IF ( proces ) THEN
		IF ( INDEX(gdatim, '-') .ne. 0 ) THEN
		    proces = .false.
		    iret = -5
		    CALL ER_WMSG  ( 'GDPTPDF', iret, ' ', ier )
		END IF
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
C*		Process the text size.
C
		CALL IN_TEXT  ( text, ier )

	    END IF
C
C*          Loop over times.
C
            itime = 1
            gottm = proces
            first = .true.
            DO  WHILE ( gottm )
C
C*              Get the next time to process from the time server.
C
                CALL DG_NTIM ( .true., .true., time, gottm, ier )
                proces = ( ier .eq. 0 .and. gottm )
                IF ( ier .ne. 0 ) THEN
                    ier = 2
                    CALL ER_WMSG ( 'GDPTPDF', ier, time(1), irr)
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
C*		Get time and vertical coordinate.
C
		IF  ( proces )  THEN
		    CALL GDPTDTM   ( timfnd, gvcord, gfunc, 
     +				    firstm, lastim, time, iret )
		    IF  ( iret .ne. 0 )  THEN
		    	CALL ER_WMSG  ( 'GDPTPDF', iret, ' ', ier )
		    	proces = .false.
		    END IF
	        END IF
C
C*	    	Get information about y axis.
C

	    	IF  ( proces .and. first )  THEN

		    CALL GDPTYMX ( gdatim, gvcord, glevel, gfunc, 
     +                             gpoint, rgx, rgy, plat, plon, 
     +				   ymax, ymin, iret )


		    CALL GDPTYAX  ( ptype, yaxis, ymin, ymax,
     +			           iyaxis, ratio, ystrt, ystop, ylbl,
     +			           nylbl, rmargn, iylbsf, iyglsf, 
     +				   iytmsf, iret )
C
		    IF  ( iret .ne. 0 )  THEN
		    	CALL ER_WMSG  ( 'GDPTPDF', iret, ' ', ier )
		    	proces = .false.
		    END IF
	        END IF
C
	        IF ( proces .and. first ) THEN
		    CALL GSTANM ( iret )
	          ELSE IF ( proces ) THEN
		    CALL GSPLOT ( iret )
	        END IF
C
C*		Get data to plot.
C
C*		GD_CPF checks if a file name was entered for the grid point
C
		IF  ( proces )  THEN
		    CALL GD_CPF(gpoint, gpoint, ier)
		    IF ( ier .ne. 0 ) THEN
			iret = -4
			CALL ER_WMSG  ( 'GDPTPDF', iret, ' ', ier )
			proces = .false.
		    END IF
C
		    CALL ST_NUMB ( prbtyp, iprbty, ier )
		    CALL GDPTDAT ( gdatim, gvcord, glevel, gfunc,
     +                             iprbty, ymin, ymax, ylbl, nylbl, 
     +				   ystrt, ystop, rgx, rgy,
     +                             npts, x, y, parm, iret ) 
		    IF  ( iret .ne. 0 )   proces = .false.
		END IF
C
C*		Get information about x axis.
	      	IF  ( proces .and. first )  THEN
C
C*		    Set the x axis and scale the data on the first time.
C
		    CALL GDPTXAX  ( xaxis, scale, iyaxis, npts, 
     +				   x, iscale, xstrt, xstop, xlbl, 
     +				   nxlbl, rmindt, rmaxdt, ixlbsf, 
     +				   ixglsf, ixtmsf, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'GDPTPDF', iret, ' ', ier )
			proces = .false.
		    END IF
	          ELSE IF  ( proces .and. ( .not. first ) )  THEN
C
C*		    Only scale the data on subsequent frames.
C
		    CALL GR_SSCL  ( iscale, 1, npts, 1, 1, 1, npts, x,
     +				rmindt, rmaxdt, iret )
		END IF
C
C*	        Give user a chance to exit.
C
	        IF  ( proces )  THEN
		    CALL GDPTDSP ( gdfile, timfnd, gfunc, 
     +				  rgx, rgy, plat, plon, npts, 
     +				  rmindt, rmaxdt, iscale, 
     +			          first, iret )
C
C*                  Stop looping if user requests exist.
C
                    IF ( iret .ne. 0 ) THEN
                        proces = .false.
                        gottm = .false.
                    END IF
C
C*                  Set first to false upon first successful plot.
C
                    first = .false.
	        END IF
C
C*	        Draw the probabilities.
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
C*		    Plot axes and data.
C
		    CALL GDPTPLT  ( border, line, marker,
     +				    iyaxis, ystrt, ystop, ylbl, 
     +				   nylbl, xstrt, xstop, xlbl, nxlbl, 
     +				   npts, x, y, ratio, rmargn, ixlbsf, 
     +				   ixglsf, ixtmsf, iylbsf, iyglsf,
     +                             iytmsf, iret )

C
C*		    Write title.
C
		    CALL IN_TITL  ( title, 0, ititl, linttl, 
     +					ttlstr, ier )
		    level(1) = -1
		    level(2) = -1
		    ivc      =  0
		    CALL GR_TITL  ( ttlstr, time, .false., level, 
     +				    ivc, parm, iscale, gpoint,
     +				    ttlstr, shrttl, iret )
		    IF  ( clear )  CALL GMESG ( shrttl, ier )
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
C*		    Write output to file, terminal or printer.
C
	            CALL GDPTOUT ( x, y, npts, ttlstr,
     +				  plat, plon, iscale,
     +				  output, ier )
C
C*                  Increment itime only if plot was successful.
C
                    itime = itime + 1
		END IF
	    END DO
C
	    CALL GENANM ( iret )
C
C*	    Prompt for next probability to be done.
C
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDPTPDF', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the interface.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
