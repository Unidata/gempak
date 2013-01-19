	PROGRAM  GDPROF
C************************************************************************
C* PROGRAM GDPROF							*
C*									*
C* This program draws profiles using grids in a grid file.		*
C*									*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 1/89	Added MARGIN				*
C* G. Huffman/GSC	 3/89	Call to GEPLOT				*
C* M. desJardins/GSFC	 4/89	Add location, min, max,...		*
C* M. desJardins/GSFC	 5/89	Use LLMXLV for max number of levels	*
C* M. desJardins/GSFC	 7/89	Added sides for labels and ticks	*
C* M. desJardins/GSFC	11/89	Changed GR_FILE to DG_OFIL		*
C* K. Brill/GSC         11/89   DG_FLNO					*
C* K. Brill/GSC         12/89   Changed call to GDPDTA for vert subset  *
C* K. Brill/GSC         12/89   Added winds				*
C* K. Brill/GSC          1/90   Pass HAVSCL to GDPXAX			*
C* K. Brill/GSC          1/90   Added TEXT				*
C* K. Brill/GSC          5/90   Changes for new IN_AXIS			*
C* K. Brill/GSC          5/90   Added background lines			*
C* K. Brill/NMC          7/90   Change for GG_SKEW			*
C* S. Schotz/GSC	 7/90	Added changed for IN_PTYP		*
C* J. Whistler/SSAI	 4/91	Changed GDPTTL to GR_TITL		*
C* J. Whistler/SSAI	 5/91	Changed output*72 to output*48		*
C* J. Nielsen/TAMU	11/91	Added filter factor			*
C* K. Brill/NMC		11/91	Changed PANEL*24 to *48			*
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* L. Sager/NMC		 7/93   Added REFVEC parameter			*
C* S. Jacobs/EAI	 9/93	Modified short title			*
C* S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* L. Williams/EAI	 7/94	Removed call to GDPUPD and added shrttl	*
C*				to user input variables			*
C* P. Bruehl/Unidata	 8/94	Added calls (GSTANM,GSPLOT,GENANM) to	*
C*				create multiple pixmaps; added logical 	*
C*				first to prompt once and calc x axis 	*
C*				once; added calls to process LIST as	*
C*				input for gdattim; added functonality to*
C*				process grids w/2 times; copied call to	*
C*				GDPDTV inside time loop to set time arry*
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* P. Bruehl/Unidata	 3/95	Fixed scaling of subsequent frames	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to GDPDSP		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL 		*
C* T. Lee/GSC		 7/01	Processed multiple files		*
C* R. Tian/SAIC		10/02	Changed arg to be .true. in DG_MFIL	*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC          2/04   Removed nuflg from DG_INTL call         *
C* T. Piper/SAIC	06/04	Added call to GD_CPF			*
C* T. Piper/SAIC	10/04	Moved GG_PANL after GCLEAR		*
C* R. Tian/SAIC         10/04   Changes for time/file management        *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), border*(LLMXLN), ptype*(LLMXLN),
     +			wind*(LLMXLN), gdatim*(LLMXLN), gfunc*(LLMXLN),
     +			gvcord*(LLMXLN), title*(LLMXLN),
     +			xaxis*(LLMXLN),  yaxis*(LLMXLN), line*(LLMXLN),
     +			gpoint*(LLMXLN), device*(LLMXLN),
     +			marker*(LLMXLN), scale*(LLMXLN), panel*(LLMXLN),
     +			winpos*(LLMXLN), gvect*(LLMXLN), text*(LLMXLN),
     +			output*(LLMXLN), thtaln*(LLMXLN),
     +			thteln*(LLMXLN), mixrln*(LLMXLN),
     +			filter*(LLMXLN), refvec*(LLMXLN),
     +			shrttl*(LLMXLN)
	LOGICAL		clear
C*
	REAL		x (LLMXLV), y (LLMXLV), xlbl (LLAXIS), 
     +			ylbl (LLAXIS), u (LLMXLV), v (LLMXLV),
     +                  yv (LLMXLV), rmargn (4)
C*
	INTEGER         ithtal (6), ithtel (6), imixrl (6), level (2)
	CHARACTER	time (2)*20, lastim*20, ttlstr*72, parm*12,
     +			parmu*12, parmv*12, firstm*20
	LOGICAL		respnd, done, proces, havwnd, havscl
	LOGICAL		first, gottm
	CHARACTER       timfnd*36
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDPROF', ier )
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
C*  Main loop to read in TAE parameters and draw profile.
C
	DO WHILE  ( .not. done )
C
C*  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*  Read in the variables from the TAE.
C
	  CALL GDPINP  ( gdfile, gdatim, gvcord, gfunc, gpoint, ptype,
     +			 xaxis,  yaxis,  border, line,  marker, title, 
     +			 clear,  device, scale,  panel, gvect,  wind,
     +                   refvec, winpos, filter, text, output, 
     +                   thtaln, thteln, mixrln, iperr )
C
C*  Exit if there is an error.
C
	  IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	   ELSE
C
C*  Process the GDFILE input.
C
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
C*  Set up the graphics device.
C
	    CALL GG_SDEV  ( device, iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
C*  Set the attributes that do not vary within the time loop.
C
	    IF  ( proces )  THEN
C
C*  Process the text size and filter factor.
C
		CALL IN_TEXT  ( text, ier )
		CALL IN_FILT  ( filter, filtfc, ier )
C
C*  Set flags for scalars and winds.
C
		havscl = .true.
		havwnd = .true.
		IF ( gfunc .eq. ' ' ) havscl = .false.
		IF ( gvect .eq. ' ' ) havwnd = .false.
C
C*		Get information about the background lines.
C
		CALL GDPLIN ( thtaln, thteln, mixrln,
     +			      ithtal, ithtel, imixrl, iret )
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
                    CALL ER_WMSG ( 'GDPROF', ier, time(1), irr)
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
		    CALL GDPDTV   ( timfnd, gvcord, gfunc, 
     +				    firstm, lastim, time, ivcord, iret )
		    IF  ( iret .ne. 0 )  THEN
		    	CALL ER_WMSG  ( 'GDPROF', iret, ' ', ier )
		    	proces = .false.
		    END IF
	        END IF
C
C*	    	Get information about y axis.
C
	    	IF  ( proces .and. first )  THEN
		    CALL GDPYAX  ( ptype, yaxis, ivcord, iyaxis, ratio, 
     +			           ystrt, ystop, ylbl, nylbl, rmargn,
     +			           iylbsf, iyglsf, iytmsf, iret )
		    IF  ( iret .ne. 0 )  THEN
		    	CALL ER_WMSG  ( 'GDPROF', iret, ' ', ier )
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
			CALL ER_WMSG  ( 'GDPROF', iret, ' ', ier )
			proces = .false.
		    END IF
		    IF ( havscl ) THEN
			CALL GDPDTA ( iflnos, timfnd, gvcord, 
     +				      gfunc, gpoint, time, ivcord, 
     +				      ystrt, ystop, rgx, rgy, rlat,
     +				      rlon, npts, x, y, parm, iret )
		    	IF  ( iret .ne. 0 )  havscl = .false.
	            	IF  ( iret .eq. -12 .or. iret .eq. -13 ) THEN
			    proces = .false.
	            	END IF
	            END IF
C
		    IF ( havwnd ) THEN
			CALL GDPDUV   ( iflnos, timfnd, gvcord,
     +					gvect, gpoint, time, ivcord,
     +					ystrt, ystop, rgx, rgy, rlat,
     +					rlon, nuv, u, v, yv, parmu, 
     +					parmv, iret )
			IF  ( iret .ne. 0 )  havwnd = .false.
	            END IF
		    IF ( .not. havscl .and. .not. havwnd ) 
     +			 proces = .false.
		END IF
C
C*		Get information about x axis.
	      	IF  ( proces .and. first )  THEN
C
C
C*		    Set the x axis and scale the data on the first time.
C
		    CALL GDPXAX  ( havscl, xaxis, scale, iyaxis, npts, 
     +				   x, iscale, xstrt, xstop, xlbl, 
     +				   nxlbl, rmindt, rmaxdt, ixlbsf, 
     +				   ixglsf, ixtmsf, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'GDPROF', iret, ' ', ier )
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
		    CALL GDPDSP ( gdfile, timfnd, gfunc, 
     +				  rgx, rgy, rlat, rlon, npts, 
     +				  rmindt, rmaxdt, iscale, 
     +			          gvect, wind, first, iret )
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
C*	        Draw the profiles.
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
		    CALL GDPPLT  ( parm, border, line, marker, xaxis, 
     +				   yaxis, iyaxis, ystrt, ystop, ylbl, 
     +				   nylbl, xstrt, xstop, xlbl, nxlbl, 
     +				   npts, x, y, ratio, rmargn, ixlbsf, 
     +				   ixglsf, ixtmsf, iylbsf, iyglsf,
     +                             iytmsf, wbszx, wbszy, iret )
C
C*		    If a vector profile exists, plot it.
C
	            IF ( havwnd .and. iret .eq. 0 ) THEN
		  	CALL GDPUVP ( gvect, u, v, yv, nuv, wind, 
     +				      winpos, filtfc, wbszx, wbszy, 
     +				      refvec, iret )
		    END IF
C
C*	            Draw background lines.
C
	            IF  ( iret .eq. 0 )
     +	                CALL GDPPLN ( ithtal, ithtel, imixrl, gfunc, 
     +				      ivcord, ystrt, ystop, 
     +				      xstrt, xstop, ier )
C
C*		    Write title.
C
		    CALL IN_TITL  ( title, 0, ititl, linttl, 
     +					ttlstr, ier )
		    level(1) = -1
		    level(2) = -1
		    CALL GR_TITL  ( ttlstr, time, .false., level, 
     +				    ivcord, parm, iscale, gpoint,
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
	            CALL GDPOUT ( havscl, havwnd, x, y, npts, u, v, yv,
     +				  nuv, ttlstr, parm, parmu, parmv, 
     +				  rlat, rlon, iscale, gvcord, 
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
C*	    Prompt for next profile to be done.
C
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDPROF', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the interface.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
