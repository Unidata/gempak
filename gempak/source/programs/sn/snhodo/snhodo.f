	PROGRAM SNHODO
C************************************************************************
C* SNHODO								*
C*									*
C* This program draws hodograms of upper air wind soundings.		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89						*
C* S. Schotz/GSC	 7/90	Updates for IN_AXIS, clean up.		*
C* S. Schotz/GSC	 8/90	Added message for no stations found	*
C* S. Schotz/GSC	10/90	Call IN_LINE for border			*
C* K. Brill/NMC		07/91	Message for only one point on graph	*
C* K. Brill/NMC		10/91	PANEL*24 --> *48			*
C* K. Brill/NMC		02/92	Stop time loop when user enters EXIT	*
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* S. Jacobs/NMC	 6/94	DEVICE*12 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to SNHUPD and added shrttl *
C*				to the user input variables		*
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* S. Jacobs/NMC	 3/95	Changed call to SNHLEV to pass file num	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* R. Curtis/EAI         8/00	Added calls to GSTANM and GENANM        *
C* T. Piper/SAIC	 4/02	Fixed UMR; init arecur, datcur, margin,	*
C*							snfcur, isnfln,	*
C* T. Piper/SAIC	10/04	Moved GG_PANL after GCLEAR		*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	snfile*(LLMXLN), area*(LLMXLN), line*(LLMXLN),
     +			marker*(LLMXLN), border*(LLMXLN),
     +			title*(LLMXLN),  xaxis*(LLMXLN), 
     +			levels*(LLMXLN), vcoord*(LLMXLN),
     +			dattim*(LLMXLN), device*(LLMXLN),
     +			panel*(LLMXLN),  text*(LLMXLN), yaxis*(LLMXLN),
     +			shrttl*(LLMXLN)
C*
	LOGICAL		clear
	REAL		xlbl (LLMXLV), ylbl (LLMXLV)
C*
	CHARACTER	snfcur*72, stn*8, arecur*48, vparm*12
	LOGICAL		done, respnd, proces, newfil, scflag
	INTEGER		ibordr (3), ilbfrq(2), 
     +                  iglfrq(2), itmfrq(2)
	CHARACTER	times (LLMXTM)*20, datcur*20, ttlstr*72, cstnm*8
	CHARACTER	ttlout*72, margin*24, ttlinp*72, filnam*72
	REAL		rlevel (LLMXLV), u (LLMXLV),
     +			v (LLMXLV),      utag (LLMXLV), vtag (LLMXLV),
     +			tag (LLMXLV)
C-----------------------------------------------------------------------
	isnfln = 0
	arecur = ' '
	datcur = ' '
	margin = ' '
	snfcur = ' '
C
C*	Initialize user interface and GEMPLT.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'SNHODO', ier )
	IF  ( iperr .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
            CALL GD_INIT  ( ier )
	    imode = 2
	    CALL GG_INIT  ( imode, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
	values = 0.
C
C*	Loop through program.
C
	DO WHILE  ( .not. done )
C
C*	    Get input from TAE.
C
            CALL SNHINP  ( snfile, area,   line,   marker, border, 
     +			   title, xaxis, yaxis, levels, vcoord, 
     +			   dattim, clear,  device, panel,  text, 
     +			   iperr )
	    IF  ( iperr .eq. 0 )  THEN
		proces = .true.
	      ELSE
		proces = .false.
		done   = .true.
	    END IF
C
C*	    Open the data file.
C
	    IF  ( proces )  THEN
		CALL FL_MFIL ( snfile, ' ', filnam, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
		CALL SNHFIL  ( filnam, snfcur, isnfln, newfil, iret )
C
C*		If file is opened, set up area.
C
		IF  ( iret .eq. 0 )  CALL LC_UARE  ( area, newfil, 
     +				          isnfln, arecur, stn, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get the times to proces.
C
	    IF  ( proces )  THEN
		CALL SNHDAT  ( isnfln, dattim, respnd, newfil, datcur,
     +			       ntimes, times, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get the levels and vertical coordinate.
C
	    IF  ( proces )  THEN
		CALL SNHLEV  ( isnfln, levels, vcoord, ivert, nlev,
     +			       rlevel, levtyp, vparm,  lvert, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Set the graphics device.
C
	    IF  ( proces )  THEN
		CALL GG_SDEV  ( device, iret )
		CALL IN_TEXT  ( text, ier )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Check the parameters to be computed.
C
	    IF  ( proces )  THEN
C
C*		Get the level parameters to compute (vert. coord. and
C*		wind).
C
		CALL SNHPRM  ( vparm, iret )
		IF  ( iret .ne. 0 )  THEN
                    proces = .false.
		    CALL ER_WMSG ( 'SNHODO', iret, ' ', ier )
                END IF
C
C*		Get information on border.
C
	        CALL IN_LINE  ( border, values, 1, ibordr(1), 
     +                          ibordr(2), ibordr (3), iblab,
     +				smth, fltr, scflag, ier )
	    END IF
C
C*	    Set marker type.
C
	    IF  ( proces )  CALL IN_MARK  ( marker, mkcolr, ier )
C
C*	    If an error was encountered, set the number of times to 0.
C
	    IF  ( .not. proces )  THEN
		ntimes = 0
		datcur = ' '
	    END IF
C
C*	    loop through all the times.
C
	    iplot = 0
	    itime = 0
	    DO WHILE ( itime .lt. ntimes )
C
C*		Set the time.
C
		itime = itime + 1
		CALL SN_STIM  ( isnfln, times (itime), ier )
		CALL SN_BEGS  ( isnfln, ier )
C
C*		Loop through stations.
C
		iout = 0
		DO WHILE  ( iout .eq. 0 )
C
C*		    Get next station.
C
		    CALL SN_SNXT  ( isnfln, stn, istnm, slat, slon,
     +				    selv, iret )
		    IF  ( iret .ne. 0 )  THEN
			iout = iret
		      ELSE
			CALL SNHDTA  ( isnfln, rlevel, nlev, lvert,
     +				       levtyp, u, v, npts, 
     +				       utag, vtag, tag, ntag, iret )
		    END IF
C
C*		    Get x- and y-axis information and set the graph 
C*		    coordinates.
C
		    IF  ( iret .eq. 0 )  THEN  
     			CALL SNHXY  ( xaxis, yaxis, u, v, npts,  xstrt, 
     +			              xstop,  xlbl, nxlbl, ystrt, 
     +				      ystop,  ylbl, nylbl, ilbfrq, 
     +                                iglfrq, itmfrq, iret )
		        IF  ( iret .ne. 0 )  THEN
			    CALL ER_WMSG ( 'SNHODO', iret, ' ', ier )
			END IF
                    END IF
C
		    IF  ( iret .eq. 0 )  
     +			CALL SNHGRF  ( xstrt,  xstop, ystrt, ystop, 
     +				       margin, iret )
C
C*                  Set the current pixmap.
C
                    CALL GSTANM (iret)
C
C*		    Allow user to exit at this point.
C
		    IF  ( iret .eq. 0 )  THEN
			WRITE  ( 6, 1000 )  times (itime), istnm, stn
1000			FORMAT ( ' Next plot - Time: ', A,
     +                           ' Station: ', I6, 3X, A )
			IF ( npts .eq. 1 ) THEN
			    WRITE ( 6, 1001 ) 
1001			    FORMAT (
     +                      ' There is only one point on hodograph--',
     +                        'check LEVELS.' )
			END IF
			IF  ( respnd )  THEN
			    CALL TM_ACCP  ( ier )
			    IF  ( ier .eq. 2 )  THEN
				iret = -1
				iout = -1
				itime = ntimes
			    END IF
			END IF
		    END IF
C
C*		    Clear screen if requested.
C
		    IF  ( iret .eq. 0 )  THEN
			iplot = iplot + 1
			IF  ( clear )  CALL GCLEAR  ( ier )
	    		CALL GG_PANL  ( panel, ier )
C
C*			Plot background and data.
C
			CALL SNHBCK  ( ibordr, xstrt, xstop, ystrt,
     +				       ystop,  xlbl,  nxlbl, ylbl, 
     +				       nylbl,  ilbfrq, iglfrq, itmfrq,
     +                                 ier )
			CALL SNHPLT  ( line,  mkcolr, u, v, npts,
     +				       utag,  vtag,   tag,  ntag,
     +				       lvert, ier )
C
C*			Create and draw the title.
C
			ipbar = INDEX ( title, '|' )
			IF  ( ipbar .ne. 0 )  THEN
			    shrttl = title(ipbar+1:)
			    IF  ( ipbar .eq. 1 )  THEN
				ttlinp = ' '
			    ELSE
				ttlinp = title(:ipbar-1)
			    END IF
			ELSE
			    ttlinp = title
			    CALL ST_LSTR ( stn, len2, ier )
			    shrttl = 'HODOGRAPH ' //
     +				     times(itime)(5:9) //
     +				     ' ' // stn(:len2)
			END IF
			IF  ( clear )  CALL GMESG ( shrttl, ier )
C
C*			Plot title.
C
			CALL IN_TITL  ( ttlinp, 2, ititl, linttl, 
     +					ttlstr, ier )
			IF  ( ititl .gt. 0 )  THEN
			    IF  ( ttlstr .eq. ' ' )  THEN
				CALL ST_INCH  ( istnm, cstnm, ier )
				IF  ( ier .ne. 0 ) cstnm = ' '
				ttlout = times (itime) ( : 11 ) // ' ' 
     +					 // cstnm // ' ' // stn
			      ELSE
				ttlout = ttlstr
			    END IF
			    CALL GSCOLR   ( ititl, ier )
			    CALL GG_WSTR  ( ttlout, linttl, ier )
			END IF
C
C*			Force out plot.
C
			CALL GEPLOT  ( ier )
		    END IF
		END DO
	    END DO
C
C*	    Write out error message if no valid stations were found
C
	    IF  ( proces .and. ( iplot .eq. 0 ) 
     +          .and. ( iout .ne. -1 )  ) THEN
		CALL ER_WMSG ( 'SNHODO', -10, ' ', ier )
            END IF
C
C*	    Call dynamic tutor.
C
	    IF  ( .not. done )  THEN
                CALL GENANM (iret)
		CALL IP_DYNM  ( done, iret )
	    END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'SNHODO', iperr, ' ', ier )
C
C*	Exit TAE and GEMPLT.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
