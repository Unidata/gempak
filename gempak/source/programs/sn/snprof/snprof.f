	PROGRAM SNPROF
C************************************************************************
C* SNPROF								*
C*									*
C* This program thermodynamic profile charts of upper air data.		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	11/89	Changes for station time		*
C* S. Schotz/GSC	 5/90	Added message for no valid stations	*
C* K. Brill/GSC          6/90   Added changes for IN_AXIS		*
C* M. desJardins/GSFC	 7/90	Added changes for IN_PTYP		*
C* S. Schotz/GSC	 8/90	Clean up error messages			*
C* J. Whistler/SSAI	 6/91	Exit time and stn loop when EXIT entered*
C* J. Nielsen/TAMU	11/91	Added filter factor			*
C* K. Brill/NMC		11/91	Changed PANEL*24 to *48			*
C* K. Brill/NMC		02/92	Added TUNIT				*
C* K. Brill/NMC		02/92	Check for valid vertical coordinate	*
C* K. Brill/NMC		02/92	Check for temp for bckgrd lines		*
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* K. Brill/NMC		05/93	CALL ST_LCUC before LV_CORD		*
C* L. Williams/EAI	03/94	Clean up declarations of input vars	*
C* S. Jacobs/NMC         6/94   DEVICE*12 --> *72                       *
C* S. Jacobs/NMC	 6/94	STNDEX*48 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to SNPUPD; Moved SHRTTL dec*
C* S. Jacobs/NMC	 8/94	Added GSTANM, GSPLOT for animation	*
C* P. Bruehl/Unidata	 8/94	Use logical first to prompt only once	*
C* S. Jacobs/NMC	10/94	Removed auto increment of wind position	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* A. Hardy/GSC		 3/99	Added priority parameter to PC_SSTN     *
C* T. Piper/SAIC	 4/02	Fixed UMR; init arecur, datcur, snfcur, *
C*							isnfln		*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	snfile*(LLMXLN), snparm*(LLMXLN), area*(LLMXLN),
     +			line*(LLMXLN), ptype*(LLMXLN), stndex*(LLMXLN),
     +			stncol*(LLMXLN), wind*(LLMXLN),	marker*(LLMXLN),
     +			border*(LLMXLN), title*(LLMXLN), thtaln*(LLMXLN),
     +			thteln*(LLMXLN), mixrln*(LLMXLN),
     +			device*(LLMXLN), yaxis*(LLMXLN), xaxis*(LLMXLN),
     +			vcoord*(LLMXLN), dattim*(LLMXLN),
     +			winpos*(LLMXLN), panel*(LLMXLN), text*(LLMXLN),
     +			filter*(LLMXLN), shrttl*(LLMXLN)
C*
	LOGICAL		clear
C*
	REAL		ylbl (LLAXIS), xlbl (LLAXIS)
	REAL		dwpt(LLMXLV,2), wnd(LLMXLV,3), temp(LLMXLV,2)
C*
	CHARACTER	snfcur*72, stn*8, arecur*48, vparm*12, tunit*1
	LOGICAL		done, respnd, proces, newfil, first, tmpflg
	CHARACTER	stparm (MMPARM)*4, pname1*4, pname2*4, wintyp*1
	INTEGER		ip1arr (3), ip2arr (3), ithtal (6), ithtel (6),
     +			imixrl (6), ibordr (3), idtype (LLMXLV)
	CHARACTER	times (LLMXTM)*20, datcur*20, ttlstr*72, cstnm*8
	CHARACTER	ttlout*72, ttlinp*72, filnam*72
	REAL		data  (LLMXDT), rmargn (4), filtfc
	DATA		ip2arr / 3 * 0 /
C-----------------------------------------------------------------------
	isnfln = 0
	arecur = ' '
	datcur = ' ' 
	snfcur = ' '
C
C*	Initialize user interface and GEMPLT.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'SNPROF', ier )
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
C*	Loop through program.
C
	DO WHILE  ( .not. done )
C
C*	    Get input from TAE.
C
            CALL SNPINP  ( snfile, area, snparm, line, ptype, stndex,
     +			   stncol, wind, marker, border, title, thtaln, 
     +			   thteln, mixrln, device, yaxis, xaxis, filter,
     +			   clear,  vcoord, dattim, winpos, panel, text, 
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
		CALL SNPFIL  ( filnam, snfcur, isnfln, newfil, iret )
C
C*		If file is opened, set up area.
C
		IF  ( iret .eq. 0 )  THEN
		    CALL LC_UARE  ( area, newfil, isnfln, arecur, stn,
     +				    iret )
		END IF
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get the times to proces.
C
	    IF  ( proces )  THEN
		CALL SNPDAT  ( isnfln, dattim, newfil, datcur,
     +			       ntimes, times, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get the vertical coordinate.
C
	    IF  ( proces )  THEN
		CALL ST_LCUC  ( vcoord, vparm, ier )
		CALL LV_CORD  ( vparm, vparm, ivcord, iret )
		IF  ( iret .ne. 0 .or.
     +		      ivcord .lt. 0 .or. ivcord .gt. 5 )  THEN
		    CALL ER_WMSG  ( 'LV', iret, vcoord, ier )
		    proces = .false.
		END IF
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
C*		Get the level parameters to compute.
C
		CALL SNPPRM  ( vparm, snparm, line, wind, pname1, 
     +			       pname2, ip1arr, ip2arr, wintyp, iwncol, 
     +			       tunit, iret )
		IF  ( iret .ne. 0 )  proces = .false.
C
C*		Get information on background lines and border.
C
		CALL SNPLIN  ( border, thtaln, thteln, mixrln,
     +			       ibordr, ithtal, ithtel, imixrl, iret )
	    END IF
C
C*	    Set TMPFLG if abscissa is temperature and background lines
C*	    can be drawn.
C
	    IF ( proces ) THEN
		it = INDEX ( pname1, 'TMP' ) + INDEX ( pname1, 'TEMP' )
		IF ( it .eq. 0 ) THEN
		    tmpflg = .false.
		ELSE
		    tmpflg = .true.
		END IF
	    END IF
C
C*	    Get the stability indicies to list.
C
	    CALL SNPSTB  ( stndex, stncol, stparm, nstprm, istcol,
     +			   iret )
C
C*	    Get y-axis information.
C
	    IF  ( proces )  THEN
		CALL SNPYAX  ( ptype, yaxis, ivcord, iyaxis, ratio, 
     +			       ystrt, ystop, ylbl, nylbl, rmargn, 
     +                         iylbfr, iyglfr, iytmfr, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'SNPROF', iret, ' ', ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Set marker type and get wind information.
C
	    IF  ( proces )  THEN
		CALL IN_MARK  ( marker, mkcolr, ier )
		CALL ST_NUMB  ( winpos, iwposn, ier )
		IF  ( ( iwposn .lt. 1 ) .or. ( iwposn .gt. 3 ) )
     +						iwposn = 1
		CALL IN_FILT  ( filter, filtfc, ier )
	    END IF
C
C*	    If an error was encountered, set the number of times to 0.
C
	    IF  ( .not. proces )  THEN
		ntimes = 0
		datcur = ' '
	    END IF
C
C*	    Loop through all the times.
C
	    first = .true.
	    iplot = 0
	    iext  = 0
	    itime = 0
	    jtime = 0
	    DO WHILE ( ( itime .lt. ntimes ) .and. ( iext .eq. 0 ) )
		itime = itime + 1
C
C*		Set the time.
C
		CALL SN_STIM  ( isnfln, times (itime), ier )
		CALL SN_BEGS  ( isnfln, ier )
C
C*		Loop through stations.
C
		jstn = 0
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
			CALL SN_RDAT ( isnfln, nlev, data, ihhmm, iret )
			IF  ( iret .eq. 0 )  CALL SN_RTYP  ( isnfln,
     +						nnlev, idtype, ier )
		    END IF
C
C*		    Set station and allow user to exit at this point.
C
		    IF  ( iret .eq. 0 )  THEN
			jstn  = jstn  + 1
			jtime = jtime + 1
			ispri = 0 
			CALL PC_SSTN  ( stn, istnm, slat, slon, selv,
     +					ispri, ihhmm, nlev, ier )
C
C*			Write a message about the station and time
C*			being plotted.
C
			WRITE  ( 6, 1000 )  times (itime), istnm, stn
1000			FORMAT ( ' Next plot - Time: ', A,
     +                           ' Station: ', I6, 3X, A )
C
C*			Set the current pixmap.
C*			If this is the first time, go to the first
C*			pixmap. If it is not the first time, go to
C*			the next pixmap.
C
C*			For the first time only, prompt the user
C*			to continue.
C
			IF  ( ( jtime .eq. 1 ) .and.
     +			      ( jstn  .eq. 1 ) )  THEN
			    CALL GSTANM ( iret )
			    CALL IP_RESP ( respnd, ier )
			    IF  ( respnd )  THEN
				CALL TM_ACCP  ( ier )
				IF  ( ier .eq. 2 )  THEN
				    iret = -1
				    iout = -1
				    iext = -1
				END IF
			    END IF
			ELSE
			    CALL GSPLOT ( iret )
			END IF
		    END IF
C
C*		    Clear screen if requested.
C
		    IF  ( iret .eq. 0 )  THEN
			iplot = iplot + 1
			IF  ( clear )  CALL GCLEAR  ( ier )
C
C*			Set panel.
C
			CALL GG_PANL  ( panel, ier )
C
C*		        Get the data for this plot.
C
			CALL SNPDTW
     +		           ( ip1arr, ip2arr, iwncol, data, nlev,
     +			     idtype, ystrt, ystop, temp, ntemp,
     +                       dwpt, ndwpt, wnd, nwind, 
     +                       xmin, xmax, iret )
C
C*	    		Get x axis information and set graph
C*			coordinates.
C
	    		IF  ( iret .eq. 0 .and. iplot .eq. 1 )
     +		          CALL SNPXAX  
     +                           ( pname1, pname2, xaxis, iyaxis,
     +                             xmin, xmax,
     +				   xstrt, xstop, xlbl, nxlbl, 
     +                             ixlbfr, ixglfr, ixtmfr, iret )
   	   	        IF  ( iret .eq. 0 .and. iplot .eq. 1 )
     +		      	  CALL SNPGRF
     +			     ( iyaxis, ratio, xstrt, xstop, ystrt, 
     +			       ystop, rmargn, windxn, windyn, iret )
   	   	        IF  ( iret .eq. 0 .and. iplot .eq. 1 .and.
     +                        iyaxis .eq. 4 ) 
     +                    CALL GG_SKEW ( xaxis, yaxis, pname1, ratio,
     +                                   xstrt, ystrt, xstop, ystop,
     +                                   xlbl, nxlbl, iret )
			IF  ( iret .eq. 0 .and. iplot .ne. 0 )  THEN
C
C*			  Draw background.
C
			  CALL SNPBCK
     +				     ( ibordr, xstrt, xstop, ystrt, 
     +			  	       ystop, xlbl, nxlbl, ylbl, nylbl, 
     +				       ixlbfr, ixglfr, ixtmfr,
     +                                 iylbfr, iyglfr, iytmfr, ier )
C
C*			  Plot data.
C
			  CALL SNPPLT
     + 			       ( temp, ntemp, dwpt, ndwpt, wnd, nwind,
     +			         ip1arr, ip2arr, iwncol, iwposn, windxn,
     +			         windyn, filtfc, wintyp, mkcolr, iret )
C
C*			  Write out stability indicies.
C
			  CALL SNPPST  ( istcol, stparm, nstprm, data,
     +				         first, iwposn, stn, istnm,
     +				         times (itime), iret )
C
C*			  Draw background lines.
C
			  IF ( tmpflg )
     +			  CALL SNPPLN  ( ithtal, ithtel, imixrl, pname1,
     +				         pname2, tunit, vparm, ystrt,
     +					 ystop, xstrt, xstop, ier )
C
C*			  Create and draw the title.
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
			    CALL ST_LSTR ( pname1, len3, ier )
			    CALL ST_LSTR ( pname2, len4, ier )
			    shrttl = 'PROFILE ' //
     +				     times(itime)(5:9) //
     +				     ' ' // stn(:len2) // ' ' //
     +				     pname1(:len3) // ' ' //
     +				     pname2(:len4)
			  END IF
			  IF  ( clear )  CALL GMESG ( shrttl, ier )
C
C*			  Plot title.
C
			  CALL IN_TITL  ( title, -1, ititl, linttl, 
     +					  ttlstr, ier )
			  IF  ( ititl .gt. 0 )  THEN
			    IF  ( ttlstr .eq. ' ' )  THEN
				CALL ST_INCH  ( istnm, cstnm, ier )
				IF  ( ier .ne. 0 ) cstnm = ' '
				ttlout = pname1 // ' ' // pname2
			    ELSE
			 	ttlout = ttlstr
			    END IF
			    CALL GSCOLR   ( ititl, ier )
			    CALL GG_WSTR  ( ttlout, linttl, ier )
			  END IF
C
C*			  Force out plot.
C
			  CALL GEPLOT  ( ier )
	       	        END IF
		    END IF
		END DO
	    END DO
C
C*	    Write out message if no valid stations were found
C
	    IF  ( proces .and. ( iplot .eq. 0  ) ) 
     +          CALL ER_WMSG ( 'SNPROF', -9, ' ', ier )
C
	    CALL GENANM ( iret )
C
C*	    Call dynamic tutor.
C
	    IF  ( .not. done )  THEN
		CALL IP_DYNM  ( done, iret )
	    END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'SNPROF', iperr, ' ', ier )
C
C*	Exit TAE and GEMPLT.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
