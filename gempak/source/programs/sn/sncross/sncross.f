	PROGRAM SNCROSS
C************************************************************************
C* PROGRAM SNCROSS							*
C*									*
C* This program draws cross sections using upper air data.		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* I. Graffman/RDS	 5/86	Added observed wind drawing		*
C* I. Graffman/RDS	 5/88	GG call, DEVICE, GFLUSH to GEPLOT	*
C* G. Huffman/GSC	11/88	GEMPAK4.1; modify parameter names	*
C* M. desJardins/GSFC	 8/89	Add IP_IDNT				*
C* M. desJardins/GSFC	 1/90	Change thtinc to thtint in SNSGTH	*
C* S. Schotz/GSC	 6/90   Updates and cleanup for GEMPAK5		*
C* S. Schotz/GSC	 7/90	Added changes for IN_PTYP, IN_AXIS	*
C* S. Schotz/GSC	 8/90	Plot winds only at wind data levels	*
C* M. desJardins/GSFC	 8/90	Changed array size; plot any parameter	*
C* J. Nielsen/SUNYA	 2/91	Read data for new set of stations	*
C* J. Whistler/SSAI	 2/91	Added subroutine SNSWWE			*
C* M. desJardins/GSFC	 3/91	Reorganized x axis for time section	*
C* M. desJardins/NMC	 4/91	Change time section title		*
C* J. Nielsen/TAMU	11/91	Added filter factor			*
C* K. Brill/NMC		11/91	Changed PANEL*24 to *48			*
C* K. Brill/NMC		12/91	Call SNSWWE only if PROCES is true	*
C* K. Brill/NMC		01/92	Changes for contour fill		*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* K. Brill/NMC		02/92	Check for new vertical coord; add parm	*
C*				conditions				*
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* S. Jacobs/EAI	 9/93	Added CLRBAR, IN_CBAR and GG_CBAR	*
C* S. Jacobs/EAI	 9/93	Changed IN_CBAR and GG_CBAR to GG_CBAR	*
C* S. Jacobs/NMC         4/94   Removed unused variables        	*
C* S. Jacobs/NMC	 6/94	DEVICE*12 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to SNSUPD and added shrttl *
C*				to the user input variables		*
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC	 	 8/96	Added ER_WMSG call after FL_MFIL call	*
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* M. Li/GSC		 1/00	Added GCNTLN and nflag; removed GCSPLN	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C* T. Lee/GSC		 8/00	Added calls to GSTANM and GENANM	*
C* T. Lee/SAIC		10/01	Added contour fill types		*
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized icvtyp & isnfln	*
C* C. Bailey/HPC	 6/06	Added contour text labeling		*
C* C. Bailey/HPC	10/06	Added suppress small contour flag	*
C* S. Gilbert/NCEP	05/07	Removed call to GCNTLN			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER	snfile*(LLMXLN), device*(LLMXLN),
     +			border*(LLMXLN), cint*(LLMXLN), line*(LLMXLN),
     +			ptype*(LLMXLN), title*(LLMXLN), wind*(LLMXLN), 
     +			taxis*(LLMXLN), filter*(LLMXLN), shrttl*(LLMXLN),
     +			panel*(LLMXLN), text*(LLMXLN), snparm*(LLMXLN),
     +			vcoord*(LLMXLN), yaxis*(LLMXLN), cxstns*(LLMXLN),
     +			dattim*(LLMXLN), curve*(LLMXLN), contur*(LLMXLN),
     +			fint*(LLMXLN), fline*(LLMXLN), ctype*(LLMXLN),
     +			clrbar*(LLMXLN), filnam*(LLMXLN)
C*
	LOGICAL		clear
C
C*	The "old" values keep track of the current values.  NEWSTN
C*	is set when station data must be read in.  NEWPRM is set when
C*	a new parameter is selected.  NEWTHA is read when new theta
C*	data for an isentropic display must be read.
C
	CHARACTER	cxsold*72, snfold*72, datold*48, prmold*4,
     +			taxold*48, prmhld*16, clbl(LLCLEV)*24
	LOGICAL		newstn, newprm, newtha, newvco
C*
	CHARACTER	times (LLMXTM)*20, stns (LLTMCX)*20, parm*4
	CHARACTER	wintyp*1, winuni*1, vcord*4
	CHARACTER	ctlbl (LLMXTM)*12, ttlstr*72
	PARAMETER	( MSDSIZ = 100000 )
	REAL		stndat ( MSDSIZ ), xtlbl (LLMXTM)
	REAL		sloc (LLTMCX), xx (LLMAXD), yy (LLMAXD), 
     +			pontha (LLMAXD,200), clvl (LLCLEV),
     +			flvl (LLCLEV), pdat (LLTMCX,200),
     +			rmargn (4), grid (LLMAXD,LLMAXD),
     +			toptmp (LLTMCX), topwnd (LLTMCX),
     +			yaxval (LLAXIS)
	INTEGER		iltype (LLCLEV), linwid (LLCLEV), 
     +			ilabel (LLCLEV), icolor (LLCLEV),
     +			ifcolr (LLCLEV), iflabl (LLCLEV),
     +			ifltyp (LLCLEV), ipsdat (LLTMCX), 
     +			nlvls (LLTMCX), idtype (LLMXLV,LLTMCX)
	LOGICAL		proces, respnd, done, isnflg, timflg,prmexs,
     +			wndexs, cflag, lflag, sflag, bflag, fflag, 
     +			nflag, scflag
C*
	DATA		snfold, cxsold, prmold, datold, taxold
     +					  / 5 * ' ' /
	DATA		newstn, newprm, newvco  / 3 * .true. /
	DATA		icvtyp / 0 /, isnfln / 0 /, ivcold / -9999 /
C------------------------------------------------------------------------
C*	Initialize TAE and GEMPLT
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
            CALL GD_INIT  ( iperr )
	    mode = 2
	    CALL GG_INIT  ( mode, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'SNCROSS', ier )
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in TAE parameters and draw cross section.
C
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
	    prmexs = .true.
	    wndexs = .true.
C
C*	    Read in variables from the TAE.
C

	    CALL SNSINP ( cxstns, cint, curve, line, border, ptype,
     +			  yaxis, snfile, snparm, vcoord, dattim,
     +			  clear, text, panel, title, device, wind, 
     +			  filter, taxis, contur, fint, fline, ctype,
     +			  clrbar, iret )
C
C*	    Exit if there is an error.
C
	    IF  ( iret .ne. 0 ) THEN
		done = .true.
	      ELSE
C
C*		Set up the graphics device.
C
		CALL GG_SDEV  ( device, iret )
		IF  ( iret .ne. 0 )  proces = .false.
C
C*		Set text attributes
C
		CALL IN_TEXT  ( text, iret )
C
C*		Open the sounding file.
C
		CALL FL_MFIL ( snfile, ' ', filnam, ier )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
		CALL SNSFIL  ( filnam, snfold, isnfln, newstn, nparms,
     +			       iret )
		IF  ( iret .ne. 0 )  THEN
		    proces  = .false.
		  ELSE
		    CALL SNSDAT  ( isnfln, dattim, datold, newstn,
     +				   times, ntime, iret )
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
		      ELSE IF  ( ntime .eq. 1 )  THEN
			timflg = .false.
		      ELSE IF  ( ntime .ge. 4 )  THEN
			timflg = .true.
			IF  ( ntime .gt. LLTMCX )  THEN
			    ntime = LLTMCX 
			END IF
		      ELSE
			iret  = -6
			CALL ER_WMSG  ( 'SNCROSS', iret, dattim, ier )
			proces = .false.
			datold = ' '
		    END IF
		END IF
C
C*		Check to see if the station(s) have changed.
C
		CALL ST_LCUC  ( cxstns, cxstns, ier )
		IF  ( cxsold .ne. cxstns )  THEN
		    newstn = .true.
		    cxsold = ' '
		END IF
C
C*		Get wind information.
C
		CALL IN_WIND  ( wind, wintyp, winuni, iwnclr, ier )
C
C*		Get the parameter to evaluate.
C
		CALL ST_CLST  ( snparm, ';', ' ', 1, prmhld, n, ier )
		parm = prmhld (1:4)
		CALL ST_LCUC  ( parm, parm, ier )
		IF  ( ( parm .eq. ' ' ) .and. ( iwnclr .eq. 0 ) )  THEN
		    iret = -13
		    CALL ER_WMSG  ( 'SNCROSS', iret, snparm, ier )
		    proces = .false.
		    newprm = .true.
		  ELSE IF  ( parm .ne. prmold )  THEN
		    newprm = .true.
		    IF  ( parm .eq. 'ISEN' )  THEN
			isnflg = .true.
		      ELSE
			isnflg = .false.
		    END IF
		END IF
C
C*		Check to see if the time axis has changed for a time
C*		series.
C
		IF  ( taxis .ne. taxold )  newstn = .true.
		taxold = taxis
C
C*		Get the data for the selected stations or times.
C
		iret = 0
		IF  ( proces .and. ( .not. timflg ) .and. newstn )
     +							    THEN
		    CALL SNSSST ( cxstns, isnfln, nparms, MSDSIZ, 
     +				  nstn, stns, ipsdat, nlvls, stndat, 
     +				  idtype, sloc, xmin, xmax, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		  ELSE IF  ( proces .and. timflg .and. newstn )  THEN
		    CALL SNSSTM ( cxstns, isnfln, nparms, MSDSIZ, times,
     +				  ntime, taxis, nstn, ipsdat, nlvls,
     +				  stndat, idtype, sloc, xmin, xmax,
     +				  xtlbl, ctlbl, nxlbl, ixlbfr, ixglfr,
     +				  ixtmfr, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*		Check for at least four stations.
C
		IF  ( proces .and. ( nstn .ge. 4 ) )  THEN
		    cxsold = cxstns
		    prmold = parm
		  ELSE
		    cxsold = ' '
		    prmold = ' '
		    proces = .false.
		END IF
C
C*		Get information about the y axes.  Find the y points
C*		on a LLMAXD by LLMAXD grid.
C
		IF  ( proces ) THEN
		    CALL SNSYAX  ( ptype, vcoord, yaxis, iytype, ratio,
     +				   rmargn, ybot, ytop, ivcord, vcord,
     +				   yaxval, nyval, iylbfr, iyglfr, 
     +				   iytmfr, iret )
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
		      ELSE IF  ( isnflg .and. ( ivcord .ne. 1 ) )  THEN
			iret = -4
			CALL ER_WMSG  ( 'SNCROSS', iret, ' ', ier )
			proces = .false.
		      ELSE
			IF ( ivcold .eq. ivcord ) newvco = .false.
		        ivcold = ivcord
			CALL SNSGRD  ( iytype, xmin, xmax, ybot, ytop, 
     +				       ratio, rmargn, yy, xx, iret )
			IF  ( iret .ne. 0 ) proces = .false.
		    END IF
		END IF
C
C*		Get the minimum pressure for temperature and winds.
C
		IF  ( proces )  THEN
		    CALL SNSRGE  ( vcord, nstn, stndat, nlvls, ipsdat, 
     +				   toptmp, topwnd, iret )
C
C*		    Get interval and curve type.
C
		    CALL SNSGTH  ( cint, curve, ithinc, icvtyp, newtha,
     +				   iret )
		END IF
C
C*		Get either grid or isentropic data.
C
		IF  ( proces )  THEN
		    IF  ( isnflg .and. 
     +			( newprm .or. newtha .or. newstn ) )  THEN
			CALL SNSTHA  ( ithinc, icvtyp, nstn, stns, 
     +				       ipsdat, nlvls, stndat, sloc, 
     +				       xx, thmin, thmax, pontha, 
     +				       pdat, iret )
			IF  ( iret .eq. 0 )  THEN
			    newprm = .false.
			  ELSE
			    prmexs = .false.
			END IF
		      ELSE IF ( ( .not. isnflg ) .and. 
     +			      ( newprm .or. newstn .or. newvco ) ) THEN
			CALL SNSPRM ( snparm, nstn, stns, ipsdat, nlvls,
     +				      stndat, sloc, xx, yy, vcord,
     +				      ivcord, icvtyp, grid, pontha,
     +				      iret )
			IF  ( iret .eq. 0 .and. parm .ne. ' ' )  THEN
			    CALL IN_CONT ( contur, ier )
			    CALL IN_CTYP ( ctype, nflag, lflag, sflag,
     +					   bflag, fflag, ier )
			    IF  ( lflag .or. sflag .or.
     +				  bflag .or. nflag )  THEN
				cflag = .true.
			    ELSE
				cflag = .false.
			    END IF
			    kgxy = LLMAXD * LLMAXD
			    CALL SNSLEV ( cflag, line, cint, fflag,
     +					  fline, fint, kgxy, grid,
     +					  nclvl, clvl, clbl, icolor, 
     +					  iltype, linwid, ilabel, nflvl, 
     +					  flvl, ifcolr, iflabl, ifltyp, 
     +					  scflag, ier )
                	    IF ( nclvl .eq. 0) cflag = .false.
                	    IF ( nflvl .eq. 0) fflag = .false.
			END IF
			IF  ( iret .ne. 0 )  THEN
			    prmexs = .false.
			  ELSE
			    CALL GSGGRF  ( 1, iytype, LLMAXD, LLMAXD,
     +					   xmin, ybot, xmax, ytop, ier )
			    IF  ( ier .ne. 0 )  THEN
				iret = -15
				CALL ER_WMSG ('SNCROSS', iret, ' ', ier)
				proces = .false.
			    END IF
			END IF
		    END IF
		END IF
C
C*		Check to see if wind data exist.		
C
	       IF ( proces ) THEN
	           CALL SNSWWE  ( winuni, vcord, ier )
	           IF  ( ier .ne. 0 )  THEN
		       wndexs = .false.
	           END IF
	       END IF
C
C*		Set the current pixmap.
C
		CALL GSTANM ( iret )
C
C*		Display options
C
		IF  ( proces .and. ( prmexs .or. wndexs ) )  THEN
		    CALL SNSDSP  ( timflg, nstn, stns, topwnd, toptmp, 
     +				   parm, vcord, clear, isnflg, nclvl,
     +				   clvl, icolor, iltype, linwid, ilabel,
     +				   nflvl, flvl, ifcolr, iflabl, ifltyp,
     +				   times (1), cxstns, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*		Draw the cross section.
C
		IF  ( proces .and. ( prmexs .or. wndexs ) )  THEN
C
C*		    Clear the device if requested and set panel.
C
		    IF  ( clear ) CALL GCLEAR ( iret )
		    CALL GG_PANL ( panel, iret )
C
C*		    Draw the isentropes or contours.
C
		    IF ( prmexs )  THEN
		        IF  ( isnflg )  THEN
			    CALL SNSISN ( line, xx, pontha, ybot, ytop, 
     +			    	          ithinc, thmin, thmax, nstn, 
     +				          sloc, pdat, iret )
		          ELSE IF  ( parm .ne. ' ' .and.
     +				     ( cflag .or. fflag ) )  THEN
                            IF  ( fflag )  THEN
                    	      CALL GCFILL ( LLMAXD, LLMAXD, grid,
     +					    0, 0, 0, nflvl, flvl,
     +					    ifcolr, iflabl, ifltyp,
     +					    iret )
                    	      IF ( iret .ne. 0 ) CALL ER_WMSG
     +					    ( 'GEMPLT', iret, ' ', ier )
                  	    END IF
                  	    IF  ( cflag )  THEN
                   	      IF  ( lflag )  THEN
                    		CALL GCLGRN ( LLMAXD, LLMAXD, grid,
     +					      0, 0, 0, nclvl, clvl,
     +					      clbl, icolor, iltype, 
     +					      linwid, ilabel, scflag, 
     +					      iret )
                    		IF ( iret .ne. 0 ) CALL ER_WMSG
     +					    ( 'GEMPLT', iret, ' ', ier )
                   	      END IF
                   	      IF  ( bflag )  THEN
                    		CALL GCBOXX ( LLMAXD, LLMAXD, grid,
     +					      0, 0, 0, nclvl, clvl,
     +					      icolor, iltype, linwid,
     +					      ilabel, iret )
                    		IF ( iret .ne. 0 ) CALL ER_WMSG
     +					    ( 'GEMPLT', iret, ' ', ier )
			      END IF
			    END IF
		        END IF
		    END IF
C
C*		    Plot the background and surface.
C
		    CALL SNSBOR  ( border, ybot, ytop, yaxval, nyval, 
     +				   iylbfr, iyglfr, iytmfr, xmin, xmax, 
     +				   nstn, stns, sloc, xx, pontha, 
     +				   toptmp, topwnd, xtlbl, ctlbl,
     +				   nxlbl, ixlbfr, ixglfr, ixtmfr, 
     +				   timflg, iret )
C
C*		    Draw the winds.
C
		    IF ( wndexs )  THEN
		        IF  ( iwnclr .gt. 0 )  THEN
			    CALL GSCOLR  ( iwnclr, ier )
			    CALL IN_FILT ( filter, filtfc, ier )
			    CALL SNSWND  ( wintyp, nstn, ipsdat, 
     +				           nlvls, stndat, idtype, sloc, 
     +				           ybot, ytop, filtfc, vcord, 
     +				           iret )
		        END IF
		    END IF
C
C*		    Plot the color bar.
C
		    IF ( fflag )  CALL GG_CBAR ( clrbar, nflvl, flvl,
     +						 ifcolr, ier )
C
C*		    Write the title.
C
		    CALL SNSTTL (title, timflg, cxstns, times, ntime,
     +				 icttl, linttl, ttlstr, shrttl, iret)
		    IF  ( clear )  CALL GMESG ( shrttl, ier )
		    IF  ( icttl .gt. 0 )  THEN
			CALL GSCOLR  ( icttl, ier )
			CALL GG_WSTR ( ttlstr, linttl, ier )
		    END IF
C
C*		    Flush the plotting buffers and update globals.
C
		    CALL GEPLOT ( iret )
		END IF
C
C*		Prompt for next cross section to be drawn.
C
		CALL GENANM ( iret )
		CALL IP_DYNM ( done, ier )
	    END IF
	END DO
C
C*	Exit from GEMPLT and the TAE.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
