	PROGRAM  SFGRAM
C************************************************************************
C* PROGRAM SFGRAM							*
C*									*
C* This program draws a meteorogram using reports in a surface file.	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 6/89						*
C* M. desJardins/GSFC	 5/90	GEMPAK 5 version			*
C* S. Schotz/GSC	 7/90	Change for call to IN_TAXS		*
C* S. Schotz/GSC	10/90	Call IN_LINE for border			*
C* J. Whistler/SSAI	 4/91	Changed taxis to *48 from *24		*
C* J. Whistler/SSAI	 5/91	Changed call to SFXGRF			*
C* J. Whistler/SSAI	 7/91	Added PANEL and call to SFXPNL		*
C* K. Brill/NMC		10/91	PANEL*24 --> *48			*
C* K. Brill/NMC		 2/92	Pass label & tic frequencies to SFXGRF	*
C* S. Jacobs/EAI	11/92	Added call to GMESG and 'shrttl'	*
C* S. Jacobs/EAI	 6/93	Added call to GSVIEW to reset view area	*
C* L. Williams/EAI       3/94	Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC	 6/94	DEVICE*24 --> *72			*
C* S. Jacobs/NMC	 6/94	Changed the panel sizes for 2 plots	*
C* L. Williams/EAI	 7/94	Removed call to SFXUPD			*
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type 	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to SFXDSP		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	 9/97	Changed call to GQTEXT			*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 9/99	Added calls to GSTANM and GENANM	*
C* T. Lee/GSC		 5/01	Retrieved data from multiple files	*
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized icolor		*
C* A. Hardy/NCEP	12/04   Increased 'witnes' 20 -> 100		*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	sffile*(LLMXLN), border*(LLMXLN),
     +			trace (5)*(LLMXLN), dattim*(LLMXLN),
     +			title*(LLMXLN), taxis*(LLMXLN), statn*(LLMXLN),
     +			device*(LLMXLN), marker*(LLMXLN), text*(LLMXLN),
     +			ntrace*(LLMXLN), panel*(LLMXLN), shrttl*(LLMXLN)
C*
	CHARACTER	parms (4,2,5)*12, prmtyp (4,2,5)*1,
     +			range (2,5)*20, witnes (2,5)*100
	INTEGER		ntparm (2,5), icolor (4,2,5), iptprm (4,2,5)
C*
	CHARACTER	ctlbl (50)*20, ctime*48, stns (10)*8,
     +			stnold*8, sffcur*72, trcur (5)*48, datcur*40,
     +			sss*48, ttlstr*72
	REAL		xtlbl (50), x (LLMXTM), view (4)
	LOGICAL		newfil, done, proces, plot, axflg, tmflg,
     +			respnd, clear, contin, exit, scflag
	REAL		data  (LLMXTM*MMPARM)
	CHARACTER	cdata (LLMXTM*MMPARM)*8
	CHARACTER	timlst (LLMXTM)*20
	CHARACTER	times (LLMXTM)*20
C*
	CHARACTER	tpanl (5,5)*24
	DATA		tpanl  / '0.;.25;1.;.75', 4 * ' ',
     +				 '0.;.525;1.;.95', '0.;.05;1.;.475', 
     +				  3 * ' ',
     +				 '0.;.70;1.;1.00', '0.;.35;1.;.65',
     +				 '0.;0.00;1.;.30', 2 * ' ',
     +				 '0.;.75;1.;1.', '0.;.50;1.;.75', 
     +				 '0.;.25;1.;.50', '0.;0.;1.;.25',
     +				 ' ',
     +				 '0.;.776;1.;1.00', '0.;.582;1.;.806',
     +				 '0.;.388;1.;.612', '0.;.194;1.;.418',
     +				 '0.;0.00;1.;.2244' /
C*
	DATA		sffcur, datcur, stnold, trcur / 8 * ' ' /
	DATA		icolor / 40 * 0 /
C------------------------------------------------------------------------
C*	Initialize TAE and GEMPLT.
C
	kntt = 1
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
            CALL GD_INIT  ( ier )
	    CALL IP_IDNT  ( 'SFGRAM', ier )
	    CALL GG_INIT  ( 2, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
	values = 0.
C
C*	Main loop to read in TAE parameters and draw profile.
C
	newfil = .true.
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL SFXINP  ( sffile, dattim, trace, statn, taxis, border, 
     +			 marker, title, text, clear, device, ntrace,
     +			 panel, iperr )
C
C*	  Decode value for trace.
C
	  CALL ST_NUMB  ( ntrace, mtrace, ier )
	  IF  ( ( mtrace .lt. 1 ) .or. ( mtrace .gt. 5 ) ) mtrace = 5
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
C*	    Get array of times based on the file name and data time.    
C
	    IF  ( proces )  THEN
		CALL SF_TLST  ( sffile, dattim, sffcur, datcur, newfil,
     +				timlst, nts, iret )
		IF  ( iret .ne. 0 )   THEN
		    CALL ER_WMSG ( 'SF', iret, ' ', ier )
		    iret = -17
		    CALL ER_WMSG ( 'SFGRAM', iret, ' ', ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Get input times to plot.
C
	    IF  ( proces )  THEN
		CALL SFXDAT   ( dattim, timlst, nts, newfil,
     +				ntime, times, ctime, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Set the station list.
C
	    IF  ( proces )  THEN
		IF  ( statn (1:1) .eq. '@' )  THEN
		    sss = statn (2: )
		  ELSE
		    sss = statn 
		END IF
		CALL ST_CLST  ( sss, ';', ' ', 10, stns, nstn, ier )
		knt = 0
		DO  i = 1, nstn
		    CALL ST_LCUC  ( stns (i), stns (i), ier )
		    IF  ( stns (i) .ne. ' ' )  knt = 1
		END DO
		DO  i = nstn + 1, 5
		    IF  ( i .gt. 1 )  stns (i) = stns (i-1)
		END DO
		IF  ( knt .eq. 0 )  THEN
		    proces = .false.
		    CALL ER_WMSG  ( 'SFGRAM', -4, statn, ier )
		END IF
	    END IF
C
C*	    Parse the input for TEXT.
C
	    IF  ( proces )  THEN
		CALL IN_TEXT  ( text, ier )
		CALL GQTEXT   ( itxfnt, itxhw, sztext, itxwid,
     +				ibrdr, irrotn, ijust, ier )
		IF  ( itxhw .eq. 2 )  THEN
		    iret = + 3
		    CALL ER_WMSG  ( 'SFGRAM', iret, ' ', ier )
		END IF
	    END IF
C
C*	    Set the current pixmap.
C
	    CALL GSTANM ( iret )
C
C*	    Set margin.
C
	    IF  ( proces )  THEN
		CALL GSGMGN  ( 7., 3., 7., 1., ier )
	    END IF
C
C*	    Get the border and marker information.
C
	    IF  ( proces )  THEN
		CALL IN_LINE ( border, values, 1, ibcolr, ibtyp, 
     +                         ibwidt, iblab, smth, fltr, scflag, ier )
		CALL IN_MARK  ( marker, mkcolr, ier )
	    END IF
C
C*	    Set the panel information and store view coordinates.
C
	    IF ( proces )  THEN
		CALL GG_PANL ( panel, ier )
		CALL GQVIEW  ( view (1), view (2), view (3), view (4),
     +			       ier ) 
	    END IF
C
C*	    Loop through the different traces.
C
	    sztmp = sztext
	    IF  ( proces )  THEN
C
	      stnold = ' '
	      exit   = .false.
	      itrace = 1
	      contin = .true.
	      DO WHILE ( ( itrace .le. mtrace ) .and. contin )
C
C*		Set variable showing there is something to plot.
C
		IF  ( stns (itrace) .eq. ' ' .or. exit )  THEN
		    plot = .false.
		  ELSE
		    plot = .true.
C
C*		    Read the data if this is a new station.
C
		    IF  ( stns (itrace) .ne. stnold ) THEN
			stnold = stns (itrace)
			CALL SFXDTA   ( sffile, times, ntime, stnold,  
     +					stns, clear, ctime, itrace, 
     +					mtrace, trace, trcur, exit,
     +					parms, ntparm, icolor, iptprm, 
     +					prmtyp, range, witnes, nparms,
     +					data, cdata, iret )
			IF  ( iret .ne. 0 ) THEN
			    plot   = .false.
			    stnold = ' '
			END IF
		    END IF
C
C*		    Continue when users opt to.
C
		    IF  ( .not. exit )  THEN
C
			IF  ( plot .and. ( itrace .eq. 1 ) )  THEN
C
C*			    Get information about x axis.
C
			    CALL IN_TAXS  ( taxis, 50, ntime, times, x, 
     +					    xstrt, xstop, xtlbl, ctlbl, 
     +					    nxlbl, xmndst, ilbfrq,
     +					    iglfrq, itmfrq, iret)
			    xmndst = xmndst * 3.
			    IF  ( iret .ne. 0 )  THEN
				contin = .false.
				plot = .false.
			    END IF
			END IF
C
C*			Set the panel for plotting of traces.
C
			IF  ( plot )  THEN
			    CALL SFXPNL ( view, tpanl (itrace, mtrace),
     +					  ier )
			END IF
C
C*			Plot on the left and then the right.
C
			IF  ( plot )  THEN
			    axflg = .true.
			    IF  ( itrace .eq. mtrace )  THEN
				tmflg = .true.
			      ELSE
				tmflg = .false.
			    END IF
			    DO  j = 1, 2
C
C*			    Set up the graph coordinates.
C
			      IF  ( ntparm ( j, itrace ) .gt. 0 )  THEN
				CALL GSCOLR  (  ibcolr, ier )
				CALL GSLINE  (  0, 0, ibwidt, 0, ier )
				CALL SFXGRF  (  j, itrace, ntime, data, 
     +						prmtyp (1,j,itrace),
     +					 	iptprm (1,j,itrace),
     +						range (j,itrace),
     +						axflg, xstrt, xstop,
     +						xtlbl, ctlbl, nxlbl, 
     +						tmflg, ilbfrq,
     +						iglfrq, itmfrq,
     +						witnes (j,itrace),
     +						parms (1,j,itrace),
     +						icolor (1,j,itrace),
     +						ntparm, iret )
				IF  ( iret .eq. 0 )  axflg = .false.
			      ELSE
				iret = -1
			      END IF
C
C*			      Plot the parameters on this side.
C
			      IF  ( iret .eq. 0 )  THEN
			        CALL SFXPLT  (  j, parms (1,j,itrace),
     +						prmtyp (1,j,itrace),
     +						icolor (1,j,itrace),
     +						data, cdata, ntime,
     +						iptprm (1,j,itrace), 
     +						mkcolr, xmndst, x, ier )
			      END IF
			    END DO
		        END IF
		    END IF
		END IF
C
		itrace = itrace + 1
C
	      END DO
C
C*	      Reset view to initial settings.
C
	      IF  ( plot .and. ( .not. exit ) )  THEN
	        CALL GSVIEW  ( view (1), view (2), view (3), view (4),
     +			       ier ) 
C
C*	        Add title at bottom.
C
	        CALL SFXTTL  ( title, stns, mtrace, ctime, icttl,
     +			     linttl, ttlstr, shrttl, ier )
	        IF  ( clear )  CALL GMESG ( shrttl, ier )
	        IF  ( icttl .gt. 0 )  THEN
		  CALL GSCOLR  ( icttl, ier )
		  CALL GG_WSTR ( ttlstr, linttl, ier )
	        END IF
	      END IF
	    END IF
C
C*	    End graphics mode.
C
	    IF  ( proces .and. .not. exit )  CALL GEPLOT  ( ier )
C
C*	    Prompt for next profile to be done.
C
	    CALL GENANM ( iret )
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF  (iperr .ne. 0)  CALL ER_WMSG  ( 'SFGRAM', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the TAE.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
	END
