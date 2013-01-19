	SUBROUTINE SFXGRF  ( iside, itrace, ntime, data, prmtyp, iptprm,
     +			     range, axflg, xstrt, xstop, xtlbl, ctlbl,
     +			     nxlbl, tmflg, italf, itagf, itatf, witnes,
     +			     parms, icolor, ntparm, iret )
C************************************************************************
C* SFXGRF								*
C*									*
C* This subroutine sets up the y axis and draws the background for	*
C* each trace.								*
C*									*
C* SFXGRF  ( ISIDE, ITRACE, NTIME, DATA, PRMTYP, IPTPRM, RANGE, AXFLG, 	*
C*	     XSTRT, XSTOP, XTLBL, CTLBL, NXLBL, TMFLG, ITALF, ITATF,	*
C*	     WITNES, PARMS, ICOLOR, NTPARM, IRET )			*
C*									*
C* Input parameters:							*
C*	ISIDE		INTEGER		Current side (1=left,2=right)	*
C*	ITRACE		INTEGER		Current trace			*
C*	NTIME		INTEGER		Number of times			*
C*	DATA (NTIME,4)	REAL		Data				*
C*	PRMTYP (4)	CHAR*		Parameter type			*
C*	IPTPRM (4)	INTEGER		Pointers to data		*
C*	RANGE		CHAR*		Input for range			*
C*	AXFLG		LOGICAL		Axis draw flag			*
C*	XSTRT		REAL		First point on x axis		*
C*	XSTOP		REAL		Last point on x axis		*
C*	XTLBL (NXLBL)	REAL		X axis label points		*
C*	CTLBL (NXLBL)	CHAR*		X axis labels			*
C*	NXLBL		INTEGER		Number of x axis labels		*
C*	TMFLG		LOGICAL		Time axis label flag		*
C*	ITALF		INTEGER		Time axis label frequency	*
C*	ITAGF		INTEGER		Time axis gridline frequency	*
C*	ITATF		INTEGER		Time axis tic mark frequency	*
C*	WITNES		CHAR*		Witness line input		*
C*	PARMS (4)	CHAR*		Parameter names			*
C*	ICOLOR (4)	INTEGER		Parameter colors		*
C*	NTPARM (2,5)	INTEGER		Number of parameters		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = invalid axes		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/90						*
C* S. Schotz/GSC	10/90	Set ndec = -1 for GDAXIS		*
C* J. Whistler/SSAI	 5/91	Changed color lables to match lines	*
C* J. Whistler/SSAI	 5/91	Add tick marks to right on single plot	*
C* K. Brill/NMC		02/92   JNielsen's fix for axis title spacing	*
C* K. Brill/NMC		02/92	Pass in tic & label frequencies		*
C* K. Brill/NMC		02/92	Label rt y axis whenever no side2 parms *
C* T. Piper/SAIC	04/02	Fixed UMR; increased ctic to 4		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NWIT = 20 )
	CHARACTER*(*)	prmtyp (*), range, ctlbl (*), witnes, parms (*)
	LOGICAL		axflg, tmflg
	REAL		data  ( NTIME, * ), xtlbl (*)
	INTEGER		iptprm (*), icolor (*), ntparm (2,*)
C*
	CHARACTER	title*20, ctic*4
	LOGICAL		realpm
	REAL		ylbl (LLAXIS), rarr (NWIT), xarr (2), yarr (2)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Set tick marks inside.
C
	CALL GSTICK  ( 4, 0., ier )
C
C*	Check for real valued parameters to determine axis.
C
	realpm = .false.
	iloc1  = 0
	iloc2  = 0
	DO  i = 1, 4
	    IF  ( ( prmtyp (i) .eq. 'R' ) .or. ( prmtyp (i) .eq. 'G' ) )
     +								THEN
		realpm = .true.
		IF  ( iloc1 .eq. 0 )  THEN
		    iloc1 = i
		    iloc2 = i
		  ELSE IF  ( iloc1 .eq. i - 1 )  THEN
		    iloc2 = i
		END IF
	    END IF
	END DO
	IF  ( iloc1 .ne. 0 )  THEN
	    il = iloc1
	    iloc1 = iptprm (il)
	    il = iloc2
	    iloc2 = iptprm (il)
	END IF
C
C*	If there are no real parameters, scale axis from 0 to 1.
C
	IF  ( .not. realpm )  THEN
	    ystrt = 0.
	    ystop = 1.
	    nylbl = 0
	  ELSE
	    CALL SFXRNG  ( ntime, data, iloc1, iloc2, range, ystrt,
     +			   ystop, nylbl, ylbl, ier )
	END IF
C
C*	Set up the graph coordinate system.
C
	CALL GSGRAF  ( 1, 1, 0., xstrt, ystrt, xstop, ystop, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -8
	    CALL ER_WMSG  ( 'SFGRAM', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Draw the x axis at the bottom for all plots.
C
	IF  ( axflg )  THEN
	    iaxtic = itatf
	    iaxlin = itagf
	    IF  ( tmflg )  THEN
		iaxlbl = italf
	      ELSE
		iaxlbl = 0
	    END IF
	    CALL GAAXIS  ( 1, ystrt, axflg, iaxlbl, iaxtic, iaxlin,
     +			   nxlbl, xtlbl, ctlbl, ier )
	END IF
C
C*	Draw the other x axis.
C
	IF  ( axflg )  THEN
	    CALL GAAXIS  ( 3, ystop, axflg, 0, 0, 0, 0, xtlbl, ctlbl,
     +			   ier )
	END IF
C
C*	Draw the y axes.
C
	IF  ( axflg )  THEN
	    CALL GAAXIS  ( 2, xstrt, axflg, 0, 0, 0, 0, xtlbl, ctlbl,
     +			   ier )
	    CALL GAAXIS  ( 4, xstop, axflg, 0, 0, 0, 0, xtlbl, ctlbl,
     +			   ier )
	END IF
C
C*	Check to see how many parameters in trace.
C
	itlen = 0
	nprm = 0
	DO  i = 1, 4
	    IF  ( parms (i) .ne. ' ' )  THEN
		itlen = itlen + 5
		nprm = nprm + 1
	    END IF
	END DO
C
C*	Check to see if there will be parameters on right side.
C
	nmprm = 0
	DO i = 1, 2
	    IF ( ntparm ( i, itrace ) .gt. 0 ) nmprm = nmprm + 1
	END DO
C*
	IF  ( nylbl .gt. 0 )  THEN
	    IF  ( ( iside .eq. 1 ) .or. ( (  nprm .eq. 1 ) .and.
     +					  ( nmprm .ne. 2 ) ) )  THEN
		jside = 2
		xxxxx = xstrt
		CALL GDAXIS  ( jside, xxxxx, .false., 1, 1, 0, -1, nylbl,
     +			       ylbl, ier )
	    END IF
	    IF  ( ( iside .ne. 1 ) .or. ( (  nprm .eq. 1 ) .and.
     +					  ( nmprm .ne. 2 ) ) .or.
     +		  ( ntparm ( 2, itrace ) .eq. 0 ) )  THEN
		jside = 4
		xxxxx = xstop
		CALL GDAXIS  ( jside, xxxxx, .false., 1, 1, 0, -1, nylbl,
     +			       ylbl, ier )
	    END IF
	END IF
C
C*	If there are real parameters, draw the witness lines requested.
C
	IF  ( realpm .and. ( witnes .ne. ' ' ) ) THEN
	    ctic = witnes (1:3)
	    CALL ST_LCUC ( ctic, ctic, ier )
C*
	    IF ( ctic .ne. 'TIC' ) THEN
C
C*	        Decode user input into real values.
C
	        CALL ST_RLST  ( witnes, ';', RMISSD, NWIT, rarr, n,
     +				ier )
	        IF  ( ( n .eq. 1 ) .and. ( ERMISS ( rarr (1) ) ) )
     +			n = 0
	        IF  ( ( n .eq. 0 ) .and. ( ( witnes (1:1) .eq. 'Y' )
     +			.or.
     +		  ( witnes (1:1) .eq. 'N' ) .or. 
     +		  ( witnes (1:1) .eq. 'y' ) .or.
     +		  ( witnes (1:1) .eq. 'c' ) ) )  THEN
		    n = 1
		    rarr (1) = ( ystrt + ystop ) / 2.
	        END IF
	    ELSE
		n = nylbl
		ifrqt = 1
		DO WHILE ( n .gt. NWIT )
		    n = nylbl / ( ifrqt + 1 )
		    ifrqt = ifrqt + 1
		END DO
		indx = 1
		DO i = 1, nylbl, ifrqt
		    rarr (indx) = ylbl (i)
		    indx = indx + 1
		END DO
		n = indx - 1
	    END IF
C
C*	    Draw lines as dots.
C
	    IF  ( n .gt. 0 )  THEN
		CALL GQLINE  ( iltyp, ilhw, ilwid, iwhw, ier )
		CALL GSLINE  ( 10, 0, 0, 0, ier )
		DO  i = 1, n
		    IF  ( .not. ERMISS ( rarr (i) ) )  THEN
			xarr (1) = xstrt
			xarr (2) = xstop
			yarr (1) = rarr (i)
			yarr (2) = rarr (i)
			CALL GLINE  ( 'M', 2, xarr, yarr, ier )
		    END IF
		END DO
		CALL GSLINE  ( iltyp, 0, 0, 0, ier )
	    END IF
	END IF
C
C*	Write title for parameters.
C
	ititle = 0
	DO  i = 1, 4
	    title = ' '
	    IF  ( parms (i) .ne. ' ' )  THEN
		ititle = ititle + 1
		title ( 1: ) = parms (i) (1:4)
		IF  ( iside .eq. 1 )  THEN
		    rotat = 90.
		    iyoff = 10 
		    xpt   = xstrt
		  ELSE
		    rotat = 270.
		    iyoff = 10 
		    xpt   = xstop
		END IF
		ymid  = ( ystrt + ystop ) / 2. 
		ixoff = ( 2 - itlen ) + ( 10 * ( ititle - 1 ) ) 
		CALL GSCOLR  ( icolor (i), ier )
		CALL GTRANS  ( 'M', 'N', 1, xpt, ymid, xnorm, ynorm, 
     +				ier )
		CALL GTEXT   ( 'N', xnorm, ynorm, title, rotat, ixoff,
     +				iyoff, ier )
	    END IF
	END DO
C
C*	Reset tick marks.
C
	CALL GSTICK  ( 1, 0., ier )
C*
	RETURN
	END
