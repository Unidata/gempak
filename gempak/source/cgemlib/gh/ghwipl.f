	SUBROUTINE GH_WIPL ( timstr, wname, origc, wocen, wadnm, wwnd, 
     +                       fdate, mxwd, lndext, nc, nstrm, tzone, 
     +                       wtype, idisp, iret )
C************************************************************************
C* GH_WIPL								*
C*									*
C* This subroutine draws the wind intensity probability graph.		*
C*									*
C* GH_WIPL ( TIMSTR, WNAME, ORIGC, WOCEN, WADNM, WWND, FDATE, MXWD,     *
C*           LNDEXT, NC, NSTRM, TZONE, WTYPE, IDISP, IRET )		*
C*									*
C* Input parameters:							*
C*      TIMSTR          CHAR*           Advisory valid time string      *
C*      WNAME           CHAR*           Tropical storm name             *
C*      ORIGC           CHAR*           Issuing center                  *
C*      WOCEN           CHAR*           Ocean identifier                *
C*      WADNM           CHAR*           Advisory number                 *
C*	WWND    	CHAR*		Current max sustained wind	*
C*      FDATE (NC,*)    CHAR*           Forecast date/time strings      *
C*      MXWD (NC,*)     INTEGER         Forecast max wind               *
C*      LNDEXT (NC,*)   CHAR*           Inland/Extratropical flag       *
C*      NC              INTEGER         Maximum number of lat/lon pairs *
C*      NSTRM           INTEGER         Number of decoded files         *
C*      TZONE           CHAR*           Current time zone		*
C*      WTYPE           CHAR*           Storm type			*
C*      IDISP           INTEGER         Storm dissipating flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01	Created					*
C* A. Hardy/GSC		 6/01	Added wtype, GQLINE, GSLINE		*
C* A. Hardy/GSC		 6/01	Added color tag; fixed prolog		*
C* A. Hardy/SAIC	 8/01	Added ddev to gh_wrel calling sequence  *
C* D. Kidwell/NCEP	 4/02	Changed PS check; cleaned up            *
C* A. Hardy/NCEP	 3/03   Modified yoffset for 'INLAND' label	*
C* m.gamazaychikov/SAIC	06/06	Added code to handle CP storms		*
c* S. Gilbert/NCEP      07/06   Added new argument origc                *
C* X. Guo/CWS		02/10   Used Post-Tropical instead of           *
C*                               Extratropical                          *
C* D. Zelinsky/NCEP/NHC 10/15   Commented out tzone assignment for EP   * 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)   wname, timstr, wocen, wadnm, tzone, wtype,
     +                  fdate(nc,*), lndext(nc,*), wwnd, origc
C 
        INTEGER         mxwd(nc,*), iyaxis(5,6), prblvl(4), iymph(2,13)
     	CHARACTER       curtim*15, chinex(6), advnm*3, ocean*6
C
	CHARACTER	panel*(LLMXLN),
     +	                parms*12, prmtyp*1, range*20, witnes*20,
     +             	ctlbl (7)*3, ttlstr*72
	CHARACTER       fcsttl*25, nhcttl1*32, timest*25, nhcttl2*28,
     +                  fmonth*3, ampm*2, zone2*3, cyear*4, cday*2,
     +                  chr*2, fuldat(6)*13, newdate*13, lndlbl*9,
     +                  fmon*9, fday*9, ddev*12, coltag*33
        INTEGER         jtarr(5)
	REAL		xtlbl (50), view (4), data(LLMXTM), 
     +                  datarng(LLMXTM), ylbl (LLAXIS), xbox (5),
     +			ybox (5)
        LOGICAL         proces, axflg, tmflg
C*
	CHARACTER	tpanl*24
	DATA		tpanl  / '.09;.10;.94;.85' /
C------------------------------------------------------------------------
C
C*      Query current settings.
C
        CALL GH_SAVE ( ier )
C
C*      Reverse black and white if device is for postscript.
C
        CALL GQDEV  ( ddev, iunit, iatyp, iret )
        IF  ( ddev(:2) .eq. 'PS' )  THEN
            iwht = 31
C
C*	    Force a black background for postscript.
C
	    CALL GSCOLR ( 1, ier )
	    xbox (1) = 0.
            ybox (1) = 0.
            xbox (2) = 0.
            ybox (2) = .8
            xbox (3) = 1.
            ybox (3) = .8
            xbox (4) = 1.
            ybox (4) = 0.
            xbox (5) = 0.
            ybox (5) = 0.
            CALL GFILL ( 'N', 5, xbox, ybox, ier )
          ELSE
            iwht = 1
        END IF
	CALL GSCOLR ( iwht, ier )
C
C*	Read in the variables.
C
        panel  =  '0'                  
	ibwidt = 3
C
C*	Set parameter information. 
C
        parms = 'SMPH'
        ntparm  = 1
        iptprm  = 1
        prmtyp  = 'R'
        range   = ' '
        witnes  = ' '
        nparms = 1
C
C*	Set information about x axis.
C
        ntime = 6
        xstrt = 2.875
        xstop = 0.0
C
        xtlbl(6) = 0.0
        xtlbl(5) = 1.0
        xtlbl(4) = 1.5 
        xtlbl(3) = 2.0
        xtlbl(2) = 2.5
        xtlbl(1) = 2.875
C
        ctlbl(1) = 'NOW'
        ctlbl(2) = '12'
        ctlbl(3) = '24'
        ctlbl(4) = '36'
        ctlbl(5) = '48'
        ctlbl(6) = '72'
C
        xmndst = 0.375
        ilbfrq = 1
        iglfrq = 1
        itmfrq = 1
	xmndst = xmndst * 3.
C
C*	Set the TEXT information.
C
        itxtfnt = 21
        itxhw = 2
        sztext = 1.2
        itxwid = 1
        ibrdr = 111
        irrotn = 0
        ijust = 2
C
C*      Calculate the probability lines.
C
        ocean = wocen
        CALL GH_WIER ( timstr, ocean, origc, wwnd, fdate, mxwd, NC,
     +                 nstrm, iyaxis, prblvl, proces, itwnd, iret )
C
C*	Set margins.
C
	IF  ( proces )  THEN
	    CALL GSGMGN  ( 7., 3., 7., 1., ier )
	END IF
C
C*	Set the panel information and store view coordinates.
C
	CALL GG_PANL ( panel, ier )
	CALL GQVIEW  ( view (1), view (2), view (3), view (4), ier ) 
C
C*      Set up the line arrays.
C
        DO jj =  1,itwnd
            iymph ( 1, jj ) = iyaxis(1,jj)
            iymph ( 1, jj+itwnd ) = iyaxis(4,(itwnd+1)-jj)
            iymph ( 2, jj ) = iyaxis(2,jj)
            iymph ( 2, jj+itwnd ) = iyaxis(3,(itwnd+1)-jj)
C
            chinex (jj) = lndext(jj,nstrm)
        END DO
        iend = (itwnd * 2 ) + 1
        iymph ( 1, iend ) = iyaxis(1,1)
        iymph ( 2, iend ) = iyaxis(2,1)
C
C*	Set full date string array.
C
        curtim = timstr 
        advnm = wadnm 
        fdate(1,1) = curtim(5:)
        DO ii = 1, itwnd
            newdate = curtim(1:4) // fdate (ii,1)
            CALL TI_DTM4 ( newdate, fuldat(ii), ier )
        END DO
C
C*      Begin the plotting.
C
	IF  ( ( proces ) .and. (idisp .ne. 2 ) )  THEN
            CALL ST_LSTR ( curtim, lens, ier)
            CALL ST_LSTR ( tzone, lenz, ier)
C
C*	    Add titles.
C
            IF ( ocean(1:2) .eq. 'CP' ) tzone = 'H'
C            IF ( ocean(1:2) .eq. 'EP' ) tzone = 'P'
            IF ( origc(1:4) .eq. 'CPHC' ) tzone = 'H'
            timest = curtim(:lens)
            CALL ST_LSTR ( timest, lent, ier)
	    CALL GH_TIME ( timest(:lent), tzone(:lenz), jtarr, hours, 
     +                     zone2, fmonth, fmon, fday, ampm, iret )
C
C*	    Add bottom time/date title.
C
            CALL ST_INCH (jtarr(1), cyear, ier)
            CALL ST_INCH (jtarr(3), cday, ier)
            CALL ST_INCH (jtarr(4), chr, ier)
            icttl = 6
            linttl = -2 
            CALL ST_LSTR ( wname, lenn, ier)
            CALL ST_LSTR ( advnm, lena, ier)
            CALL ST_LSTR ( chr, lenh, ier)
            CALL ST_LSTR ( ampm, lenp, ier)
            CALL ST_LSTR ( zone2, lenz, ier)
            CALL ST_LSTR ( fmon, lenm, ier)
            CALL ST_LSTR ( cday, lend, ier)
            CALL ST_LSTR ( cyear, leny, ier)
C
            CALL ST_LCUC ( wname, wname, ier )
            ttlstr  =  wname ( :lenn ) // '    ADVISORY   ' 
     +                   // advnm( :lena)  //'  '// chr(:lenh)//':00' 
     +                   // '  ' // ampm(:lenp) //'  '// zone2 (:lenz)
     +                   // '  '// fmon(:lenm)
     +                   //'  '// cday(:lend) // '  ' // cyear(:leny)
C
            coltag = 'i_date_title'
       	    CALL ST_LSTR ( coltag, lens, ier )
            CALL GH_COLR ( coltag(:lens), 6, ier)
            CALL GSTEXT ( 22, 2, 1.0, 1, 110, 1, 2, ier )
            CALL GG_WSTR ( ttlstr, linttl, ier )
C
C*	    Add bottom time label.
C
 	    CALL GSCOLR  ( iwht, ier )
            fcsttl = 'FORECAST PERIOD (HOURS)'
            linbot = -4
	    CALL GG_WSTR ( fcsttl, linbot, ier )
C                 
C*	    Add top two labels.
C
            nhcttl1 = 'NHC MAXIMUM 1-MINUTE WIND SPEED'
            IF ( origc (:4) .eq. 'CPHC' ) 
     +         nhcttl1 = 'CPHC MAXIMUM 1-MINUTE WIND SPEED'
            lintop = +4
	    CALL GG_WSTR ( nhcttl1, lintop, ier )
            nhcttl2 = 'FORECAST AND PROBABILITIES'
            lintop = +6
	    CALL GG_WSTR ( nhcttl2, lintop, ier )
C
C*	    Set the panel for plotting of winds.
C
	    CALL GH_WPNL  ( view, tpanl, ier )
C
C*	    Plot on the left and then the right.
C
	    axflg = .true.
	    tmflg = .true.
            ntot = iend
            ii = 1
            DO mm = 1,2
               DO kl = 1, ntot
                  datarng(ii) = iymph(mm,kl)
                  ii = ii + 1
               END DO
            END DO 
            itot = ii - 1
	    DO  j = 1, 2
C
C*	        Set up the graph coordinates.
C
	        IF  ( ntparm .gt. 0 )  THEN
	           CALL GSCOLR  ( iwht, ier )
	           CALL GSLINE  ( 1, 0, ibwidt, 0, ier )
                   CALL GSTEXT ( 2, 2, 1.2, 1, 110, 1, 2, ier )
C
 	           CALL GH_WGRF  ( j, 1, itot, datarng, 
     +			  	     prmtyp, iptprm, range,
     +				     axflg, xstrt, xstop, xtlbl, ctlbl,
     +				     ntime, tmflg, ilbfrq, itmfrq,
     +				     witnes, parms, ntparm,  
     +                               ystrt, nylbl, ylbl, iwht, iret )
                    iret = 0
		    IF ( iret .eq. 0 )  axflg = .false.
                  ELSE
		    iret = -1
	        END IF
C
C*	        Plot the parameters.
C
                DO ii = 1, iend
                    data(ii) = iymph(j,ii)
                END DO
C
	        IF  ( iret .eq. 0 )  THEN
                    IF ( j .eq. 1 ) THEN
                        coltag = 'i_ten_percent_fill'
       			CALL ST_LSTR ( coltag, lens, ier )
        		CALL GH_COLR ( coltag(:lens), 17, ier)
                      ELSE IF  (j .eq. 2 ) THEN
                        coltag = 'i_twenty_thirty_fill'
       			CALL ST_LSTR ( coltag, lens, ier )
        		CALL GH_COLR ( coltag(:lens), 2, ier)
                    END IF
C
	            CALL GH_WREL  ( origc, ddev, parms, data, ntot, 
     +                             xmndst, xtlbl, iyaxis, prblvl, 
     +                             itwnd, iend, ier )
	        END IF
	    END DO
C
C*	    Draw the bottom x axis, labels, tick marks and witness lines.
C
            axflg = .true.
            CALL GSTEXT ( 2, 2, 1.2, 1, 110, 1, 1, ier )
            CALL GSCOLR ( iwht, ier )
	    CALL GSTICK  ( 2, 1.5, ier )
            CALL GSLINE  ( 10, 0, 2, 0, ier )
	    CALL GAAXIS  ( 1, ystrt, axflg, ilbfrq, itmfrq,
     +			           iglfrq, ntime, xtlbl, ctlbl, ier )
C
C*	    Draw the y axes, labels, tick marks and witness lines.
C
 	    CALL GAAXIS  ( 2, xstrt, axflg, 0, 0, 0, 0, xtlbl, ctlbl,
     +			   ier )
 	    CALL GAAXIS  ( 4, xstop, axflg, 0, 0, 0, 0, xtlbl, ctlbl,
     +			   ier )
C
	    jside = 2
	    xxxxx = xstrt
	    CALL GDAXIS  ( jside, xstrt, .false., 1, 1, 0, 0, nylbl,
     +			       ylbl, ier )
            CALL GSTEXT ( 2, 2, 1.2, 1, 110, 1, 2, ier )
            jside = 4
	    CALL GDAXIS  ( jside, xstop, .false., 1, 1, 0, 0, nylbl,
     +			       ylbl, ier )
C
            ixoff = 0
            iyoff = -2 
            CALL GSTEXT ( 2, 2, 1.0, 1, 111, 1, 2, ier )
            DO im = 2, itwnd
                IF ( (chinex (im) .eq. 'I') .or.
     +                           (chinex (im) .eq. 'P' ) )THEN
                    IF (chinex (im) .eq. 'I') THEN
                        lndlbl = 'INLAND'
                        iyoff = -1 
                        IF ( im .eq. 6)  THEN
                            ixoff = -8
                        END IF
                      ELSE IF  (chinex (im) .eq. 'P' ) THEN
                        lndlbl = 'POST-TROP'
                        IF ( im .eq. 6)  ixoff = -4
                    END IF
                    CALL ST_LSTR ( lndlbl, lenl, ier )
 		    CALL GTEXT  ( 'M', xtlbl(im), ystrt+4,
     +                             lndlbl(:lenl), 0.0,ixoff,iyoff, ier)
                END IF
            END DO
C
C*          If last forecast or extra tropical at 12h.
C
	  ELSE 
            CALL ST_LSTR ( advnm, lena, ier)
	    CALL GH_NOPB  ( advnm, wtype, wname, ier )
        END IF
C
C*      Plot NOAA logo.
C
        CALL GLOGO ( 'N', .93, .06, 2.5, 2, 1, ier )
C
C*      Plot NWS logo.
C
        CALL GLOGO ( 'N', .08, .06, 2.5, 2, 2, ier )
C
C*	Reset view to initial settings.
C
	CALL GSVIEW  ( view (1), view (2), view (3), view (4), ier ) 
C
C*      Reset the saved attributes.
C
        CALL GH_REST ( ier )
C*
        RETURN
	END
