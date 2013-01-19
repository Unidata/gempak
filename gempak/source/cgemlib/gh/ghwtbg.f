	SUBROUTINE GH_WTBG ( origc, timstr, strnam, advnm, tzone, nwrn, 
     +                       iwht, iblk, iret)
C************************************************************************
C* GH_WTBG								*
C*									*
C* This subroutine draws the lines and plots the labels for the         *
C* speed intensity table.						*
C*									*
C* GH_WTBG ( ORIGC, TIMSTR, STRNAM, ADVNM, TZONE, NWRN, IWHT, IBLK,     *
C*		IRET) 							*
C*									*
C* Input parameters:							*
C*	ORIGC		CHAR*		Issuing center			*
C*	TIMSTR (*)	CHAR*		Date/time string		*
C*	STRNAM		CHAR*		Storm name			*
C*	ADVNM		CHAR*		Advisory number			*
C*	TZONE		CHAR*		Local time zone			*
C*	NWRN		INTEGER		Number of advisories for a storm*
C*      IWHT            INTEGER         Device color for white		*
C*      IBLK            INTEGER         Device color for black		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01	Created					*
C* A. Hardy/SAIC	 8/01	Increased line width, font size for ps  *
C* D. Kidwell/NCEP	 4/02	Changed PS check                        *
C* m.gamazaychikov/SAIC	06/06	Changed CS, setting of prbstr		*
C* S. Gilbert/NCEP	 7/06	Changed stormid (wocen) to issuing      *
C*                              center (origc)                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        CHARACTER*(*)	timstr(*), strnam, advnm, tzone, origc
C*
        CHARACTER       prbstr(3)*30, prbttl*128, time*4, 
     +                  wsinm*30, hr*5, diss*10, td*30, ts*30, 
     +                  hur*30, hrlbl*10, cat1*15, cat2*15, cat3*15,
     +                  cat4*15, cat1nm*10, cat2nm*10, cat3nm*10,
     +                  cat4nm*10, tdnm*8, tsnm*8, hurnm*8,
     +                  h12*2, h24*2, h36*2, h48*2, h72*2, ddev*12
        CHARACTER       fmonth*3, ampm*2, zone2*3, cyear*4, cday*2,
     +                  chr*2, date*30, fmon*9, fday*9
        INTEGER         jtarr(5)
        REAL            xlin(2), ylin(2), xbox(5), ybox(5)
        REAL            hours
        LOGICAL         psflg
C------------------------------------------------------------------------
        iret = 0
C
        CALL GQDEV  ( ddev, iunit, iatyp, iret )
        IF  ( ddev(:2) .ne. 'PS' )  THEN
            psflg = .false.
          ELSE
            psflg = .true.
        END IF
C
	CALL GSCOLR  ( iwht, ier )
C
C*	Set the TEXT information.
C
        xbox(1) = 0.
        ybox(1) = 0.
        xbox(2) = 0.
        ybox(2) = .8
        xbox(3) = 1.
        ybox(3) = .8
        xbox(4) = 1.
        ybox(4) = 0.
        xbox(5) = 0.
        ybox(5) = 0.
        CALL GFILL ( 'N', 5, xbox, ybox, ier)
	CALL GSCOLR  ( iblk, ier )
        ilwid = 5
        IF ( psflg ) ilwid = ilwid * 16
	CALL GSLINE  ( 1, 1, ilwid, 1, ier )
C
C*      Draw the 2 thick horizontal lines.
C
        xlin(1) = 0.
        ylin(1) = .58
        xlin(2) = 1.
        ylin(2) = .58
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
        xlin(1) = 0.
        ylin(1) = .40
        xlin(2) = 1.
        ylin(2) = .40
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
C
C*      Draw the 2 thick vertical lines.
C
        xlin(1) = .08
        ylin(1) = .58
        xlin(2) = .08
        ylin(2) = 0.
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
        xlin(1) = .60
        ylin(1) = .52
        xlin(2) = .60
        ylin(2) = 0.
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
C
C*      Draw the 'Wind Speed interval' horz. line.
C
        ilwid = 3
        IF ( psflg ) ilwid = ilwid * 12
	CALL GSLINE  ( 1, 1, ilwid, 1, ier )
        xlin(1) = 0.
        ylin(1) = .52
        xlin(2) = 1.
        ylin(2) = .52
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
C
C*      Draw the 'Hurricane' horz. line.
C
        ilwid = 2
        IF ( psflg ) ilwid = ilwid * 12
	CALL GSLINE  ( 1, 1, ilwid, 1, ier )
        xlin(1) = .60
        ylin(1) = .48
        xlin(2) = 1.
        ylin(2) = .48
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
C
C*      Draw the 4 horz. forecast lines - top -> bottom.
C
        ilwid = 1
        IF ( psflg ) ilwid = ilwid * 12
	CALL GSLINE  ( 1, 1, ilwid, 1, ier )
        xlin(1) = 0.
        ylin(1) = .32
        xlin(2) = 1.
        ylin(2) = .32
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
        xlin(1) = 0.
        ylin(1) = .24
        xlin(2) = 1.
        ylin(2) = .24
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
        xlin(1) = 0.
        ylin(1) = .16
        xlin(2) = 1.
        ylin(2) = .16
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
        xlin(1) = 0.
        ylin(1) = .08
        xlin(2) = 1.
        ylin(2) = .08
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
C
C*      Draw the 3 vert. forecast lines - left -> right.
C
        xlin(1) = .21
        ylin(1) = .52
        xlin(2) = .21
        ylin(2) = 0.
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
        xlin(1) = .355
        ylin(1) = .52
        xlin(2) = .355
        ylin(2) = 0.
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
        xlin(1) = .475
        ylin(1) = .52
        xlin(2) = .475
        ylin(2) = 0.
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
C
C*      Draw the 3 horiz. catagory lines - left -> right.
C
        xlin(1) = .70
        ylin(1) = .48
        xlin(2) = .70
        ylin(2) = 0.
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
        xlin(1) = .80
        ylin(1) = .48
        xlin(2) = .80
        ylin(2) = 0.
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
        xlin(1) = .90
        ylin(1) = .48
        xlin(2) = .90
        ylin(2) = 0.
        CALL GLINE ( 'N', 2, xlin, ylin, ier)
C
C*	Set the time string.
C
 	CALL GH_TIME ( timstr, tzone, jtarr, hours, zone2, fmonth, 
     +                 fmon, fday, ampm, iret )
        CALL ST_INCH (jtarr(1), cyear, ier)
        CALL ST_INCH (jtarr(3), cday, ier)
        CALL ST_INCH (jtarr(4), chr, ier)
        CALL ST_LSTR ( cyear, leny, ier)
        CALL ST_LSTR ( cday, lend, ier)
        CALL ST_LSTR ( chr, lenh, ier)
        CALL ST_LSTR ( ampm, lena, ier)
        CALL ST_LSTR ( zone2, lenz, ier)
        CALL ST_LSTR ( fmon, lenm, ier)
        date = chr(:lenh) // ':00 ' // ampm(:lena) // ' ' // 
     +         zone2(:lenz) // ' ' // fmon(:lenm) // ' ' // 
     +          cday(:lend) // ' ' //cyear(:leny)
        CALL ST_LSTR ( date, lend, ier)
C
C*	Set the title text label.
C
        ixoff = 0
        iyoff = 0
        prbstr (1) = 'WIND SPEED FORECAST FOR '
        prbstr (2) = 'EXPRESSED AS PROBABILITY'
        prbstr (3) = 'FROM NHC ADVISORY' 
        IF ( origc (:4) .eq. 'CPHC' )  prbstr (3) = 'FROM CPHC ADVISORY'
        CALL ST_LSTR ( advnm, lena, ier)
        CALL ST_LSTR ( strnam, lenw, ier)
        CALL ST_LSTR ( prbstr(1), leno, ier )
        CALL ST_LSTR ( prbstr(2), lent, ier )
        CALL ST_LSTR ( prbstr(3), lenh, ier )
        prbttl = prbstr(1) (:leno) // ' ' // strnam(:lenw) // CHCR // 
     +               CHCR // prbstr(2) (:lent)// CHCR // CHCR //
     +               prbstr(3) (:lenh) // '   ' //advnm(:lena) //
     +               CHCR // CHCR // date(:lend)
        CALL ST_LSTR ( prbttl, lens, ier )
        CALL GSTEXT ( 22, 2, 1.4, 1, 111, 1, 2, ier )
        CALL GTEXT  ( 'N',.5, .70 , prbttl(:lens), 0.0,
     +                    ixoff,iyoff, ier)
C
C*	Set the 2nd title text labels.
C
 
        time = 'TIME' 
        CALL ST_LSTR ( time, lens, ier )
        CALL GSTEXT ( 2, 2, 1.5, 1, 111, 1, 2, ier )
        CALL GTEXT  ( 'N',.035, .55 , time(:lens), 0.0, 0, 0, ier)
C
        wsinm = 'WIND SPEED INTERVAL IN MPH' 
        CALL ST_LSTR ( wsinm, lens, ier )
        CALL GTEXT  ( 'N',.54, .55, wsinm(:lens), 0.0, 0, 0, ier)
C
C*	Set the storm type text labels.
C
        CALL GSTEXT ( 2, 2, 1.15, 1, 111, 1, 2, ier )
        hr= 'HOURS' 
        CALL ST_LSTR ( hr, lens, ier )
        CALL GTEXT  ( 'N',.036, .49 , hr(:lens), 0.0, 0, 0, ier)
        diss = 'DISSIPATED' 
        CALL ST_LSTR ( diss, lens, ier )
        CALL GTEXT  ( 'N',.145, .49 , diss(:lens), 0.0, 0, 0, ier)
C
        td = 'TROPICAL'// CHCR // CHCR // 'DEPRESSION'
        CALL ST_LSTR ( td, lens, ier )
        CALL GTEXT  ( 'N',.28, .475, td(:lens), 0.0, 0, 0, ier)
C
        ts = 'TROPICAL'// CHCR // CHCR // 'STORM' 
        CALL ST_LSTR ( ts, lens, ier )
        CALL GTEXT  ( 'N',.415, .475, ts(:lens), 0.0, 0, 0, ier)
C
        hur = 'HURRICANE'
        CALL ST_LSTR ( hur, lens, ier )
        CALL GTEXT  ( 'N',.536, .49 , hur(:lens), 0.0, 0, 0, ier)
        CALL GSTEXT ( 2, 2, 1.5, 1, 111, 1, 2, ier )
        tdnm = '< 39'
        CALL ST_LSTR ( tdnm, lens, ier )
        CALL GTEXT  ( 'N',.28, .42 , tdnm(:lens), 0.0, 0, 0, ier)
C
        tsnm = '39 - 73'
        CALL ST_LSTR ( tsnm, lens, ier )
        CALL GTEXT  ( 'N',.415, .42 , tsnm(:lens), 0.0, 0, 0, ier)
C
        hurnm = '>= 74'
        CALL ST_LSTR ( hurnm, lens, ier )
        CALL GTEXT  ( 'N',.54, .42 , hurnm(:lens), 0.0, 0, 0, ier)
C
C*	Set the hurricane catagories 1-5 text labels.
C
        CALL GSTEXT ( 2, 2, 1.5, 1, 111, 1, 2, ier )
        hrlbl = 'HURRICANE '
        CALL ST_LSTR ( hrlbl, lens, ier )
        CALL GTEXT  ( 'N',.80, .50 , hrlbl(:lens), 0.0, 0, 0, ier)
C
        CALL GSTEXT ( 2, 2, 1.15, 1, 111, 1, 2, ier )
        cat1 = 'CAT. 1'
        CALL ST_LSTR ( cat1, lens, ier )
        CALL GTEXT  ( 'N',.65, .46 , cat1(:lens), 0.0, 0, 0, ier)
C
        cat2 = 'CAT. 2'
        CALL ST_LSTR ( cat2, lens, ier )
        CALL GTEXT  ( 'N',.75, .46 , cat2(:lens), 0.0, 0, 0, ier)
C
        cat3 = 'CAT. 3'
        CALL ST_LSTR ( cat3, lens, ier )
        CALL GTEXT  ( 'N',.85, .46 , cat3(:lens), 0.0, 0, 0, ier)
C
        cat4 = 'CAT. 4-5'
        CALL ST_LSTR ( cat4, lens, ier )
        CALL GTEXT  ( 'N',.95, .46 , cat4(:lens), 0.0, 0, 0, ier)
C
        CALL GSTEXT ( 2, 2, 1.5, 1, 111, 1, 2, ier )
        cat1nm = '74 - 95'
        CALL ST_LSTR ( cat1nm, lens, ier )
        CALL GTEXT  ( 'N',.65, .42 , cat1nm(:lens), 0.0, 0, 0, ier)
        cat2nm = '96 - 110'
        CALL ST_LSTR ( cat2nm, lens, ier )
        CALL GTEXT  ( 'N',.75, .42 , cat2nm(:lens), 0.0, 0, 0, ier)
        cat3nm = '111 - 130'
        CALL ST_LSTR ( cat3nm, lens, ier )
        CALL GTEXT  ( 'N',.85, .42 , cat3nm(:lens), 0.0, 0, 0, ier)
        cat4nm = '>= 131'
        CALL ST_LSTR ( cat4nm, lens, ier )
        CALL GTEXT  ( 'N',.95, .42 , cat4nm(:lens), 0.0, 0, 0, ier)
C
C*	Set the hours labels.
C
        CALL GSTEXT ( 2, 2, 2., 1, 111, 1, 2, ier )
        h12 = '12'
        CALL ST_LSTR ( h12, lens, ier )
        CALL GTEXT  ( 'N',.035, .36 , h12(:lens), 0.0, 0, 0, ier)
        h24 = '24'
        CALL ST_LSTR ( h24, lens, ier )
        CALL GTEXT  ( 'N',.035, .28 , h24(:lens), 0.0, 0, 0, ier)
        h36 = '36'
        CALL ST_LSTR ( h36, lens, ier )
        CALL GTEXT  ( 'N',.035, .20 , h36(:lens), 0.0, 0, 0, ier)
        h48 = '48'
        CALL ST_LSTR ( h48, lens, ier )
        CALL GTEXT  ( 'N',.035, .12 , h48(:lens), 0.0, 0, 0, ier)
        h72 = '72'
        CALL ST_LSTR ( h72, lens, ier )
        CALL GTEXT  ( 'N',.035, .04 , h72(:lens), 0.0, 0, 0, ier)
C*
        RETURN
	END
