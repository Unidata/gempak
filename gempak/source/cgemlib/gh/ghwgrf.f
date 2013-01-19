	SUBROUTINE GH_WGRF  ( iside, itrace, ntime, data, prmtyp, iptprm,
     +			     range, axflg, xstrt, xstop, xtlbl, ctlbl,
     +			     nxlbl, tmflg, italf, itatf, witnes, parms,
     +			     ntparm, ystrt, nylbl, ylbl, iwht, iret )
C************************************************************************
C* GH_WGRF								*
C*									*
C* This subroutine sets up the y axis and draws the background for	*
C* the TPC wind intensity graphic.					*
C*									*
C* GH_WGRF  ( ISIDE, ITRACE, NTIME, DATA, PRMTYP, IPTPRM, RANGE, AXFLG, *
C*            XSTRT, XSTOP, XTLBL, CTLBL, NXLBL, TMFLG, ITALF, ITATF,   *
C*            WITNES, PARMS, NTPARM, YSTRT, NYLBL, YLBL, IWHT, IRET )   *
C*									*
C* Input parameters:							*
C*	ISIDE		INTEGER		Current side (1=left,2=right)	*
C*	ITRACE		INTEGER		Current trace			*
C*	NTIME		INTEGER		Number of times			*
C*	DATA (NTIME,4)	REAL		Data				*
C*	PRMTYP    	CHAR*		Parameter type			*
C*	IPTPRM    	INTEGER		Pointers to data		*
C*	RANGE		CHAR*		Input for range			*
C*	AXFLG		LOGICAL		Axis draw flag			*
C*	XSTRT		REAL		First point on x axis		*
C*	XSTOP		REAL		Last point on x axis		*
C*	XTLBL (NXLBL)	REAL		X axis label points		*
C*	CTLBL (NXLBL)	CHAR*		X axis labels			*
C*	NXLBL		INTEGER		Number of x axis labels		*
C*	TMFLG		LOGICAL		Time axis label flag		*
C*	ITALF		INTEGER		Time axis label frequency	*
C*	ITATF		INTEGER		Time axis tic mark frequency	*
C*	WITNES		CHAR*		Witness line input		*
C*	PARMS    	CHAR*		Parameter names			*
C*	NTPARM 		INTEGER		Number of parameters		*
C*									*
C* Output parameters:							*
C*      YSTRT		REAL		Starting y axis position  	*
C*	NYLBL		INTEGER		Number of y axis labels		*
C*    	YLBL (NYLBL)	REAL		Array of y axis labels		*
C*	IWHT		INTEGER		Gempak color of strings		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = invalid axes		*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01	Modified from SFXGRF			*
C* A. Hardy/GSC		 6/01	Added color tag				*
C* A. Hardy/GSC		 7/01	Removed unecessary code			*
C* A. Hardy/SAIC         8/01   Reorganized CAT labeling and PS colors	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NWIT = 20 )
	CHARACTER*(*)	prmtyp, range, ctlbl (*), witnes, parms 
	LOGICAL		axflg, tmflg
	REAL		data  ( * ), xtlbl (*), ylbl(*)
C*
	CHARACTER	title*30, myparm(3)*5, sshs(5), catlb*4,
     +                  coltag*33
	LOGICAL		realpm, docat
	REAL		ycat(5), ycat2(5),
     +                  xline(2), yline(2), xnlin(2), ynlin(2)
C*
        DATA            ycat / 84.75, 103.0, 120.5, 142.75, 165.0 /
        DATA            ycat2 / 74.0, 95.5, 110.5, 130.5, 155.0 /
C------------------------------------------------------------------------
	iret = 0
        docat = .false.
C
C*	Set tick marks inside.
C
	CALL GSTICK  ( 2, 1., ier )
C
C*	Check for real valued parameters to determine axis.
C
	realpm = .false.
	iloc1 = iptprm 
	iloc2 = iptprm 
C
C*	Scale the axis. 
C
	CALL GH_WRNG  ( ntime, data, iloc1, iloc2, range, ystrt,
     +		       ystop, nylbl, ylbl, ier )
C
C*	Set up the graph coordinate system.
C
	CALL GSGRAF  ( 1, 1, 0., xstrt, ystrt, xstop, ystop, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -8
	    CALL ER_WMSG  ( 'GPTPC', -10, ' ', ier )
	    RETURN
	END IF
C
C*	Draw the top x axis.
C
	CALL GSCOLR  ( iwht, ier )
	IF  ( axflg )  THEN
	    CALL GAAXIS  ( 3, ystop, axflg, 0, 0, 0, 0, xtlbl, ctlbl,
     +			   ier )
	END IF
C
C*	Write title for parameters.
C
	CALL GQLINE  ( iltyp, ilhw, ilwid, iwhw, ier )
	IF  ( iside .eq. 1 )  THEN
        myparm(1) = 'WIND'
        myparm(2) = 'SPEED'
        myparm(3) = '(MPH)'
	title = ' '
C
	CALL ST_LSTR ( myparm(1), lenw, iret)  
        CALL ST_LSTR ( myparm(2), lens, iret)  
        CALL ST_LSTR ( myparm(3), lenm, iret)  
        title = myparm(1)(:lenw) //CHCR//CHCR//myparm(2)(:lens) //
     +            CHCR//CHCR// myparm(3)(:lenm)
        CALL ST_LSTR ( title, lent, iret)  
	    rotat = 0.
            ixoff = -14
	    iyoff = -2 
	    xpt   = xstrt
	ymid  = ( ystrt + ystop ) / 2. 
	CALL GSCOLR  ( iwht, ier )
	CALL GQTEXT   ( itxfnt, itxhw, sztext, itxwid,
     +				ibrdr, irrotn, ijust, ier )
	CALL GTRANS  ( 'M', 'N', 1, xpt, ymid, xnorm, ynorm, ier )
	CALL GTEXT   ( 'N', xnorm, ynorm, title(:lent), rotat, 
     +			ixoff, iyoff, ier )
	END IF
C
C*	Determine the hurricane catagories and draw break lines.
C
        do ii = 1,5
            sshs (ii) =  ' '
        end do
        IF ( ystop .gt. 155 ) THEN
            icat = 5
            docat = .true.
          ELSE IF (  ystop .gt. 131 ) THEN
            icat = 4
            docat = .true.
          ELSE IF (  ystop .gt. 111 ) THEN
            icat = 3
            docat = .true.
          ELSE IF (  ystop .gt. 96 ) THEN
            icat = 2
            docat = .true.
          ELSE IF (  ystop .gt. 74 ) THEN
            icat = 1
            docat = .true.
        END IF
C
        ixoff = 0
        iyoff = 0
	coltag = 'i_categories_label'
	CALL ST_LSTR ( coltag, lens, ier )
	CALL GH_COLR ( coltag(:lens), 2, ier)
	CALL GSTEXT ( 2, 2, 1.2, 1, 110, 1, 3, ier )
C
        IF ( ( docat ) .and. ( iside .eq. 2 ) ) THEN
C
C*          Add CAT. label.
C
            catlb = 'CAT.'
 	    rotat = 0.
            ixoff = 0
 	    iyoff = 0 
 	    xpt   = xstrt
 	    CALL GSTEXT ( 2, 2, 1.2, 1, 110, 1, 2, ier )
 	    CALL GTRANS  ( 'M', 'N', 1, xstop, ystop, xnormc, ynormc, 
     +                     ier)
 	    CALL GTEXT   ( 'N', xnormc+.08, ynormc+.03, catlb, rotat, 
     +			    ixoff, iyoff, ier )
C
            DO ii = 1, icat
C
C*              Draw sshs catagory lines.
C
                ydiff2 = ycat2 (ii) - ystrt 
                xline(1) =  xstop - 0.44
                yline(1) = ystrt+ ydiff2 
                xline(2) = xstop - 0.27
                yline(2) = ystrt+ ydiff2 
                CALL GTRANS  ( 'M', 'N', 2, xline, yline,
     +                          xnlin, ynlin,  ier )
                CALL GLINE ( 'N', 2, xnlin, ynlin, ier )
C
C*              Plot SSHS numbers.
C
                CALL ST_INCH ( ii, sshs (ii), ier )
                ydiff = ycat (ii) - ystrt 
                ydiff1 = ystop - ycat (ii) 
                yplace = ystrt + ydiff
                IF (  ABS (ydiff1) .le. 5.0 ) yplace = ystop - 3
                IF (  ystop .le. 76.0 ) yplace = ystop 
                CALL GTRANS  ( 'M', 'N', 1, xstop, yplace,
     +                          xnorm, ynorm,  ier )
                IF ( (ii .eq. icat ) .and. ( ynorm .gt. ynormc ) ) THEN
                    ynorm = ( ( ynorm + ynormc) / 2.) -0.008
                END IF
 	        CALL GTEXT ( 'N', xnorm+.08, ynorm, sshs(ii), rotat, 
     +			      ixoff, iyoff, ier )
            END DO
        END IF
C
C*	Reset tick marks.
C
	CALL GSTEXT   ( itxfnt, itxhw, sztext, itxwid,
     +				ibrdr, irrotn, ijust, ier )
	CALL GSTICK  ( 1, 0., ier )
C*
	RETURN
	END
