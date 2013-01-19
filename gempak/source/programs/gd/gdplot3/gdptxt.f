	SUBROUTINE GDPTXT ( txtcol, txtype, txtfil, txtloc, column, 
     +			    iret )
C************************************************************************
C* GPTEXT 								*
C*									*
C* This subroutine plots the contents of a text file to the device.	*
C*									*
C* GDPTXT ( TXTCOL, TXTYPE, TXTFIL, TXTLOC, COLUMN, IRET )		*
C*									*
C* Input parameters:							*
C*	TXTCOL		CHAR*		Text color			*
C*	TXTYPE		CHAR*		Text attributes			*
C*	TXTFIL		CHAR*		ASCII text file			*
C*	TXTLOC		CHAR*		Text location			*
C*	COLUMN		CHAR*		Number of columns		*
C*									*
C* output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = error setting navigation	*
C*					 -7 = file does not exist	*
C*									*
C**									*
C* Log:									*
C* T. Lee/GSC		10/97						*
C* S. Jacobs/NCEP	10/97	Removed TITLE; Added TXTLOC		*
C* S. Jacobs/NCEP	10/97	Added calls to GSTANM and GENANM	*
C* S. Jacobs/NCEP	10/97	Use GTEXT instead of GG_WSTR		*
C* I. Durham/GSC	01/98	Added column option			*
C* S. Jacobs/NCEP	 5/98	Modified line spacing, start location,	*
C*				and column spacing			*
C* S. Jacobs/NCEP	 5/98	Fixed problem with start location	*
C* T. Lee/GSC		10/98	Added map coordinates and NOAA logo	*
C* T. Lee/GSC		12/98	Fixed arg type in ST_RLST		*
C* S. Jacobs/NCEP	 2/99	Added check for <=10 lines in text file	*
C* T. Lee/GSC		 6/99	Removed y-dir offset for map coordinate	*
C* A. Hardy/GSC		 5/00	Added logo emblem color mode            *
C* J. Wu/GSC		 3/01   Added logo emblem ID  			*
C* S. Jacobs/NCEP	 4/01	Added NOAA and NWS keywords for logos	*
C* S. Jacobs/NCEP	 7/01	Fixed y-offset calculation 		*
C* T. Lee/SAIC		 1/06	Restructured for GDPLOT3		* 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	txtcol, txtype, txtfil, txtloc, column
	CHARACTER	utxtfil*(LLMXLN), textfl(3)*(LLMXLN)
C*
	CHARACTER	intext*132, txtstr*400
	REAL		rarr (2)
	LOGICAL		proces, compl, logo
C-----------------------------------------------------------------------
	iret = 0
	logo = .false.
	proces = .true.
C
C*	Query current settings and set defaults.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr, jrrotn, 
     +		      jjust, ier )
        CALL GSCOLR ( 1, ier )
        CALL GSTEXT ( 1, 1, 1., 1, 111, 1, 1, ier )
C
C*	Set text.
C
	CALL IN_TEXT ( txtype, ier )
C
C*	Open the input file.
C
	jpos = INDEX ( txtfil, '|' )
	IF  ( jpos .ne. 0 )  THEN
C
C*	    Parse size of the logo.
C
	    CALL ST_CLST ( txtfil, '|', ' ', 3, textfl, num, ier )
	  ELSE
	    textfl (1) = txtfil
	END IF
	CALL ST_LCUC ( textfl(1), utxtfil, ier )
	IF  ( textfl (1) .eq. ' ' )  THEN
	    proces = .false.
	  ELSE IF ( utxtfil .eq. 'LOGO' ) THEN
	    logo  = .true.
	    ilogo = 1
	  ELSE IF ( utxtfil .eq. 'NOAA' ) THEN
	    logo  = .true.
	    ilogo = 1
	  ELSE IF ( utxtfil .eq. 'NWS' ) THEN
	    logo  = .true.
	    ilogo = 2
	  ELSE
	    CALL FL_SOPN ( textfl(1), lun, ier )	
	    IF  ( ier .ne. 0 )  THEN
		iret = -7
		proces = .false.
		CALL ER_WMSG  ( 'GDPLOT3', iret, textfl(1), ier )
		CALL FL_CLOS  ( lun, ier )
	    END IF
	END IF
C
C*	Check if map location is requested. Set text location.
C
	IF ( proces )  THEN
	    ipos = INDEX ( txtloc, '#' )
	    IF  ( ipos .eq. 0 )  THEN
		CALL ST_RLST ( txtloc, ';', 0., 2, rarr, num, ier )
	      ELSE
		CALL ST_RLST  ( txtloc ( ipos+1: ), ';', 0., 2, rarr, 
     +				num, ier )
		CALL GTRANS   ( 'M', 'V', 1, rarr(1), rarr(2), rarr (1),
     +				rarr (2), ier )
		IF ( ier .ne. 0 )  THEN
		    iret = -5
		    proces = .false.
		    CALL ER_WMSG ( 'GDPLOT3', iret, textfl(1), ier )
		END IF
	    END IF
	END IF
C
C*	Set colors.
C
	IF  ( proces )  THEN
	    CALL IN_COLR ( txtcol, 1, icol, ier )
	    CALL GSCOLR ( icol, ier )
C
C*	    Set size and color mode of the logo.
C
	    IF  ( logo )  THEN
		CALL ST_CRNM ( textfl(2), size, ier )
		IF  ( size .le. 0. ) size = 1. 
C
		CALL ST_LCUC ( textfl(3), textfl(3), ier )
                IF ( textfl(3) .eq. 'C' ) THEN
		    iclmod = 2 
                  ELSE
		    iclmod = 1 
                END IF
	    END IF
C
	    IF  ( logo .and. ipos .ne. 0 )  THEN
C
C*		Plot logo.
C
		CALL GLOGO  ( 'V', rarr(1), rarr(2), size, iclmod, 
     +			      ilogo, ier )
	      ELSE
C
C*		Find the bounds of the device display area.
C
		CALL GQSYSZ ( rxszmk, ryszmk, rxsztx, rysztx, rxszwb, 
     +			      ryszwb, ier )
		CALL GQTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr, 
     +			      irrotn, ijust, ier )
		CALL GQBND ( 'V', xvl, yvb, xvr, yvt, ier )
C
C*		Declare the initial conditions.
C
		IF  ( logo )  THEN
		    adj = 0.
		  ELSE
		    adj = .01 * sztext
		END IF
C
C*		Compute new coordinates from the view area.
C
		IF  ( ipos .eq. 0 )  THEN
		    xp = xvl + ( rarr (1) * ( xvr - xvl ) ) + adj
		    yp = yvb + ( rarr (2) * ( yvt - yvb ) ) - adj
		  ELSE
		    xp = rarr (1)
		    yp = rarr (2)
		END IF
C 
C*		Plot logo on normal coordinates.
C
		IF  ( logo )   THEN
		    CALL GLOGO ('V', xp, yp, size, iclmod, ilogo, ier)
		  ELSE
C
C*		    Set the number of columns.
C
		    CALL ST_NUMB ( column, ncol, ier )
		    IF ( ncol .le. 0 .or. ipos .ne. 0 ) ncol = 1
C
C*		    Get the number of lines in the file.
C
		    jostat = 0
		    nline  = 0
		    DO WHILE  ( jostat .eq. 0 )
			READ (lun, 1000, IOSTAT = jostat) intext
			IF (jostat .eq. 0) nline = nline + 1
		    END DO
		    CALL FL_REWD ( lun, ier )
C
		    IF  ( ( ncol .eq. 1 ) .and. ( nline .le. 10 ) ) THEN
C
C*			If there is only one column and 10 lines or 
C*			less, treat the text the text to be in a single 
C*			box, etc.
C
			iostat = 0
			txtstr = ' '
			DO  i = 1, nline
			    READ (lun, 1000, IOSTAT=iostat) intext
			    CALL ST_LSTR ( intext, ntext, ier)
			    CALL ST_UTAB ( intext, ntext, intext, ier )
			    CALL ST_LSTR ( intext, ntext, ier)
			    CALL ST_LSTR ( txtstr, nstr, ier)
			    IF  ( ntext .gt. 0 )  THEN
				txtstr( nstr+1: ) = intext ( :ntext )
			    END IF
			    IF  ( i .ne. nline )  THEN
				m = nstr + 1 + ntext
				txtstr(m:m) = CHCR
			    END IF
			END DO
C
     			iyo = (-1) * ( nline - 1)
C
			CALL GTEXT ( 'V', xp, yp, txtstr, 0., 0, iyo, 
     +				     ierr )
		      ELSE
C
C*			Read the text file line by one line 
C*			and switch to a new column at the 
C*			bottom of the panel.
C
			i  = 0
			xb = xvr - adj
			yb = yp - ( (nline/ncol+ MOD (nline,ncol) ) *
     +			     rysztx) + .005
			IF ( yb .lt. ( yvb + .01 ) ) yb = yvb + .01
C
			x  = xp
			y  = yp
			compl  = .false.
			iostat = 0
C
			DO WHILE  ( ( iostat .eq. 0 ) .and.
     +					        ( .not. compl ) )
			    yc = y + ( (i/3.0) * rysztx )
			    IF  ( yc .le. yb )  THEN
				i = 0
				x = ((xvr-xp) / ncol) + x 
				IF  ( x .gt. xb ) compl = .true.
			    END IF
			    IF  ( .not. compl )  THEN
				READ (lun, 1000, IOSTAT=iostat) intext
1000				FORMAT ( A )
				IF ( iostat .eq. 0 ) THEN
				    CALL ST_LSTR ( intext, ntext, ier)
				    CALL ST_UTAB ( intext, ntext,
     +						   intext, ier )
C
C*				    Write out TEXT file.
C
				    CALL GTEXT ('V', x, y, intext,
     +						0., 0, i, ierr )
				    i = i - 3
				ENDIF  
			    END IF
			END DO
		    END IF
		    CALL FL_CLOS ( lun, ier )
		END IF
	    END IF
	END IF
C
C*      Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr, jrrotn, 
     +		      jjust, ier )
C
	RETURN
C*
	END
