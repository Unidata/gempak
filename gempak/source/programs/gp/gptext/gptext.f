	PROGRAM GPTEXT 
C************************************************************************
C* GPTEXT								*
C*									*
C* This program sets up the graphics device and draws the contents of	*
C* a text file to the device.						*
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
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		clear
	CHARACTER	device*(LLMXLN), panel*(LLMXLN),
     +			text*(LLMXLN),   colors*(LLMXLN),
     +			txtfil*(LLMXLN), txtloc*(LLMXLN),
     +			column*(LLMXLN), utxtfil*(LLMXLN), 
     +			textfl(3)*(LLMXLN)
C*
	CHARACTER	intext*132, txtstr*400
	REAL		rarr (2)
	LOGICAL		respnd, done, proces, compl, logo
C-----------------------------------------------------------------------
C*	Initialize user interface and graphics.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GPTEXT', -1, ' ', ier )
	    CALL SS_EXIT
	END IF
C
C*	Initialize graphics.
C
	CALL GG_INIT  ( 1, iret )
	IF  ( iret .eq. 0 )  THEN
	    done = .false.
	  ELSE 
	    CALL ER_WMSG  ( 'GPTEXT', -3, ' ', ier )
	    done = .true.
	END IF
	CALL IP_IDNT ( 'GPTEXT', ier ) 
C
	DO WHILE  ( .not. done )
C	
	    proces = .true.
	    logo   = .false.
C
C*	    Get input parameters.
C
	    CALL GPTINP  ( device, colors, panel, text, clear, 
     +			   txtfil, txtloc, column, iperr )
	    IF  ( iperr .eq. 0 )  THEN
C
C*		Set graphic device. 
C
		CALL GG_SDEV  ( device, ier )
		IF  ( ier .ne. 0 )  proces = .false.
		IF  ( proces )  THEN
		    CALL IN_TEXT ( text, ier )
C
C*		    Go to the first frame.
C
		    CALL GSTANM ( ier )
C
C*		    Display user options, allow program exit.
C
      		    CALL GPTOPT ( device, txtfil, txtloc, panel, 
     +				  column, clear, ier )
		    IF  ( ier .ne. 0 )  proces = .false.
C
C*		    Open the input file.
C
		    IF  ( proces )  THEN
	    		jpos = INDEX ( txtfil, '|' )
			IF  ( jpos .ne. 0 )  THEN
C
C*			    Parse size of the logo.
C
			    CALL ST_CLST ( txtfil, '|', ' ', 3, 
     +					   textfl, num, ier )
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
			  ELSE IF ( utxtfil .eq. 'NOAANOTX' ) THEN
			    logo  = .true.
			    ilogo = 3
			  ELSE IF ( utxtfil .eq. 'NWS' ) THEN
			    logo  = .true.
			    ilogo = 2
			  ELSE
			    CALL FL_SOPN (textfl(1), lun, iret)	
			    IF  ( iret .ne. 0 )  THEN
				proces = .false.
				CALL ER_WMSG  ( 'GPTEXT', -4, 
     +						textfl(1), ier )
				CALL FL_CLOS  ( lun, ier )
			    END IF
			END IF
		    END IF
C
C*		    Clear the screen, if requested, and set
C*		    the panel.
C
		    IF  ( proces )  THEN
			IF  ( clear ) CALL GCLEAR ( iret )
			CALL GG_PANL ( panel, iret )
C
C*			Check if map location is requested. Set
C*			text location.
C
			ipos = INDEX ( txtloc, '#' )
			IF  ( ipos .eq. 0 )  THEN
			    CALL ST_RLST ( txtloc, ';', 0., 2,
     +						 rarr, num, ier )
			  ELSE
			    CALL ST_RLST ( txtloc ( ipos+1: ), ';', 
     +					   0., 2, rarr, num, ier )
			    CALL GTRANS  ( 'M', 'V', 1, rarr(1), 
     +					   rarr(2), rarr (1), 
     +					   rarr (2), ier )
			    IF ( ier .ne. 0 )  THEN
				proces = .false.
				CALL ER_WMSG ( 'GPTEXT', -5, ' ', ier )
			    END IF
			END IF
		    END IF
C
C*		    Set colors.
C
		    IF  ( proces )  THEN
			CALL IN_COLR ( colors, 1, icol, iret)
			CALL GSCOLR ( icol, ier )
C
C*			Set size and color mode of the logo.
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
C*			    Plot logo.
C
			    CALL GLOGO  ( 'V', rarr(1), rarr(2), 
     +					 size, iclmod, ilogo, ier )
			  ELSE
C
C*			    Find the bounds of the device display 
C*			    area.
C
			    CALL GQSYSZ ( rxszmk, ryszmk, rxsztx, 
     +					  rysztx, rxszwb, ryszwb, iret )
			    CALL GQTEXT ( itxfn, itxhw, sztext, itxwid, 
     +					  ibrdr, irrotn, ijust, iret )
			    CALL GQBND ( 'V', xvl, yvb, xvr, yvt, iret )
C
C*			    Declare the initial conditions.
C
			    IF  ( logo )  THEN
				adj = 0.
			      ELSE
				adj = .01 * sztext
			    END IF
C
C*			    Compute new coordinates from the view area.
C
			    IF  ( ipos .eq. 0 )  THEN
				xp = xvl + ( rarr (1) * ( xvr - xvl ) )
     +					 + adj
				yp = yvb + ( rarr (2) * ( yvt - yvb ) )
     +					 - adj
			      ELSE
				xp = rarr (1)
				yp = rarr (2)
			    END IF
C  
C*			    Plot logo on normal coordinates.
C
			    IF  ( logo )   THEN
				CALL GLOGO ('V', xp, yp, size, 
     +					     iclmod, ilogo, ier)
			      ELSE
C
C*				Set the number of columns.
C
				CALL ST_NUMB ( column, ncol, ier )
				IF ( ncol .le. 0 .or. ipos .ne. 0 ) 
     +				     ncol = 1
C
C*				Get the number of lines in the file.
C
				jostat = 0
				nline  = 0
				DO WHILE  ( jostat .eq. 0 )
				    READ (lun, 1000, IOSTAT = jostat) 
     +					  intext
				    IF (jostat .eq. 0) nline = nline+1
				END DO
				CALL FL_REWD ( lun, ier )
C
				IF  ( ( ncol .eq. 1 ) .and.
     +				      ( nline .le. 10 ) )  THEN
C
C*				    If there is only one column and
C*				    10 lines or less, treat the text
C*				    as a single string. This allows
C*				    the text to be in a single box, etc.
C
				    iostat = 0
				    txtstr = ' '
				    DO  i = 1, nline
					READ (lun, 1000, IOSTAT=iostat) 
     +					     intext
					CALL ST_LSTR ( intext, ntext, 
     +						       ier)
					CALL ST_UTAB ( intext, ntext,
     +						       intext, ier )
					CALL ST_LSTR ( intext, ntext, 
     +						       ier)
					CALL ST_LSTR ( txtstr, nstr, 
     +						       ier)
					IF  ( ntext .gt. 0 )  THEN
					    txtstr(nstr+1:) =
     +						intext(:ntext)
					END IF
					IF  ( i .ne. nline )  THEN
					    m = nstr + 1 + ntext
					    txtstr(m:m) = CHCR
					END IF
				    END DO
C
     				    iyo = (-1) * (nline - 1)
C
				    CALL GTEXT ( 'V', xp, yp, txtstr,
     +						  0., 0, iyo, ierr )
				  ELSE
C
C*				    Read the text file line by one line 
C*				    and switch to a new column at the 
C*				    bottom of the panel.
C
				    i  = 0
				    xb = xvr - adj
				    yb = yp -
     +			                 ((nline/ncol+MOD(nline,ncol)) *
     +			                   rysztx) + .005
				    IF ( yb .lt. yvb+.01) yb = yvb+.01
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
					    IF  ( x .gt. xb )
     +						compl = .true.
					END IF
					IF  ( .not. compl )  THEN
					    READ (lun, 1000,
     +						  IOSTAT=iostat) 
     +					         intext
1000					    FORMAT ( A )
					    IF ( iostat .eq. 0 ) THEN
					      CALL ST_LSTR ( intext,
     +							     ntext, 
     +							     ier)
					      CALL ST_UTAB ( intext,
     +							     ntext,
     +							     intext,
     +							     ier )
C
C*					      Write out TEXT file.
C
					      CALL GTEXT ('V', x, y,
     +							  intext,
     +							  0., 0, i,
     +							  ierr )
					      i = i - 3
					    ENDIF  
					END IF
				    END DO
				END IF
				CALL FL_CLOS ( lun, ier )
			    END IF
			END IF
C
C*			Flush the graphics buffer.
C
			CALL GEPLOT  ( iret )
C
C*			Mark the end of the animation sequence.
C
			CALL GENANM ( iret )
		    END IF
		END IF
	    END IF
C
C*	    Call the dynamic tutor.
C
	    CALL IP_DYNM ( done, iret )
	END DO
C*
	IF  ( iperr .ne. 0 ) CALL ER_WMSG  ( 'GPTEXT', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
