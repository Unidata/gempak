	SUBROUTINE ITXSY ( iwndw, itype, isym, ijust, ixoff, iyoff,
     +			   rotat, x, y, cchar, lens, iret )
C************************************************************************
C* ITXSY								*
C*									*
C* This subroutine plots a special text and symbol to any coordinate    *
C* system.  The special text is centered on the given reference point   *
C* (X,Y).  The text is drawn using attributes defined by GSTEXT, and    *
C* the surrounding box is drawn using attributes defined by GSLINE.     *
C* Depending upon special text type, the box may be filled with the	*
C* background color.							*
C*									*
C* ITXSY ( IWNDW, ITYPE, ISYM, IJUST, IXOFF, IYOFF, ROTAT, X, Y,	*
C*	   CCHAR, LENS, IRET ) 						*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*      ITYPE           INTEGER         Special Text type               *
C*                                          1 = low pressure box        *
C*                                          2 = high pressure box       *
C*                                          3 = bounded text		*
C*                                          4 = filled, bounded text	*
C*                                          5 = filled, unbounded text	*
C*                                          6 = freezing level box	*
C*                                          7 = turbulence symbol       *
C*					    8 = cloud level		*
C*					    9 = high level turbulence   *
C*					   10 = underline		*
C*					   11 = underline, fill box	*
C*					   12 = midlevel icing		*
C*					   13 = overline		*
C*					   14 = overline, fill box	*
C*					   15 = "Big Box" for mid-level	*
C*	ISYM		INTEGER		Symbol number			*
C*	IJUST		INTEGER		Justification (-1, 0, 1)	*
C*	IXOFF		INTEGER		X Offset			*
C*	IYOFF		INTEGER		Y Offset			*
C*	ROTAT		REAL   		Rotation			*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	CCHAR		CHAR*		Text string to plot		*
C*	LENS		INTEGER		Length of string		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	 6/97	Initial coding -- modified from DTXSY   *
C* E. Safford/GSC	 6/97	Fixed problem with XXX in type 7 & 8    *
C* S. Jacobs/NCEP	 7/97	Removed color setting for text & lines;	*
C*				Set fill color to the background only	*
C* E. Safford/GSC	 7/97   Added ijust, ixoff, iyoff, rotat	*
C* S. Jacobs/NCEP	 7/97	Recoded to use offsets and rotation	*
C* S. Jacobs/NCEP	 7/97	Added use of text justification value	*
C* S. Jacobs/NCEP	 7/97	Fixed a bug when plotting type 7	*
C* S. Jacobs/NCEP	12/97	Removed call to DTEXTC			*
C* J. Whistler/AWC	 1/98	Reduced the space around the boxed text	*
C* S. Jacobs/NCEP	 4/98	Changed to use text attr for box/fill	*
C* S. Danz/AWC		 6/98	Fixed to not rotate the line for type 9	*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C* S. Jacobs/NCEP	 7/98	Fixed justification for turbulence syms	*
C* S. Jacobs/NCEP	 8/98	Added size change for turb syms		*
C* S. Jacobs/NCEP	 9/98	Added check to not rotate line for hw tx*
C* S. Jacobs/NCEP	 1/99	Added type 11 - underline with fill	*
C* M. Li/SAIC		10/01	Added type 12 - midlevel icing		*
C* T. Lee/SAIc		 8/02	Added type 13,14 - overline & with fill	*
C* S. Jacobs/NCEP	 2/03	Added type 15 - midlevel big box	*
C* L. Hinson/AWC        12/06   Added type 16 - GFA text label          *
C* L. Hinson/AWC        07/09   Added type 17 - CCF text label          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
	CHARACTER*(*)	cchar 
C*
	INTEGER		jxoff(6), jyoff(6),
     +			jxo(6),   jyo(6),
     +			jxc(6),   jyc(6)
	CHARACTER	tchar*160
C------------------------------------------------------------------------
	iret   = NORMAL
C
C*	Save the text border setting.
C
	jbrdr = mbrdr
C
C*	Adjust the character string for the type of symbol.
C
	IF  ( itype .eq. 1 )  THEN
	    ibrdr = 222
	    tchar = cchar
C
	  ELSE IF  ( itype .eq. 2 )  THEN
	    ibrdr = 223
	    tchar = cchar
C
	  ELSE IF  ( itype .eq. 3 )  THEN
	    ibrdr = 211
	    tchar = cchar
C
	  ELSE IF  ( itype .eq. 4 )  THEN
	    ibrdr = 221
	    tchar = cchar
C
	  ELSE IF  ( itype .eq. 5 )  THEN
	    ibrdr = 121
	    tchar = cchar
C
	  ELSE IF  ( itype .eq. 6 )  THEN
	    ibrdr = 214
	    tchar = cchar
C
	  ELSE IF  ( itype .eq. 7 )  THEN
C
	    ibrdr = 111
	    ipos = INDEX ( cchar, '/' )
	    IF  ( ipos .gt. 0 )  THEN
		tchar = cchar
	      ELSE
		tchar = cchar(1:lens) // '/'
	    END IF
C
	  ELSE IF  ( ( itype .eq. 8 ) .or.
     +		     ( itype .eq. 9 ) .or. 
     +	             ( itype. eq. 12) )  THEN
C
	    ibrdr = 111
	    ipos = INDEX ( cchar, '/' )
	    IF  ( ( ipos .eq. 0 ) .or. ( ipos .eq. lens ) )  THEN
		IF  ( ipos .eq. 0 )  THEN
		    tchar = cchar(1:lens) // CHCR // 'XXX'
		  ELSE
		    tchar = cchar(1:lens-1) // CHCR // 'XXX'
		END IF
	      ELSE IF  ( ipos .eq. 1 )  THEN
		tchar = 'XXX' // CHCR // cchar(2:lens)
	      ELSE
		tchar = cchar(1:ipos-1) // CHCR // cchar(ipos+1:lens)
	    END IF
C
	  ELSE IF  ( itype .eq. 10 )  THEN
	    ibrdr = 215
	    tchar = cchar
C
	  ELSE IF  ( itype .eq. 11 )  THEN
	    ibrdr = 225
	    tchar = cchar
C
	  ELSE IF ( itype .eq. 13 )  THEN
	    ibrdr = 216
	    tchar = cchar
C
	  ELSE IF ( itype .eq. 14 )  THEN
	    ibrdr = 226
	    tchar = cchar
	  ELSE IF ( itype .eq. 15 )  THEN
	    CALL ITXMLV ( iwndw, ijust, ixoff, iyoff, rotat, x, y,
     +			  cchar, lens , ier )
            RETURN
          ELSE IF ( itype .eq. 16 )  THEN
            CALL ITXGFA ( iwndw, ijust, ixoff, iyoff, rotat, x, y,
     +                    cchar, lens, ier )
	    RETURN
          ELSE IF ( itype .eq. 17 ) THEN
            CALL ITXCCF ( iwndw, ijust, ixoff, iyoff, rotat, x, y,
     +                    cchar, lens, ier )
            RETURN
	  ELSE
	    RETURN
	END IF
C
C*	Get the length of the final string.
C
	CALL ST_LSTR ( tchar, lent, ier )
C
C*	Find the length of the longest line and the number of lines.
C
	lines = 0
	mxlen = 0
	nlen  = 0
	DO  i = 1, lent
	    IF  ( ( tchar(i:i) .eq. CHCR ) .or.
     +		  ( tchar(i:i) .eq. CHLF ) )  THEN
		lines = lines + 1
		mxlen = MAX ( mxlen, nlen )
		nlen  = 0
	      ELSE
		nlen = nlen + 1
	    END IF
	END DO
	lines = lines + 1
	mxlen = MAX ( mxlen, nlen )
C
C*	For types 8, 9, and 12 compute the end points for the dividing line
C*	between the lines of text.
C
	IF  ( ( itype .eq. 8 ) .or. ( itype .eq. 9 ) .or. 
     +        ( itype .eq. 12 ) )  THEN
C
C*	    Compute the offsets for the string based on text character
C*	    spacing.
C
	    IF  ( MOD ( ixoff, 2 ) .ne. 0 ) THEN
		ixo = ( ixoff - 1 ) / 2 * 7 + 4
	      ELSE
		ixo = ixoff / 2 * 7
	    END IF
	    IF  ( MOD ( iyoff, 2 ) .ne. 0 ) THEN
		iyo = ( iyoff - 1 ) / 2 * 9 + 5
	      ELSE
		iyo = iyoff / 2 * 9
	    END IF
C
C*	    Compute offsets for the corners of the bounding box based on
C*	    the text character spacing.
C
	    IF  ( ijust .lt. 0 )  THEN
		nleft  = -1
		nright = 4*mxlen + 1
	      ELSE IF  ( ijust .gt. 0 )  THEN
		nleft  = -(4*mxlen + 1)
		nright = 1
	      ELSE
		nleft  = -(2*mxlen + 1)
		nright =   2*mxlen + 1
	    END IF
	    nbot   = -(2*lines + 1)
	    ntop   =   2*lines + 1
C
	    jxoff(1) = nleft
	    jyoff(1) = ntop
	    jxoff(2) = nleft
	    jyoff(2) = nbot
	    jxoff(3) = nright
	    jyoff(3) = nbot
	    jxoff(4) = nright
	    jyoff(4) = ntop
	    jxoff(5) = nleft
	    jyoff(5) = 0
	    jxoff(6) = nright
	    jyoff(6) = 0
	    np = 6
C
	    DO  i = 1, np
		irmx =  MOD ( jxoff(i), 4 )
		IF  ( irmx .ne. 0 )  THEN
		  IF ( irmx .eq. 1 ) THEN
		    jxo(i) = ( jxoff(i) - 1 ) / 4 * 7 + 2
		  ELSE IF ( irmx .eq. -1 ) THEN
		    jxo(i) = ( jxoff(i) - 3 ) / 4 * 7 + 5
		  ELSE IF ( irmx .eq. 2 .or. irmx .eq. -2) THEN
		    jxo(i) = ( jxoff(i) - 2 ) / 4 * 7 + 4
		  ELSE IF ( irmx .eq. 3 ) THEN
		    jxo(i) = ( jxoff(i) - 3 ) / 4 * 7 + 5
		  ELSE IF ( irmx .eq. -3 ) THEN
		    jxo(i) = ( jxoff(i) - 1 ) / 4 * 7 + 2
		  END IF
		ELSE
		    jxo(i) = jxoff(i) / 4 * 7
		END IF
C*
		irmy =  MOD ( jyoff(i), 4 )
		IF  ( irmy .ne. 0 )  THEN
		  IF ( irmy .eq. 1 ) THEN
		    jyo(i) = ( jyoff(i) - 1 ) / 4 * 9 + 2
		  ELSE IF ( irmy .eq. -1 ) THEN
		    jyo(i) = ( jyoff(i) - 3 ) / 4 * 9 + 7
		  ELSE IF ( irmy .eq. 2 .or. irmy .eq. -2 ) THEN
		    jyo(i) = ( jyoff(i) - 1 ) / 4 * 9 + 5
		  ELSE IF ( irmy .eq. 3 ) THEN
		    jyo(i) = ( jyoff(i) - 3 ) / 4 * 9 + 7
		  ELSE IF ( irmy .eq. -3 ) THEN
		    jyo(i) = ( jyoff(i) - 1 ) / 4 * 9 + 2
		  END IF
		ELSE
		    jyo(i) = jyoff(i) / 4 * 9
		END IF
	    END DO
C
C*	    Do not rotate the line for the turbulence or icing symbol.
C
	    IF  ( ( ( itype .eq. 9 ) .or. ( itype .eq. 12 ) ) .or.
     +		  ( mtxhw .eq. 2 ) )  THEN
		rotn = 0.0
	      ELSE
		rotn = rotat
	    END IF
C
C*	    Compute the bounding box corner locations by applying the
C*	    offsets and rotation.
C
	    cosrot = COS ( rotn * DTR )
	    sinrot = SIN ( rotn * DTR )
	    size = txsize * bscalc
	    DO  i = 1, np
		xd     = ( ixo + jxo(i) ) * size
		yd     = ( iyo + jyo(i) ) * size
C
		xprimd = xd * cosrot - yd * sinrot
		yprimd = xd * sinrot + yd * cosrot
C
		jxc(i) = x + ispanx * NINT ( xprimd )
		jyc(i) = y + ispany * NINT ( yprimd )
	    END DO
C
C*	    Fill in the bounding box with the background color.
C
	    mmcolr = mcolr
	    CALL DSCOLR ( 101, imclr, ier )
	    CALL IFILL  ( 4, jxc, jyc, ier )
	    CALL DSCOLR ( mmcolr, imclr, ier )
C
C*	    Draw a line between the two lines.
C
	    CALL ILINE  ( 2, jxc(5), jyc(5), ier )
	END IF
C
C*	Plot the turbulence or icing symbol.
C*	Also, do not rotate the text for the turbulence or icing symbol.
C
	IF  ( ( itype .eq. 7 ) .or. ( itype .eq. 9 ) .or.
     +	      ( itype .eq. 12 ) )  THEN
	    rotn = 0.0
	    rsym = isym
C
C*	    Adjust the offsets for the symbols.
C
	    IF  ( ijust .lt. 0 )  THEN
		ixoff2 = ixoff + mxlen - 1
	      ELSE IF  ( ijust .gt. 0 )  THEN
		ixoff2 = ixoff - mxlen + 1
	      ELSE
		ixoff2 = ixoff
	    END IF
	    iyoff2 = iyoff + lines + 1
C
	    IF  ( ( itype .eq. 7 ) .or. ( itype .eq. 9 ) ) THEN
	        trbsiz = ttursz
	        ttursz = ttxsz
	        CALL DTURB (iwndw, 1, rsym, x, y, ixoff2, iyoff2, ier)
		ttursz = trbsiz
	      ELSE IF ( itype .eq. 12 ) THEN
		ticsiz = tcersz
                tcersz = ttxsz
                CALL DICNG (iwndw, 1, rsym, x, y, ixoff2, iyoff2, ier)
                tcersz = ticsiz
	    END IF
	  ELSE
	    rotn = rotat
	END IF
C
C*	Set the justifcation and plot the text.
C
	ijst  = ijust + 2
	jjust = mjust
	CALL DSTEXT ( 0, 0, 0, 0, ibrdr, 0, ijst,
     +		      jf, jh, asz, jw, jb, jr, jj, ier )
C
	CALL DTEXT  ( iwndw, x, y, tchar, lent, rotn,
     +		      ixoff, iyoff, ier )
C
	CALL DSTEXT ( 0, 0, 0, 0, jbrdr, 0, jjust,
     +		      jf, jh, asz, jw, jb, jr, jj, ier )
C*
	RETURN
	END
