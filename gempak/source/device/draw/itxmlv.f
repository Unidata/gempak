	SUBROUTINE ITXMLV ( iwndw, ijust, ixoff, iyoff,
     +			    rotat, x, y, cchar, lens, iret )
C************************************************************************
C* ITXMLV								*
C*									*
C* This subroutine plots the Mid-level text box. The box contains the 	*
C* cloud coverage, cloud types, turbulence symbol and layer, icing	*
C* symbol and layer, and convection information.			*
C*									*
C* The information is encoded in the text string according to the	*
C* following format:							*
C*									*
C*	Cld types | Cld cover | Icing symbol | Icing levels |		*
C*		Turbulence symbol | Turbulence levels |			*
C*		Convection types | Convection levels			*
C*									*
C* The cloud types, cloud cover and convection types are lists of	*
C* values separated by semicolons. The icing and turbulence symbols	*
C* are the symbol code numbers for drawing the symbol. The levels	*
C* for icing, turbulence and convection are each a pair of numbers,	*
C* separated by a slash, representing the top and bottom flight levels	*
C* affected by the phenonmenon. If either level is missing, it will	*
C* be replaced with "XXX".						*
C*									*
C* ITXMLV ( IWNDW, ITYPE, ISYM, IJUST, IXOFF, IYOFF, ROTAT, X, Y,	*
C*	   CCHAR, LENS, IRET ) 						*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
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
C* S. Jacobs/NCEP	 2/03	Created					*
C* S. Jacobs/NCEP	 3/03	Changed spacing when no convection	*
C* J. Wu/SAIC		 7/03	Do not draw turb/icing if not forecasted*
C* J. Wu/SAIC		 8/03	Adjust x offset for drawing turbs 	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
C*
	CHARACTER*(*)	cchar 
C*
	CHARACTER	carr(8)*48, cldtyp(10)*4, cldcvr(10)*4,
     +			cicelv(2)*4, ctrblv(2)*4, cnvtyp(10)*4,
     +			ccnvlv(2)*4
C
	CHARACTER	outstr*400
C------------------------------------------------------------------------
	iret   = NORMAL
C
C*	Parse the string into the components of the Mid-level text box.
C
	CALL ST_CLST ( cchar, '|', ' ', 8, carr, num, ier )
C
C*	Get the cloud types from part 1.
C
	CALL ST_CLST ( carr(1), ';', ' ', 10, cldtyp, ncld, ier )
C
C*	Get the cloud coverage from part 2.
C
	CALL ST_CLST ( carr(2), ';', ' ', 10, cldcvr, ncvr, ier )
C
C*	Get the icing symbol number from part 3.
C
	CALL ST_CRNM ( carr(3), symice, ier )
	IF  ( symice .ge. 0 )  THEN
C
C*	    Get the icing levels from part 4.
C
	    CALL ST_CLST ( carr(4), '/', 'XXX', 2, cicelv, nice, ier )
	    IF  ( nice .eq. 0 )  THEN
		cicelv(1) = 'XXX'
		cicelv(2) = 'XXX'
	    END IF
	  ELSE
	    cicelv(1) = ' '
	    cicelv(2) = ' '
	END IF
C
C*	Get the turbulence symbol number from part 5.
C
	CALL ST_CRNM ( carr(5), symtrb, ier )
	IF  ( symtrb .ge. 0 )  THEN
C
C*	    Get the turbulence levels from part 6.
C
	    CALL ST_CLST ( carr(6), '/', 'XXX', 2, ctrblv, ntrb, ier )
	    IF  ( ntrb .eq. 0 )  THEN
		ctrblv(1) = 'XXX'
		ctrblv(2) = 'XXX'
	    END IF
	  ELSE
	    ctrblv(1) = ' '
	    ctrblv(2) = ' '
	END IF
C
C*	Get the convection type information from part 7.
C
	CALL ST_CLST ( carr(7), ';', ' ', 10, cnvtyp, ncnv, ier )
	IF  ( ncnv .gt. 0 )  THEN
C
C*	    Get the convection levels from part 8.
C
	    CALL ST_CLST ( carr(8), '/', 'XXX', 2, ccnvlv, nclv, ier )
	    IF  ( nclv .eq. 0 )  THEN
		ccnvlv(1) = 'XXX'
		ccnvlv(2) = 'XXX'
	    END IF
	  ELSE
	    ccnvlv(1) = ' '
	    ccnvlv(2) = ' '
	END IF
C
C*	Check for no input.
C
	IF  ( ( ncld .eq. 0 ) .and.
     +	      ( ncvr .eq. 0 ) .and.
     +	      ( symice .lt. 0 ) .and.
     +	      ( symtrb .lt. 0 ) .and.
     +	      ( ncnv .eq. 0 ) )  THEN
     	    RETURN
	END IF
C
C*	Save the current drawing attributes.
C
	jbrdr = mbrdr
	jjust = mjust
	sztrb = ttursz
	itwid = mtuwid
	szice = tcersz
	iiwid = mcewid
C
C*	Set the number of characters per group differently if
C*	there is no convection information.
C
	IF  ( ncnv .eq. 0 )  THEN
	    ilen = 3
	  ELSE
	    ilen = 4
	END IF
C
C*	Set specific drawing attributes.
C
	ijst  = 1
	ibdr  = 221
	CALL DSTEXT ( 0, 0, 0, 0, ibdr, 0, ijst,
     +		      jf, jh, asz, jw, jb, jr, jj, ier )
C
	CALL DSTURB ( asz, jw, tsz, kw, ier )
C
	CALL DSICNG ( asz, jw, csz, kw, ier )
C
C*	Construct the output string formatted into columns.
C
	outstr = ' '
	ipos = 1
	iloc = 1
C
C*	Add the cloud cover to the output string.
C
	DO  i = 1, ncvr
	    outstr(ipos:ipos+(ilen-1)) = cldcvr(i)
	    IF  ( MOD ( iloc, 2 ) .eq. 0 )  THEN
		outstr(ipos+ilen:ipos+ilen) = CHCR
	    END IF
	    ipos = ipos + (ilen + 1)
	    iloc = iloc + 1
	END DO
C
C*	Add the cloud types to the output string.
C
	DO  i = 1, ncld
	    outstr(ipos:ipos+(ilen-1)) = cldtyp(i)
	    IF  ( MOD ( iloc, 2 ) .eq. 0 )  THEN
		outstr(ipos+ilen:ipos+ilen) = CHCR
	    END IF
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	END DO
C
C*	Add a carriage return if there are an odd number of
C*	cloud strings.
C
	IF  ( MOD ( (ncvr+ncld), 2 ) .eq. 1 )  THEN
	    IF  ( MOD ( iloc, 2 ) .eq. 0 )  THEN
		outstr(ipos+ilen:ipos+ilen) = CHCR
	    END IF
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	END IF
C
C*	Add the turbulence levels to the output string.
C
	ilctrb = 0
	IF  ( symtrb .ge. 0 .and. ntrb .gt. 0 )  THEN
	    ilctrb = iloc
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	    outstr(ipos:ipos+(ilen-1)) = ctrblv(1)
	    outstr(ipos+ilen:ipos+ilen) = CHCR
C
	    ipos = ipos + ((ilen+1)*2)
	    iloc = iloc + 2
	    outstr(ipos:ipos+(ilen-1)) = ctrblv(2)
	    outstr(ipos+ilen:ipos+ilen) = CHCR
C
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	END IF
C
C*	Add the icing levels to the output string.
C
	ilcice = 0
	IF  ( symice .ge. 0 .and. nice .gt. 0 )  THEN
	    ilcice = iloc
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	    outstr(ipos:ipos+(ilen-1)) = cicelv(1)
	    outstr(ipos+ilen:ipos+ilen) = CHCR
C
	    ipos = ipos + ((ilen+1)*2)
	    iloc = iloc + 2
	    outstr(ipos:ipos+(ilen-1)) = cicelv(2)
	    outstr(ipos+ilen:ipos+ilen) = CHCR
C
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	END IF
C
C*	Add the convection type information to the output string.
C
	DO  i = 1, ncnv
	    outstr(ipos:ipos+(ilen-1)) = cnvtyp(i)
	    IF  ( MOD ( iloc, 2 ) .eq. 0 )  THEN
		outstr(ipos+ilen:ipos+ilen) = CHCR
	    END IF
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	END DO
C
C*	Add a carriage return if there are an odd number of
C*	convection type strings.
C
	IF  ( MOD ( ncnv, 2 ) .eq. 1 )  THEN
	    IF  ( MOD ( iloc, 2 ) .eq. 0 )  THEN
		outstr(ipos+ilen:ipos+ilen) = CHCR
	    END IF
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	END IF
C
C*	Add the convection levels to the output string.
C
	IF  ( ncnv .gt. 0 )  THEN
	    outstr(ipos:ipos+(ilen-1)) = 'CB  '
C
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	    outstr(ipos:ipos+(ilen-1)) = ccnvlv(1)
	    outstr(ipos+ilen:ipos+ilen) = CHCR
C
	    ipos = ipos + ((ilen+1)*2)
	    iloc = iloc + 2
	    outstr(ipos:ipos+(ilen-1)) = ccnvlv(2)
	    outstr(ipos+ilen:ipos+ilen) = CHCR
C
	    ipos = ipos + (ilen+1)
	    iloc = iloc + 1
	END IF
C
	ilctot = iloc
C
C*	Get the final length of the output string.
C*	If the last character is a carriage return, remove it.
C
	CALL ST_LSTR ( outstr, lent, ier )
	IF  ( outstr(lent:lent) .eq. CHCR )  THEN
	    outstr(lent:lent) = ' '
	    lent = lent - 1
	END IF
C
C*	Set the X offset based on the justification requested.
C
	IF  ( ijust .eq. -1 )  THEN
	    kxoff = ixoff
	  ELSE IF ( ijust .eq. 0 )  THEN
	    kxoff = ixoff - (ilen*2)
	  ELSE IF ( ijust .eq. 1 )  THEN
	    kxoff = ixoff - (ilen*4)
	END IF
C
C*	Do not allow rotation for the text box.
C
	rotn = 0.0
C
C*	Draw the text.
C
	CALL DTEXT  ( iwndw, x, y, outstr, lent, rotn,
     +		      kxoff, iyoff, ier )
C
C*	Draw the turbulence and icing symbols.
C
	IF  ( symtrb .ge. 0 .and. ntrb .gt. 0 )  THEN
	    IF ( symtrb .gt. 10 ) THEN
	        jxoff = kxoff + 3
	      ELSE 
	        jxoff = kxoff + 2	         
	    END IF
	    jyoff = iyoff + ( ( (ilctot/2) + 1) - ilctrb ) - 2
	    CALL DTURB ( iwndw, 1, symtrb, x, y, jxoff, jyoff, ier )
	END IF
C
	IF  ( symice .ge. 0 .and. nice .gt. 0  )  THEN
	    jxoff = kxoff + 2
	    jyoff = iyoff + ( ( (ilctot/2) + 1) - ilcice ) - 2
	    CALL DICNG ( iwndw, 1, symice, x, y, jxoff, jyoff, ier )
	END IF
C
C*	Reset the drawing attributes.
C
	CALL DSTEXT ( 0, 0, 0, 0, jbrdr, 0, jjust,
     +		      jf, jh, asz, jw, jb, jr, jj, ier )
C
	CALL DSTURB ( sztrb, itwid, tsz, kw, ier )
C
	CALL DSICNG ( szice, iiwid, csz, kw, ier )
C*
	RETURN
	END
