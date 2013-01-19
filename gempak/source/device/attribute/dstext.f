	SUBROUTINE DSTEXT  ( itxfn, itxhw, sztext, itxwid,
     +			     ibrdr, irrotn, ijust,
     +			     jtxfn, jtxhw, size, jtxwid,
     +			     jbrdr, jrrotn, jjust, iret )
C************************************************************************
C* DSTEXT 								*
C* 									*
C* This subroutine sets the text attributes including the font		*
C* number, the text software/hardware flag, the text size and width,	*
C* the text border and blank fill flag, the text relative rotation	*
C* flag, and the text justification.					*
C*									*
C* The border and blank fill flag is a three digit number, ABC, with	*
C* the following definitions:						*
C*									*
C*	A	Border		1 = no,  2 = yes			*
C*	B	Blank fill	1 = no,  2 = yes			*
C*	C	Border type						*
C*									*
C* DSTEXT  ( ITXFN, ITXHW, SZTEXT, ITXWID, IBRDR, IRROTN, IJUST,	*
C*	     JTXFN, JTXHW, SIZE, JTXWID, JBRDR, JRROTN, JJUST, IRET )	*
C*									*
C* Input parameters:							*
C*	ITXFN		INTEGER		Text font			*
C*					  <=0 = no change		*
C*	ITXHW		INTEGER		Text sw/hw flag			*
C*					  1 = software text		*
C*					  2 = hardware text		*
C*					  otherwise no change		*
C*	SZTEXT		REAL		Text size			*
C*					  <=0 = no change		*
C* 	ITXWID		INTEGER		Text width			*
C*					  <=0 = no change		*
C* 	IBRDR		INTEGER		Text border/blank fill flag	*
C*					  <=0 = no change		*
C*	IRROTN		INTEGER		Text north-relative rot flag	*
C*					  1 = screen relative		*
C*					  2 = north relative		*
C*					  otherwise no change		*
C*	IJUST		INTEGER		Text justification		*
C*					   1 = left			*
C*					   2 = center			*
C*					   3 = right			*
C*					  11 = left no clipping		*
C*					  12 = center no clipping	*
C*					  13 = right no clipping	*
C*					  otherwise no change		*
C*									*
C* Output parameters:							*
C*	JTXFN		INTEGER		Text font			*
C*	JTXHW		INTEGER		Text sw/hw flag			*
C*	SIZE		REAL		Text size			*
C*	JTXWID		INTEGER		Text width			*
C* 	JBRDR		INTEGER		Text border/blank fill flag	*
C*	JRROTN		INTEGER		Text north-relative rot flag	*
C*	JJUST		INTEGER		Text justification		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	11/84	GEMPLT Version 3.0                      *
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added text width			*
C* M. desJardins/NMC	10/94	Always set hw text info			*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver		*
C* S. Jacobs/NCEP	 9/97	Added border, rotation and just flags	*
C* S. Jacobs/NCEP	 9/97	Removed special VG driver check		*
C* S. Jacobs/NCEP	 3/98	Modified font numbers set for SW fonts	*
C* S. Jacobs/NCEP	 7/98	Changed call to HSTEXT for txsize	*
C* S. Jacobs/NCEP	10/07	Added s/w fonts 3 and above		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret   = NORMAL
	jtxfn  = mtxfn
	jtxhw  = mtxhw
	size   = ttxsz
	jtxwid = mtxwid
	jbrdr  = mbrdr
	jrrotn = mrrotn
	jjust  = mjust
C
C*	Set the text font.
C
	IF  ( itxfn .gt. 0 )  jtxfn = itxfn
C
C*	Set the text hardware flag.
C
	IF  ( itxhw .eq. 1 )  THEN
	    jtxhw = 1
C
C*	    Get the font style and number.
C
	    istyl = jtxfn / 10
	    ifont = MOD ( jtxfn, 10 )
C
C*	    Check the font style.
C
	    jstyl = istyl
C
C*	    Check the font number.
C*	    Italic and Bold Italic are not valid for fonts 1, 2 and 3.
C*	    Set Italic to Regular and Bold Italic to Bold.
C
	    IF  ( ifont .eq. 2 )  THEN
		jfont = 2
		IF  ( jstyl .eq. 1 )  jstyl = 0
		IF  ( jstyl .eq. 3 )  jstyl = 2
	      ELSE IF  ( ifont .eq. 3 )  THEN
		jfont = 3
		IF  ( jstyl .eq. 1 )  jstyl = 0
		IF  ( jstyl .eq. 3 )  jstyl = 2
	      ELSE IF  ( ifont .eq. 4 )  THEN
		jfont = 4
	      ELSE
		jfont = 1
		IF  ( jstyl .eq. 1 )  jstyl = 0
		IF  ( jstyl .eq. 3 )  jstyl = 2
	    END IF
C
C*	    Set the software font code.
C
	    jtxfn = jstyl * 10 + jfont
C
	  ELSE IF  ( itxhw .eq. 2 )  THEN
	    IF  ( ntxhw .eq. 1 )  THEN
C
C*		Hardware fonts are supported by the driver.
C
	        jtxhw = 2
	      ELSE
C
C*		The driver does not support hw fonts, so set up
C*		the proper emulation.
C
	        jtxhw = 1
C
C*	    	Get the font style and number.
C
		istyl = jtxfn / 10
		ifont = MOD ( jtxfn, 10 )
C
C*	    	Check the font style.
C
		jstyl = istyl
C
C*	    	Check the font number.
C*	    	Italic and Bold Italic are not valid for fonts 1, 2, 3.
C*	    	Set Italic to Regular and Bold Italic to Bold.
C
		IF  ( ifont .eq. 2 )  THEN
C*		    Emulate the HELVETICA font with sw font 3.
		    jfont = 3
		    IF  ( jstyl .eq. 1 )  jstyl = 0
		    IF  ( jstyl .eq. 3 )  jstyl = 2
		  ELSE IF  ( ifont .eq. 3 )  THEN
C*		    Emulate the TIMES font with sw font 4.
		    jfont = 4
		  ELSE
C*		    Set other hw fonts to sw font 1.
		    jfont = 1
		    IF  ( jstyl .eq. 1 )  jstyl = 0
		    IF  ( jstyl .eq. 3 )  jstyl = 2
		END IF
C
C*	    	Set the software font code.
C*		Apply the appropriate software font number and style.
C
		jtxfn = jstyl * 10 + jfont
	    END IF
	END IF
C
C*	Set text size.
C
	IF  ( sztext .gt. 0. )  size = sztext
C
C*	Set text width.
C
	IF  ( itxwid .gt. 0 )  jtxwid = itxwid
C
C*	Set the border and blank fill flag.
C
	IF  ( ibrdr .gt. 0 )  jbrdr = ibrdr
C
C*	Set the relative rotation flag.
C
	IF  ( irrotn .gt. 0 )  jrrotn = irrotn
C
C*	Set the text justification.
C
	IF  ( ijust .gt. 0 )  jjust = ijust
C
C*	Set characteristics in device driver for hw text.
C
	txsize = size
	IF  ( ( jtxhw .eq. 2 ) .or. ( ddev .eq. 'VG' ) )  THEN
	    CALL HSTEXT  ( jtxfn, jtxhw, size, jtxwid,
     +			   jbrdr, jrrotn, jjust, txtsiz, iret )
	    txsize = txtsiz
	END IF
C
C*	Save values selected.
C
	mtxfn  = jtxfn
	mtxhw  = jtxhw
	ttxsz  = size
	mtxwid = jtxwid
	mbrdr  = jbrdr
	mrrotn = jrrotn
	mjust  = jjust
C*
	RETURN
	END
