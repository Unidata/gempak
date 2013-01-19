	SUBROUTINE DTXSY ( iwndw, itype, isym, ijust, ixoff, iyoff,
     +			   rotn, x, y, cchar, lens, iret )
C************************************************************************
C* DTXSY								*
C*									*
C* This subroutine plots a special text and symbol to any coordinate    *
C* system.  The special text is centered on the given reference point   *
C* (X,Y).  The text is drawn using attributes defined by GSTEXT, and    *
C* the surrounding box is drawn using attributes defined by GSLINE.     *
C* Depending upon special text type, the box may be filled.             *
C*									*
C* DTXSY ( IWNDW, ITYPE, ISYM, IJUST, IXOFF, IYOFF, ROTN, X, Y, CCHAR,  *
C*			LENS, IRET ) 					*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*      ITYPE           INTEGER         Special Text type               *
C*                                          1 = low pressure box        *
C*                                          2 = high pressure box       *
C*                                          3 = pressure box            *
C*                                                  bounded, unfilled   *
C*                                          4 = pressure box            *
C*                                                  bounded, filled     *
C*                                          5 = pressure box,           *
C*                                                  unbounded, filled   *
C*                                          6 = freezing level symbol   *
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
C*					    One or two digits for       *
C*					    the turbulence symbol(s).  	*
C*	IJUST		INTEGER		Justification (-1, 0, 1) 	*
C*	IXOFF		INTEGER		X Offset			*
C*	IYOFF		INTEGER		Y Offset			*
C*	ROTN		REAL   		Rotation			*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	CCHAR		CHAR*		Text string to plot		*
C*	LENS		INTEGER		Length of string		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	 4/97	Initial coding -- modified from DTEXTC  *
C* E. Safford/GSC	 6/97	Completed coding of text types 6 - 9    *
C*					and added itxsy (draw) level	*
C* E. Safford/GSC	 6/97	Modified call to vg driver              *
C* E. Safford/GSC	 7/97   Added ijust, ixoff, iyoff, rotn		*
C* S. Jacobs/NCEP	 7/98	Changed HRTXSY to HTXSY			*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C* S. Jacobs/NCEP	 1/99	Added type 11 for underline w/ fill box	*
C* M. Li/SAIC		11/01	Added type 12 for icing			*
C* T. Lee/SAIC		 8/02	Added overline to prolog		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	cchar 
C------------------------------------------------------------------------
	iret  = NORMAL
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HTXSY ( itype, isym, ijust, ixoff, iyoff, rotn, x, y, 
     +			 cchar(:lens), lens, iret)
	    RETURN
	END IF
C
C*	Save line type and line width.
C
	IF  ( ( mltyp .ne. 1 ) .or. ( mlwid .ne. mtxwid ) ) THEN
	    jltyp = mltyp
	    jlwid = mlwid
	    CALL DSLINE ( 1, 0, mtxwid, 0, i1, i2, i3, i4, ier )
	  ELSE
	    jltyp = 0
	    jlwid = 0
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
	size = txsize * bscalc
C
C*	Draw text if size is greater than 0.
C
	IF  ( size .gt. 0. ) THEN
	    CALL ITXSY ( iwndw, itype, isym, ijust, ixoff, iyoff, rotn, 
     +			 x, y, cchar(:lens), lens, iret )
	END IF
C
C*	Restore line type and line width.
C
	IF  ( ( jltyp .ne. 0 ) .or. ( jlwid .ne. 0 ) )
     +	    CALL DSLINE ( jltyp, 0, jlwid, 0, i1, i2, i3, i4, ier )
C*
	RETURN
	END
