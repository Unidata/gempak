	SUBROUTINE DTICMK ( iwndw, itick, size, np, x, y, iret )
C************************************************************************
C* DTICMK								*
C*									*
C* This subroutine draws tic marks on the current graphics device.	*
C*									*
C* DTICMK ( IWNDW, ITICK, SIZE, NP, X, Y, IRET )			*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	ITICK		INTEGER		Tick mark number		*
C*	SIZE		REAL		Tick size			*
C*	NP		INTEGER		Number of tick marks		*
C*	X (NP)		REAL		X coordinates in device units	*
C*	Y (NP)		REAL		Y coordinates in device units	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	 6/89	Added size				*
C* M. Linda/GSC		12/96	Changed X and Y to reals		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	REAL		x (*), y (*)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Save line type and line width.
C
	IF  ( ( mltyp .ne. 1 ) .or. ( mlwid .ne. 1 ) ) THEN
	    jltyp = mltyp
	    jlwid = mlwid
	    CALL DSLINE ( 1, 0, 1, 0, i1, i2, i3, i4, ier )
	  ELSE
	    jltyp = 0
	    jlwid = 0
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Compute tic mark size.
C
	ssize = size * 2 * bscalm
C
C*	Loop through tic marks.
C
	DO  i = 1, np
	    ix = NINT ( x (i) )
	    iy = NINT ( y (i) )
	    CALL ITICMK ( ix, iy, itick, ssize, ier )
	END DO
C
C*	Restore line type and line width.
C
	IF  ( ( jltyp .ne. 0 ) .or. ( jlwid .ne. 0 ) )
     +	    CALL DSLINE ( jltyp, 0, jlwid, 0, i1, i2, i3, i4, ier )
C*
	RETURN
	END
