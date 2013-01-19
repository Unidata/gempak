	SUBROUTINE DARC ( iwndw, x, y, xrad, yrad, np, ang1, ang2, 
     +                    endpts, iret )
C************************************************************************
C* DARC									*
C*									*
C* This subroutine will draw an arc with points xrad and yrad on the  	*
C* circumference between angles ang1 and ang2.    			*
C*									*
C* DARC  ( IWNDW, X, Y, XRAD, YRAD, NP, ANG1, ANG2, ENDPTS, IRET )	*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER	 	Clipping window 		*
C*	X		REAL		X coordinate 			*
C*	Y		REAL		Y coordinate 			*
C*	XRAD            REAL		X circumference radius coord. 	*
C*	YRAD            REAL		Y circumference radius coord. 	*
C*	NP		INTEGER		Number of points		*
C*	ANG1		REAL		Starting angle			*
C*	ANG2		REAL		Ending angle			*
C*									*
C* Output parameters:							*
C*	ENDPTS (4)	REAL		Beginning/Ending arc points     *
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC		 6/00		Modified from DCIRCL		*
C************************************************************************
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'ERROR.PRM'
C*
	REAL		x, y, xrad, yrad, rad, ang1, ang2, endpts(4)
	INTEGER		iwndw, ix, iy, np 
        LOGICAL         flgfil, flgcls
C------------------------------------------------------------------------
	iret = NORMAL
        flgfil = .false.
        flgcls = .false.
C
C*	Checking for VG driver.
C
	IF ( ddev .eq. 'VG' ) THEN
	    RETURN
	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP  ( iwndw, ier )
	rad = SQRT ( (x-xrad)**2 + (y-yrad)**2 )
C
C*      Draw arc using software commands.
C
	ix = NINT (x)
	iy = NINT (y)
	CALL IARC   ( ix, iy, rad, rad, np, ang1, ang2, endpts,
     +                flgfil, flgcls, iret )
C*
	RETURN
	END
