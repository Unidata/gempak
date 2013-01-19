	SUBROUTINE SNHBCK  ( ibordr, xstrt, xstop, ystrt, ystop,
     +			     xlbl, nxlbl, ylbl, nylbl, ilbfrq, 
     +                       iglfrq, itmfrq, iret )
C************************************************************************
C* SNHBCK								*
C*									*
C* This subroutine draws the background for a hodograph.		*
C*									*
C* SNHBCK  ( IBORDR, XSTRT, XSTOP, YSTRT, YSTOP, XLBL, NXLBL, YLBL,	*
C*           NYLBL, ILBFRQ, IGLFRQ, ITMFRQ, IRET )			*
C*									*
C* Input parameters:							*
C*	IBORDR (3)	INTEGER		Border color/ type/ width	*
C*	XSTRT		REAL		Left x				*
C*	XSTOP		REAL		Right x				*
C*	YSTRT		REAL		Bottom y			*
C*	YSTOP		REAL		Top y				*
C*	XLBL  (NXLBL)	REAL		X labels			*
C*	NXLBL		INTEGER		Number of x labels		*
C*	YLBL  (NYLBL)	REAL		Y labels			*
C*	NYLBL		INTEGER		Number of y labels		*
C* 	ILBFRQ (2)	INTEGER		Label frequency for x,y axes	*
C*	IGLFRQ (2)	INTEGER		Grid line frequency for axes	*
C*	ITMFRQ (2)	INTEGER		Tick mark frequency for axes	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from SNPBCK			*
C* S. Schotz/GSC	 7/90	Updates for IN_AXIS parameters		*
C* S. Schotz/GSC	10/90	Set ndec = -1 for GDAXIS		*
C************************************************************************
	INTEGER		ibordr (*), ilbfrq (*), iglfrq (*), itmfrq (*)
	REAL		xlbl (*), ylbl (*)
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Check that background is to be drawn.
C
	IF  ( ibordr (1) .le. 0 )  RETURN
C
C*	Get line type. 
C
	CALL GQLINE  ( ioltyp, ilhw, iwid, iwhw, ier )
C
C*	Set line type requested.
C
	CALL GSLINE  ( ibordr (2), 0, ibordr (3), 0, ier )
	CALL GSCOLR  ( ibordr (1), ier )
C
C*	Draw axes.
C
	IF  ( xstop .lt. 0. )  THEN
	    xax = xstop
	  ELSE IF  ( xstrt .gt. 0. )  THEN
	    xax = xstrt
	  ELSE
	    xax = 0.
	END IF
	IF  ( ystop .lt. 0. )  THEN
	    yax = ystop
	  ELSE IF  ( ystrt .gt. 0. )  THEN
	    yax = ystrt
	  ELSE
	    yax = 0.
	END IF
C
	CALL GDAXIS  ( 1, yax, .true., ilbfrq (1), itmfrq (1), 
     +                 iglfrq (1), -1, nxlbl, xlbl, ier )
	CALL GDAXIS  ( 2, xax, .true., ilbfrq (2), itmfrq (2), 
     +                 iglfrq (2), -1, nylbl, ylbl, ier )
C
C*	Restore original line type.
C
	CALL GSLINE  ( ioltyp, 0, iwid, 0, ier )
C*
	RETURN
	END
