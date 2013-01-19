	SUBROUTINE SNPBCK  ( ibordr, xstrt, xstop, ystrt, ystop,
     +			     xlbl, nxlbl, ylbl, nylbl, 
     +                       ixlbfr, ixglfr, ixtmfr, iylbfr,iyglfr,
     +                       iytmfr, iret )
C************************************************************************
C* SNPBCK								*
C*									*
C* This subroutine draws the background line for a thermodynamic	*
C* chart.								*
C*									*
C* SNPBCK  ( IBORDR, XSTRT, XSTOP, YSTRT, YSTOP, XLBL, NXLBL, YLBL,	*
C*           NYLBL, IXLBFR, IXGLFR, IXTMFR, IYLBFR, IYGLFR,		*
C*           IYTMFR, IRET )						*
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
C*      IXLBFR          INTEGER         X axis label frequency		*
C*      IXGLFR          INTEGER         X axis grid line frequency	*
C*      IXTMFR          INTEGER         X axis tick mark frequency	*
C*      IYLBFR          INTEGER         Y axis label frequency		*
C*      IYGLFR          INTEGER         Y axis grid line frequency	*
C*      IYTMFR          INTEGER         Y axis tick mark frequency      *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          5/90   Changes for IN_AXIS (ixlbfr...)		*
C* K. Brill/NMC         10/90   Change NDEC to -1 in GDAXIS		*
C************************************************************************
	INTEGER		ibordr (*)
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
	CALL GDAXIS  ( 1, ystrt, .true., ixlbfr, ixtmfr, ixglfr,
     +		       -1, nxlbl, xlbl, ier )
	CALL GDAXIS  ( 2, xstrt, .true., iylbfr, iytmfr, iyglfr,
     +		       -1, nylbl, ylbl, ier )
	CALL GDAXIS  ( 3, ystop, .true.,   0,   0,   0, 0, 0,
     +		       ylbl, ier )
	CALL GDAXIS  ( 4, xstop, .true.,   0,   0,   0, 0, 0, 
     +		       xlbl, ier )
C
C*	Restore original line type.
C
	CALL GSLINE  ( ioltyp, 0, iwid, 0, ier )
C*
	RETURN
	END
