	SUBROUTINE SNHXY  ( xaxis, yaxis, u, v,  npts,  xstrt, xstop, 
     +			    xlbl, nxlbl,  ystrt, ystop, ylbl,  nylbl, 
     +                      ilbfrq, iglfrq, itmfrq, iret )
C************************************************************************
C* SNHXY								*
C*									*
C* This subroutine defines the x and y axes to be drawn in SNHODO.	*
C*									*
C* SNHXY  ( XAXIS, YAXIS, U, V, NPTS,  XSTRT, XSTOP, XLBL, NXLBL, 	*
C*          YSTRT, YSTOP,  YLBL, NYLBL, ILBFRQ, IGLFRQ, ITMFRQ, IRET )	*
C*									*
C* Input parameters:							*
C*	XAXIS		CHAR*		XAXIS input			*
C*	YAXIS		CHAR*		YAXIS input			*
C*	U  (NPTS)	REAL		U wind				*
C*	V  (NPTS)	REAL		V wind				*
C*	NPTS		INTEGER		Number of data points		*
C*									*
C* Output parameters:							*
C*	XSTRT		REAL		Left x				*
C*	XSTOP		REAL		Right x				*
C*	XLBL  (NXLBL)	REAL		X labels			*
C*	NXLBL		INTEGER		Number of x labels		*
C*	YSTRT		REAL		Bottom y			*
C*	YSTOP		REAL		Top y				*
C*	YLBL  (NYLBL)	REAL		Y labels			*
C*	NYLBL		INTEGER		Number of y labels		*
C* 	ILBFRQ (2)	INTEGER		Label frequency for x, y axes	*
C*	IGLFRQ (2)	INTEGER		Grid line frequency for axes	*
C* 	ITMFRQ (2)	INTEGER		Tick frequency for axes		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = error specifing x axis	*
C*					 -5 = error specifing y axis		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 8/89	Adapted from SNPXAX			*
C* S. Schotz/GSC	 7/90   Changes for call to IN_AXIS		*
C* K. Brill/NMC 	10/90   Prevent nonzero origin			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	xaxis, yaxis
	REAL		u (*), v (*), xlbl (*), ylbl (*)
	INTEGER		ilbfrq (*), iglfrq (*), itmfrq (*)
C----------------------------------------------------------------------
	iret = 0
C
C*      Get minimum and maximum u wind.
C
	CALL GR_STAT  ( u, 1, npts, 1, 1, 1, npts, umin, 
     +			umax, uavg, udev, ier )
C
C* 	Set umin and umax to prevent nonzero origin.
C
 	IF ( umin .gt. 0 ) umin = 0.0
 	IF ( umax .lt. 0 ) umax = 0.0
C
C*	Get values from XAXIS
C
	CALL IN_AXIS ( xaxis, 0, .false., ' ', umin, umax, 1,
     +                 0, 1, xstrt, xstop, xlbl, nxlbl, ilbfrq(1),
     +                 iglfrq(1), itmfrq(1), ier )
	IF  ( ier .ne. 0 ) THEN
	    iret = -4
	    RETURN
	END IF
C
C*      Get minimum and maximum v wind.
C
	CALL GR_STAT  ( v, 1, npts, 1, 1, 1, npts, vmin, 
     +			vmax, vavg, vdev, ier )
C
C*	Set vmin and vmax to prevent nonzero origin.
C
     	IF ( vmin .gt. 0 ) vmin = 0.0
  	IF ( vmax .lt. 0 ) vmax = 0.0
C
C*	Get values from YAXIS
C
	CALL IN_AXIS ( yaxis, 0, .false., ' ', vmin, vmax, 1,
     +                 0, 1, ystrt, ystop, ylbl, nylbl, ilbfrq(2), 
     +                 iglfrq(2), itmfrq(2), ier )
	IF  ( ier .ne. 0 ) THEN
	    iret = -5
	END IF
C*
	RETURN
	END
