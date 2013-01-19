	SUBROUTINE DLOGO ( iwndw, x, y, size, iclmod, ilogo, iret )
C************************************************************************
C* DLOGO								*
C*									*
C* This subroutine draws a speciffied emblem such as the NOAA seagull.	*
C*									*
C* DLOGO ( IWNDW, X, Y, SIZE, ICLMOD, ILOGO, IRET )			*
C*									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	X		REAL		X coordinate in device units	*
C*	Y		REAL		Y coordinate in device units	*
C*	SIZE		REAL		Emblem size			*
C*	ICLMOD		INTEGER		Emblem color mode		*
C*						'1' = monochrome	*
C*						'2' = color		*
C*	ILOGO		INTEGER		Emblem ID			*
C*					  '1' = NOAA			*
C*					  '2' = NWS			*
C*					  '3' = NOAA w/o text		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 7/97	Original				*
C* S. Jacobs/NCEP	 8/97	Added check for UTF driver		*
C* S. Jacobs/NCEP	 9/97	Added call to DSFILL to set solid fill	*
C* S. Jacobs/NCEP	11/97	Fixed calling sequence for DSTEXT	*
C* S. Jacobs/NCEP	 3/98	Changed value of solid fill in DSFILL	*
C* S. Jacobs/NCEP	 7/98	Changed ttxsz to txsize			*
C* A. Hardy/GSC          9/98   Added check for RBK driver              *
C* A. Hardy/GSC		 5/00	Add emblem color mode; changed tsiz     *
C*				.015->.012;dec. chrs no. by 1 in DCRVTXT*
C* J. Wu/GSC		 3/01   Added emblem ID & moved actual drawing  *
C*				to I-level function (INOAA)		*
C* S. Jacobs/NCEP	 4/01	Added NWS logo				*
C* S. Jacobs/NCEP	 4/10	Add option for no text on NOAA logo	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DVWNDW.CMN'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the driver is VG, then send the points directly to the device.
C
	IF  ( ( ddev .eq. 'VG' ) .or. ( ddev .eq. 'UTF' ) .or.
     +        ( ddev .eq. 'RBK' ) ) THEN
	    CALL HLOGO ( x, y, size, iclmod, ilogo, iret )
	    RETURN
	END IF
C
C*	Draw the specified logo.
C
	IF ( ilogo .eq. 1 ) THEN
     	    CALL INOAA ( iwndw, x, y, size, iclmod, 1, iret )
	  ELSE IF ( ilogo .eq. 3 ) THEN
     	    CALL INOAA ( iwndw, x, y, size, iclmod, 2, iret )
	  ELSE IF ( ilogo .eq. 2 ) THEN
     	    CALL INWS  ( iwndw, x, y, size, iclmod, iret )
	END IF
C*
	RETURN
	END
