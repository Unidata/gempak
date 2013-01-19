	SUBROUTINE HCIRC  ( xcen, ycen, xrad, yrad, np, iret )
C************************************************************************
C* HCIRC - XW								*
C*									*
C* This subroutine draws circles on a graphics device.			*
C*									*
C* HCIRC  ( XCEN, YCEN, XRAD, YRAD, NP, IRET )                          *
C*									*
C* Input parameters:							*
C*	XCEN 		REAL		X center coordinate		*
C*	YCEN		REAL		Y center coordinate		*
C*	XRAD 		REAL		X radius point                  *
C*	YRAD 		REAL		Y radius point                  *
C*	NP		INTEGER		Number of points on the circle  *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC		11/98						*
C************************************************************************
	REAL  xcen, ycen, xrad, yrad
C------------------------------------------------------------------------
C*	Draw the circle.
C
	CALL XCIRC ( xcen, ycen, xrad, yrad, iret )
C*
	RETURN
	END
