	SUBROUTINE HCIRC  ( xcen, ycen, xrad, yrad, iret )
C************************************************************************
C* HCIRC - XWP								*
C*									*
C* This subroutine draws circles on a graphics device.			*
C*									*
C* HCIRC  ( XCEN, YCEN, XRAD, YRAD, IRET )				*
C*									*
C* Input parameters:							*
C*	XCEN 		REAL		X center coordinate		*
C*	YCEN		REAL		Y center coordinate		*
C*	XRAD 		REAL		X radius point                  *
C*	YRAD 		REAL		Y radius point                  *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC		12/98		Modified from HLINE             *
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
C*
	REAL      	 xcen, ycen, xrad, yrad
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check the requested device.
C*	Draw the line.
C
	IF  ( ( ddev .eq. 'XW' ) .or. ( ddev .eq. 'XWP' ) )  THEN
	    CALL XCIRC ( xcen, ycen, xrad, yrad, iret )
	  ELSE IF  ( ddev .eq. 'PS' )  THEN
	    CALL PCIRC (  xcen, ycen, xrad, yrad, iret )
	END IF
C*
	RETURN
	END
