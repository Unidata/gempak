	SUBROUTINE HCIRC  ( ix, iy, rad, np, iret )
C************************************************************************
C* HCIRC - GN                                                           *
C*                                                                      *
C* This subroutine draws circles to the device.                         *
C*                                                                      *
C* HCIRC ( IX, IY, RAD, NP, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*	IX		INTEGER		X latitude/coordinate center pt.*
C*	IY		INTEGER		Y latitude/coordinate center pt.*
C*	RAD		REAL		Radius of the circle            *
C*	NP		INTEGER		Number of points on the circle  *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/GSC		11/98		Created			        *
C************************************************************************
        INCLUDE         'ERROR.PRM'
        INCLUDE         'DEVCHR.CMN'
        INCLUDE         'DVWNDW.CMN'
C*
        REAL            rad 
        INTEGER         ix, iy, np
C------------------------------------------------------------------------
        iret = NORMAL
C*
        RETURN
        END
