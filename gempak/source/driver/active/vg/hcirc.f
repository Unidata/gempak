	SUBROUTINE HCIRC  ( x, y, xrad, yrad, np, iret )
C************************************************************************
C* HCIRC - VG                                                           *
C*                                                                      *
C* This subroutine draws circles to the device.                         *
C*                                                                      *
C* HCIRC ( X, Y, XRAD, YRAD, NP, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*	X               REAL            X coordinate                    *
C*      Y               REAL            Y coordinate                    *
C*      XRAD            REAL            X circumference coord.   	*
C*      YRAD            REAL            Y circumference coord.   	*
C*	NP		INTEGER		Number of points on the circle  *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/GSC		11/98		Created				*
C************************************************************************
        INCLUDE         'ERROR.PRM'
        INCLUDE         'DEVCHR.CMN'
        INCLUDE         'DVWNDW.CMN'
C*
        REAL            x, y, xrad, yrad 
C------------------------------------------------------------------------
        iret = NORMAL
C
        CALL VCIRC ( x, y, xrad, yrad, np, iret )
C*
        RETURN
        END
