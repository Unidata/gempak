      SUBROUTINE DROAM  ( ityp, x, y, iret )
C************************************************************************
C* DROAM								*
C* 									*
C* This subroutine roams the current window to the specified position   *
C* in any coordinate system except 'S'. The base point of the roam can  *
C* be upper left of the screen or the center of the screen.             *
C* 									*
C* DROAM  ( ITYP, X, Y, IRET )						*
C* 									*
C* Input parameters:                                                    *
C*      ITYP            INTEGER         The base point of roam          *
C*                                        0 = upper left screen corner  *
C*                                        1 = center of the screen      *
C*                                        2 = delta_x, delta_y      	*
C*      X          REAL         Upper left window x coordinate (device) *
C*      Y          REAL         Upper left window y coordinate (device) *
C*                                                                      *
C* Output parameters:							*
C* 	IRET		INTEGER 	Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI	 	6/97						*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call driver
C
	CALL HROAM  ( ityp, x, y, iret )
C*
	RETURN
	END
