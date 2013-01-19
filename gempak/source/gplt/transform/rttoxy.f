	SUBROUTINE RTTOXY  ( r, theta, x, y, iret )
C************************************************************************
C* RTTOXY								*
C* 									*
C* This subroutine converts r, theta in polar coordinates to x, y	*
C* in linear coordinates.						*
C*									*
C* RTTOXY  ( R, THETA, X, Y, IRET )					*
C*									*
C* Input parameters:							*
C* 	R		REAL		Value of r			*
C*	THETA		REAL		Value of theta in degrees	*
C*									*
C* Output parameters:							*
C*	X		REAL		Value of x			*
C*	Y		REAL		Value of y			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/86						*
C* K. Brill/EMC		 3/96	Input and output variables can be same	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
C-------------------------------------------------------------------------
	iret = NORMAL
C
C*	Do transformation.
C
	rr = r
	th = theta
	x = rr * COS ( th * DTR )
	y = rr * SIN ( th * DTR )
C*
	RETURN
	END
