	SUBROUTINE GSGRAF ( ixtyp, iytyp, yszxsz, xl, yb, xr, yt, iret )
C************************************************************************
C* GSGRAF								*
C* 									*
C* This subroutine defines a coordinate system for plotting graphs.	*
C* The X and Y axis coordinate types are specified independently.  	*
C* The user may control the height to width ratio of the plot by 	*
C* setting the parameter YSZXSZ to a positive value.  If YSZXSZ is 	*
C* not positive the plot fills the available space.			*
C* 									*
C* For polar plots, X and Y are the distance and angle (R,THETA)	*
C* respectively.  YSZXSZ is ignored so that R is equidistant in         *
C* all directions.  A centered plot with radius R may be defined 	*
C* by setting XL = R and YB = XR = YT = 0.				*
C* 									*
C* GSGRAF  ( IXTYP, IYTYP, YSZXSZ, XL, YB, XR, YT, IRET )		*
C* 									*
C* Input parameters:							*
C* 	IXTYP		INTEGER		X coordinate type number	*
C*					   1 = linear			*
C*					   2 = logarithmic		*
C*					   3 = ** KAPPA (2/7)		*
C*					   5 =  polar (R) 		*
C* 	IYTYP		INTEGER		Y coordinate type number	*
C*					   1 = linear			*
C*					   2 = logarithmic		*
C*					   3 = ** KAPPA (2/7)		*
C*					   4 = skew			*
C*					   5 = polar (THETA)	        *
C* 	YSZXSZ		REAL		Height to width ratio of plot	*
C* 	XL		REAL		Left limit of X axis 		*
C* 	YB		REAL		Bottom limit of Y axis 		*
C* 	XR		REAL		Right limit of X axis		*
C* 	YT		REAL		Top limit of Y axis 		*
C* 									*
C* Output parameters:							*
C* 	IRET 		INTEGER		Return code			*
C** 									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (4)
	REAL 		rsend (5)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 9
	isend (2) = FSGRAF
	isend (3) = ixtyp
	isend (4) = iytyp
C
	rsend (1) = yszxsz
	rsend (2) = xl
	rsend (3) = yb
	rsend (4) = xr
	rsend (5) = yt
C
	CALL GPUT  ( isend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( rsend, 5, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
