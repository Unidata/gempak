	SUBROUTINE GSGGRF ( ixtyp, iytyp, kx, ky, xll, yll, xur, 
     +			    yur,   iret )
C************************************************************************
C* GSGGRF								*
C* 									*
C* This subroutine defines the coordinate system for a grid which 	*
C* is evenly spaced in a graph coordinate system.  If the grid is 	*
C* defined in a polar coordinates system, the grid rows correspond 	*
C* to constant THETA values; the grid columns correspond to 		*
C* constant values of R.  XLL, YLL, XUR and YUR correspond to the 	*
C* min R, min THETA, max R and max THETA.  XLL must be greater than 	*
C* or equal to 0.							*
C* 									*
C* GSGGRF  ( IXTYP, IYTYP, KX, KY, XLL, YLL, XUR, YUR, IRET )		*
C* 							        	*
C* Input parameters:					        	*
C*	IXTYP		INTEGER		X coordinate type 		*
C*					  1 = linear			*
C*					  2 = logarithmic		*
C*					  3 = ** KAPPA (2/7)		*
C*					  5 = polar (R)			*
C*	IYTYP		INTEGER		Y coordinate type		*
C*					  1 = linear			*
C*					  2 = logarithmic		*
C*					  3 = ** KAPPA (2/7)		*
C*					  5 = polar (THETA)		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	XLL 		REAL		Lower left X value		*
C*	YLL		REAL		Lower left Y value		*
C*	XUR		REAL		Upper right X value		*
C*	YUR		REAL		Upper right Y value		*
C*									*
C* Output parameters:							*
C* 	IRET 		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (6)
	REAL	 	rsend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer then write them to the mailbox.
C
	isend (1) = 10
	isend (2) = FSGGRF
	isend (3) = ixtyp
	isend (4) = iytyp
	isend (5) = kx
	isend (6) = ky
	CALL GPUT  ( isend, 6, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	rsend (1) = xll
	rsend (2) = yll
	rsend (3) = xur
	rsend (4) = yur
	CALL GPUTR  ( rsend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
