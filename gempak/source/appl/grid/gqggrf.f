	SUBROUTINE GQGGRF  ( ixtyp, iytyp, kx, ky, xll, yll, xur, 
     +			     yur,   iret )
C************************************************************************
C* GQGGRF								*
C* 									*
C* This subroutine returns the current coordinate system definition	*
C* for a grid which is evenly spaced in a graph coordinate system.	*
C* The grid coordinate system is defined by GSGGRF.			*
C* 									*
C* GQGGRF  ( IXTYP, IYTYP, KX, KY, XLL, YLL, XUR, YUR, IRET )		*
C*									*
C* Output parameters:							*
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
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Vilardo/RDS	11/84	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (4)
	REAL		rrcv (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer then write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQGGRF
C
	CALL GPUT  ( isend, 2, iret  )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGET  ( isend, 4, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
	    ixtyp  = isend ( 1 )
	    iytyp  = isend ( 2 )
	    kx     = isend ( 3 )
	    ky     = isend ( 4 )
	  ELSE
	    iret   = ierr
	    RETURN
	END IF
C
	CALL GGETR  ( rrcv, 4, ierr )
	IF  ( ierr .eq. NORMAL )  THEN
	    xll    = rrcv ( 1 )
	    yll    = rrcv ( 2 )
	    xur    = rrcv ( 3 )
	    yur    = rrcv ( 4 )
	  ELSE
	    iret   = ierr
	END IF
C*
	RETURN
	END
