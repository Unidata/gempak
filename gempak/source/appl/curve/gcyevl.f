	SUBROUTINE GCYEVL ( itype, np, xarray, yarray, npout, xeval,
     +			    yeval, iret )
C************************************************************************
C* GCYEVL								*
C* 									*
C* This subroutine fits a curve to a set of input points and evaluates	*
C* the curve for values of x.  The input points must be in M 		*
C* coordinates; the current mode must be graph mode.  The input points	*
C* must be strictly monotonic in x:					*
C* 									*
C*     xarray (1) < xarray (2) < ... < xarray (np)			*
C*			or						*
C*     xarray (1) > xarray (2) > ... > xarray (np)			*
C* 									*
C* GCYEVL ( ITYPE, NP, XARRAY, YARRAY, NPOUT, XEVAL, YEVAL, IRET )	*
C* 									*
C* Input parameters:							*
C*	ITYPE		INTEGER		Type of curve			*
C*					  1 = piecewise linear		*
C*					  2 = cubic spline		*
C*	NP		INTEGER		Number of input points		*
C* 	XARRAY (NP)	REAL		X input coordinates 		*
C* 	YARRAY (NP)	REAL		Y input coordinates 		*
C*	NPOUT		INTEGER		Number of evaluations		*
C*	XEVAL (NPOUT)	REAL		X evaluation coordinates 	*
C*									*
C* Output parameters:							*
C* 	YEVAL (NPOUT)	REAL		Y evaluated coordinates 	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 6/88	Clean up				*
C* L. Williams/EAi	 3/94	Removed blank comments from header	*
C* M. Linda/GSC		 3/96	Changed check for GPLT buffer overflow	*
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'GEMPRM.PRM'
C*
	REAL		xarray (*), yarray (*), xeval (*), yeval (*)
C
	INTEGER 	isend (5)
C------------------------------------------------------------------------
C
C*	Check if GPLT buffer will overflow.
C
	isnd = 2 + ( 2 + ( 2 * np ) + 1 + npout )
	ircv = 1 + ( npout + 1 )
	IF ( ( isnd + ircv ) .gt. IGBSIZ ) THEN
	    iret = NOBUFF
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = isnd
	isend (2) = FCYEVL
	isend (3) = itype
	isend (4) = np
C
	CALL GPUT ( isend, 4, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( xarray, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( yarray, np, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUT ( npout, 1, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
	CALL GPUTR ( xeval, npout, iret )
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get output parameters.
C
	CALL GGET ( iret, 1, ierr )
	IF ( ierr .ne. NORMAL ) THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR ( yeval, npout, ierr )
	IF ( ierr .ne. NORMAL ) iret = ierr
C*
 	RETURN
	END
