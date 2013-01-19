	SUBROUTINE GSGPRJ  (  proj,  angle1, angle2, angle3, kx, ky,
     +			     dlatll, dlonll, dlatur, dlonur, iret )
C************************************************************************
C* GSGPRJ								*
C* 									*
C* This subroutine defines the coordinate system for a grid which is	*
C* evenly spaced on a general map projection.  Information about map	*
C* projections is given in GSMPRJ.					*
C* 									*
C* GSGPRJ  (  PROJ,  ANGLE1, ANGLE2, ANGLE3, KX, KY, DLATLL, DLONLL,	*
C*           DLATUR, DLONUR, IRET )					*
C* 									*
C* Input parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	ANGLE1		REAL		Reference angle 1		*
C*	ANGLE2		REAL		Reference angle 2		*
C*	ANGLE3		REAL		Reference angle 3		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		 Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	proj
C*
	CHARACTER	cprj*4
	INTEGER		isend (3)
	REAL		rsend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 12
	isend (2) = FSGPRJ
	cprj      = proj
	CALL ST_STOI  ( cprj, 4, nv, iprj, ier )
	isend (3) = iprj
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	rsend (1)  = angle1
	rsend (2)  = angle2
	rsend (3)  = angle3
	CALL GPUTR  ( rsend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1)  = kx
	isend (2)  = ky
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	rsend (1)  = dlatll
	rsend (2)  = dlonll
	rsend (3)  = dlatur
	rsend (4)  = dlonur
	CALL GPUTR  ( rsend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get return code.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
