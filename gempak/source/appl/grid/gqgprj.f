	SUBROUTINE GQGPRJ  (  proj,  angle1, angle2, angle3, kx, ky,
     +			     dlatll, dlonll, dlatur, dlonur, iret )
C************************************************************************
C* GQGPRJ								*
C* 									*
C* This subroutine returns the current coordinate system definition 	*
C* for a grid which is evenly spaced on a general map projection.  	*
C* 									*
C* GQGPRJ  ( PROJ,  ANGLE1, ANGLE2, ANGLE3, KX, KY, DLATLL, DLONLL, 	*
C*           DLATUR, DLONUR, IRET )					*
C*									*
C* Output parameters:							*
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
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M.Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	proj
C*
	INTEGER		isend (2), ircv (2)
	REAL		rcv (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQGPRJ
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret , 1, ierr ) 
	IF  ( ierr .ne. 0 )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGET  ( iprj, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
	CALL ST_ITOS  ( iprj, 1, nc, proj, ier )
C
	CALL GGETR  ( rcv, 3, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	  ELSE
	    angle1 = rcv (1)
	    angle2 = rcv (2)
	    angle3 = rcv (3)
	END IF
C
	CALL GGET  ( ircv, 2, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	  ELSE
	    kx = ircv (1)
	    ky = ircv (2 )
	END IF
C
	CALL GGETR  ( rcv, 4, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	  ELSE
	    dlatll = rcv (1)
	    dlonll = rcv (2)
	    dlatur = rcv (3)
	    dlonur = rcv (4)
	END IF
C*
	RETURN
	END
