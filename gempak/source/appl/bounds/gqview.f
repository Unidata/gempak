	SUBROUTINE GQVIEW  ( xllf, yllf, xurf, yurf, iret )	
C************************************************************************
C* GQVIEW								*
C* 									*
C* This subroutine returns the current view region boundaries.	 	*
C*									*
C* GQVIEW  ( XLLF, YLLF, XURF, YURF, IRET )				*
C*									*
C* Output parameters:							*
C*	XLLF		REAL		Lower left x fraction		*
C*	YLLF		REAL		Lower left y fraction		*
C*	XURF		REAL		Upper right x fraction		*
C*	YURF		REAL		Upper right y fraction		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2)
	REAL		rcv  (4)
C------------------------------------------------------------------------
C*	Send the request through the mailbox.
C
	isend (1) = 2
	isend (2) = FQVIEW
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR  ( rcv, 4, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
	xllf  = rcv (1)
	yllf  = rcv (2)
	xurf  = rcv (3)
	yurf  = rcv (4)
C*
	RETURN
	END
