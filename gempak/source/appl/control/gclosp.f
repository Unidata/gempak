	SUBROUTINE GCLOSP  ( iret )
C************************************************************************
C* GCLOSP								*
C* 									*
C* This subroutine closes any intermediate plot file used by a		*
C* sequential access type device.  When plotting is continued, a new	*
C* file is opened.  No action is taken if the current device does not   *
C* use an intermediate plot file.				        *
C*									*
C* GCLOSP  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* L. Williams /EAi      3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FCLOSP
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
