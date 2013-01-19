	SUBROUTINE GQMRKR  ( imark, imkhw, szmark, imkwid, iret )
C************************************************************************
C* GQMRKR								*
C*									*
C* This subroutine returns the current marker attributes, including 	*
C* the marker number, the marker flag, the marker size and the marker   *
C* line width.								*
C*									*
C* GQMRKR  ( IMARK, IMKHW, SZMARK, IMKWID, IRET )			*
C*									*
C* Output parameters:							*
C*	IMARK		INTEGER		Marker number			*
C*	IMKHW		INTEGER		Marker flag			*
C*					  1 = software			*
C*					  2 = hardware			*
C*	SZMARK		REAL		Marker size 			*
C*	IMKWID		INTEGER		Line width 			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 1/90	Added marker width			*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C
	INTEGER		isend (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQMRKR
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGET  ( ircv, 2, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
	imark  = ircv (1)
	imkhw  = ircv (2)
C
	CALL GGETR  ( szmark, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr
C
	CALL GGET ( imkwid, 1, ierr )
	IF  ( ierr .ne. NORMAL )  iret = ierr	
C*
	RETURN
	END
