	SUBROUTINE GSMRKR  ( imark, imkhw, szmark, imkwid, iret ) 
C************************************************************************
C* GSMRKR								*
C* 									*
C* This subroutine sets the current marker attributes, including the    *
C* marker number, the marker flag, the marker size and the marker line  *
C* width.  If these parameters are not positive, no change is made.	*
C* 									*
C* GSMRKR  ( IMARK, IMKHW, SZMARK, IMKWID, IRET )			*
C*									*
C* Input parameters:							*
C*	IMARK		INTEGER		Marker number			*
C*	IMKHW		INTEGER		Marker flag			*
C*					  1 = software 			*
C*					  2 = hardware 			*
C*					  otherwise no change		*
C*	SZMARK		REAL		Marker size 			*
C*	IMKWID		INTEGER		Line width 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added marker width			*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = FSMRKR
	isend (3) = imark
	isend (4) = imkhw
C
	CALL GPUT  ( isend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szmark, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( imkwid, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END

