	SUBROUTINE GSBARB  ( szbarb, ibrwid, ibrtyp, iret )
C************************************************************************
C* GSBARB								*
C* 									*
C* This subroutine sets the wind barb size, barb line width and barb    *
C* type.  If these parameters are not positive, no change is made. 	*
C*									*
C* GSBARB  ( SZBARB, IBRWID, IBRTYP, IRET )				*
C*									*
C* Input parameters:							*
C*	SZBARB		REAL		Wind barb size 			*
C*      IBRWID		INTEGER		Line width 			*
C*	IBRTYP		INTEGER		Barb type			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added barb width and type		*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 5
	isend (2) = FSBARB
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szbarb, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1) = ibrwid
	isend (2) = ibrtyp
	CALL GPUT  ( isend, 2, iret)
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
