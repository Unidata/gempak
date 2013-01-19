	SUBROUTINE GSSPLN  ( isltyp, islstr, isldir, slsiz, islwid,
     +			     iret )
C************************************************************************
C* GSSPLN								*
C* 									*
C* This subroutine sets special line attributes, including the special	*
C* line type, stroke size, direction indicator, pip size and line       *
C* width.								*
C* 									*
C* GSSPLN  ( ISLTYP, ISLSTR, ISLDIR, SLSIZ, ISLWID, IRET )		*
C*									*
C* Input parameters:							*
C*	ISLTYP		INTEGER		Special line type       	*
C*	ISLSTR		INTEGER		Stroke size - not used          *
C*	ISLDIR		INTEGER		Direction indicator		*
C*					   1 = up or out		*
C*					   0 = no change		*
C*					  -1 = down or in		*
C*	SLSIZ		REAL		Pip size			*
C*	ISLWID		INTEGER		Line width 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 3/97	Copied from GSLINE			*
C* S. Jacobs/NCEP	 4/98	Cleaned up prolog			*
C* A. Hardy/GSC		 6/98	Cleaned up prolog			*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		 isend (5)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 7
	isend (2) = FSSPLN
	isend (3) = isltyp
	isend (4) = islstr
	isend (5) = isldir
C
	CALL GPUT  ( isend, 5, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( slsiz, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( islwid, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( iret .ne. NORMAL )  iret = ier
C*
	RETURN
	END
