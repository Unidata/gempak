	SUBROUTINE GSDASH  ( szdsh, iret )
C************************************************************************
C* GSDASH								*
C* 									*
C* This subroutine sets the line dashing scale.  If this parameter is   *
C* not positive, no change is made.     				*
C* 									*
C* GSDASH  ( SZDSH, IRET )						*
C*									*
C* Input parameters:							*
C* 	SZDSH		REAL		Line dashing scale		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/98						*
C* A. Hardy/GSC		 6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3
	isend (2) = FSDASH
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szdsh, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
