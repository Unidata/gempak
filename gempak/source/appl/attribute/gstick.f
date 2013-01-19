	SUBROUTINE GSTICK  ( itktyp, sztick, iret )
C************************************************************************
C* GSTICK								*
C* 									*
C* This subroutine sets the tick attributes, including the tick type	*
C* and tick size.  If these parameters are not positive, no change is   *
C* made.								*
C* 									*
C* GSTICK  ( ITKTYP, SZTICK, IRET )					*
C*									*
C* Input parameters:							*
C*	ITKTYP		INTEGER		Tick type			*
C*					  1 = major tick outside	*
C*					  2 = major tick inside		*
C*					  3 = minor tick outside	*
C*					  4 = minor tick inside		*
C*					  5 = major tick out and inside	*
C*					  6 = minor tick out and inside	*
C*	SZTICK		REAL		Tick size 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/89	GEMPAK 5				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (3)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = FSTICK
	isend (3) = itktyp
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( sztick, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END

