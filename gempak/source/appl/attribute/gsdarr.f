	SUBROUTINE GSDARR  ( szdarw, szdarh, idarwd, idartp, iret )
C************************************************************************
C* GSDARR								*
C* 									*
C* This subroutine sets the directional arrow size, arrow head size, 	*
C* line width and arrow type.  If these parameters are not positive,    *
C* no change is made.			                        	*
C*									*
C* GSDARR  ( SZDARW, SZDARH, IDARWD, IDARTP, IRET )			*
C*									*
C* Input parameters:							*
C*	SZDARW		REAL		Directional arrow size 		*
C*	SZDARH		REAL		Arrow head size			*
C*	IDARWD		INTEGER		Line width			*
C*	IDARTP		INTEGER         Arrow type			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C* A. Hardy/GSC          6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = FSDARR
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR ( szdarw, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR ( szdarh, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1) = idarwd
	isend (2) = idartp
	CALL GPUT ( isend, 2, iret)
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
