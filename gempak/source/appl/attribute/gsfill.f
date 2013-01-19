	SUBROUTINE GSFILL  ( szfil, iftyp, iret )
C************************************************************************
C* GSFILL								*
C* 									*
C* This subroutine sets the fill pattern size and the fill pattern type.*
C* If these parameters are not positive, no change is made.             *
C* 									*
C* GSFILL  ( SZFIL, IFTYP, IRET )					*
C*									*
C* Input parameters:							*
C* 	SZFIL		REAL		Fill pattern size 	  	*
C*	IFTYP		INTEGER		Fill pattern type 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 6/97						*
C* A. Hardy/GSC          6/98		Cleaned up code                 *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = FSFILL
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szfil, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( iftyp, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
