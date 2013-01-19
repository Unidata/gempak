	SUBROUTINE DGTPNT  (  ityp, ix, iy, iret )
C************************************************************************
C* DGTPNT								*
C*									*
C* This subroutine returns the requested number of points from the 	*
C* cursor position when the mouse button is pressed.			*
C*									*
C* DGTPNT  ( ITYP, IX, IY, IRET )					*
C*									*
C* Input parameters:							*
C*	ITYP		INTEGER		Type of cursor			*
C*					   1 = point, NP = 1		*
C*					   2 = line,  NP = 2		*
C*					   3 = box,   NP = 2		*
C*									*
C* Output parameters:							*
C*	IX (NP)		INTEGER		X coordinates			*
C*	IY (NP)		INTEGER		Y coordinates			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Chou/EAI		 6/93						*
C* S. Jacobs/EAI	 6/93	Clean up				*
C* S. Jacobs/EAI	 9/93	Added ITYP				*
C* S. Jacobs/NCEP	 6/98	Removed NP from calling sequence	*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ix ( * ), iy ( * )
C*
	INTEGER		isend ( 3 )
C------------------------------------------------------------------------
C*      Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3
	isend (2) = CGTPNT
	isend (3) = ityp
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*      Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
C*	Get cursor points.
C
	IF  ( ityp .eq. 1 )  THEN
	    np = 1
	  ELSE
	    np = 2
	END IF
C
	CALL GGET  ( ix, np, ierr )
	IF  ( ierr .eq. NORMAL )  CALL GGET  ( iy, np, ierr )
C
	IF  ( ierr .ne. NORMAL )  iret = ierr
C*
	RETURN
	END
