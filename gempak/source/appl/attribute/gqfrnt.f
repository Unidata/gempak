	SUBROUTINE GQFRNT ( ifcod, pipsz, ipipst, ipipdr, iret )
C************************************************************************
C* GQFRNT								*
C*									*
C* This subroutine returns the current front attributes including the 	*
C* front code, the pip size, the pip stroke size and the pip direction. *
C* See subroutine GSFRNT for an explanation of the front code.          *
C*									*
C* GQFRNT  ( IFCOD, PIPSZ, IPIPST, IPIPDR, IRET )			*
C*									*
C* Output parameters:							*
C* 	IFCOD		INTEGER		Front code			*
C*	PIPSZ		REAL		Pip size           		*
C*	IPIPST		INTEGER		Pip stroke size - not used	*
C* 	IPIPDR		INTEGER		Pip direction            	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Wehner/EAi	10/96	Created					*
C* E. Wehner/EAi	11/96	Eliminated width parameter		*
C* T. Piper/GSC		 5/98	Corrected prolog                	*
C* S. Jacobs/NCEP	 6/98	Changed int IPIPSZ to real PIPSZ	*
C* A. Hardy/GSC		 6/98	Cleaned up prolog                	*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQFRNT
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGET  ( ifcod, 1, ierr )
	IF  ( iret .eq. NORMAL )  iret = ierr
C
	CALL GGETR  ( pipsz, 1, ierr )
	IF  ( iret .eq. NORMAL )  iret = ierr
C
	CALL GGET  ( ircv, 2, ierr )
	IF  ( iret .eq. NORMAL )  iret = ierr
	ipipst  = ircv (1)
	ipipdr  = ircv (2)
C*
	RETURN
	END
