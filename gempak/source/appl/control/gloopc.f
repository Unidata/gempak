	SUBROUTINE GLOOPC  ( icomm, iret )
C************************************************************************
C* GLOOPC								*
C* 									*
C* This subroutine activates loop control commands entered in the       *
C* GEMPAK dynamic tutor.						*
C* 									*
C* GLOOPC  ( ICOMM, IRET )						*
C* 									*
C* Input parameters:							*
C*	ICOMM		INTEGER		Loop command number		*
C*					  1 = loop forward              *
C*					  3 = step forward		*
C*					  4 = step back                 *
C*					  5 = reverse loop              *
C*					  6 = rock frames               *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI		12/93						*
C* L. Williams/EAi       3/94   Removed blank comments from header,     *
C*                              changed file description and added      *
C*                              variable description of ICOMM.          *
C* A. Hardy/GSC		6/98    Cleaned up prolog                       *
C************************************************************************
        INCLUDE         'FUNCCODE.PRM'
        INCLUDE         'ERROR.PRM'
C*
        INTEGER         isend (3)
C------------------------------------------------------------------------
C*      Load input parameters into buffer and write them to the mailbox.
C
        isend (1) = 3
        isend (2) = FLOOPC
	isend (3) = icomm
C
        CALL GPUT  ( isend, 3, iret )
        IF  ( iret .ne. NORMAL )  RETURN
C
        CALL GGET  ( iret , 1, ierr )
        IF  ( ierr .ne. NORMAL )  iret = ierr
C*
        RETURN
        END
