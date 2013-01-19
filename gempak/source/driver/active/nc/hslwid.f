	SUBROUTINE HSLWID  ( ilwid, iret )
C************************************************************************
C* HSLWID - NC								*
C* 									*
C* This subroutine sets the hardware line width.			*
C*									*
C* HSLWID  ( ILWID, IRET )						*
C*									*
C* Input parameters:							*
C* 	ILWID		INTEGER		 Line width			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI          4/94                                           *
C* S. Jacobs/NMC	 6/94		General clean up		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL MSLWID ( ilwid, iret )
C*
	RETURN
	END
