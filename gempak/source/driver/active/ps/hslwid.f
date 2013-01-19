	SUBROUTINE HSLWID  ( ilwid, iret )
C************************************************************************
C* HSLWID - PS								*
C* 									*
C* This subroutine sets the hardware line width.			*
C*									*
C* HSLWID  ( ILWID, IRET )						*
C*									*
C* Input parameters:							*
C* 	ILWID		INTEGER	 	Line width 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/90						*
C* A. Chang/EAI          2/94   Modified to call C routine              *
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
	CALL PSLWID ( ilwid, iret )
C*
	RETURN
	END
