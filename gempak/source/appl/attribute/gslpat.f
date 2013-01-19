	SUBROUTINE GSLPAT  ( lpat, iret )
C************************************************************************
C* GSLPAT								*
C* 									*
C* This subroutine sets the current line pattern to the given pattern.  *
C* The current line type is set to zero.  The values specified in the   *
C* line pattern designate the length of alternating on and off segments.*
C* For example, a long-dash short-dash line pattern could be specified  *
C* by LPAT values of:							*
C*									*
C*		10 5 5 5 0 0 0 0					*
C*									*
C* This line pattern draws a line for 10 units, space for 5 units,      *
C* a line for 5 units, and space for 5 units and repeats this pattern.  *
C* Dots are specified using negative numbers.  The absolute value of    *
C* the number corresponds to the space in which the dot is centered.    *
C*									*
C* GSLPAT  ( LPAT, IRET )						*
C*									*
C* Input parameters:							*
C*	LPAT (8)	INTEGER		Line pattern values		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (2), lpat (8)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 10
	isend (2) = FSLPAT
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( lpat, 8, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
