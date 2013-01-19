	SUBROUTINE DSLPAT  ( lpat, jltyp, iret )
C************************************************************************
C* DSLPAT								*
C* 									*
C* This subroutine sets the current line pattern to the given pattern.  *
C* The line type number will be set to zero.  The values specified in	*
C* the line pattern designate the length of alternating on and off 	*
C* segments.  For example, a long-dash short-dash line pattern could be	*
C* specified by LPAT values of:						*
C*									*
C*		10 5 5 5 0 0 0 0					*
C*									*
C* This pattern designation would result in a line pattern that displays*
C* a line for 10 units, space for 5 units, a line for 5 units, and space*
C* for 5 units.  							*
C*									*
C* DSLPAT  ( LPAT, JLTYP, IRET )					*
C*									*
C* Input parameters:							*
C*	LPAT ( 8 )	INTEGER		Line pattern values		*
C* 									*
C* Output parameters:							*	
C* 	JLTYP		INTEGER		Line pattern number		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	11/84	GEMPLT Version 3.0                      *
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 9/88	Doc and returns				*	
C* S. Schotz/GSC	 8/90	Return line pattern number		*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
        INCLUDE		'DEVACT.CMN'
C*
	INTEGER		lpat (*)
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 10
	isend (2) = CSLPAT
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( lpat, 8, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get line pattern number
C
	CALL GGET  ( jltyp, 1, ier )
	mltyp = jltyp
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
