	SUBROUTINE GQHASH  ( szhsh, ihwid, ilwid, iret )
C************************************************************************
C* GQHASH 								*
C* 									*
C* This subroutine returns the current hash mark size, line width, and	*
C* line spacing.							*
C* 									*
C* GQHASH  ( SZHSH, IHWID, ILWID, IRET )				*
C*									*
C* Output parameters:							*
C* 	SZHSH		REAL	 	Hash mark size 			*
C* 	IHWID		INTEGER		Line width			*
C*	ILWID		INTEGER		Line spacing			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C* A. Hardy/GSC		06/98		Cleaned up prolog		*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQHASH
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ierr )
	IF  ( ierr .ne. NORMAL )  THEN
	    iret = ierr
	    RETURN
	END IF
C
	CALL GGETR  ( szhsh, 1, ierr )
	IF  ( ierr .eq. NORMAL ) THEN
	    CALL GGET ( ircv, 2, ierr )
            IF  ( ierr .eq. NORMAL ) THEN
	        ihwid = ircv (1)
                ilwid = ircv (2)
            END IF
        END IF
C*
	RETURN
	END
