	SUBROUTINE GQBARB  ( szbarb, ibrwid, ibrtyp, iret )
C************************************************************************
C* GQBARB								*
C*									*
C* This subroutine returns the current wind barb size, barb line width  *
C* and barb type. 						        *
C*									*
C* GQBARB  ( SZBARB, IBRWID, IBRTYP, IRET )				*
C*									*
C* Output parameters:							*
C*	SZBARB		REAL		Wind barb size 			*
C* 	IBRWID		INTEGER		Line width 			*
C*	IBRTYP		INTEGER		Barb type			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 1/90	Added wind bard width and type		*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2), ircv(2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQBARB
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
	CALL GGETR  ( szbarb, 1, ierr )
	IF ( ierr .eq. NORMAL ) THEN
	    CALL GGET ( ircv, 2, ierr )
	    IF ( ierr .eq. NORMAL ) THEN
                ibrwid = ircv (1)
                ibrtyp = ircv (2)
	    END IF
        END IF
C* 
	RETURN
	END
