	SUBROUTINE GQARRW  ( szarrw, szarrh, iarwid, iartyp, iret )
C************************************************************************
C* GQARRW 								*
C* 									*
C* This subroutine returns the current wind arrow size, arrow head 	*
C* size, line width and arrow type.					*
C* 									*
C* GQARRW  ( SZARRW, SZARRH, IARWID, IARTYP, IRET )			*
C*									*
C* Output parameters:							*
C* 	SZARRW		REAL	 	Wind arrow size 		*
C* 	SZARRH		REAL		Arrow head size			*
C* 	IARWID		INTEGER		Line width			*
C*	IARTYP		INTEGER		Arrow type			*
C*					  1 = plot arrow for calm wind  *
C*					  2 = do not plot for calm wind *
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 1/90	Added wind arrow width and type		*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQARRW
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
	CALL GGETR  ( szarrw, 1, ierr )
	IF  ( ierr .eq. NORMAL ) THEN
            CALL GGETR ( szarrh, 1, ierr )
            IF  ( ierr .eq. NORMAL ) THEN
	        CALL GGET ( ircv, 2, ierr )
                IF  ( ierr .eq. NORMAL ) THEN
		    iarwid = ircv (1)
                    iartyp = ircv (2)
                END IF
            END IF
        END IF
C*
	RETURN
	END
