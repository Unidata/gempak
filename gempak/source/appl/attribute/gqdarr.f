	SUBROUTINE GQDARR  ( szdarw, szdarh, idarwd, idartp, iret )
C************************************************************************
C* GQDARR 								*
C* 									*
C* This subroutine returns the current directional arrow size, arrow 	*
C* head size, line width and arrow type.				*
C* 									*
C* GQDARR  ( SZDARW, SZDARH, IDARWD, IDARTP, IRET )			*
C*									*
C* Output parameters:							*
C* 	SZDARW		REAL	 	Directional arrow size 		*
C* 	SZDARH		REAL		Arrow head size			*
C* 	IDARWD		INTEGER		Line width			*
C*	IDARTP		INTEGER		Arrow type			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C* A. Hardy/GSC          6/98		Cleaned up prolog               *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2
	isend (2) = FQDARR
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
	CALL GGETR  ( szdarw, 1, ierr )
	IF  ( ierr .eq. NORMAL ) THEN
            CALL GGETR ( szdarh, 1, ierr )
            IF  ( ierr .eq. NORMAL ) THEN
	        CALL GGET ( ircv, 2, ierr )
                IF  ( ierr .eq. NORMAL ) THEN
		    idarwd = ircv (1)
                    idartp = ircv (2)
                END IF
            END IF
        END IF
C*
	RETURN
	END
