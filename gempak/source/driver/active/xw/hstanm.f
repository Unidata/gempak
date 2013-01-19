	SUBROUTINE HSTANM  ( iret )
C************************************************************************
C* HSTANM - XW								*
C* 									*
C* This subroutine starts a new animation sequence.			*
C* 									*
C* HSTANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI            7/94   	Multi-pixmap             	*
C* C. Lin/EAI		 8/94		Added call to XXFLSH		*
C* S. Maxwell/GSC        6/97   	Documentation changes           *
C************************************************************************
C------------------------------------------------------------------------
C*	Initialize the pixmap counter.
C
        CALL XXFLSH ( .true., iret )
        CALL XSTANM ( iret )
C*
	RETURN
	END
