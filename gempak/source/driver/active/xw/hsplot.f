	SUBROUTINE HSPLOT  ( iret )
C************************************************************************
C* HSPLOT - XW								*
C* 									*
C* This subroutine is called when a plot is started.			*
C* 									*
C* HSPLOT  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI            7/94   multi-pixmap             		*
C* S. Maxwell/GSC	 6/97 	Documentation changes			*
C************************************************************************
C------------------------------------------------------------------------
C*	Increment the pixmap counter.
C
	CALL XSPLOT ( iret )
C*
	RETURN
	END
