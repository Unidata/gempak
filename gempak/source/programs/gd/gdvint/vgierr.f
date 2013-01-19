	SUBROUTINE VG_IERR  ( ierror, iret )
C************************************************************************
C* VG_IERR								*
C*									*
C* This subroutine writes the return code from the vertical interpo-	*
C* lation program.							*
C*									*
C* VG_IERR ( IERROR, IRET )						*
C*									*
C* Input parameters:							*
C*	IERROR		INTEGER		Error code			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      07/92						*
C************************************************************************
	iret = 0
	IF ( ierror .eq. 0 ) THEN
	    WRITE (6,*) ' Interpolation is done.  No errors. '
	ELSE
	    CALL ER_WMSG ( 'GDVINT', ierror, ' ', ier )
	END IF
C*
	RETURN
	END
