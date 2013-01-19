	SUBROUTINE G2T_FILL ( iret )
C************************************************************************
C* G2T_FILL								*
C*									*
C* This subroutine fills the warning flags from non-synoptic time to	*
C* synoptic time.							*
C*									*
C* G2T_FILL ( IRET )							*
C*									*
C* Input parameters:							*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		12/06						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Fill the flags.
C
	nt = 1
	DO WHILE ( nt .le. ( ngrdtm - 2 ) )
	    DO kk = nt, nt + 2
		IF  ( fhurr  ( kk )  )  THEN
		    fhurr  ( nt ) = .true.
		    fhurr  ( nt + 1 ) = .true.
		END IF
C
		IF  ( fstorm ( kk )  )  THEN
		    fstorm ( nt ) = .true.
		    fstorm ( nt + 1 ) = .true.
		END IF
C
		IF  ( fgale  ( kk )  )  THEN
		    fgale  ( nt ) = .true.
		    fgale  ( nt + 1 ) = .true.
		END IF
	    END DO
	    nt = nt + 2
	END DO
C*
	RETURN
	END
