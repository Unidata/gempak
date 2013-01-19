	SUBROUTINE G2T_MISS ( ntype, istart, iend, iret )
C************************************************************************
C* G2T_MISS								*
C*									*
C* This subroutine sets missing values and flags for specific storm to	*
C* the headline message.						*
C*									*
C* G2T_MISS ( NTYPE, ISTART, IEND, IRET )				*
C*									*
C* Input parameters:							*
C*	NTYPE		INTEGER		Storm type			*
C*					  1: Hurricane			*
C*					  2: Storm			*
C*					  3: Gale			*
C*	ISTART		INTEGER		Starting index			*
C*	IEND		INTEGER		Ending index			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = incorrect storm type	*
C**									*
C* Log:									*
C* T. Lee/SAIC		12/06						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Set missing value for the appropriate storm type.
C
	IF ( ntype .eq. 1 )  THEN
	    DO kk = istart, iend
		fhurr ( kk ) = .false.
	    END DO
	  ELSE IF ( ntype .eq. 2 )  THEN
	    DO kk = istart, iend
		fstorm ( kk ) = .false.
	    END DO
	  ELSE IF ( ntype .eq. 3 )  THEN
	    DO kk = istart, iend
		fgale ( kk ) = .false.
	    END DO
	  ELSE
	    iret = -10
	END IF
C*
	RETURN
	END
