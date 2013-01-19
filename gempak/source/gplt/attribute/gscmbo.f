	SUBROUTINE GSCMBO  ( szcmwx, icmbwd, iret )
C************************************************************************
C* GSCMBO								*
C* 									*
C* This subroutine sets the combination weather symbol size and line    *
C* width.								*
C* 									*
C* GSCMBO  ( SZCMWX, ICMBWD, IRET )					*
C*                                                                    	*
C* Input parameters:							*
C*	SZCMWX		REAL		Combination symbol size		*
C*					  <=0 = no change		*
C*									*
C*	ICMBWD		INTEGER		Combination symbol line width	*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC		10/98		Copied from GSWTHR              *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	First check if these are the current requested characteristics.
C*	If so, do nothing.
C
        IF  ( szcmwx .eq. rcsysz .and. icmbwd .eq. kcsywd ) THEN
	ELSE 
C
C*	    Set requested parameters
C
            IF  ( szcmwx .gt. 0 ) rcsysz = szcmwx
	    IF  ( icmbwd .ge. 1 ) kcsywd = icmbwd
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*              Only set if requested different than what is already set
C
                IF ( rcsysz .ne. scsysz .or. kcsywd .ne. lcsywd) THEN
                    CALL DSCMBO ( rcsysz, kcsywd, scsysz, lcsywd, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
