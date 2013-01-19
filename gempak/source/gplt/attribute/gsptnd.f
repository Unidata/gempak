	SUBROUTINE GSPTND  ( szptnd, iptwid, iret )
C************************************************************************
C* GSPTND								*
C* 									*
C* This subroutine sets the pressure tendency symbols size and line	*
C* width.								*
C*									*
C* Note that this subroutine is not currently implemented.		*
C*									*
C* GSPTND  ( SZPTND, IPTWID, IRET )					*
C*                                                                    	*
C* Input parameters:							*
C*	SZPTND		REAL		Pressure tendency symbol size	*
C*					  <=0 = no change		*
C*	IPTWID		INTEGER		Pressure tendency line width	*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90   					*
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
        IF  ( szptnd .eq. rptnsz .and. iptwid .eq. kptwid ) THEN
	ELSE 
C
C*	    Set requested parameters
C
            IF  ( szptnd .gt. 0 ) rptnsz = szptnd
	    IF  ( iptwid .ge. 1 ) kptwid = iptwid
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*              Only set if requested different than what is already set
C
                IF ( rptnsz .ne. sptnsz .or. kptwid .ne. lptwid) THEN
                    CALL DSPTND ( rptnsz, kptwid, sptnsz, lptwid, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
