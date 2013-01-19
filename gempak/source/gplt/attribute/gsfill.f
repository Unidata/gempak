	SUBROUTINE GSFILL  ( szfil, iftyp, iret )
C************************************************************************
C* GSFILL								*
C* 									*
C* This subroutine sets the polygon fill attributes.			*
C* 									*
C* GSFILL  ( SZFIL, IFTYP, IRET )					*
C*                                                                    	*
C* Input parameters:							*
C*	SZFIL		REAL		Fill pattern size		*
C*					  <=0 = no change		*
C*									*
C*	IFTYP		INTEGER		Fill pattern type		*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Maxwell/GSC	 6/97   					*
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
        IF  ( szfil .eq. rfilsz .and. iftyp .eq. kfltyp ) THEN
	ELSE 
C
C*	    Set requested parameters
C
            IF  ( szfil .gt. 0 ) rfilsz = szfil
	    IF  ( iftyp .ge. 1 ) kfltyp = iftyp
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*              Only set if requested different than what is already set
C
                IF ( rfilsz .ne. sfilsz .or. kfltyp .ne. lfltyp) THEN
                    CALL DSFILL ( rfilsz, kfltyp, sfilsz, lfltyp, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
