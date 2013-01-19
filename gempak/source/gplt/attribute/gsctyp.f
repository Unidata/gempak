	SUBROUTINE GSCTYP  ( szctyp, ictwid, iret )
C************************************************************************
C* GSCTYP								*
C* 									*
C* This subroutine sets the cloud type symbol attributes.  If these	*
C* parameters are not positive, no changes are made.			*
C* 									*
C* Note that this subroutine is not currently implemented.		*
C*									*
C* GSCTYP  ( SZCTYP, ICTWID, IRET )					*
C*                                                                    	*
C* Input parameters:							*
C*	SZCTYP		REAL		Cloud type symbol size		*
C*	ICTWID		INTEGER		Cloud type line width		*
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
        IF  ( szctyp .eq. rctsz .and. ictwid .eq. kctwid ) THEN
	ELSE 
C
C*	    Set requested parameters
C
            IF  ( szctyp .gt. 0 ) rctsz = szctyp
	    IF  ( ictwid .ge. 1 ) kctwid = ictwid
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*              Only set if requested different than what is already set
C
                IF ( rctsz .ne. sctsz .or. kctwid .ne. lctwid ) THEN
                    CALL DSCTYP ( rctsz, kctwid, sctsz, lctwid, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
