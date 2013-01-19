	SUBROUTINE GSWTHR  ( szwthr, iwtwid, iret )
C************************************************************************
C* GSWTHR								*
C* 									*
C* This subroutine sets the weather symbol size and line width.		*
C* 									*
C* GSWTHR  ( SZWTHR, IWTWID, IRET )					*
C*                                                                    	*
C* Input parameters:							*
C*	SZWTHR		REAL		Weather symbol size		*
C*					  <=0 = no change		*
C*									*
C*	IWTWID		INTEGER		Weather symbol line width	*
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
        IF  ( szwthr .eq. rwtrsz .and. iwtwid .eq. kwtwid ) THEN
	ELSE 
C
C*	    Set requested parameters
C
            IF  ( szwthr .gt. 0 ) rwtrsz = szwthr
	    IF  ( iwtwid .ge. 1 ) kwtwid = iwtwid
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*              Only set if requested different than what is already set
C
                IF ( rwtrsz .ne. swtrsz .or. kwtwid .ne. lwtwid) THEN
                    CALL DSWTHR ( rwtrsz, kwtwid, swtrsz, lwtwid, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
