	SUBROUTINE GSPWTH  ( szpwth, ipwwid, iret )
C************************************************************************
C* GSPWTH								*
C* 									*
C* This subroutine sets the past weather symbol size and line width.	*
C* 									*
C* Note that this subroutine is not currently implemented.		*
C*									*
C* GSPWTH  ( SZPWTH, IPWWID, IRET )					*
C*                                                                    	*
C* Input parameters:							*
C*	SZPWTH		REAL		Past weather symbol size	*
C*					  <=0 = no change		*
C*	IPWWID		INTEGER		Past weather line width		*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90   					*
C* G. Krueger/EAI	 5/95	Fixed kptwid -> kpwwid			*
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
        IF  ( szpwth .eq. rpwtsz .and. ipwwid .eq. kpwwid ) THEN
	ELSE 
C
C*	    Set requested parameters
C
            IF  ( szpwth .gt. 0 ) rpwtsz = szpwth
	    IF  ( ipwwid .ge. 1 ) kpwwid = ipwwid
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*              Only set if requested different than what is already set
C
                IF ( rpwtsz .ne. spwtsz .or. kpwwid .ne. lpwwid) THEN
                    CALL DSPWTH ( rpwtsz, kpwwid, spwtsz, lpwwid, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
