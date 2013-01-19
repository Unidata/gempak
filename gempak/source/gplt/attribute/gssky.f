	SUBROUTINE GSSKY  ( szsky, isktyp, iskwid, iret )
C************************************************************************
C* GSSKY								*
C* 									*
C* This subroutine sets the sky coverage symbol size, line width and	*
C* the symbol type.							*
C* 									*
C* Note that this subroutine is not currently implemented.		*
C*									*
C* GSSKY  ( SZSKY, ISKTYP, ISKWID, IRET )				*
C*                                                                    	*
C* Input parameters:							*
C*	SZSKY		REAL		Sky coverage size multiplier	*
C*					  <=0 = no change		*
C*	ISKTYP		INTEGER		Sky coverage symbol type	*
C*					  1 = not filled in		*
C*					  2 = filled in			*
C*	ISKWID		INTEGER		Sky coverage line width		*
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
        IF  ( szsky .eq. rskysz .and. isktyp .eq. ksktyp .and.
     +        iskwid .eq. kskwid )THEN
	ELSE 
C
C*	    Set requested parameters
C
            IF  ( szsky .gt. 0 )  rskysz = szsky
            IF  ( isktyp .eq. 1 .or. isktyp .eq. 2) ksktyp = isktyp
            IF  ( iskwid .ge. 1 ) kskwid = iskwid
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*              Only set if requested different than what is already set
C
                IF ( rskysz .ne. sskysz .or. ksktyp .ne. lsktyp .or.
     +               kskwid .ne. lskwid ) THEN
                    CALL DSSKY   ( rskysz, ksktyp, kskwid, 
     +                             sskysz, lsktyp, lskwid, iret )
                END IF
	    END IF
	END IF
C*
	RETURN
	END
