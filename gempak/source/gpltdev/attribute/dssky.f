	SUBROUTINE DSSKY  ( szsky, isktyp, iskwid, size, jsktyp,
     +                       jskwid, iret )
C************************************************************************
C* DSSKY								*
C* 									*
C* This subroutine sets the sky symbol parameters.  If these 		*
C* parameters are not positive, no changes are made.   			*
C* 									*
C* DSSKY  ( SZSKY, ISKTYP, ISKWID, SIZE, JSKTYP, JSKWID, IRET )						*
C*                                                                    	*
C* Input parameters:							*
C* 	SZSKY		REAL		Sky coverage size multiplier	*
C* 					  <=0 = no change		*
C*	ISKTYP		INTEGER		Sky coverage symbol type	*
C*					  <=0 = no change, 1 = not 	*
C*					  filled, 2 = filled		*
C*      ISKWID		INTEGER		Sky coverage width multiplier	*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Sky coverge symbol size		*
C*	JSKTYP		INTEGER		Sky coverage symbol type	*
C*	JSKWID		INTEGER		Sky coverage symbol width	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER 	isend (2), ircv (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 5
	isend (2) = CSSKY
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szsky, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
        isend (1) = isktyp
        isend (2) = iskwid
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	CALL GGETR  ( size, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	ELSE
C
C*	    Save the ACTIVE sky coverage symbol size common block 
C*	    variable.
C
	    tskysz = size
	END IF
C*
        CALL GGET  ( ircv, 2, ier )
        IF  ( ier .ne. NORMAL ) THEN
            iret = ier
        ELSE
C
C*          Save the ACTIVE parameters in common block and
C*          output variables
C
            jsktyp = ircv(1)
            jskwid = ircv(2)
            msktyp = jsktyp
            mskwid = jskwid
        END IF
C*
	RETURN
	END
