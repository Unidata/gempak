	SUBROUTINE DSCTYP  ( szctyp, ictwid, size, jctwid, iret )
C************************************************************************
C* DSCTYP								*
C* 									*
C* This subroutine sets the cloud type symbol parameters.  If these 	*
C* parameters are not positive, no changes are made.   			*
C* 									*
C* DSCTYP  ( SZCTYP, ICTWID, SIZE, JCTWID, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZCTYP		REAL		Cloud type symbol size 		*
C*                                      multiplier			*
C* 					  <=0 = no change		*
C*      ICTWID		INTEGER		Cloud type symbol width 	* 
C*					multiplier			*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Cloud type symbol size		*
C*	JCTWID		INTEGER		Cloud type symbol width		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 3/90						*
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
C*
	INTEGER 	isend (2), ircv
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 4
	isend (2) = CSCTYP
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szctyp, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( ictwid, 1, iret )
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
C*	    Save the ACTIVE weather symbol size in common block 
C*	    variable.
C
	    tctsz = size
	END IF
C*
        CALL GGET  ( ircv, 1, ier )
        IF  ( ier .ne. NORMAL ) THEN
            iret = ier
        ELSE
C
C*          Save the ACTIVE width in common block and
C*          output variable
C
            jctwid = ircv
            mctwid = jctwid
        END IF
C*
	RETURN
	END
