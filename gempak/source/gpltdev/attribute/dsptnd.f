	SUBROUTINE DSPTND  ( szptnd, iptwid, size, jptwid, iret )
C************************************************************************
C* DSPTND								*
C* 									*
C* This subroutine sets the pressure tendency symbol parameters.  If 	*
C* parameters are not positive, no changes are made.   			*
C* 									*
C* DSPTND  ( SZPTND, IPTWID, SIZE, JPTWID, IRET )			*
C*                                                                 	*
C* Input parameters:							*
C* 	SZPTND		REAL		Pressure tendency symbol size 	*
C*                                      multiplier			*
C* 					  <=0 = no change		*
C*      IPTWID		INTEGER		Pressure tendency symbol width  * 
C*					multiplier			*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Pressure tendency symbol size	*
C*	JPTWID		INTEGER		Pressure tendency symbol width	*
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
	isend (2) = CSPTND
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szptnd, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( iptwid, 1, iret )
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
	    tptnsz = size
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
            jptwid = ircv
            mptwid = jptwid
        END IF
C*
	RETURN
	END
