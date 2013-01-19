	SUBROUTINE DSWTHR  ( szwthr, iwtwid, size, jwtwid, iret )
C************************************************************************
C* DSWTHR								*
C* 									*
C* This subroutine sets the weather symbol parameters.  If these 	*
C* parameters are not positive, no changes are made.   			*
C* 									*
C* DSWTHR  ( SZWTHR, IWTWID, SIZE, JWTWID, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZWTHR		REAL		Weather symbol size multiplier	*
C* 					  <=0 = no change		*
C*      IWTWID		INTEGER		Weather symbol width multiplier	*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Weather symbol size		*
C*	JWTWID		INTEGER		Weather symbol width		*
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
	isend (2) = CSWTHR
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szwthr, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( iwtwid, 1, iret )
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
	    twtrsz = size
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
            jwtwid = ircv
            mwtwid = jwtwid
        END IF
C*
	RETURN
	END
