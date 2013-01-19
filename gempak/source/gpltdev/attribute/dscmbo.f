	SUBROUTINE DSCMBO  ( szcmwx, icmbwd, size, jcmbwd, iret )
C************************************************************************
C* DSCMBO    								*
C* 									*
C* This subroutine sets the combination weather symbol parameters.  If  *
C* these parameters are not positive, no changes are made.   		*
C* 									*
C* DSCMBO  ( SZCMWX, ICMBWD, SIZE, JCMBWD, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZCMWX		REAL	     Combination symbol size multiplier	*
C* 					  <=0 = no change		*
C*      ICMBWD		INTEGER	     Combination symbol width multiplier*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL	     Combination weather symbol size    *
C*	JCMBWD		INTEGER	     Combination weather symbol width	*
C* 	IRET		INTEGER	     Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC	 10/98			Copied from DSWTHR		*
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
	isend (2) = CSCMBO
	CALL GPUT  ( isend,  2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( szcmwx, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUT  ( icmbwd, 1, iret )
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
	    tcsysz = size
	END IF
C*
        CALL GGET  ( ircv , 1, ier )
        IF  ( ier .ne. NORMAL ) THEN
            iret = ier
        ELSE
C
C*          Save the ACTIVE width in common block and
C*          output variable
C
            jcmbwd = ircv
            mcsywd = ircv
        END IF
C*
	RETURN
	END
