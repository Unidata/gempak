	SUBROUTINE IP_RESP  ( respnd, iret )
C************************************************************************
C* IP_RESP								*
C*									*
C* This subroutine determines whether the program being run should	*
C* allow interactive response by the user.  The value of $RESPOND	*
C* is checked.								*
C*									*
C* IP_RESP  ( RESPND, IRET )						*
C*									*
C* Output parameters:							*
C*	RESPND		LOGICAL		Respond flag			*
C*	IRET		INTEGER		Return code			*
C*				  	  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/84	Original source				*
C* M. desJardins/GSFC	 6/88	Fixed for non-TAE programs		*
C* S. Schotz/GSC	 5/90	Replaced IP-STR with IP_LOG, also 	*
C* 				removed $RUNTYPE for no TAE path	*
C* M. desJardins/NMC	 6/94	Eliminate TAE				*
C* K. Tyle/GSC		 7/96	Eliminated reference to $RUNTYPE 	*
C************************************************************************
	LOGICAL        respnd
C-----------------------------------------------------------------------
	iret   = 0
	respnd = .true.
	CALL IP_LOG  ( '$RESPOND', respnd, ier1 )
C*
	RETURN
	END
