	SUBROUTINE GPUTR  ( data, nw, iret )
C************************************************************************
C* GPUTR								*
C*									*
C* This subroutine sends part or all of a message.  			*
C*									*
C* NOTE: This subroutine was created to avoid conflict of return value 	*
C* type on linux platforms						*
C*									*
C* GPUTR ( DATA, NW, IRET )						*
C*									*
C* Input parameters:							*
C*	DATA (NW)	REAL		Message part			*
C*	NW		INTEGER		Length of message part		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* m.gamazaychikov/SAIC	01/04	CREATED					*
C************************************************************************
        INCLUDE         'ERROR.PRM'
C*
	REAL	data(*)
C------------------------------------------------------------------------
	iret = NORMAL
	CALL GPUT (data, nw, iret )
C*
	RETURN
	END
