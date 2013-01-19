	SUBROUTINE GGETR  ( data, nw, iret )
C************************************************************************
C* GGETR								*
C*									*
C* This subroutine gets part or all of a message.  This subroutine	*
C* waits for a message to be received.					*
C*									*
C* NOTE: NW is an INPUT parameter and DATA is an OUTPUT parameter.	*
C* Most GEMPAK subroutines have the input parameters before the		*
C* output parameters.							*
C*									*
C* This subroutine was created to avoid conflict of return value type	*
C* on linux platforms							*
C*									*
C* GGETR ( DATA, NW, IRET )						*
C*									*
C* Output parameters:							*
C*	DATA (NW)	REAL		Message part			*
C*									*
C* Input parameters:							*
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
	CALL GGET(data, nw, iret )
C*
	RETURN
	END
