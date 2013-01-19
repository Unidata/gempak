	SUBROUTINE RS_RRRD  ( temp, nout, iret )
C************************************************************************
C* RS_RRRD								*
C*									*
C* This subroutine decodes the three-digit rainfall group.  The output	*
C* is in 10ths of mm, with a trace decodes as 9999.			*
C*									*
C* RS_RRRD ( TEMP, NOUT, IRET )						*
C*									*
C* Input parameters:							*
C*	TEMP		INTEGER		Input wmo code			*
C*									*
C* Output parameters:							*
C*	NOUT		INTEGER		Output rainfall amount		*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal completion		*
C*					 -1 = Unexpected code		*
C**									*
C* Log:									*
C* J. Nielsen/MIT	10/86						*
C* J. Nielsen/TAMU	 2/91	Gempacized				*
C************************************************************************
C--	INTEGER  	iret
C
	INTEGER		nout, iret
C------------------------------------------------------------------------
	IF  ( ( temp .lt. 0 ) .or. ( temp .ge. 1000 ) )  THEN
	    iret = -1
	    RETURN
	ENDIF
	IF  ( temp .le. 989 )  THEN
	    nout = temp * 10
	ELSE IF  ( temp .eq. 990 )  THEN
	    nout = 9999
	ELSE
	    nout = temp - 990
	ENDIF
	RETURN
	END
