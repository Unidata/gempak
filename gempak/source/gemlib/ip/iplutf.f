	SUBROUTINE IP_LUTF  ( lutfil, iret )
C************************************************************************
C* IP_LUTF								*
C*									*
C* This subroutine applies a new LUT (LookUpTable) to a displayed	*
C* image.								*
C*									*
C* IP_LUTF  ( LUTFIL, IRET )						*
C*									*
C* Input parameters:							*
C*	LUTFIL		CHAR*		Input LUT name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C**									*
C* Log:									*
C* J. Cowie/COMET	12/94	original				*
C* S. Jacobs/NMC	 8/95	Removed ITYPE from call sequence	*
C* K. Tyle/GSC		 8/96	Renamed from NT_LUTF			*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	lutfil
C------------------------------------------------------------------------
	iret = 0
C
C*	Remove any leading spaces on the LUT filename, then apply the
C*	LUT to the current image.
C
	CALL ST_LDSP ( lutfil, lutfil, ilen, ier )
	CALL IM_LUTF ( lutfil, ier )
C
	CALL GEPLOT ( ier )
C*
	RETURN
	END
