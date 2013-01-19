	SUBROUTINE SN_CHKF ( isnfln, iret )
C************************************************************************
C* SN_CHKF								*
C*									*
C* This subroutine checks that the input sounding file number is valid.	*
C*									*
C* SN_CHKF  ( ISNFLN, IRET )						*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		File number to check		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
C------------------------------------------------------------------------
C*	Check that file number is in proper range and then see that a 
C*	file has been opened using that file number, i.e. the file
C*	number for the file is non-negative.
C
	iret = 0
	IF  ( ( isnfln .lt. 1) .or. ( isnfln .gt. MMFILE ) ) THEN
	    iret = -4
	  ELSE
	    IF  ( isndfn ( isnfln ) .lt. 0 ) iret = -4
	END IF
C*
	RETURN
	END
