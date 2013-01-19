	SUBROUTINE SF_CHKF  ( isffln, iret )
C************************************************************************
C* SF_CHKF								*
C*									*
C* This subroutine checks that the input surface file number 		*
C* corresponds to an open file.						*
C*									*
C* SF_CHKF  ( ISFFLN, IRET )						*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		File number to check		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = file not open 		*
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
	IF  ( ( isffln .lt. 1 ) .or. ( isffln .gt. MMFILE ) )  THEN
	    iret = -3
	  ELSE
	    IF  ( isfcfn (isffln) .lt. 0 ) iret = -3
	END IF
C*
	RETURN
	END
