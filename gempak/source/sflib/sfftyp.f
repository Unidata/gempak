	SUBROUTINE SF_FTYP  ( isffln, stndrd, iret )
C************************************************************************
C* SF_FTYP								*
C*									*
C* This subroutine determines whether a surface data file is standard	*
C* or non-standard.							*
C*									*
C* SF_FTYP  ( ISFFLN, STNDRD, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	STNDRD		LOGICAL		Standard file flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C**									*
C* Log:									*
C* S. Jacobs/NMC	10/94						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	LOGICAL		stndrd
C------------------------------------------------------------------------
C*	Check for an open file.
C
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check for a standard or non-standard file.
C*	A standard file will have date/time in ROWs and stations in COLs.
C*	A non-standard file will have both in COLs.
C
	stndrd = ( dttype (isffln) .ne. sttype (isffln) )
C*
	RETURN
	END
