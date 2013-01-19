	SUBROUTINE IPREDBOOK  ( iret )
C************************************************************************
C* IPREDBOOK								*
C*									*
C* This subroutine initializes the GEMPAK user interface.		*
C* IN_BDTA must already be called by dc_init				*
C*									*
C* IPREDBOOK  ( IRET )							*	
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*					  0 = normal return		*
C*					 -1 = too many variables	*
C**									*
C* Log:									*
C* S. Chiswell/Unidata	11/02	Copied from IP_INIT for "dc" environ	*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	LOGICAL		respnd
C------------------------------------------------------------------------
	iret   = 0
C
C*	Initialize common variables.
C
	ncparm = 0                                   
	cprogm = ' '
C
C* 	Set initial value of $MAPFIL.
C
	CALL IP_PUTV ( '$MAPFIL', 'mepowo.gsf', ierr )
C*
	RETURN
	END
