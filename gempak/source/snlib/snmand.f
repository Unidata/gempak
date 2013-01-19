	SUBROUTINE SN_MAND  ( isnfln, mandat, iret )
C************************************************************************
C* SN_MAND								*
C*									*
C* This subroutine allows the user to set a flag requesting that only	*
C* mandatory data be returned when the upper-air dataset is an 		*
C* unmerged file.  The default is to merge all data.			*
C*									*
C* SN_MAND  ( ISNFLN, MANDAT, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	MANDAT		LOGICAL		Only mandatory data flag	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sncmn.cmn'
C*
	LOGICAL		mandat
C------------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set the mandatory flag.
C
	manflg ( isnfln ) = mandat
C*
	RETURN
	END
