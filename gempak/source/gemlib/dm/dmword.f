	SUBROUTINE DM_WORD  ( iword, irec, isword, iret )
C************************************************************************
C* DM_WORD								*
C*									*
C* This subroutine finds the record and start word within the record	*
C* for any word within a DM file.					*
C*									*
C* DM_WORD  ( IWORD, IREC, ISWORD, IRET )				*
C*									*
C* Input parameters:							*
C*	IWORD		INTEGER		Word number from start of file	*
C*									*
C* Output parameters:							*
C*	IREC		INTEGER		Record number			*
C*	ISWORD		INTEGER		Start word within record	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C------------------------------------------------------------------------
	iret   = 0
	irec   = ( iword - 1 ) / MBLKSZ + 1
	isword = iword - ( irec - 1 ) * MBLKSZ
C*
	RETURN
	END
