	SUBROUTINE DINITP  ( iret )
C************************************************************************
C* DINITP								*
C*									*
C* This subroutine is the first subroutine called by any applications	*
C* program using GEMPLT.						*
C*									*
C* DINITP ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* M. desJardins/NMC	 1/92	Return code missing from calling seq	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	CALL HINITP  ( iret )
C*
	RETURN
	END
