	SUBROUTINE G2T_GSZONE ( nz, ns, iret )
C************************************************************************
C* G2T_GSZONE								*
C*									*
C* This subroutine gets the number of subzones for the NZth zone area.	*
C*									*
C* G2T_GSZONE ( NZ, NS, IRET )						*
C*									*
C* Input parameters:							*
C*	NZ		INTEGER		Zone number			*
C*									*
C* Output parameters:							*
C*	NS		INTEGER		Number of subzones in zone area	*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C************************************************************************
	INCLUDE		'goftxt.cmn'
C------------------------------------------------------------------------
	iret = 0
C
C*	Retrieve zone info from the common.
C
	ns = nsubzn ( nz )
C
	RETURN
	END
