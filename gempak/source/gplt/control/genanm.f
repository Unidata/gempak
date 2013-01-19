	SUBROUTINE GENANM  ( iret )
C************************************************************************
C* GENANM								*
C* 									*
C* This subroutine ends an animation sequence.                          *
C* 									*
C* GENANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* A. Chang/EAI	 	12/93						*
C* S. Jacobs/NMC	 2/94	Renamed GEPIXM to GENANM		*
C* M. Linda/GSC		 2/97	Removed PLOTBF.CMN			*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
	IF  ( ddev .ne. ' ' )  THEN
	    CALL DENANM  ( iret )
	END IF
C*
	RETURN
	END
