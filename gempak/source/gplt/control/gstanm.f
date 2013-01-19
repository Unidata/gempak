	SUBROUTINE GSTANM  ( iret )
C************************************************************************
C* GSTANM								*
C* 									*
C* This subroutine defines the start of a new animation sequence.       *
C* 									*
C* GSTANM  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* A. Chang/EAI	 	12/93						*
C* S. Jacobs/NMC	 3/94	Renamed GSPIXM to GSTANM		*
C* M. Linda/GSC		 2/97	Removed PLOTBF.CMN			*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
	IF  ( ddev .ne. ' ' )  THEN
	    CALL DSTANM  ( iret )
	END IF
C*
	RETURN
	END
