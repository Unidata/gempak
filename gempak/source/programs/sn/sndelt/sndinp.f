	SUBROUTINE SNDINP  ( snfile, dattim, area, iret )
C************************************************************************
C* SNDINP								*
C*									*
C* This subroutine gets user input for SNDELT.				*
C*									*
C* SNDINP  ( SNFILE, DATTIM, AREA, IRET )				*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C************************************************************************
	CHARACTER*(*)	 snfile, dattim, area
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SNFILE', snfile, ier1 )
	CALL IP_STR  ( 'DATTIM', dattim, ier2 )
	CALL IP_STR  ( 'AREA',   area,   ier3 )
C
	ier = ier1 + ier2 + ier3
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN
	END
