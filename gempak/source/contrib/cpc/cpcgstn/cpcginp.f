	SUBROUTINE CPCGINP ( vgfile, dattim, stnfil, output, iret )
C************************************************************************
C* CPCGINP								*
C*									*
C* This subroutine gets the input variables for the CPCGSTN program.	*
C*									*
C* CPCGINP  ( VGFILE, DATTIM, STNFIL, OUTPUT, IRET )                	*
C**									*
C* Log:									*
C* M. Li/SAIC		08/01						*
C************************************************************************
	CHARACTER*(*)  vgfile, output, dattim, stnfil
C------------------------------------------------------------------------
	CALL IP_STR  ( 'VGFILE', vgfile, ier1 )
	CALL IP_STR  ( 'DATTIM', dattim, ier2 )
	CALL IP_STR  ( 'STNFIL', stnfil, ier3 )
	CALL IP_STR  ( 'OUTPUT', output, ier4 )
C*
	iret = ier1 + ier2 + ier3 + ier4
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
