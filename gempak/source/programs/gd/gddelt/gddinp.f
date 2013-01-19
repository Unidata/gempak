	SUBROUTINE GDDINP  ( gdfile, gvcord, glevel,
     +			     gdattim, gfunc, iret )
C************************************************************************
C* GDDINP								*
C*									*
C* This subroutine gets the input parameters for GDDELT from the TAE.	*
C*									*
C* GDDINP  ( GDFILE, GVCORD, GLEVEL, GDATTIM, GFUNC, IRET )		*
C**									*
C* Log:									*
C* M. Goodman/RDS	10/85						*
C* M. desJardins/GSFC	 8/88	Cleaned up				*
C* D. McCann/AWC	 8/95	Added gvcord, glevel, gdattim, grdnam	*
C* S. Maxwell/GSC	10/96	Removed gdnum, grdnam; added gfunc	*
C************************************************************************
	CHARACTER*(*)	gdfile, gvcord, glevel, gdattim, gfunc
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier2 )
	CALL IP_STR  ( 'GLEVEL',  glevel,  ier3 )
	CALL IP_STR  ( 'GDATTIM', gdattim, ier4 )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier5 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
