	SUBROUTINE GDTINP  ( gdfile, garea, gdattim, gvcord, gfunc,
     +				topofl, ijskip, iret )
C************************************************************************
C* GDTINP								*
C*									*
C* This subroutine gets the input parameters for GDTOPO from the TAE.	*
C*									*
C* GDTINP  ( GDFILE, GAREA, GDATTIM, GVCORD, GFUNC, TOPOFL, IJSKIP, 	*
C*		IRET )							*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	11/91						*
C************************************************************************
	CHARACTER*(*)	gdfile, garea, gdattim, gvcord, gfunc, topofl, 
     +			ijskip
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GAREA',   garea,   ier2 )
	CALL IP_STR  ( 'GDATTIM', gdattim, ier3 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier4 )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier5 )
	CALL IP_STR  ( 'TOPOFL',  topofl,  ier6 )
	CALL IP_STR  ( 'IJSKIP',  ijskip,  ier7 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
