	SUBROUTINE SECINP  ( satfil, outfil, garea, pixres, iret )
C************************************************************************
C* SECINP								*
C*									*
C* This subroutine gets the input parameters for SECTOR.		*
C*									*
C* SECINP  ( SATFIL, OUTFIL, GAREA, PIXRES, IRET )			*
C*									*
C* Output parameters:							*
C*	SATFIL		char*		Satellite file name		*
C*	OUTFIL		char*		Output file name		*
C*	GAREA		char*		Graphics area			*
C*	PIXRES		char*		Pixel resolution		*
C*	IRET		integer		Return sytatus			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 7/96						*
C************************************************************************
	CHARACTER*(*)	satfil, outfil, garea, pixres
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SATFIL', satfil, ier1 )
	CALL IP_STR  ( 'OUTFIL', outfil, ier2 )
	CALL IP_STR  ( 'GAREA',  garea,  ier3 )
	CALL IP_STR  ( 'PIXRES', pixres, ier4 )
	iret = ier1 + ier2 + ier3 + ier4
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
