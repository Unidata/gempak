	SUBROUTINE GDCINP  ( gdfile, proj, gdarea, kxky, maxgrd,
     +			     cpyfil, anlyss, iret )
C************************************************************************
C* GDCINP								*
C*									*
C* This subroutine gets the input parameters for GDCFIL.		*
C*									*
C* GDCINP  ( GDFILE, PROJ, GDAREA, KXKY, MAXGRD, CPYFIL, ANLYSS, IRET )	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/88						*
C* K. Brill/GSC          4/90		Added ANLYSS			*
C************************************************************************
	CHARACTER*(*)	gdfile, proj, gdarea, kxky, maxgrd, cpyfil,
     +                  anlyss
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( 'GDOUTF', gdfile, ier1 )
	CALL IP_STR  ( 'PROJ',   proj,   ier2 )
	CALL IP_STR  ( 'GRDAREA', gdarea, ier3 )
	CALL IP_STR  ( 'KXKY',   kxky,   ier4 )
	CALL IP_STR  ( 'MAXGRD', maxgrd, ier5 )
	CALL IP_STR  ( 'CPYFIL', cpyfil, ier6 )
	CALL IP_STR  ( 'ANLYSS', anlyss, ier7 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
