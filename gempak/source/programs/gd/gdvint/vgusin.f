	SUBROUTINE VG_USIN  ( iret )
C************************************************************************
C* VG_USIN								*
C*									*
C* This subroutine gets the input parameters for the vertical interpo-	*
C* lation program VI.  The GEMPAK user interface is used.		*
C*									*
C* VG_USIN  ( IRET )							*
C*									*
C* Input parameters							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 -2 = invalid input		*
C**									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* K. Brill/NMC		08/92	Added area for subsetting		*
C* K. Brill/NMC		10/92	Added SFVC				*
C* G. Hull/SAIC         04/08   Use common variables instead of params  *
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GDOUTF',  gdoutf,  ier2 )
	CALL IP_STR  ( 'GDATTIM', gdattm, ier3 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier4 )
	CALL IP_STR  ( 'GLEVEL',  glevel, ier5 )
	CALL IP_STR  ( 'MAXGRD',  maxgrd, ier6 )
	CALL IP_STR  ( 'GAREA',   area, ier7 )
	CALL IP_STR  ( 'VCOORD',  vcoord, ier8 )

	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8

	IF  ( iret .ne. 0 )  iret = -2

	RETURN
	END
