	SUBROUTINE GPINP  ( proj, gdarea, kxky, grdnam, gdfile, glevel, 
     +                      gvcord, gdattim, cpyfil, maxgrd, imgfil,
     +			    imgtim, calimg, iret )
C************************************************************************
C* GPINP								*
C*									*
C* This subroutine gets the input for IMG2GD.				*
C*									*
C* GPINP  ( PROJ, GRDAREA, KXKY, GRDNAM, GDFILE, GLEVEL, GVCORD,	*
C*	  GDATTIM, CPYFIL, MAXGRD, IMGFIL, CALIMG, IRET)		*
C**									*
C* Log:									*
C* Chiz/Unidata	 4/07	Developed					*
C************************************************************************
	CHARACTER*(*)	proj, gdarea, kxky, grdnam, gdfile, glevel,
     +			gvcord, gdattim, cpyfil, maxgrd, imgfil, imgtim
	LOGICAL		calimg
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'PROJ',    proj,   ier1 )
	CALL IP_STR  ( 'GRDAREA', gdarea, ier2 )
	CALL IP_STR  ( 'KXKY',    kxky,   ier3 )
	CALL IP_STR  ( 'GRDNAM',  grdnam, ier4 )
	CALL IP_STR  ( 'GDFILE',  gdfile, ier5 )
	CALL IP_STR  ( 'GLEVEL',  glevel, ier6 )
	CALL IP_STR  ( 'GVCORD',  gvcord, ier7 )
	CALL IP_STR  ( 'GDATTIM', gdattim,ier8 )
	CALL IP_STR  ( 'CPYFIL',  cpyfil, ier9 )
	CALL IP_STR  ( 'MAXGRD',  maxgrd, ier10 )
	CALL IP_STR  ( 'IMGFIL',  imgfil, ier11 )
	CALL IP_STR  ( 'IMGTIM',  imgtim, ier12 )
	CALL IP_LOG  ( 'CALIMG',  calimg, ier13 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 
     +		+ ier9 + ier10 + ier11 + ier12 + ier13
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
