	SUBROUTINE GPNINP ( device, gdfile, satfil, radfil, proj, 
     +                      garea, panel, clear, shape, info, loci,
     +                      ctype, line, iret )
C************************************************************************
C* GPNINP								*
C*									*
C* This subroutine gets the input for GPANOT.				*
C*									*
C* GPNINP  ( DEVICE, GDFILE, SATFIL, RADFIL, PROJ, GAREA, PANEL, CLEAR, *
C*	      SHAPE, INFO, LOCI, FILL, IRET )                           *
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C************************************************************************
	CHARACTER*(*)	device, gdfile, satfil, radfil, proj, garea,
     +			panel, shape, info, loci, ctype, line
	LOGICAL		clear
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'DEVICE', device, ier1  )
	CALL IP_STR  ( 'GDFILE', gdfile, ier2  )
	CALL IP_STR  ( 'SATFIL', satfil, ier3  )
	CALL IP_STR  ( 'RADFIL', radfil, ier4  )
	CALL IP_STR  ( 'PROJ',   proj,   ier5  )
	CALL IP_STR  ( 'GAREA',  garea,  ier6  )
	CALL IP_STR  ( 'PANEL',  panel,  ier7  )
	CALL IP_LOG  ( 'CLEAR',  clear,  ier8  )
	CALL IP_STR  ( 'SHAPE',  shape,  ier9  )
	CALL IP_STR  ( 'INFO',   info,   ier10 )
	CALL IP_STR  ( 'LOCI',   loci,   ier11 )
	CALL IP_STR  ( 'CTYPE',  ctype,  ier12 )
	CALL IP_STR  ( 'LINE',   line,   ier13 )
C
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
     +		ier8 + ier9 + ier10 + ier11 + ier12 + ier13
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
