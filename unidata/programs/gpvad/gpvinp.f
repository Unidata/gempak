	SUBROUTINE GPVINP  ( radfil, radtim, clear, text, 
     +			     panel, title, device, wind, clrbar, 
     +			     output, iret )
C************************************************************************
C* GPVINP								*
C*									*
C* This subroutine gets the input parameters for GPVAD.			*
C*									*
C* GPVAD  ( RADFIL, RADTIM, CLEAR, TEXT, PANEL,				*
C*	     TITLE, DEVICE, WIND, CLRBAR, OUTPUT, IRET )		*
C**									*
C* Log:									*
C************************************************************************
	CHARACTER*(*)	radfil, radtim, text, panel, 
     +			title, device, wind, clrbar, output
	LOGICAL		clear
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR ( 'RADFIL', radfil, ier1 )
	CALL IP_STR ( 'RADTIM', radtim, ier2 )
	CALL IP_LOG ( 'CLEAR',  clear,  ier3 )
	CALL IP_STR ( 'TEXT',   text,   ier4 )
	CALL IP_STR ( 'PANEL',  panel,  ier5 )
	CALL IP_STR ( 'TITLE',  title,  ier6 )
	CALL IP_STR ( 'DEVICE', device, ier7 )
	CALL IP_STR ( 'WIND',   wind,   ier8 )
	CALL IP_STR ( 'CLRBAR', clrbar, ier9 )
	CALL IP_STR ( 'OUTPUT', output, ier10 )
C*
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 +
     +	       ier9 + ier10
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
