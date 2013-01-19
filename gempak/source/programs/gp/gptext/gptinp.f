	SUBROUTINE GPTINP  ( device, color, panel, text, clear,
     +			     txtfil, txtloc, column, iret )
C************************************************************************
C* GPTINP								*
C*									*
C* This subroutine gets the input for GPTEXT.				*
C*									*
C* GPTINP  ( DEVICE, COLOR, PANEL, TEXT, CLEAR, TXTFIL, TXTLOC,		*
C*           COLUMN, IRET )						*
C**									*
C* Log:									*
C* T. Lee/GSC		10/97						*
C* S. Jacobs/NCEP	10/97	Removed TITLE; Added TXTLOC		*
C* I. Durham/GSC	01/98	Added COLUMN				*
C************************************************************************
	CHARACTER*(*)	device, color, panel, text, txtfil, txtloc
	CHARACTER*(*)	column
	LOGICAL		clear
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'DEVICE', device, ier1 )
	CALL IP_STR  ( 'COLORS', color,  ier2 )
	CALL IP_STR  ( 'PANEL',  panel,  ier3 )
	CALL IP_STR  ( 'TEXT',   text,   ier4 )
	CALL IP_LOG  ( 'CLEAR',  clear,  ier5 )
	CALL IP_STR  ( 'TXTFIL', txtfil, ier6 )
	CALL IP_STR  ( 'TXTLOC', txtloc, ier7 )
	CALL IP_STR  ( 'COLUMN', column, ier8 )
C
	iret =  ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
