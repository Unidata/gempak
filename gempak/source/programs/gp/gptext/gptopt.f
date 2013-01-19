	SUBROUTINE GPTOPT ( device, txtfil, txtloc, panel, column,
     +			    clear, iret )
C************************************************************************
C* GPTOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* GPTOPT ( DEVICE, TXTFIL, TXTLOC, PANEL, COLUMN, CLEAR, IRET )	*
C*									*
C* Input Parameters:							*
C*	DEVICE		CHAR*		Device				*
C*	TXTFIL		CHAR*		Text file			*
C*	TXTLOC		CHAR*		Text location in Normal coords	*
C*	PANEL		CHAR*		Panel input			*
C*	COLUMN		CHAR*		Number of columns		*
C*	CLEAR		LOGICAL		Clear option flag		*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C**									*
C* Log:									*
C* T. Lee/GSC		10/97						*
C* S. Jacobs/NCEP	10/97	Removed TITLE; Added TXTLOC		*
C* I. Durham/GSC	01/98	Added COLUMN				*
C************************************************************************
	CHARACTER*(*)	device, txtfil, txtloc, panel, column
	LOGICAL		clear
C*
	LOGICAL		respnd
	CHARACTER	clr*3
C*
C------------------------------------------------------------------------
C*
	iret = 0
	IF  ( clear )  THEN
	    clr = 'YES'
	ELSE
	    clr = 'NO'
        END IF
	WRITE ( 6, 5000) device, txtfil, txtloc, panel, column, clr
C*
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd ) THEN
	    CALL TM_ACCP ( ier )
	    IF ( ier .eq. 2 ) iret = -1
	END IF
C*
5000	FORMAT ( ' GPTEXT PARAMETERS:',//
     +           ' Device:        ', A,/
     +           ' Text file:     ', A,/
     +           ' Text location: ', A,/
     +           ' Panel:         ', A,/
     +           ' Num Columns    ', A,/
     +           ' Clear:         ', A )      
C*
	RETURN
	END
