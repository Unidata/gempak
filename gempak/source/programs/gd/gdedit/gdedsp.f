	SUBROUTINE GDEDSP  ( time, level, ivcord, parm, iret )
C************************************************************************
C* GDEDSP 								*
C*									*
C* This subroutine allows the user to accept a grid in GDEDIT.		*
C*									*
C* GDEDSP  ( TIME, LEVEL, IVCORD, PARM, IRET )				*
C*									*
C* Input parameters:							*
C*	TIME  (2)	CHAR*		Grid time			*
C*	LEVEL (2)	INTEGER		Grid levels			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	PARM		CHAR*		Parameter name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   3 = user entered "EXIT"	*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/88						*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C************************************************************************
	CHARACTER*(*)	time  (*), parm
	INTEGER		level (*)
	LOGICAL		respnd
C------------------------------------------------------------------------
	iret = 0
C
C*	Write name of grid to terminal.
C
	WRITE  ( 6, 1001 )
1001	FORMAT ( // ' GDEDIT PARAMETERS: ' / 
     +              ' Grid to be added: ' )
C
C*	Write the grid identifier.
C
	CALL GR_WTRM ( 6, .true., -1, time, level, ivcord, parm, ier )
C
C*	Give user a change to respond.
C
	CALL IP_RESP ( respnd, ier)
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = 3
	END IF
C*
	RETURN
	END
