	SUBROUTINE GPMOPT ( device, proj, garea, map, title, panel,
     +                      latlon, clear, iret )
C************************************************************************
C* GPMOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* GPMOPT ( DEVICE, PROJ, GAREA, MAP, TITLE, PANEL, LATLON, CLEAR, 	*
C*          IRET )							*
C*									*
C* Input Parameters:							*
C*	DEVICE		CHAR*		Device				*
C*	PROJ		CHAR*		Projection			*
C*	GAREA		CHAR*		Graphics area name		*
C*	MAP		CHAR*		Map options			*
C*	TITLE		CHAR*		Title input			*
C*	PANEL		CHAR*		Panel input			*
C* 	LATLON		CHAR*		Latlon input			*
C*	CLEAR		LOGICAL		Clear option flag		*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C**									*
C* Log:									*
C* S. Schotz/GSC	 8/90	GEMPAK5					*
C* M. desJardins/NMC	 8/94	Fixed spelling error			*
C************************************************************************
	CHARACTER*(*)	device, proj, garea, map, title, panel, latlon
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
	WRITE ( 6, 5000) device, proj, garea, map, title, panel, 
     +                   latlon, clr
C*
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd ) THEN
	    CALL TM_ACCP ( ier )
	    IF ( ier .eq. 2 ) iret = -1
	END IF
C*
5000	FORMAT ( ' GPMAP PARAMETERS:',//
     +           ' Device:              ', A,/
     +           ' Projection:          ', A,/
     +           ' Graphics area name:  ', A,/
     +           ' Map:                 ', A,/
     +           ' Title:               ', A,/
     +           ' Panel:               ', A,/
     +           ' Latlon:              ', A,/
     +           ' Clear:               ', A )      
C*
	RETURN
	END
