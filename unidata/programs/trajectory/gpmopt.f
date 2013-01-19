	SUBROUTINE GPMOPT ( device, proj, garea, map, title, panel,
     +                      latlon, clear, gdattim, gdfile, gvect, 
     +			    glevel, gvcord, backtraj, tstep, iret )
C************************************************************************
C* GPMOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* GPMOPT ( DEVICE, PROJ, GAREA, MAP, TITLE, PANEL, LATLON, CLEAR, 	*
C*          GDATTIM, GDFILE, GVECT, GLEVEL, GVCORD, BACKTRAJ, TSTEP,	*
C*	    IRET )							*
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
C*	GDATTIM		CHAR*		Grid Time range			*
C*	GDFILE		CHAR*		Grid File			*
C*	GVECT		CHAR*		Grid advection vector		*
C*	GLEVEL		CHAR*		Grid level			*
C*	GVCORD		CHAR*		Grid vertical coordinate	*
C*	BACKTRAJ	LOGICAL		Trajectory direction		*
C*	TSTEP		CHAR*		Trajectory time step		*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C**									*
C* Log:									*
C* S. Chiswell/Unidata	 8/99	Created					*
C************************************************************************
	CHARACTER*(*)	device, proj, garea, map, title, panel, latlon,
     +			gdattim, gdfile, gvect, glevel, gvcord, tstep
	LOGICAL		clear, backtraj
C*
	LOGICAL		respnd
	CHARACTER	clr*3, btraj*3
C*
C------------------------------------------------------------------------
C*
	iret = 0
	IF  ( clear )  THEN
	    clr = 'YES'
	ELSE
	    clr = 'NO'
        END IF

	IF  ( backtraj )  THEN
	    btraj = 'YES'
	ELSE
	    btraj = 'NO'
        END IF
	WRITE ( 6, 5000) device, proj, garea, map, title, panel, 
     +                   latlon, gdattim, gdfile, gvect, glevel, 
     +			 gvcord, btraj, tstep, clr
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
     +           ' Grid time range:     ', A,/
     +           ' Grid file:           ', A,/
     +           ' Grid Vector:         ', A,/
     +           ' Grid level:          ', A,/
     +           ' Vertical Coordinate: ', A,/      
     +           ' Back Trajectory:     ', A,/      
     +           ' Time Step:           ', A,/      
     +           ' Clear:               ', A)
C*
	RETURN
	END
