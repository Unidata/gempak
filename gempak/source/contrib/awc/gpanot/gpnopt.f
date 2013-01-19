	SUBROUTINE GPNOPT ( device, gdfile, satfil, radfil, proj, 
     +                      garea, panel, clear, shape, info, loci, 
     +                      line, iret )
C************************************************************************
C* GPNOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* GPNOPT ( DEVICE, GDFILE, SATFIL, RADFIL, PROJ, GAREA, PANEL, CLEAR,  *
C*          SHAPE, INFO	LOCI, FILL, IRET )				*
C*									*
C* Input Parameters:							*
C*	DEVICE		CHAR*		Device				*
C*	GDFILE		CHAR*		Grid file			*
C*	PROJ		CHAR*		Map projection			*
C*	GAREA		CHAR*		Graphics area			*
C*	PANEL		CHAR*		Panel input			*
C*	CLEAR		LOGICAL		Clear option flag		*
C*	SHAPE		CHAR*		Shape to plot			*
C*	INFO		CHAR*		Plot info about the shape	*
C*	LOCI		CHAR*		Point(s) associated with shape	*
C*	FILL		CHAR*		Fill input for shape		*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	12/91						*
C************************************************************************
	CHARACTER*(*)	device, gdfile, satfil, radfil, proj, garea,
     +			panel, shape, info, loci, line
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
	WRITE ( 6, 5000) device, gdfile, satfil, radfil, proj, garea,
     +			 panel, clr, shape, info, loci, line
C*
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd ) THEN
	    CALL TM_ACCP ( ier )
	    IF ( ier .eq. 2 ) iret = -1
	END IF
C*
5000	FORMAT ( ' GPANOT PARAMETERS:',//
     +           ' Device:           ', A,/
     +           ' Grid File:        ', A,/
     +           ' Satellite File:   ', A,/
     +           ' Radar File:       ', A,/
     +           ' Projection:       ', A,/
     +           ' Graphics Area:    ', A,/
     +           ' Panel:            ', A,/
     +           ' Clear:            ', A,//
     +           ' Shape:            ', A,/
     +           ' Shape Info:       ', A,/
     +           ' Shape Points:     ', A,/
     +           ' Line:             ', A )
C*
	RETURN
	END
