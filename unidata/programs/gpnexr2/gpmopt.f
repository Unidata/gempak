	SUBROUTINE GPMOPT ( device, proj, garea, map, title, panel,
     +                      latlon, clear, imgfil, numimg, radparm,
     +			    tilt, iret )
C************************************************************************
C* GPMOPT								*
C*									*
C* This subroutine displays a summary of the users options.		*
C*									*
C* GPMOPT ( DEVICE, PROJ, GAREA, MAP, TITLE, PANEL, LATLON, CLEAR, 	*
C*          IMGFIL, IRET )						*
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
C*	IMGFIL		CHAR*		Image file name			*
C*	RADPARM		CHAR*		Radar parameter		*
C*	TILT		CHAR*		Sweep tilt number		*
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
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	device, proj, garea, map, title, panel, latlon,
     +			imgfil(numimg), tilt, radparm
	LOGICAL		clear
	INTEGER		numimg
C*
	LOGICAL		respnd
C*
	CHARACTER	clr*3, cstr*(LLMXLN)
C*
C------------------------------------------------------------------------
C*
	iret = 0
	IF  ( clear )  THEN
	    clr = 'YES'
	ELSE
	    clr = 'NO'
        END IF
C
        CALL ST_LCUC ( tilt, tilt, ier )
C
	WRITE ( 6, 5000) device, proj
	WRITE ( 6, 5002) garea, map, title, panel, 
     +                   latlon, clr
C
        DO ifile = 1, numimg
	    WRITE ( 6, 5001) imgfil(ifile)
C
            IF ( tilt(1:4) .eq. 'LIST' ) THEN
                CALL ST_LSTR ( imgfil(ifile), lenfil, ier )
                CALL ST_LSTR ( radparm, lenparm, ier )
                CALL nexr_list_lev ( imgfil(ifile), lenfil,
     +                               radparm, lenparm, ier )
            END IF
        END DO
C*
	CALL IP_RESP ( respnd, ier )
	IF ( tilt(1:4) .eq. 'LIST' ) THEN
	    IF ( .not. respnd ) THEN
		iret = -1
	    ELSE
	        CALL TM_STR ( 'Enter radar elevation angle', .false., 
     +				.false., cstr, ier)
	    	IF ( ier .eq. 0 ) THEN
		    tilt = cstr
	        ELSE
		    iret = -1
	        END IF
	    END IF
        ELSE
	    IF  ( respnd ) THEN
	        CALL TM_ACCP ( ier )
	        IF ( ier .eq. 2 ) iret = -1
	    END IF
	END IF
C*
5000	FORMAT ( ' GPMAP PARAMETERS:',//
     +           ' Device:              ', A,/
     +           ' Projection:          ', A )

5001	FORMAT ( ' Image file:          ', A )

5002	FORMAT ( ' Graphics area name:  ', A,/
     +           ' Map:                 ', A,/
     +           ' Title:               ', A,/
     +           ' Panel:               ', A,/
     +           ' Latlon:              ', A,/
     +           ' Clear:               ', A )      
C*
	RETURN
	END
