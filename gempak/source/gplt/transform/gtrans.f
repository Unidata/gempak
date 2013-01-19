	SUBROUTINE GTRANS  ( sysin, sysout, np, xin, yin, xout, yout, 
     +			     iret )	
C************************************************************************
C* GTRANS								*
C* 									*
C* This subroutine transforms an array of points in any input 		*
C* coordinate system into the specified output coordinate system.  	*
C* In the 'M' coordinate system, X represents latitude, and Y 		*
C* represents longitude.						*
C* 									*
C* GTRANS  ( SYSIN, SYSOUT, NP, XIN, YIN, XOUT, YOUT, IRET )		*
C* 									*
C* Input parameters:							*
C*	SYSIN		CHAR*		Input coordinate system		*
C*					  'S' = screen coordinates	*
C*					  'D' = device coordinates	*
C*					  'N' = normalized coordinates	*
C*					  'V' = view coordinates	*
C*					  'P' = plot coordinates	*
C*					  'L' = linear proj plane coords*
C*					  'U' = linear proj plane-unrot *
C*					  'W' = rotated map coords	*
C*					  'M' = map coordinates		*
C*					  'Q' = rotated grid map coords *
C*					  'I' = linear proj plane coords*
C*					  'G' = grid coordinates	*
C*	SYSOUT		CHAR*		Output coordinate system	*
C*					  SDNVPLWMQIG as defined above	*
C*	NP		INTEGER		Number of points		*
C* 	XIN (NP)	REAL		X input coordinates/latitudes	*
C* 	YIN (NP)	REAL		Y input coordinates/longitudes	*
C* 									*
C* Output parameters:							*
C* 	XOUT (NP)	REAL		X output coordinates/latitudes	*
C* 	YOUT (NP)	REAL		Y output coordinates/longitudes	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 1/85	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 4/87	Fixed so input arrays will not be	*
C*				changed on output			*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* M. desJardins/GSFC	 6/88	Added I coordinates			*
C* K. Brill/EMC		 3/96	Changes for general rotated projections *
C* K. Brill/EMC		 6/96	Check for igmode=1 for CALL PRNLON	*
C* S. Jacobs/NCEP	 3/97	Added check for VG driver and D coord	*
C* C. Lin/EAI	 	 6/97	Modified for adding 'S' in sysup	*
C* C. Lin/EAI	 	 6/97	Added 'S' coordinate transformation	*
C* S. Gilbert/NCEP	 8/06	Added 'U' coordinate transformation	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	REAL    	xin (*), yin (*), xout (*), yout (*)
	CHARACTER*(*) 	sysin, sysout
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check for VG driver and input in Device coordinates.
C
	IF  ( ( ddev .eq. 'VG' ) .and.
     +	      ( ( sysin  .eq. 'D' ) .or. ( sysin  .eq. 'd' ) .or.
     +		( sysout .eq. 'D' ) .or. ( sysout .eq. 'd' ) ) )  THEN
	    iret = NOCORD
	    RETURN
	END IF
C
C*	Check the coordinate systems.
C
	indin  =  INDEX ( sysup, sysin  ) + INDEX ( syslo, sysin  )
	indout =  INDEX ( sysup, sysout ) + INDEX ( syslo, sysout )
C
C*	Check for valid coordinate systems.
C
	IF  ( ( indin .eq. 0 ) .or. ( indout .eq. 0 ) ) THEN
	    iret = NOCORD
	    RETURN
	END IF
C
C*	Move input values into output arrays.
C
	DO  i = 1, np
	    xout (i) = xin (i)
	    yout (i) = yin (i)
	END DO
C
C*	Check first for coordinate systems going from D --> G.
C
	IF  ( indin .le. indout ) THEN
C
C*          Translate from S --> D if required.
C
            IF  ( ( indin .eq. 1 ) .and. ( indout .gt. 1 ) ) THEN
                DO  i = 1, np
                    IF  ( ( .not. ERMISS ( xout (i) ) ) .and.
     +                    ( .not. ERMISS ( yout (i) ) ) )  THEN
                        xout (i) =  xout (i) + ixos
                        yout (i) =  yout (i) + iyos
                    END IF
                END DO
            END IF
C
C*	    Translate from D --> N if required.
C
	    IF  ( ( indin .le. 2 ) .and. ( indout .gt. 2 ) ) THEN
		DO  i = 1, np
		    IF  ( ( .not. ERMISS ( xout (i) ) ) .and.
     +			  ( .not. ERMISS ( yout (i) ) ) )  THEN
			xout (i) = ( xout (i) - andx0 ) / andx1
			yout (i) = ( yout (i) - andy0 ) / andy1
		    END IF
		END DO
	    END IF
C
C*	    Translate from N --> L if required.  
C
	    IF  ( ( indin .le. 5 ) .and. ( indout .gt. 5 ) ) THEN
		DO  i = 1, np
		    IF  ( ( .not. ERMISS ( xout (i) ) ) .and.
     +			  ( .not. ERMISS ( yout (i) ) ) )  THEN
			xout (i) = ( xout (i) - alpx0 ) / alpx1
			yout (i) = ( yout (i) - alpy0 ) / alpy1
		    END IF
		END DO
	    END IF
C
C*	    Translate from L -- > U if required.
C
	    IF  ( ( indin .le. 6 ) .and. ( indout .gt. 6 ) ) THEN
		CALL GLTOU  ( np, xout, yout, xout, yout, iret ) 
 		IF ( indout .eq. 7 .and. igmode .eq. 1 ) THEN
 		    CALL PRNLON ( np, yout, ier )
 		END IF
	    END IF
C
C*	    Translate from U -- > W if required.
C
	    IF  ( ( indin .le. 7 ) .and. ( indout .gt. 7 ) ) THEN
		CALL GUTOW  ( np, xout, yout, xout, yout, iret ) 
		IF ( indout .eq. 8 .and. igmode .eq. 1 ) THEN
		    CALL PRNLON ( np, yout, ier )
		END IF
	    END IF
C
C*	    Translate from W -- > M if required.
C
	    IF  ( ( indin .le. 8 ) .and. ( indout .gt. 8 ) ) THEN
C
C*		Check for general rotation.
C
		IF ( mtmtyp .ge. 2 .and. igmode .eq. 1 ) THEN
		    CALL GWTOM  ( np, xout, yout, xout, yout, iret )
		END IF
		IF ( indout .eq. 9 .and. igmode .eq. 1 ) THEN
		    CALL PRNLON ( np, yout, ier )
		END IF
	    END IF
C
C*	    Translate from M --> Q if necessary.
C
	    IF  ( ( indin .le. 9 ) .and. ( indout .gt. 9 ) .and.
     +		  ( iret .eq. NORMAL ) ) THEN
C
C*		Check for general rotation.
C
		IF ( jtmtyp .ge. 2 .and. igmode .eq. 1 ) THEN
		    CALL GMTOQ  ( np, xout, yout, xout, yout, iret )
		END IF
		IF ( indout .eq. 10 .and. igmode .eq. 1 ) THEN
		    CALL PRNLON ( np, yout, ier )
		END IF
	    END IF
C
C*	    Translate from Q --> I if necessary.
C
	    IF  ( ( indin .le. 10 ) .and. ( indout .gt. 10 ) .and.
     +		  ( iret .eq. NORMAL ) ) THEN
		CALL GQTOI  ( np, xout, yout, xout, yout, iret )
	    END IF
C
C*	    Translate from I --> G if necessary.
C
	    IF  ( ( indin .le. 11 ) .and. ( indout .gt. 11 ) .and.
     +		  ( iret .eq. NORMAL ) ) THEN
		CALL GITOG  ( np, xout, yout, xout, yout, iret )
	    END IF
C
C*	    Now take care of the cases where the coodinate systems are
C*	    decreasing, ie. going from G-->D.
C
	  ELSE
C
C*	    Translate from G --> I  if required.
C
	    IF  ( indin .eq. 12 ) THEN
		CALL GGTOI  ( np, xout, yout, xout, yout, iret )
	    END IF
C
C*	    Translate from I --> Q  if required.
C
	    IF  ( ( indin .ge. 11 ) .and. ( indout .lt. 11 ) .and.
     +		  ( iret .eq. NORMAL ) ) THEN
		CALL GITOQ  ( np, xout, yout, xout, yout, iret )
		IF ( indout .eq. 10 .and. igmode .eq. 1 ) THEN
		    CALL PRNLON ( np, yout, ier )
		END IF
	    END IF
C
C*	    Translate from Q --> M  if required.
C
	    IF  ( ( indin .ge. 10 ) .and. ( indout .lt. 10 ) .and.
     +		  ( iret .eq. NORMAL ) ) THEN
C
C*		Check for simple longitudinal rotation about z.
C
		IF ( jtmtyp .ge. 2 .and. igmode .eq. 1 ) THEN
		    CALL GQTOM  ( np, xout, yout, xout, yout, iret )
		END IF
		IF ( indout .eq. 9 .and. igmode .eq. 1 ) THEN
		    CALL PRNLON ( np, yout, ier )
		END IF
	    END IF
C
C*	    Tranlate from M --> W if required.
C
	    IF  ( ( indin .ge. 9 ) .and. ( indout .lt. 9 ) .and.
     +		  ( iret .eq. NORMAL ) ) THEN
C
C*		Check for simple longitudinal rotation about z.
C
		IF ( mtmtyp .ge. 2 .and. igmode .eq. 1 ) THEN
		    CALL GMTOW  ( np, xout, yout, xout, yout, iret )
		END IF
		IF ( indout .eq. 8 .and. igmode .eq. 1 ) THEN
		    CALL PRNLON ( np, yout, ier )
		END IF
	    END IF
C
C*	    Tranlate from W --> U if required.
C
	    IF  ( ( indin .ge. 8 ) .and. ( indout .lt. 8 ) .and.
     +		  ( iret .eq. NORMAL ) ) THEN
		CALL GWTOU  ( np, xout, yout, xout, yout, iret )
	    END IF
C
C*	    Tranlate from U --> L if required.
C
	    IF  ( ( indin .ge. 7 ) .and. ( indout .lt. 7 ) .and.
     +		  ( iret .eq. NORMAL ) ) THEN
		CALL GUTOL  ( np, xout, yout, xout, yout, iret )
	    END IF
C
C*	    Translate from L -- > N if required.
C
	    IF  ( ( indin .ge. 6 ) .and. ( indout .lt. 6 ) ) THEN
		DO  i = 1, np
		    IF  ( ( .not. ERMISS ( xout (i) ) ) .and.
     +			  ( .not. ERMISS ( yout (i) ) ) )  THEN
			xout (i) = alpx1 * xout (i) + alpx0
			yout (i) = alpy1 * yout (i) + alpy0
		    END IF
		END DO
	    END IF
C
C*	    Translate from N --> D if necessary.
C
	    IF  ( ( indin .ge. 3 ) .and. ( indout .lt. 3 ) ) THEN
		DO  i = 1, np
		    IF  ( ( .not. ERMISS ( xout (i) ) ) .and.
     +			  ( .not. ERMISS ( yout (i) ) ) )  THEN
			xout (i) = andx1 * xout (i) + andx0
			yout (i) = andy1 * yout (i) + andy0
		    END IF
		END DO
	    END IF
C
C*          Translate from D --> S if necessary.
C
            IF  ( indout .eq. 1 ) THEN
                DO  i = 1, np
                    IF  ( ( .not. ERMISS ( xout (i) ) ) .and.
     +                    ( .not. ERMISS ( yout (i) ) ) )  THEN
                        xout (i) = xout (i) - ixos
                        yout (i) = yout (i) - iyos
                    END IF
                END DO
            END IF
	END IF
C*
	RETURN
	END
