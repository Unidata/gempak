        SUBROUTINE  MA_DDFF( marrpt, ipt, iret )
C************************************************************************
C* MA_DDFF                                                              *
C*                                                                      *
C* This subroutine decodes the 0ddff group in the drifting buoy report. *
C* This group contains the wind direction and speed parameters.         *
C*                                                                      *
C* MA_DDFF  ( MARRPT, IPT, IRET ) 		                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
C*					                                *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         On input, points to start of    *
C*					first d in 0ddff group; on      *
C*					output, points to last f        *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRDRCT)  REAL            Wind direction in degrees       *
C*	RIVALS(IRSPED)	REAL		Wind speed in meters/sec        *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       8/96   Set wind direction to missing, if code  *
C*                              figure is 99                            *
C* D. Kidwell/NCEP 	 4/97	Removed interface calls, reorganized    *
C*				header and comments			*
C* D. Kidwell/NCEP	 4/97	Added parameter initialization		*
C* D. Kidwell/NCEP 	10/97	Changed interface, set wind direction   *
C*                              to 99 if code figure is 99              *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER   	fld2*2
C------------------------------------------------------------------------
        iret  = 0
	wdrct = RMISSD
C
        IF ( marrpt ( ipt:ipt+3 ) .eq. '////' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Get wind direction in degrees.
C
        fld2 = marrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )
        IF ( ier .eq. 0 ) THEN
            IF ( ival .ge. 0 .and. ival .le. 36 ) THEN
                wdrct = 10. * FLOAT ( ival ) 
	      ELSE IF ( ival .eq. 99 ) THEN
C
C*		Direction is variable, or all directions (WMO Code
C*		Table 0877).
C
		wdrct = 99.
	    END IF
        END IF
C
C*      Get wind speed in meters/second.
C
        ipt    = ipt + 2
        iparam = 0
        CALL  MA_WSPD  ( marrpt, iparam, ipt, jret )
C
C*	If direction is variable but speed is 0, set direction missing.
C
	IF ( ival .eq. 99 .and. rivals ( irsped ) .eq. 0.0 ) 
     +	     wdrct = RMISSD
C
	rivals ( irdrct ) = wdrct
C*
        RETURN
        END
