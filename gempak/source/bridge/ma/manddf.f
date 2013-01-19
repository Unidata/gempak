        SUBROUTINE  MA_NDDF ( marrpt, ipt, iret )
C************************************************************************
C* MA_NDDF                                                              *
C*                                                                      *
C* This subroutine decodes the Nddff group.  This group contains the    *
C* total cloud cover and wind direction and speed parameters.           *
C*                                                                      *
C* MA_NDDF  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         	CHAR*           Report array                    *
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER 	On input, points to start of    *
C*					Nddff group; on output, to end  *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRCFRT) 	REAL            Total cloud cover in oktas      *
C*      RIVALS(IRDRCT)  REAL            Wind direction in degrees       *
C*      RIVALS(IRSPED) 	REAL            Wind speed in meters/second     *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP      8/96    Set wind direction to missing, if code  *
C*                              figure is 99                            *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*				header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface, set wind direction   *
C*                              to 99 if code figure is 99              *
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C
        CHARACTER*(*)    marrpt
C
        CHARACTER        fld2*2, fld1*1
C------------------------------------------------------------------------
        iret   = 0
        wdrct  = RMISSD
	ival   = IMISSD
C
C*      There is at most one space before start of group.
C
        IF ( marrpt ( ipt:ipt ) .eq. ' ' ) ipt = ipt + 1
C
C*      Check that the group exists.
C         
        iend = isec1 + lsec1 - 1
        IF ( ( ipt + 4 ) .gt. iend ) THEN
            iret = 1
            RETURN
        END IF
C
C*      Get total cloud cover.  WMO Table 2700 value.
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( ircfrt ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
C*      Get wind direction in degrees.
C
        IF ( marrpt ( ipt:ipt ) .ne. '/' .and.
     +       marrpt ( ipt:ipt+1 ) .ne. '/' ) THEN
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) THEN
                IF ( ival .ge. 0 .and. ival .le. 36 ) THEN
                    wdrct = 10. * FLOAT ( ival ) 
                  ELSE IF ( ival .eq. 99 ) THEN
C
C*		    Direction is variable, or all directions (WMO Code
C*		    Table 0877).
C
		    wdrct = 99.
                END IF
            END IF
        END IF
C
C*      Get wind speed in meters/second.
C
        ipt    = ipt + 2
        iparam = 0
        CALL  MA_WSPD  ( marrpt, iparam, ipt, iret )
C
C*	If direction is variable but speed is 0, set direction missing.
C
	IF ( ival .eq. 99 .and. rivals ( irsped ) .eq. 0.0 )
     +       wdrct = RMISSD
C
	rivals ( irdrct ) = wdrct
C*
        RETURN
        END
