        SUBROUTINE  MA_S4LL ( marrpt, ipt, iret )
C************************************************************************
C* MA_S4LL                                                              *
C*                                                                      *
C* This subroutine decodes the QL(a)L(a)L(a)L(a)L(a) and                *
C* L(o)L(o)L(o)L(o)L(o)L(o) groups in section 444 of the drifting buoy  *
C* report.  These two groups give the latitude/longitude postion of the *
C* second possible solution (symmetrical to the satellite subtrack).    *
C*                                                                      *
C* MA_S4LL ( MARRPT, IPT, IRET )                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         CHAR*     Report array                           *
C*					                                *
C* Input and output parameters:                                         *
C*	IPT	       INTEGER	 On input, points to a char. in group   *
C*                               preceding the group QLLLLL; on output, *
C*				 to first of last two characters decoded*
C* Output parameters:                                                   *
C*      RIVALS(IRDLAT) REAL      Latitude in degrees                    *
C*      RIVALS(IRDLON) REAL      Longitude in degrees                   *
C*      IRET           INTEGER   Return code                            *
C*                                 0 = normal return                    *
C*                                 1 = problems                         *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP       9/99                                           *
C************************************************************************
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER   	fld1*1
        LOGICAL         more
C------------------------------------------------------------------------
        iret  = 0
        more  = .true.
C
        ii = ipt + 4
C
        DO WHILE ( more )
            ipt = ipt + 1
            IF ( marrpt ( ipt:ipt ) .eq. ' ' ) THEN
                more = .false.
            ELSE IF ( ipt .ge. ii ) THEN
               RETURN
            END IF
        END DO
C
C*      Get quadrant of the globe.
C
        ip   = ipt + 1
        fld1 = marrpt ( ip:ip )
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) THEN
            iquad = ival
          ELSE
            iret  = 1
            RETURN
        END IF
C 
C*      Get latitude.
C
        lenm1 = 0
        ip    = ip + 1
        IF ( marrpt ( ip+3:ip+4 ) .eq. '//' ) THEN
C
C*          Latitude is reported in tenths of degrees.
C
            lenm1 = 2
            y     = .1
          ELSE IF ( marrpt ( ip+4:ip+4 ) .eq. '/' ) THEN
C
C*          Latitude is reported in hundredths of degrees.
C
            lenm1 = 3
            y     = .01
          ELSE 
C
C*          Latitude is reported in thousandths of degrees.
C
            lenm1 = 4
            y     = .001
        END IF
C
	CALL ST_INTG ( marrpt ( ip:ip+lenm1 ), ival, ier )
        IF ( ier .eq. 0 ) THEN
            xlat =  y * FLOAT ( ival ) 
          ELSE
            iret = 1
            RETURN
        END IF
C
        IF ( xlat .gt. 90.0 ) THEN
            iret = 1
            RETURN
        END IF
C
C*      Get longitude.
C
        lenm1 = 0
        ip    = ip + 6
C
        IF ( marrpt ( ip+4:ip+5 ) .eq. '//' ) THEN
C
C*          Longitude is reported in tenths of degrees.
C
            lenm1 = 3
            y     = .1
          ELSE IF ( marrpt ( ip+5:ip+5 ) .eq. '/' ) THEN
C
C*          Longitude is reported in hundredths of degrees.
C
            lenm1 = 4
            y     = .01
          ELSE 
C
C*          Longitude is reported in thousandths of degrees.
C
            lenm1 = 5
            y     = .001
        END IF
C
        CALL  ST_INTG ( marrpt ( ip:ip+lenm1 ), ival, ier )
        IF ( ier .eq. 0 ) THEN
            xlong = y * FLOAT ( ival )
          ELSE
            iret  = 1
            RETURN
        END IF
C
        IF ( xlong .gt. 180.0 ) THEN
            iret = 1
            RETURN
        END IF
C
        ipt = ip + 5
C
C*      Determine the sign of the lat/long from quadrant of globe.
C
        IF ( iquad .eq. 7 ) THEN
            xlong = -xlong
          ELSE IF ( iquad .eq. 5 ) THEN
            xlat  = -xlat
            xlong = -xlong
          ELSE IF ( iquad .eq. 3 ) THEN
            xlat  = -xlat
          ELSE IF ( iquad .eq. 1 ) THEN
          ELSE
            iret  = 1
            RETURN
        END IF
C 
        rivals ( irdlat ) = xlat
        rivals ( irdlon ) = xlong
C*
	RETURN
	END
