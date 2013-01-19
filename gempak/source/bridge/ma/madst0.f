        SUBROUTINE  MA_DST0 ( marrpt, ipt, iret )
C************************************************************************
C* MA_DST0                                                              *
C*                                                                      *
C* This subroutine gets (1) the drifting buoy ID, (2) the report obs GMT*
C* time, (3) the indicator for the source and units of wind speed, (4)  *
C* the quadrant of the globe where the drifter is located, (5) the      *
C* latitude and longitude of the drifter, and (6) the quality control   *
C* indicators for position and time.  These data are in Section 0 of    *
C* the report.  Also, the routine stores the receipt time and obs time  *
C* of the report in the interface array.  Since the drifting buoy is    *
C* assumed to be in the ocean, its elevation is set to 0.0 meters.      *
C*                                                                      *
C* MA_DST0 ( MARRPT, IPT, IRET )                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         CHAR*             Report array                   *
C*      RCTIM (*)      REAL              Receipt date/time of bulletin  *
C*					                                *
C* Output parameters:                                                   *
C*	RIVALS(IRCORN) REAL		 Report correction indicator    *
C*      CIVALS(ICSTID) CHAR*             Report ID                      *
C*      RIVALS(IRBPID) REAL              Buoy platform ID               *
C*      IHOUR          INTEGER           Hour of observation of report  *
C*      IRPTDT (*)     INTEGER           Report date-time               *
C*                                       (YYYY, MM, DD, HH, MM)         *
C*      RIVALS(IRYEAR) REAL              Report year  -  YYYY           *
C*      RIVALS(IRMNTH) REAL              Report month -  MM             *
C*      RIVALS(IRDAYS) REAL              Report day   -  DD             *
C*      RIVALS(IRHOUR) REAL              Report hour  -  HH             *
C*      RIVALS(IRMINU) REAL              Report minute - MM             *
C*      RIVALS(IRSUWS) REAL              Indicator for source and units *
C*                                       of wind speed                  *
C*      RIVALS(IRSELV) REAL              Elevation of station in meters *
C*      RIVALS(IRSLAT) REAL              Latitude (coarse) in degrees   *
C*      RIVALS(IRSLON) REAL              Longitude (coarse) in degrees  *
C*      RIVALS(IRSLTH) REAL              Latitude (high) in degrees     *
C*      RIVALS(IRSLNH) REAL              Longitude (high) in degrees    *
C*      RIVALS(IRTOST) REAL              Type of station indicator      *
C*      IPT            INTEGER           Points to start of next group  *
C*                                       in marrpt                      *
C*      IRET           INTEGER           Return code                    *
C*                                         0 = Normal return            *
C*                                         1 = Problems                 *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       9/96   Added RCTS to receipt time data         *
C* R. Hollern/NCEP      10/96   Added corrected report indicator logic  *
C* R. Hollern/NCEP      12/96   Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP	 4/97	Changed interface and cleaned up code   *
C* D. Kidwell/NCEP	10/97	Changed interface, cleaned up code      *
C* R. Hollern/NCEP       7/99   Added call to MA_D0QC; added report     *
C*                              date/time and high accuracy lat/long    *
C* D. Kidwell/NCEP	 3/02	Check validity of iuwind                *
C* S. Chiswell/Unidata	 1/06	Fix year processing		 	*
C* C. Magee/NCEP	 1/06	Adjust year processing for special case *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
	CHARACTER	stnid*8, fld5*5, fld2*2, fld1*1
        LOGICAL  	more
C------------------------------------------------------------------------
        iret  = 0
        more  = .TRUE.
        stnid = ' '
C
C*      Get corrected report indicator.
C
        IF ( bbb (1:3) .eq. 'COR' ) THEN
            rivals ( ircorn ) = 1.0
          ELSE
            rivals ( ircorn ) = 0.0
        END IF
C
C*      Skip over ZZYY string.
C
        ip = 5
C
C*      Get drifter ID.
C
        DO WHILE (more)
C
            ip = ip + 1
            IF ( marrpt ( ip:ip ) .ne.' ' ) THEN
                stnid ( 1:5 ) = marrpt ( ip:ip+4 )
                ip = ip + 6
                more = .false.
            END IF
C
        END DO
C
        civals (icstid ) = stnid
C
C*      Convert character buoy ID to numeric.
C
        fld5 = stnid
        CALL  ST_INTG ( fld5, ival, ier )   
        IF ( ier .eq. 0 ) rivals ( irbpid ) = FLOAT ( ival )
C
C*      Set the elevation to 0.0 meters.       
C
        rivals ( irselv ) = 0.0
C
C*      Get day of month of observation.
C
        fld2 = marrpt ( ip:ip+1 )
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            ip = ip + 2
            IF ( ival .gt. 0 .and. ival .lt. 32 ) THEN
                imnday = ival
                irptdt ( 3 ) = ival
              ELSE
                ierrno = 7
                CALL MA_ERRS ( ierrno, marrpt, kret )
                iret = 1
                RETURN
            END IF
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get month of observation.
C
        fld2 = marrpt ( ip:ip+1 )
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            ip = ip + 2
            IF ( ival .ge. 1 .and. ival .le. 12 ) THEN
                irptdt ( 2 ) = ival
              ELSE
                ierrno = 6
                CALL MA_ERRS ( ierrno, marrpt, kret )
                iret = 1
                RETURN
            END IF
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get year of observation - (units digit of year only).
C
        fld1 = marrpt ( ip:ip )
        CALL  ST_INTG ( fld1, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            ip = ip + 2
C
C*	    Prevent report year from being greater than 1 more than
C*	    the specified clock year.
C*
C*	    Special case for a report time in 20[x]0, but a bulletin
C*	    time in 20[x-1]9.
C
	    iyrdif = ival - MOD ( INT ( rctim(2) ), 10 )
            IF ( iyrdif .gt. 1 ) THEN
                irptdt ( 1 ) = rctim(2) - 10 + iyrdif
              ELSE IF ( iyrdif .eq. -9 ) THEN
                irptdt ( 1 ) = rctim(2) + 10 + iyrdif
              ELSE
                irptdt ( 1 ) = rctim(2) + iyrdif
            END IF
          ELSE
            ierrno = 5
            CALL MA_ERRS ( ierrno, marrpt, kret )
            iret = 1
            RETURN
        END IF
C
C*      Get hour of observation.
C
        fld2 = marrpt ( ip:ip+1 )
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            ip = ip + 2
            IF ( ival .ge. 0 .and. ival .le. 23 ) THEN
                ihour = ival
                irptdt ( 4 ) = ival
              ELSE
                ierrno = 8
                CALL MA_ERRS ( ierrno, marrpt, kret )
                iret = 1
                RETURN
            END IF
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get minutes of hour of observation.
C
        fld2 = marrpt ( ip:ip+1 )
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            ip = ip + 2
            IF ( ival .ge. 0 .and. ival .le. 59 ) THEN
                imins = ival
                irptdt ( 5 ) = ival
              ELSE
                ierrno = 9
                CALL MA_ERRS ( ierrno, marrpt, kret )
                iret = 1
                RETURN
            END IF
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Save date/time data in interface array.
C
        rivals (iryear) = FLOAT ( irptdt (1) )
        rivals (irmnth) = FLOAT ( irptdt (2) )
        rivals (irdays) = FLOAT ( irptdt (3) )
        rivals (irhour) = FLOAT ( irptdt (4) )
        rivals (irminu) = FLOAT ( irptdt (5) )
C
C*      Get indicator of units of wind speed.
C
        fld1 = marrpt (ip:ip)
        CALL  ST_INTG ( fld1, iuwind, ier )
        IF ( ier .eq. 0 ) THEN
            IF  ( ( iuwind .eq. 0 ) .or. ( iuwind .eq. 1 ) .or.
     +            ( iuwind .eq. 3 ) .or. ( iuwind .eq. 4 ) )  THEN
                rivals ( irsuws ) = FLOAT ( iuwind )
            ELSE
                iuwind = IMISSD
            END IF
        END IF
C
C*      Get quadrant of the globe.
C
        ip = ip + 2
        fld1 = marrpt ( ip:ip )
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) THEN
            iquad = ival
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get latitude.
C
        lenm1 = 0
        ip = ip + 1
        IF ( marrpt ( ip+3:ip+4 ) .eq. '//' ) THEN
C
C*          Latitude is reported in tenths of degrees.
C
            lenm1 = 2
            y = .1
          ELSE IF ( marrpt ( ip+4:ip+4 ) .eq. '/' ) THEN
C
C*          Latitude is reported in hundredths of degrees.
C
            lenm1 = 3
            y = .01
          ELSE 
C
C*          Latitude is reported in thousandths of degrees.
C
            lenm1 = 4
            y = .001
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
        ip = ip + 6
C
        IF ( marrpt ( ip+4:ip+5 ) .eq. '//' ) THEN
C
C*          Longitude is reported in tenths of degrees.
C
            lenm1 = 3
            y = .1
          ELSE IF ( marrpt ( ip+5:ip+5 ) .eq. '/' ) THEN
C
C*          Longitude is reported in hundredths of degrees.
C
            lenm1 = 4
            y = .01
          ELSE 
C
C*          Longitude is reported in thousandths of degrees.
C
            lenm1 = 5
            y = .001
        END IF
C
        CALL  ST_INTG ( marrpt ( ip:ip+lenm1 ), ival, ier )
        IF ( ier .eq. 0 ) THEN
            xlong = y * FLOAT ( ival )
          ELSE
            iret = 1
            RETURN
        END IF
C
        IF ( xlong .gt. 180.0 ) THEN
            iret = 1
            RETURN
        END IF
C
        ip = ip + 6
C
C*      Determine the sign of the lat/long from quadrant of globe.
C
        IF ( iquad .eq. 7 ) THEN
            xlong = -xlong
          ELSE IF ( iquad .eq. 5 ) THEN
            xlat = -xlat
            xlong = -xlong
          ELSE IF ( iquad .eq. 3 ) THEN
            xlat = -xlat
          ELSE IF ( iquad .eq. 1 ) THEN
          ELSE
            iret = 1
            RETURN
        END IF
C 
        rivals ( irslat ) = xlat
        rivals ( irslon ) = xlong
C
C*      Get the quality control indicators for position and time.
C
        IF ( marrpt (ip:ip+1) .eq. ' 6' ) THEN
            ip = ip + 2
            CALL MA_D0QC ( marrpt, ip, iret )
        END IF
C
C*      Set type of station to automatic.
C
        rivals ( irtost ) = 0.0
C
        ipt = ip
C*
	RETURN
	END
