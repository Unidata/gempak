        SUBROUTINE  MA_BST0 ( marrpt, istarr, ipt, iret )
C************************************************************************
C* MA_BST0                                                              *
C*                                                                      *
C* This subroutine gets from the station report: the report ID which is *
C* the call ship sign or buoy ID, the report observational GMT time, the*
C* indicator for the source and units of wind speed, the station        *
C* latitude and longitude position, and the quadrant of the globe where *
C* the station is located.  These data are in section 0 of the report.  *
C* Also, the routine stores the receipt time in the interface array     *
C* and gets the station elevation.                                      *
C*                                                                      *
C* MA_BST0 ( MARRPT, ISTARR, IPT, IRET )                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         CHAR*             Report array                   *
C*      ISTARR (*)     INTEGER           System time - YYYY,MM,DD,HH,MM *
C*      RCTIM (*)      REAL              Receipt date/time of bulletin  *
C*					                                *
C* Output parameters:                                                   *
C*      CIVALS(ICSTID) CHAR*             Report ID                      *
C*      RIVALS(IRBPID) REAL              Buoy platform ID               *
C*      IFBUOY         INTEGER           Set to 0, if fixed buoy        *
C*                                       report; set to 1, if not       *
C*      IFLGCO         INTEGER           US/Canadian report flag        *
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
C*      RIVALS(IRSLAT) REAL              Latitude in degrees            *
C*      RIVALS(IRSLON) REAL              Longitude in degrees           *
C*      IPT            INTEGER           Points to start of next group  *
C*                                       in marrpt                      *
C*      IRET           INTEGER           Return code                    *
C*                                         0 = normal return            *
C*                                         1 = problems                 *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP       9/96   Added RCTS to receipt time data         *
C* R. Hollern/NCEP      10/96   Added corrected report indicator logic  *
C* R. Hollern/NCEP      12/96   Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP	 4/97	Changed interface, cleaned up code      *
C* D. Kidwell/NCEP	10/97	Changed interface, cleaned up code      *
C* R. Hollern/NCEP	12/97	Replaced MA_RTIM call with DC_RTIM      *
C* R. Hollern/NCEP	 7/99	Added date/time data to interface array *
C* R. Hollern/NCEP	 9/99	Moved station elev. call after lat/lon  *
C* J. Ator/NCEP         12/00   Check validity of iuwind                *
C* C. C. Magee/NCEP     05/01   Add code to check for SeaKeepers obs    *
C*                              (callsign of form SKxxx) and only keep  *
C*                              if bulletin originator is KWUM.         *
C* C. C. Magee/NCEP     06/01   Clean up code that sets lat/long based  *
C*                              on quadrant reported, and add comments. *
C* C. C. Magee/NCEP     06/01   Add code to check for 'TEST' within     *
C*                              callsigns and skip the report if found. *
C* C. C. Magee/NCEP     08/01   Change SeaKeepers callsign from 'SKxxx' *
C*                              to 'KSxxx'.                             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
        INTEGER      	istarr (*)
C*
        CHARACTER   	stnid*8, fld5*5, fld4*4, fld3*3, fld2*2, fld1*1
        LOGICAL  	more
C------------------------------------------------------------------------
        iret = 0
        more = .true.
        jp = 0
        ip = 0
        ifbuoy = 0
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
C*      Get ship or buoy ID.
C*      Ip points to start of ID. Locate next space, the end of ID.
C
        DO  WHILE ( more )
C
            ip = ip + 1
            IF ( marrpt ( ip:ip ) .ne. ' ' ) THEN
                jp = jp + 1
                IF ( jp .lt. 9 ) THEN
                    stnid ( jp:jp ) = marrpt ( ip:ip )
C
C*                  If ID is all numerics, then have fixed buoy report.
C
                    IF ( .not. ( marrpt (ip:ip) .ge. '0' .and. 
     +                           marrpt (ip:ip) .le. '9' ) ) ifbuoy = 1
                END IF
              ELSE
                more = .false.
            END IF
C
        END DO
C
C*      Check to see if callsign contains the characters 'TEST'.  If so,
C*      this is a test report which may contain invalid data, so skip it.
C
        IF ( INDEX ( stnid(1:8), 'TEST' ) .ne. 0 ) THEN
           iret = 1
           RETURN
        END IF
C
C*      Check to see if this is a SeaKeepers report (5 characters long and
C*      ID of form KSxxx, where xxx is 3 numbers).  If so, and if bulletin
C*      originator is not KWUM, then skip (don't decode) this report.  We
C*      only want SeaKeepers reports from KWUM, as those from Tinker (KAWN)
C*      or KWBC (which may be forwarding Tinker obs) may be corrupted.
C
        IF ( jp .eq. 5 .and. stnid (1:2) .eq. 'KS' ) THEN
           IF (( stnid (3:3) .ge. '0' .and. stnid (3:3) .le. '9' ) .and.
     +         ( stnid (4:4) .ge. '0' .and. stnid (4:4) .le. '9' ) .and.
     +         ( stnid (5:5) .ge. '0' .and. stnid (5:5) .le. '9' )) THEN
              IF ( orign(1:4) .ne. 'KWUM' ) THEN
                 iret = 1
                 RETURN
              END IF
           END IF
        END IF
C
        IF ( ifbuoy .eq. 0 ) THEN
C
C*          Check for a US or Canadian report.
C
            IF ( stnid (1:2) .ge. '41' .and. stnid (1:2) .le. '51' )
     +           iflgco = 1
        END IF
C
C*      Save report ID.
C   
        civals ( icstid ) = stnid
C
C*      If we have a fixed buoy ID (all numerics),
C*      convert the buoy ID character string to an integer.
C
        IF ( ifbuoy .eq. 0 ) THEN
            fld5 = stnid
            CALL  ST_INTG ( fld5, ival, ier )   
            IF ( ier .eq. 0 ) THEN
                rivals ( irbpid ) = FLOAT ( ival )
              ELSE
                iret = 1
                RETURN
            END IF
        END IF
C
C*      Get day of month of observation.
C
        ip = ip + 1
        fld2 = marrpt (ip:ip+1)
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            ip = ip + 2
            IF ( ival .gt. 0 .and. ival .lt. 32 ) THEN
                imnday = ival
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
C*      Get hour of observation.
C
        fld2 = marrpt (ip:ip+1)
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            ip = ip + 2
            IF ( ival .ge. 0 .and. ival .lt. 24 ) THEN
                ihour = ival
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
C*      Set minutes of obs hour to zero.
C
        imins = 0
C
C*      Combine the run times and obs times into a report date-time.
C
        ndays = 10
        CALL  DC_RTIM ( istarr, imnday, ihour, imins, ndays, irptdt,
     +                  jret )
C
        IF ( jret .ne. 0 ) THEN
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
        fld1 = marrpt ( ip:ip )
        CALL  ST_INTG ( fld1, iuwind, ier )
        IF ( ier .eq. 0 ) THEN
            ip = ip + 1
            IF  ( ( iuwind .eq. 0 ) .or. ( iuwind .eq. 1 ) .or.
     +            ( iuwind .eq. 3 ) .or. ( iuwind .eq. 4 ) )  THEN
            rivals ( irsuws ) = FLOAT ( iuwind )
          ELSE
                iuwind = IMISSD
            END IF
        END IF
C
C*      Get latitude from next group.
C
        IF ( marrpt ( ip:ip+2 )  .ne. ' 99' ) THEN
            iret = 1
            RETURN
        END IF
C
        ip = ip + 3
        fld3 = marrpt ( ip:ip+2 )
        CALL  ST_INTG ( fld3, ival, ier )
        rval = FLOAT ( ival )
        IF ( ier .eq. 0 .and. rval .le. 900. ) THEN
            ip = ip + 3
            xlat = .1 * rval
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      The next character is a space; if not, problems.
C
        IF ( marrpt ( ip:ip ) .eq. ' ' ) THEN
            ip = ip + 1
          ELSE
            iret = 1
        END IF
C
C*      Get quadrant of the globe.
C
        fld1 = marrpt ( ip:ip )
        CALL  ST_INTG ( fld1, iquad, ier )
        IF ( ier .eq. 0 ) THEN
            ip = ip + 1
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get the longitude of ship or buoy.
C
        fld4 = marrpt ( ip:ip+3 )
        CALL  ST_INTG ( fld4, ival, ier )
        rval = FLOAT ( ival )
        IF ( ier .eq. 0 .and. rval .lt. 1800. ) THEN
            ipt = ip + 4
            xlong = .1 * rval
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Determine the sign of the lat/long from quadrant of globe.
C
        IF ( iquad .eq. 7 ) THEN
C
C*          North lat, West long (e.g. North America)
C
            xlong = -xlong
          ELSE IF ( iquad .eq. 5 ) THEN
C
C*          South lat, West long (e.g. South America)
C
            xlat  = -xlat
            xlong = -xlong
          ELSE IF ( iquad .eq. 3 ) THEN
C
C*          South lat, East long (e.g. Australia, part of Africa)
C
            xlat  = -xlat
          ELSE IF ( iquad .eq. 1 ) THEN
C
C*          North lat, East long (e.g. Europe, most of Asia)
C
          ELSE
C
C*          So anything at this point other than 1 is an invalid quadrant
C*          value.
C
            iret = 1
            RETURN
        END IF
C 
	rivals ( irslat ) = xlat
	rivals ( irslon ) = xlong
C 
C*      Get the station elevation in meters. 
C
        CALL MA_ELEV ( stnid, iret )
C*
	RETURN
	END
