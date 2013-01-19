        SUBROUTINE  MA_CST0 ( marrpt, istarr, icmand, ipt, iret)
C************************************************************************
C* MA_CST0                                                              *
C*                                                                      *
C* This subroutine gets the (1) CMAN station report ID, (2) the report  *
C* GMT observational time, (3) the indicator for the source and units   *
C* of wind speed, (4) the latitude of CMAN station, (5) the longitude   *
C* of the CMAN station, (6) the quadrant of the globe where the         *
C* station is located, and (7) the elevation of the station in meters.  *
C* The elevation, the latitude, and the longitude data are not part of  *
C* the report, but are gotten from a station table data file.           *
C* The routine also stores the receipt time of the report in the        *
C* interface array.                                                     *
C*                                                                      *
C* MA_CST0 ( MARRPT, ISTARR, ICMAND, IPT, IRET )                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         CHAR*             Report array                   *
C*      ISTARR (*)     INTEGER           System time - YY,MM,DD,HH,MM   *
C*      ICMAND         CHAR*             YYGGi(w) group in CMAN bulletn *
C*      JSTNID (*)     CHAR*             Station IDs                    *
C*      ELEV (*)       INTEGER           Elevations of stations in m    *
C*      YLAT (*)       REAL              Latitude of stations in        *
C*                                       hundredths of degrees          *
C*      YLONG (*)      REAL              Longitude of stations in       *
C*                                       hundredths of degrees          *
C*      RCTIM (*)      REAL              Receipt date/time of bulletin  *
C*					                                *
C* Output parameters:                                                   *
C*      CIVALS(ICSTID) CHAR*             Report ID (CMAN station id)    *
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
C*      RIVALS(IRSELV  REAL              Elevation of station in meters *
C*      RIVALS(IRSLAT) REAL              Latitude in degrees            *
C*      RIVALS(IRSLON) REAL              Longitude in degrees           *
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
C* R. Hollern/NCEP      11/96   Changed wording of LOG message          *
C* D. Kidwell/NCEP	 4/97 	Changed interface and cleaned up code   *
C* D. Kidwell/NCEP	10/97 	Changed interface , cleaned up code     *
C* R. Hollern/NCEP	12/97 	Replaced MA_RTIM call with DC_RTIM	*
C* R. Hollern/NCEP	 7/99 	Added date/time data to interface array *
C* J. Ator/NCEP         12/00   Check validity of iuwind                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt, icmand
        INTEGER  	istarr (*)
C*
        CHARACTER   	stnid*8, fld2*2, fld1*1
        LOGICAL  	more, again
C------------------------------------------------------------------------
        iret = 0
        more = .true.
        jp = 0
        ip = 0
        stnid = ' '
C
C*      Get corrected report indicator 
C
        IF ( bbb (1:3) .eq. 'COR' ) THEN
            rivals ( ircorn ) = 1.0
          ELSE
            rivals ( ircorn ) = 0.0
        END IF
C
C*      Get CMAN station ID.
C*      Ip points to start of ID. Locate next space, the end of ID.
C
        DO WHILE (more)
C
            ip = ip + 1
            IF ( marrpt ( ip:ip ) .ne. ' ' ) THEN
                jp = jp + 1
                IF ( jp .lt. 9 ) stnid ( jp:jp ) = marrpt ( ip:ip )
              ELSE
                more = .false.
            END IF
C
        END DO
C
C*      Save report id (CMAN station id).
C
        civals ( icstid ) = stnid
C
        ipt = ip
C
C*      Get the station elevation, latitude, and longitude
C*      from the station table data.
C
        again = .true.
        i = 0
C
        DO WHILE ( again )
C
            i = i + 1
            IF ( stnid ( 1:5 ) .eq. jstnid (i) ( 1:5 ) ) THEN
                rivals ( irselv ) = elev ( i ) 
                rivals ( irslat ) = ylat ( i ) 
                rivals ( irslon ) = ylong ( i )
                again = .false.
            END IF
C
            IF ( i .gt. jstn ) THEN
C
C*              If CMAN station ID is not in dictionary, skip decoding
C*              station data for now.
C
                loglvl = 2
                CALL ST_UNPR( marrpt, 80, logmsg, len, iunpr )
                CALL DC_WLOG( loglvl, 'MA', 3, logmsg (1:len), ierwlg )
	        logmsg = ' '
                iret   = 1
                RETURN
            END IF
C
        END DO
C
C*      Get day of month of observation.
C
        fld2 = icmand ( 1:2 )     
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
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
        fld2 = icmand ( 3:4 )     
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
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
        CALL DC_RTIM ( istarr, imnday, ihour, imins, ndays, irptdt,
     +                 jret  )
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
        fld1 = icmand ( 5:5 )   
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
C*      Set quadrant of the globe (not currently used).
C
        iquad = 7
C*
	RETURN
	END
