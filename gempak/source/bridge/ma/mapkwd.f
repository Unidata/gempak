        SUBROUTINE MA_PKWD ( marrpt, iparam, ipt, iret )
C************************************************************************
C* MA_PKWD                                                              *
C*                                                                      *
C* This subroutine decodes the peak wind groups in the 555 section of   *
C* the report.  It decodes the 3GGgg and 4ddf(m)f(m)f(m) groups which   *
C* contain the time, direction, and speed of the peak 5-second wind in  *
C* the previous hour.  It also decodes the 8ddf(m)f(m)f(m) and 9GGgg    *
C* groups which contain the direction, speed and time of the peak       *
C* 1-minute wind in the previous hour.  For fixed buoy reports the      *
C* 4-group is 4ddf(m)f(m) and the 8-group is 8ddf(m)f(m).               *
C*                                                                      *
C* MA_PKWD ( MARRPT, IPARAM, IPT, IRET )                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
C*	IPARAM		INTEGER		Flag value for group type       *
C*					  = 0 - peak 5-second wind      *
C*					  = 1 - peak 1-minute wind      *
C*      IUWIND          INTEGER         Indicator for source and units  *
C*                                      of wind speed                   *
C*	IRPTDT (*)	INTEGER		Report date-time		*
C*					(YYYY, MM, DD, HH, MM)          *
C*					                                *
C* Input and output parameters:                                         *
C*	IPT		INTEGER		On input, points to 2nd char in *
C*					1st group; on output, points to *
C*					last char in second group       *
C*					                                *
C* Output parameters:                                                   *
C*	RIVALS(IRPWYR) 	REAL		Time of 5-sec peak wind (5 vals)*
C*	RIVALS(IRPWDR)	REAL		Peak 5-sec wind direction	*
C*	RIVALS(IRPWSP)	REAL		Peak 5-sec wind speed, m/sec    *
C*	RIVALS(IRPKWD)	REAL		Peak 1-min wind direction	*
C*	RIVALS(IRPKWS)	REAL		Peak 1-min wind speed, m/sec    *
C*	RIVALS(IRPKWT) 	REAL		Time of 1-min peak wind (hhmm)  *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     11/96    Added call to PR_KNMS                   *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*				header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C* F. J. Yen/NCEP       7/01	Accounted for invalid iuwind values     *
C* D. Kidwell/NCEP	4/05	CSC for iparam, added groups 8 & 9      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        INTEGER         mrptdt (5)
        CHARACTER   	fld3*3, fld2*2, fld4*4
C------------------------------------------------------------------------
        iret = 0
C
	IF ( iparam .eq. 0 ) THEN
            IF ( marrpt ( ipt:ipt+3 ) .eq. '////' ) THEN
                ipt = ipt + 3
                RETURN
            END IF
C
C*          Get the UTC hour and minutes after the hour of the 5-second
C*	    peak wind.
C
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ihh, ier )
            IF ( ier .ne. 0 ) RETURN               
C
            ipt = ipt + 2
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, imm, ier )
            IF ( ier .ne. 0 ) RETURN               
C
C*          Get year, month, day of month, hour, and minutes of 5-second
C*	    peak wind observation time.
C
            mrptdt (1) = irptdt (1)
            mrptdt (2) = irptdt (2)
            mrptdt (3) = irptdt (3)
            mrptdt (4) = ihh
            mrptdt (5) = imm
C
C*          Check if obs time of peak wind is in previous day.
C
            IF ( ihh .gt. irptdt (4) ) CALL  TI_SUBD ( mrptdt, mrptdt, 
     +		                                       ier )
C
            DO i = 0, 4
                rivals ( irpwyr + i ) = FLOAT ( mrptdt ( i + 1 ) )
            END DO
C
            ipt = ipt + 2
C
            IF ( marrpt ( ipt:ipt+1 ) .ne. ' 4' ) THEN
		ipt = ipt - 1
		RETURN
	    END IF
            ipt = ipt + 2
	END IF
C
C*      Get true direction of peak wind in tens of degrees.
C
        fld2 = marrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )
        IF ( ier .eq. 0 ) THEN
            pwdr = 10. * FLOAT ( ival )
          ELSE
            pwdr = RMISSD
        END IF
C
C*      Get wind speed in meters/second.
C
        ipt  = ipt + 2
	pwsp = RMISSD
C
        IF ( iuwind .eq. 3 .or. iuwind .eq. 4 ) THEN
C
C*          Have a CMAN station. 
C
            fld3 = marrpt ( ipt:ipt+2 )
            ipt = ipt + 2
            CALL  ST_INTG ( fld3, ival, ier )
            IF ( ier .eq. 0 ) pwsp = PR_KNMS (FLOAT (ival))
          ELSE IF ( iuwind .eq. 0 .or. iuwind .eq. 1 ) THEN
C
C*          Have a fixed buoy report.
C
            fld2 = marrpt ( ipt:ipt+1 )
            ipt = ipt + 1
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) pwsp = FLOAT ( ival )
        END IF
C
C*	Store 5-second or 1-minute peak wind direction and speed.
C
	IF ( iparam .eq. 0 ) THEN
	    rivals ( irpwdr ) = pwdr
	    rivals ( irpwsp ) = pwsp
	    RETURN
	  ELSE IF ( iparam .eq. 1 ) THEN
	    rivals ( irpkwd ) = pwdr
	    rivals ( irpkws ) = pwsp
	    ipt = ipt + 1
	    IF ( marrpt ( ipt:ipt+1 ) .ne. ' 9' ) THEN
		ipt = ipt - 1
		RETURN
	    END IF
C
	    ipt = ipt + 2
C
C*          Get the UTC hour and minutes after the hour of the 1-minute
C*	    peak wind.
C
            fld4 = marrpt ( ipt:ipt+3 )
            CALL ST_INTG ( fld4, ihhmm, ier )
            IF ( ier .eq. 0 ) rivals ( irpkwt ) = FLOAT ( ihhmm )
            ipt = ipt + 3
	END IF
C*
        RETURN
        END
