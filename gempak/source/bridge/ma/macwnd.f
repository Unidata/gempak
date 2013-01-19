        SUBROUTINE  MA_CWND ( marrpt, ipt, iret )
C************************************************************************
C* MA_CWND                                                              *
C*                                                                      *
C* This subroutine decodes the continuous wind data groups 6GGgg through*
C* d(6)d(6)d(6)f(6)f(6)f(6).  The continuous wind data consists of six  *
C* readings of the wind direction and speed taken 10 minutes apart      *
C* starting in the preceding observation hour.  The 6-group contains    *
C* the end time of the most recent reading.  The group                  *
C* d(1)d(1)d(1)f(1)f(1)f(1) is the most recent wind measurement, with   *
C* each succeeding wind group representing the next older wind.  If the *
C* wind speed is reported in knots, it is converted to meters/sec.      *
C*                                                                      *
C* MA_CWND ( MARRPT, IPT, IRET )                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
C*      IUWIND          INTEGER         Indicator for source and units  *
C*                                      of wind speed                   *
C*	IRPTDT (*)	INTEGER		Report date-time		*
C*					(YYYY, MM, DD, HH, MM)		*
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to first 'G' in*
C*					6GGgg group; on output, points  *
C*					to last 'f' in last wind group  *
C*					                                *
C* Output parameters:                                                   *
C*	RIVALS(IRNCWD)	REAL		Number of layers of cont. wind  *
C*	RIVALS(IRTPMI)	REAL		Time displacement, minutes      *
C*	RIVALS(IRWDRC)	REAL		Continuous wind direction       *
C*	RIVALS(IRWDSC)	REAL		Continuous wind speed, m/sec    *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return		*
C*                                        1 = Problems			*
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      7/96                                            *
C* R. Hollern/NCEP      8/96    Corrected time displacement calculation *
C* R. Hollern/NCEP     11/96    Added call to PR_KNMS                   *
C* R. Hollern/NCEP     12/96    Replaced TPOD with TPMI and replaced    *
C*                              ST_C2R with ST_INTG                     *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized	*
C*				header and comments			*
C* D. Kidwell/NCEP     10/97	Changed interface                   	*
C* F. J. Yen/NCEP	7/01	Accounted for invalid iuwind values	*
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER     	fld3*3, fld2*2
C------------------------------------------------------------------------
        iret = 0
        IF ( marrpt ( ipt:ipt+3 ) .eq. '////' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Get the end time in hour and minutes of the latest 10-minute
C*      continuous wind data.
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
        itmcwd = 100 * ihh + imm
C
C*      Get report obs hour and minutes.
C
        jtmobs = 100 * irptdt ( 4 ) + irptdt ( 5 )
C
C*      Compute the first time displacement in minutes with respect
C*      to the observation hour.
C
        IF ( irptdt ( 4 ) .eq. 0 .and. itmcwd .gt. 2300 ) THEN
            ifirst = 2360 + irptdt ( 5 ) 
          ELSE IF ( ihh .eq. irptdt ( 4 ) ) THEN
            ifirst = jtmobs
          ELSE
            ifirst = 100 * ( irptdt ( 4 ) - 1 ) + 60 + irptdt ( 5 ) 
        END IF
C
        idifmn = itmcwd - ifirst
        ipt = ipt + 1
C
C*	Loop over six sets of values.
C
        DO i = 1, 6
C
            ipt = ipt + 2
C
C*          Get time displacement.
C
            rivals ( irtpmi ( i ) ) = idifmn
            idifmn = idifmn - 10
C
C*          Get wind direction in whole degrees.
C
            fld3 = marrpt ( ipt:ipt+2 )
            CALL  ST_INTG ( fld3, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irwdrc ( i ) ) = FLOAT ( ival )
C
C*          Get wind speed in meters/second.
C
            ipt = ipt + 3
            IF ( iuwind .eq. 3 .or. iuwind .eq. 4 ) THEN
C
                fld3 = marrpt ( ipt:ipt+2 )
                ipt = ipt + 2
                CALL  ST_INTG ( fld3, ival, ier )
                IF ( ier .eq. 0 ) 
     +              rivals ( irwdsc ( i ) ) = PR_KNMS ( FLOAT ( ival ) )
              ELSE IF ( iuwind .eq. 0 .or. iuwind .eq. 1 ) THEN
                fld3 = marrpt ( ipt:ipt+2 )
                ipt = ipt + 2
                CALL  ST_INTG ( fld3, ival, ier )
                IF ( ier .eq. 0 ) THEN
                    rivals ( irwdsc ( i ) ) = .1 * FLOAT ( ival )
                END IF
            END IF
C
        END DO
C
C*	Store the number of layers of continuous wind.
C
	rivals ( irncwd ) = 6
C*
        RETURN
        END
