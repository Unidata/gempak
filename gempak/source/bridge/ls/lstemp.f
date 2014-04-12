        SUBROUTINE  LS_TEMP  ( lsfrpt, iparam, ipt, iret )
C************************************************************************
C* LS_TEMP                                                              *
C*                                                                      *
C* This subroutine decodes the air temperature group 1sTTT, the dew     *
C* point temperature group 2sTTT, the sea surface temperature group     *
C* 0sTTT, the wet bulb temperature group 8sTTT, the maximum temperature *
C* group 1sT(x)T(x)T(x), or the minimum temperature group               *
C* 2sT(n)T(n)T(n).  The temperature is saved in the appropriate units.  *
C* IPARAM is the flag for the type of temperature as follows:           *
C*         1 - air temp                                                 *
C*         2 - dew point                                                *
C*         3 - sea surface temp                                         *
C*         4 - wet bulb temp                                            *
C*         5 - max temperature                                          *
C*         6 - min temperature                                          *
C*                                                                      *
C* LS_TEMP  ( LSFRPT, IPARAM, IPT, IRET )                               *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT 		CHAR*		Report array                    *
C*      IPARAM    	INTEGER         Flag value for temperature type *
C*      IHOUR     	INTEGER         Hour of observation             *
C*      KWMO    	INTEGER         WMO Region number               *
C*					                                *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         On input, points to s in nsTTT  *
C*					group; on output, points to     *
C*					last T in group                 *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRMSST)  REAL            Indicator for sea surface temp  *
C*      RIVALS(IRSTWC)  REAL            Indicator for wet bulb temp     *
C*      RIVALS(IRTMPC)  REAL            Air temperature, degrees C      *
C*      RIVALS(IRDWPC)  REAL            Dew point temperature, degrees C*
C*      RIVALS(IRSSTC)  REAL            Sea surface temp, degrees C     *
C*      RIVALS(IRTMWC)  REAL            Wet bulb temperature, degrees C *
C*      RIVALS(IRDTV1)  REAL            Time period for max temperature *
C*      RIVALS(IRMXTM)  REAL            Maximum temperature, degrees C  *
C*      RIVALS(IRDTV2)  REAL            Time period for min temperature *
C*      RIVALS(IRMITM)  REAL            Minimum temperaure, degrees C   *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     10/96   Added calls to PR_TMCK                   *
C* R. Hollern/NCEP     12/96   Replaced ST_C2R with ST_INTG             *
C* R. Hollern/NCEP      1/98   Changes based on MA_TEMP                 *
C* A. Hardy/GSC         1/98   Reordered calling sequence,cleaned prolog*
C* D. Kidwell/NCEP      3/98   Fixed documentation                      *
C* D. Kidwell/NCEP      5/98   TDXC --> MXTM, TDNC --> MITM, for max/min*
C* C. Caruso Magee/NCEP 3/07    Correct indicators for sign of wet-bulb *
C*                              for values 2 and 7 (iced bulb) to       *
C*                              negative (were positive previously).    *
C* M. James/Unidata     10/08   Some fix                                * 
C* M. James/Unidata     04/14   Review of fix but unsure what it does   *
C************************************************************************
	INCLUDE         'GEMPRM.PRM' 
	INCLUDE         'lscmn.cmn' 
C*
        CHARACTER*(*)   lsfrpt
C*
        INTEGER         mintbl (4,7),   maxtbl (4,7)
        CHARACTER   	fld3*3, fld1*1
C*
        DATA  mintbl  / 99, 12, 99, 99,
     +                  12, 12, 12, 12,
     +                  99, 99, 12, 99,
     +                  18, 24, 12, 24,
     +                  24, 99, 99, 99,
     +                  99, 12, 99, 99,
     +                  12, 99, 12, 99/
C*
        DATA  maxtbl  / 99, 99, 99, 12,
     +                  12, 12, 12, 12,
     +                  12, 99, 99, 99,
     +                  12, 24, 24, 12,
     +                  99, 99, 24, 99,
     +                  99, 99, 99, 12,
     +                  12, 99, 12, 99/
C------------------------------------------------------------------------
        iret = 0
C
C*      Get sign of air temperature, dew point temp, maximum temp,
C*      or minimum temp.
C
        IF ( iparam .eq. 1 .or. iparam .eq. 2 .or.
     +       iparam .eq. 5 .or. iparam .eq. 6 .or.
     +       iparam .eq. 7 ) THEN
            IF ( lsfrpt ( ipt:ipt ) .eq. '0' ) THEN
                xsign = 1.0
              ELSE IF ( lsfrpt ( ipt:ipt ) .eq. '1' ) THEN
                xsign = -1.0
              ELSE
                ipt = ipt + 3
                RETURN
            END IF
        END IF
C
C*      Get indicator for sign and type of measurement of
C*      sea-surface temperature.  Reference WMO Code Table 3850.
C
        IF ( iparam .eq. 3 ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rval = FLOAT ( ival )
                IF ( fld1 .eq. '0' .or. fld1 .eq. '2' .or. 
     +               fld1 .eq. '4' .or. fld1 .eq. '6' ) THEN
                    xsign = 1.0
                  ELSE IF ( fld1 .eq. '1' .or. fld1 .eq. '3' .or. 
     +                      fld1 .eq. '5' .or. fld1 .eq. '7' ) THEN
                    xsign = -1.0
                  ELSE
                    ipt = ipt + 3
                    RETURN
                END IF
                rivals ( irmsst ) = rval
              ELSE 
                ipt = ipt + 3
                RETURN
            END IF
        END IF
C
C*      Get indicator for sign and type of measurement of
C*      wet bulb temperature.  Reference WMO Code Table 3855.
C
        IF ( iparam .eq. 4 ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rval = FLOAT ( ival )
                IF ( fld1 .eq. '0' .or. fld1 .eq. '5' ) THEN
                    xsign = 1.0
                  ELSE IF ( fld1 .eq. '1' .or. fld1 .eq. '2' .or.
     +               fld1 .eq. '6' .or. fld1 .eq. '7' ) THEN
                    xsign = -1.0
                  ELSE
                    ipt = ipt + 3
                    RETURN
                END IF
                rivals ( irstwc ) = rval
              ELSE 
                ipt = ipt + 3
                RETURN
            END IF
        END IF
C
C*      Get temperature value.
C
        ipt = ipt + 1
        j1 = ipt + 2
        IF ( lsfrpt ( j1:j1 ) .eq. '/' ) lsfrpt ( j1:j1 ) = '0' 
        fld3 = lsfrpt ( ipt:ipt+2 )
        CALL  ST_INTG ( fld3, ival, ier )
        ipt = ipt + 2
        IF ( ier .eq. 0 ) THEN
            rval = .1 * xsign * FLOAT ( ival )
          ELSE
            RETURN
        END IF
C
        IF ( iparam .eq. 1 ) THEN
            xtmpk = PR_TMCK ( rval )
            rivals ( irtmpc ) = rval
          ELSE IF ( iparam .eq. 2 ) THEN
            rivals ( irdwpc ) = rval
          ELSE IF ( iparam .eq. 3 ) THEN
            rivals ( irsstc ) = rval
          ELSE IF ( iparam .eq. 4 ) THEN
            rivals ( irtmwc ) = rval
          ELSE IF ( iparam .eq. 5 ) THEN
            rivals ( irdtv1 ) = RMISSD
            rivals ( irmxtm ) = rval
          ELSE IF ( iparam .eq. 6 ) THEN
            rivals ( irdtv2 ) = RMISSD
            rivals ( irmitm ) = rval
        END IF
C
C*      Determine the duration of period for max/min temperatures.
C
        IF ( iparam .eq. 5 .or. iparam .eq. 6 ) THEN
C
            IF ( ihour .ge. 0 .and. ihour .lt. 5 ) THEN
                ii = 1
              ELSE IF ( ihour .eq. 23 ) THEN
                ii = 1
              ELSE IF ( ihour .ge. 5 .and. ihour .lt. 11 ) THEN
                ii = 2
              ELSE IF ( ihour .ge. 11 .and. ihour .lt. 17 ) THEN
                ii = 3
              ELSE IF ( ihour .ge. 17 .and. ihour .lt. 23 ) THEN
                ii = 4
              ELSE
                RETURN
            END IF
C
C*          Get WMO Region Number.
C
            If ( kwmo .lt. 1 .or. kwmo .gt. 7 ) RETURN
C
            IF ( iparam .eq. 5 ) THEN
C
C*              Get period of max temperature.
C
                ipr = maxtbl ( ii, kwmo )
                IF ( ipr .eq. 99 ) RETURN
                rivals ( irdtv1 ) = FLOAT ( ipr )
                IF ( ipr .lt. 24 ) RETURN
                rivals ( irt12x ) = rivals ( irmxtm )
              ELSE IF ( iparam .eq. 6 ) THEN
C
C*              Get period of min temperature.
C
                ipr = mintbl ( ii, kwmo )
                IF ( ipr .eq. 99 ) RETURN
                rivals ( irdtv2 ) = FLOAT ( ipr )
                IF ( ipr .lt. 24 )
     +             rivals (irt12n ) = rivals ( irmitm )
            END IF
        END IF
C*
        RETURN
        END
