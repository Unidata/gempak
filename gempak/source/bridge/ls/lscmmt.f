        SUBROUTINE  LS_CMMT( lsfrpt, ipt, iret )
C************************************************************************
C* LS_CMMT                                                              *
C*                                                                      *
C* This subroutine decodes the city maximum/minimum temperature group   *
C* sT(x)T(x)sT(n)T(n) in section 5 of the report.  The TT is in         *
C* degrees Farenheit.  The max/min temperatures will be converted to    *
C* deg Kelvin.                                                          *
C*                                                                      *
C* LS_CMMT  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         CHAR*	         Report array                   *
C*					                                *
C* Input and Output parameters:                                         *
C*      IPT            INTEGER           On input, points to 's' in 1sTT*
C*                                       group; on output, points to the*
C*                                       last T in the 1sTTsTT group    *
C*                                                                      *
C* Output parameters                                                    *
C*      RIVALS(IRCTMX) REAL              City max temperature in deg K  *
C*      RIVALS(IRCTMN) REAL              City min temperature in deg K  *
C*      CTYFLG         LOGICAL           Flag is true if city temp data *
C*	IRET           INTEGER           Return code                    *
C*				   	 0 = normal return 	        *
C* 								        *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP      11/96   Added calls to PR_TMFK                  *
C* R. Hollern/NCEP      12/96   Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP       1/98   Changed interface, cleaned up code      *
C************************************************************************
        INCLUDE          'GEMPRM.PRM'
        INCLUDE          'lscmn.cmn'
C*
        CHARACTER*(*)    lsfrpt
C*
        CHARACTER        fld2*2
C------------------------------------------------------------------------
        iret = 0
C
C*      Get the sign of city max temperature.
C
        IF ( lsfrpt ( ipt:ipt ) .eq.'0' ) THEN
            xsign = 1.0
          ELSE IF ( lsfrpt ( ipt:ipt ) .eq.'1' ) THEN
            xsign = -1.0
          ELSE
            ipt = ipt + 5
            RETURN
        END IF
C
C*      Get max temperature which is in Fahrenheit.
C
        ipt = ipt + 1
        fld2 = lsfrpt ( ipt:ipt+1 )
        ipt = ipt + 1
        CALL  ST_INTG ( fld2, ival, ier )
        IF ( ier .eq. 0 ) THEN
            tmax = xsign * FLOAT ( ival )
          ELSE
            ipt = ipt + 5
            RETURN
        END IF
C
C*      Get the sign of city min temperature.
C
        ipt = ipt + 1
        IF ( lsfrpt ( ipt:ipt ) .eq.'0' ) THEN
            xsign = 1.0
          ELSE IF ( lsfrpt ( ipt:ipt ) .eq.'1' ) THEN
            xsign = -1.0
          ELSE
            ipt = ipt + 2
            RETURN
        END IF
C
C*      Get min temperature which is in Fahrenheit.
C
        nexp = 1
        ipt = ipt + 1
        fld2 = lsfrpt ( ipt:ipt+1 )
        ipt = ipt + 1
        CALL  ST_INTG ( fld2, ival, ier )
        IF ( ier .eq. 0 ) THEN
            tmin = xsign * FLOAT ( ival )
          ELSE
            RETURN
        END IF
C
        xtmax = tmax
C
        IF ( tmax .ge. 0.0 .and. tmax .lt. 30. ) THEN
C
C*          Add 100 to tmax to determine if tmax is 
C*          greater than 100 degrees.

            xtmax = tmax + 100.
C
C*          Compare max/min temperature difference.
C
            xdif = ABS ( tmax - tmin )
            ydif = ABS ( xtmax - tmin )
C
            IF ( xdif .gt. ydif ) THEN
C
C*              City max temp is 100 deg or more.
C
                tmax = xtmax
            END IF 
        END IF
C
        ctyflg = .true.
C
C*      Convert temps from deg F to deg K.
C
        cmax =  PR_TMFK( tmax )
        rivals ( irctmx ) = cmax
C
        cmin =  PR_TMFK( tmin )
        rivals ( irctmn ) = cmin
C*
        RETURN
        END
