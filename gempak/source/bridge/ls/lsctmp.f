        SUBROUTINE  LS_CTMP( lsfrpt, ipt, iret )
C************************************************************************
C* LS_CTMP                                                              *
C*                                                                      *
C* This subroutine decodes the city air temperature group 1sTT in       *
C* section 5 of the report.  The TT is in degrees Farenheit and will be *
C* converted to deg Centigrade.                                         *
C*                                                                      *
C* LS_CTMP  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         CHAR*             Report array                   *
C*					                                *
C* Input and Output parameters:                                         *
C*      IPT            INTEGER           On input, points to 's' and on *
C*                                       on output, points to the last  *
C*                                       T in the 1sTT group            *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRCTTP) REAL              City temperature in deg C      *
C*      CTYFLG         LOGICAL           Flag value for city temp data  *
C*                                        true  = data, false = no data *
C*	IRET           INTEGER           Return code                    *
C*				   	 0 = normal return 	        *
C* 								        *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP      11/96   Added calls to PR_TMFK PR_TMKF          *
C* R. Hollern/NCEP      12/96   Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP       1/98   Changed interface, cleaned up code      *
C* A. Hardy/GSC          1/98   Cleaned up prolog                       *
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
C*      Get the sign of city air temperature.
C
        IF ( lsfrpt ( ipt:ipt ) .eq. '0' ) THEN
            xsign = 1.0
          ELSE IF ( lsfrpt ( ipt:ipt ) .eq. '1' ) THEN
            xsign = -1.0
          ELSE
            ipt = ipt + 2
            RETURN
        END IF
C
C*      Get temperature which is in deg F.
C
        ipt = ipt + 1
        fld2 = lsfrpt ( ipt:ipt+1 )
C
C*      Get the sign of city air temperature.
C
        ipt = ipt + 1
        CALL  ST_INTG ( fld2, ival, ier )
        IF ( ier .eq. 0 ) THEN
            rval = xsign * FLOAT ( ival )
          ELSE
            RETURN
        END IF
C
        xrval = rval
C
        IF ( rval .ge. 0.0 .and. rval .lt. 30. ) THEN
C
C*          Add 100 to temp to determine if temp is 
C*          greater than 100 deg F.
C
            xrval = rval + 100.
C
C*          Compare airport temperature with city temperature.
C
            IF ( xtmpk .ne. RMISSD ) THEN
C
C*              Convert airport temp to deg F.
C
                xtemp = PR_TMKF( xtmpk )
C
                xdif = ABS ( xtemp - rval )
                ydif = ABS ( xtemp - xrval )
C
C*               Check if city temp is 100 deg F or more.
C
                IF ( xdif .gt. ydif ) rval = xrval
              ELSE
                RETURN
            END IF
        END IF
C
        ctyflg = .true.
C
C*      Convert city temp from deg F to deg C.
C
        rivals ( ircttp ) = PR_TMFC ( rval )
C*
        RETURN
        END
