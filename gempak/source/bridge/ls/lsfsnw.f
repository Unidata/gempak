        SUBROUTINE  LS_FSNW( lsfrpt, ipt, iret )
C************************************************************************
C* LS_FSNW                                                              *
C*                                                                      *
C* This subroutine decodes the special phenomenom group 931ss.  The ss  *
C* is a code table value which gives the depth of the newly fallen snow *
C* at the station in mm.  It is converted to meters for storage in the  *
C* INTERFACE array.  The duration of the period of the snow fall is     *
C* gotten from the 907tt group. If this group is missing, the period is *
C* assumed to be 6 hours, unless the observation is from WMO Region VII.*
C*                                                                      *
C* LS_FSNW  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         CHAR*             Report array                   *
C*					                                *
C* Input and Output parameters:                                         *
C*      IPT            INTEGER           On input points to the first   *
C*                                       's'; on output, points to the  *
C*                                       last s in 931ss group          *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRDOFS) REAL              Depth of fresh snow in meters  *
C*      RIVALS(IRDHFS) REAL              Period of fresh snow in hours  *
C*	IRET           INTEGER           Return code                    *
C*				   	 0 = normal return 	        *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP       1/97   Changed the duration of freshly fallen  *
C*                              snow from minutes to hours              *
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
C*      Check if either precip data or time period flag are missing.
C
        IF ( lsfrpt ( ipt:ipt+1 ) .eq. '//' ) THEN
            ipt = ipt + 1
            RETURN
        END IF
C
C*      Depth of newly fallen snow is in mm (WMO Table 3870).
C
        fld2 = lsfrpt ( ipt:ipt+1 )
        ipt = ipt + 1
        CALL  ST_INTG ( fld2, ival, ier )
        IF ( ier .eq. 0 ) THEN
            IF ( ival .lt. 56 ) THEN
                isnamt = 10 * ival
              ELSE IF ( ival .gt. 55 .and. ival .lt. 91 ) THEN
                isnamt = (ival - 50) * 100
              ELSE IF ( ival .gt. 90 .and. ival .lt. 97 ) THEN
                isnamt = ival - 90
              ELSE 
                RETURN
            END IF
          ELSE
            RETURN
        END IF
C
C*      Convert from mm to meters.
C
        snwamt = .001 * REAL ( isnamt )
        rivals(irdofs) = snwamt
C
C*      Duration of period of fallen snow in hours.
C
        If ( xm907s .ne. RMISSD ) THEN
            rivals ( irdhfs ) = xm907s
            RETURN
        END IF
C
C*      In region VII the time period is the time since the last
C*      00Z observation.
C
        IF ( kwmo .eq. 7 .or. kwmo .eq. 0 ) THEN
C
C*          Set duration of period to missing.
C
            rivals ( irdhfs ) = xm907s
            RETURN
        END IF
C
C*      There was no 907tt group preceding the 931ss group.  Use 
C*      6 hours for duration of period of fresh snow.
C
        xm907s = 6.
        rivals ( irdhfs ) = xm907s
C*
        RETURN
        END
