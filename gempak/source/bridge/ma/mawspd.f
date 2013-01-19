        SUBROUTINE MA_WSPD  ( marrpt, iparam, ipt, iret )
C************************************************************************
C* MA_WSPD                                                              *
C*                                                                      *
C* This subroutine decodes the wind speed in the Nddff group, the wind  *
C* gust in the 910ff, 911ff, or 912ff groups, the 10 meter extrapolated *
C* wind in the 11fff group, and the 20 meter extrapolated wind in the   *
C* 22fff group.  If the speed is reported in knots, it is converted to  *
C* meters per second.  When the speed is greater than 98 units, the ff  *
C* is set to 99, and a 00fff group follows.  In this case the fff       *
C* contains the wind speed.                                             *
C*                                                                      *
C* MA_WSPD  ( MARRPT, IPARAM, IPT, IRET )                               *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT		CHAR*           Report array                    *
C*      IPARAM 		INTEGER         Flag set to 0, 1, 2, 3, 4, or   *
C*                                      5, if decoding, respectively,   *
C*                                      the groups Nddff, 910ff,        *
C*                                      912ff, 11fff, 22fff, or 911ff   *
C*      IUWIND		INTEGER         Indicator for source and units  *
C*                                      of wind speed                   *
C*      XM907		REAL		Duration of reference period    *
C*					for gust			*
C*      IFLGCO          INTEGER         Flag set to 1, if US or         *
C*                                      Canadian report; otherwise 0    *
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER 	On input, points to start of    *
C*					data; on output, points to last *
C*					f in group                      *
C*									*
C* Output parameters:                                                   *
C*	RIVALS(IRSPED)  REAL            Wind speed in m/sec             *
C*	RIVALS(IRGUM0)  REAL            Wind gust in m/sec from 910ff   *
C*	RIVALS(IRGUM2)  REAL            Wind gust in m/sec from 912ff   *
C*	RIVALS(IRGUM1)  REAL            Wind gust in m/sec from 911ff   *
C*	RIVALS(IRXS10)  REAL            Wind speed in m/sec from 11fff  *
C*	RIVALS(IRXS20)  REAL            Wind speed in m/sec from 22fff  *
C*      XDTFVM          REAL            Duration in minutes of wind     *
C*                                      gust measurement                *
C*	GUMS		REAL		Wind gust speed in m/sec        *
C*	WDGFLG		LOGICAL		Wind gust flag                  *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      4/96                                            *
C* R. Hollern/NCEP     11/96    Added call to PR_KNMS                   *
C* D. Kidwell/NCEP      4/97	Removed interface calls, reorganized    *
C* 				header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*) 	marrpt
C*
        CHARACTER  	fld3*3, fld2*2
C------------------------------------------------------------------------
        iret = 0
C
C*      Check if wind indicator is missing.
C
        IF ( iuwind .eq. IMISSD ) THEN
            ipt = ipt + 1
            RETURN
        END IF
C
        IF ( iparam .eq. 0 .or. iparam .eq. 1 .or.
     +       iparam .eq. 2 .or. iparam .eq. 5 ) THEN
C
            IF ( marrpt ( ipt:ipt ) .ne. '/' .and.
     +           marrpt ( ipt+1:ipt+1 ) .ne. '/' ) THEN
                fld2 = marrpt ( ipt:ipt+1 )
                CALL  ST_INTG ( fld2, ival, ier )
                ipt = ipt + 1
                IF ( ier .eq. 0 ) THEN
                    IF ( ival .ge. 0 .and. ival .le. 99 ) THEN
                        xval = ival     
                      ELSE
                        RETURN
                    END IF
                  ELSE
                    RETURN
                END IF
              ELSE
                ipt = ipt + 1
                RETURN
            END IF
C
C*          If wind speed equals 99 units, then the next group 00fff
C*          contains the wind speed.
C
            IF ( ival .eq. 99 ) THEN
                ipt = ipt + 1
                IF ( marrpt ( ipt:ipt+2 ) .eq. ' 00' ) THEN
                    ipt = ipt + 3
                    fld3 = marrpt ( ipt:ipt+2 )
                    CALL  ST_INTG ( fld3, jval, ier )
                    ipt = ipt + 2
                    IF ( ier .eq. 0 ) THEN
                        xval = jval     
                      ELSE
                        RETURN
                    END IF
                END IF
            END IF
C
        END IF
C
        IF ( iparam .eq. 3 .or. iparam .eq. 4 ) THEN
C
C*          Get 10 or 20 meter extrapolated wind. 
C
            IF ( marrpt ( ipt:ipt+2 ) .ne. '///' ) THEN
                fld3 = marrpt ( ipt:ipt+2 )
                CALL  ST_INTG( fld3, ival, ier )
                ipt = ipt + 2
                IF ( ier .eq. 0 ) THEN
                    IF ( iuwind .eq. 3 .or. iuwind .eq. 4 ) THEN
C
C*                      Units are in whole knots.
C
                        xval = FLOAT ( ival )
                      ELSE
C
C*                      Convert from tenths of m/sec to m/sec.
C
                        xval = .1 * FLOAT ( ival )
                    END IF
                  ELSE 
                    RETURN
                END IF
              ELSE
                ipt = ipt + 2
                RETURN
            END IF
C
        END IF
C
        IF ( iuwind .eq. 3 .or. iuwind .eq. 4 ) THEN
C
C*          Convert from knots to m/sec.
C
            wdspd = PR_KNMS ( xval )
          ELSE IF ( iuwind .eq. 0 .or. iuwind .eq. 1 ) THEN
            wdspd = xval
          ELSE
            RETURN
        END IF
C
        IF ( iparam .eq. 0 ) THEN
	    rivals ( irsped ) = wdspd
          ELSE IF ( iparam .eq. 1 ) THEN
C
C*          Get highest gust during the 10-minute period preceding obs.
C
            xdtfvm = 10.
            gums = wdspd
            wdgflg = .true.
            rivals ( irgum0 ) = gums
          ELSE IF ( iparam .eq. 2 ) THEN
C
C*          Get highest mean wind.
C
            rivals ( irgum2 ) = wdspd
            IF ( .not. wdgflg ) THEN
                IF ( iflgco .eq. 1 ) xdtfvm = 0.0
                gums = wdspd
            END IF
          ELSE IF ( iparam .eq. 5 ) THEN
C
C*          Get highest gust.
C
            rivals ( irgum1 ) = wdspd
C 
            IF ( xm907 .ge. 0.0 .and. xm907 .lt. 61. ) THEN
C
C*              Choose 911ff over 910ff and 912ff, since the gust
C*              is within 1 hour of obs time.
C
                wdgflg = .true.
                xdtfvm = xm907
                gums = wdspd   
              ELSE IF ( gums .eq. RMISSD ) THEN
C
C*              Output the wind gust even if the time period of
C*              wind gust is missing and 910ff gust is also missing.
C
                wdgflg = .true.
                gums = wdspd
            END IF
          ELSE IF ( iparam .eq. 3 ) THEN
C
C*          Get 10 meter extrapolated wind speed. 
C
            rivals ( irxs10 ) = wdspd
          ELSE IF ( iparam .eq. 4 ) THEN
C
C*          Get 20 meter extrapolated wind speed.
C
            rivals ( irxs20 ) = wdspd
        END IF
C*
        RETURN
        END
