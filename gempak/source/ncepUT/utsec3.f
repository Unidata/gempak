        SUBROUTINE  UT_SEC3 ( lenrpt, report, istrtc, inform, iret )
C************************************************************************
C* UT_SEC3                                                              *
C*                                                                      *
C* This subroutine decodes the current data groups in section 3 of an   * 
C* FM64 tesac report and section 3 of an FM18 buoy report.              *
C* The section 3 groups contain the method of removing the velocity and *
C* motion of the ship or buoy from current measurement, the period of   *
C* current measurement (tesac only) , the duration and time of current  *
C* measurement, and the depth, direction, and speed of current          *
C* (beginning w/ the surface).                                          *
C*                                                                      *
C* UT_SEC3 ( LENRPT, REPORT, ISTRS3, INFORM, IRET )                     *
C*                                                                      *
C* Input parameters:                                                    *
C*                                                                      *
C*      REPORT          CHAR*           Report array                    *
C*      LENRPT          INTEGER         Length of report minus section 4*
C*      ISTRTC          INTEGER         Points to blank preceding       *
C*                                      66k(6)k(4)k(3) in section 3.    *
C*      INFORM          CHAR*           Indicator of whether this s/r   *
C*                                      is being called by dcdrbu or    *
C*                                      dcbthy.                         *
C*					                                *
C* Output parameters:                                                   *
C*					                                *
C*      RMRMV           REAL            Method of removing velocity and *
C*                                      movement of platform from       *
C*                                      current measurement.            *
C*      RPOCM           REAL            Period of current measurement   *
C*      RDTCC           REAL            Duration and time of current    *
C*                                      measurement.                    *
C*      IRET            INTEGER         Return code                     *
C*                                         0 = Normal return            *
C*                                         1 = No section 3 data        *
C*                                                                      *
C**                                                                     *
C* C. Caruso Magee/NCEP 11/02   New subroutine.                         *
C************************************************************************
        INCLUDE         'CURR.CMN'
C*
        CHARACTER*(*)   report, inform
C*
        CHARACTER       fld1*1
C------------------------------------------------------------------------
        iret  = 0
        ip = istrtc
C
C*      Check if section 3 current data.
C
        IF ( report(ip:ip+2) .ne. ' 66' ) THEN
            iret = 1 
            RETURN
        END IF
C
C*      Get the method of removing velocity and motion of ship or buoy
C*      from current measurement.
C
        ip = ip + 3
C
        fld1 = report ( ip:ip )
C
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) THEN
            rmrmv  = FLOAT ( ival )
        END IF
C
        ip = ip + 1
C
C*      If TESAC, get the period of current measurement (drift method).        
C
        IF ( inform .eq. 'TESAC' ) THEN
          fld1 = report ( ip:ip )
C*
          CALL  ST_INTG ( fld1, ival, ier )
          IF ( ier .eq. 0 ) THEN
            rpocm  = FLOAT ( ival )
          END IF
        END IF
C
        ip = ip + 1
C
C*      Get the duration and time of current measurement.            
C
        fld1 = report ( ip:ip )
C*
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) THEN
            rdtcc  = FLOAT ( ival )
        END IF
C
        iends3 = lenrpt
C
C*      Decode the current groups.    
C
        istrt = ip + 1 
        CALL UT_CURR ( istrt, iends3, report, ierr1 )
C
        IF ( ierr1 .eq. 1 ) THEN
            iret = 1 
            RETURN
        END IF
C*
	RETURN
	END
