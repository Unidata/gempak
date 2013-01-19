        SUBROUTINE  MA_S4TP ( marrpt, ipt, iret )
C************************************************************************
C* MA_S4TP                                                              *
C*                                                                      *
C* This subroutine decodes the drifting buoy section 4 groups YYMMJ     *
C* GGgg/ 7V(B)V(B)d(B)d(B).  These groups give the exact time of the    *
C* last known postion of the buoy, and at that position, the direction  *
C* and speed of the buoy.                                               *
C*                                                                      *
C* MA_S4TP ( MARRPT, IPT, IRET )                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         CHAR*     Report array                           *
C*					                                *
C*					                                *
C* Input and output parameters:                                         *
C*	IPT	       INTEGER	 On input, points to character in group *
C*                               preceding the group YYMMJ; on output,  *
C*				 to first of last two characters decoded*
C* Output parameters:                                                   *
C*      RIVALS(IRPSYR) REAL      Year at last known buoy position - YYYY*
C*      RIVALS(IRPSMN) REAL      Month at last known buoy position - MM *
C*      RIVALS(IRPSDY) REAL      Day at last known buoy position - DD   *
C*      RIVALS(IRPSHR) REAL      Hour at last known buoy position - HH  *
C*      RIVALS(IRPSMI) REAL      Minute at last known buoy position - MI*
C*      RIVALS(IRDBVV) REAL      Drift speed of buoy in cm/sec          *
C*      RIVALS(IRDBDD) REAL      Drift direction of buoy in tens of deg.*
C*      IRET           INTEGER   Return code                            *
C*                                 0 = normal return                    *
C*                                 1 = problems                         *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP       9/99                                           *
C************************************************************************
        INCLUDE         'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER       fld2*2, fld1*1
        LOGICAL         more
C------------------------------------------------------------------------
        iret  = 0
        more  = .true.
C
        ii = ipt + 4
C
        DO WHILE ( more )
            ipt = ipt + 1
            IF ( marrpt ( ipt:ipt ) .eq. ' ' ) THEN
                more = .false.
            ELSE IF ( ipt .ge. ii ) THEN
               RETURN
            END IF
        END DO
C
        ipt = ipt + 1
           
C*      Get day of month of observation.
C
        fld2 = marrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            IF ( ival .gt. 0 .and. ival .lt. 32 ) THEN
                rivals ( irpsdy )  =  FLOAT ( ival ) 
              ELSE
                iret = 1
                RETURN
            END IF
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get month of observation.
C
        ipt = ipt + 2
C
        fld2 = marrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            IF ( ival .ge. 1 .and. ival .le. 12 ) THEN
                rivals ( irpsmn )  =  FLOAT ( ival )
              ELSE
                iret = 1
                RETURN
            END IF
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get year of observation - (units digit of year only).
C*	Values of 6 or more will be translated to 1996 - 1999, and
C*	values less than 6 will be translated to 2000 - 2005.
C
        ipt = ipt + 2
        fld1 = marrpt ( ipt:ipt )
        CALL  ST_INTG ( fld1, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            IF ( ival .gt. 5 ) THEN
                itm = 1990 + ival
                rivals ( irpsyr )  = FLOAT ( itm )
              ELSE
                itm = 2000 + ival
                rivals ( irpsyr )  = FLOAT ( itm )
            END IF
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get hour of observation.
C
        ipt = ipt + 2
        fld2 = marrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            IF ( ival .ge. 0 .and. ival .le. 23 ) THEN
                rivals ( irpshr )  = FLOAT ( ival )
              ELSE
                iret = 1
                RETURN
            END IF
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get minutes of hour of observation.
C
        ipt = ipt + 2
        fld2 = marrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            IF ( ival .ge. 0 .and. ival .le. 59 ) THEN
                rivals ( irpsmi )  = FLOAT ( ival )
              ELSE
                iret = 1
                RETURN
            END IF
          ELSE
            iret = 1
            RETURN
        END IF
C
C*      Get drifting speed, in cm/sec, of buoy.
C
        ipt = ipt + 3
C
        IF ( marrpt (ipt:ipt+1) .eq. ' 7' ) THEN
            ipt = ipt + 2
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )   
            IF ( ier .eq. 0 ) THEN
                rivals ( irdbvv )  =  FLOAT ( ival )
              ELSE
                iret = 1
                RETURN
            END IF
C
C*          Get drift direction of buoy, in tens of degrees.
C
            ipt = ipt + 2
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )   
            IF ( ier .eq. 0 ) THEN
                rivals ( irdbdd )  =  10.0 * FLOAT ( ival )
              ELSE
                iret = 1
                RETURN
            END IF
        END IF
C*
	RETURN
	END
