        SUBROUTINE  LS_TDUR( lsfrpt, ipt, iret )
C************************************************************************
C* LS_TDUR                                                              *
C*                                                                      *
C* This subroutine decodes the 907tt special phenomenon group.  It      *
C* gives the duration of period of reference, ending at the time of     *
C* observation, of weather phenomenon reported in a following group.    *
C* The tt is a code figure in WMO Table 4077.                           *
C*                                                                      *
C* LS_TDUR  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT          CHAR*           Report array                    *
C*					                                *
C* Input and output parameters:                                         *
C*	IPT		INTEGER		On input, points to first 't'   *
C*					in 907tt; on output, to last 't'*
C*					                                *
C* Output parameters:                                                   *
C*      XM907           REAL            The tt in 907tt converted to    *
C*                                      minutes                         *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      4/96                                            *
C* R. Hollern/NCEP      1/97   Changed period of freshly fallen snow    *
C*                             from minutes to hours                    *
C* R. Hollern/NCEP      1/98   Cleaned up code                          *
C* A. Hardy/GSC		1/98   Added GEMINC                             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE         'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER       fld2*2
C------------------------------------------------------------------------
        iret = 0
        xm907 = -1.
C
        fld2 = lsfrpt ( ipt:ipt+1 )
        ipt = ipt + 1
        IF ( fld2 .ne. '//' ) THEN
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) THEN
                IF ( ival .lt. 61 ) THEN
                    xm907 = 6. * FLOAT ( ival )
                  ELSE IF ( ival .lt. 67 ) THEN
                    xm907 = 60. * ( FLOAT ( ival ) - 60. ) + 360.
                  ELSE IF ( ival .eq. 67 ) THEN
C
C*                  Duration of period is 12 to 18 hours.
C
                    xm907 = 1080.
                END IF
            END IF
        END IF
C
        IF ( lsfrpt ( ipt+1:ipt+4 ) .eq. ' 931' ) THEN
            IF ( xm907 .ne. -1.0 ) THEN
C
C*               Get time period of the freshly fallen snow in hours.
C
                 xm907s = xm907 / 60.
                 IF ( xm907s .lt. 1.0 ) xm907s = 1.0
             END IF
          ELSE IF ( lsfrpt ( ipt+1:ipt+4 ) .eq. ' 911' ) THEN
C
C*          Get time period of the wind gust.
C
            xm907g = xm907
        END IF
C*
        RETURN
        END
