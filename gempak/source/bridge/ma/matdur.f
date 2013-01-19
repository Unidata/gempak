        SUBROUTINE  MA_TDUR ( marrpt, ipt, iret )
C************************************************************************
C* MA_TDUR                                                              *
C*                                                                      *
C* This subroutine decodes the 907tt special phenomenon group.  It      *
C* gives the duration of period of reference, ending at the time of     *
C* observation, of weather phenomenon reported in a following group.    *
C* The tt is a code figure in WMO Table 4077.                           *
C*                                                                      *
C* MA_TDUR  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
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
C* R. Hollern/NCEP      1/97    Changed the 12 to 18 hour period to     *
C*                              1080 minutes                            *
C* D. Kidwell/NCEP	4/97	Reorganized header and comments,        *
C*				cleaned up code                         *
C* D. Kidwell/NCEP     10/97	Cleaned up, fixed prologue              *
C************************************************************************
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER      	fld2*2
C------------------------------------------------------------------------
        iret = 0
C
        fld2 = marrpt ( ipt:ipt+1 )
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
C*                  Duration of period is 12 to 18 hours
C
                    xm907 = 1080.
                END IF
            END IF
        END IF
C*
        RETURN
        END
