        SUBROUTINE  LS_MAWX ( lsfrpt, ipt, iret )
C************************************************************************
C* LS_MAWX                                                              *
C*                                                                      *
C* This subroutine decodes the present and past weather group,          *
C* 7wwW(1)W(2) from a manned station or 7w(a)w(a)W(a1)W(a2) from an     *
C* automatic station.  The ww's and WW's are WMO code table values.     *
C* The station indicator IXIND has a value of 1 or 4 for a manned       *
C* station, and a value of 7 for an automated station.                  *
C*                                                                      *
C* LS_MAWX  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         	CHAR*           Report array                    *
C*      IXIND          	INTEGER         Indicator for type of station   *
C*                                      operation and WX data           *
C* Input and output parameters:                                         *
C*	IPT		INTEGER		On input, points to first 'w' in*
C*					7wwWW; on output, points to last*
C*					'W' in group                    *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRWWMO) 	REAL            Present weather WMO table 4677  *
C*      RIVALS(IRWWMA) 	REAL            Present weather WMO table 4680  *
C*      RIVALS(IRPWWM) 	REAL            Past weather 1 WMO table 4561   *
C*      RIVALS(IRPWWA) 	REAL            Past weather 1 WMO table 4531   *
C*      RIVALS(IRPSW2) 	REAL            Past weather 2 WMO table 4561   *
C*      RIVALS(IRPWA2) 	REAL            Past weather 2 WMO table 4531   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP      1/98    Changes based on MA_MAWX                *
C* R. Hollern/NCEP      8/99	Defined interface array variable for    *
C*                              past weather 2 from an automatic station*
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER       fld2*2, fld1*1
C------------------------------------------------------------------------
        iret = 0
C
C*      Get present weather.
C
        IF ( lsfrpt ( ipt:ipt+1 ) .ne. '//' ) THEN
            fld2 = lsfrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) THEN
                IF ( ixind .eq. 1 .or. ixind .eq. 4 ) THEN
                    rivals ( irwwmo ) = FLOAT ( ival )
                  ELSE IF ( ixind .eq. 7  ) THEN
                    rivals ( irwwma ) = FLOAT ( ival )
                END IF
            END IF
        END IF
C
C*      Get past weather 1.
C
        ipt = ipt + 2
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                IF ( ixind .eq. 1 .or. ixind .eq. 4 ) THEN
                    rivals ( irpwwm ) = FLOAT ( ival )
                  ELSE IF ( ixind .eq. 7 ) THEN
                    rivals ( irpwwa ) = FLOAT ( ival )
                END IF
            END IF
        END IF
C
C*      Get past weather 2.
C
        ipt = ipt + 1
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                IF ( ixind .eq. 1 .or. ixind .eq. 4 ) THEN
                    rivals ( irpsw2 ) = FLOAT ( ival )
                ELSE IF ( ixind .eq. 7 ) THEN
                    rivals ( irpwa2 ) = FLOAT ( ival )
                END IF
            END IF
        END IF
C*
        RETURN
        END
