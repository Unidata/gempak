        SUBROUTINE  LS_CLD4 (  lsfrpt, ipt, lvl, iret )
C************************************************************************
C* LS_CLD4                                                              *
C*                                                                      *
C* This subroutine decodes the section 4 cloud groups N'C'H'H'C(t).     *
C* This group contains the amount of cloud whose base is below the level*
C* of the station, the genus of cloud whose base is below the level of  *
C* the station, the altitude of the upper surface of clouds reported by *
C* C', in hundreds of meters, and the description of the top of cloud   *
C* whose base is below the level of the station.                        *
C*                                                                      *
C* LS_CLD4  ( LSFRPT, IPT, LVL, IRET ) 	                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT          CHAR*           Report array                    *
C*      LVL             INTEGER         Cloud layer being decoded (1-2) *
C*      IPT             INTEGER         Pointer to start of cloud group *
C*                                                                      *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to N' in cloud *
C*                                      group; on output, points to     *
C*                                      C(t)                            *
C*                                                                      *
C* Output parameters:                                                   *
C*	RIVALS(IRNCL4)  REAL            Number of layers of cloud data  *
C*	RIVALS(IRCLDT)  REAL            Description of the top of cloud *
C*	RIVALS(IRCLA4)  REAL            Cloud amount                    *
C*	RIVALS(IRCLT4)  REAL            Cloud type                      *
C*	RIVALS(IRHCT4)  REAL            Altitude of upper surface of    *
C*                                      clouds reported by C'           *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      8/99                                            *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE 	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER   	fld2*2, fld1*1
	REAL		yverts(0:9)
C*
C*      Vertical significance from WMO BUFR table 0 08 002 
C*
        DATA  yverts / 9., 9., 9., 8., 8., 8., 7., 7., 7., 4. /
C------------------------------------------------------------------------
        iret = 0
C
C*      Get cloud amount whose base is below the level of the station,
C*      referencing WMO Code Table 2700.
C
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 )  THEN
                rivals ( ircla4 ( lvl ) ) = FLOAT ( ival )
            END IF
        END IF
C
C*      Get genus of cloud whose base is below the level of the station,
C*      referencing WMO code table 500.
C
        ipt = ipt + 1
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rivals ( irclt4 ( lvl ) ) = FLOAT ( ival )
                rivals ( irvss4 ( lvl ) ) = yverts ( ival )
C
C*              Check to see if it is a cumulonimbus layer.
C
                IF ( ival .eq. 9 ) THEN
                    rivals ( irvss4 ( lvl ) ) = 4.0
                END IF
            END IF
        END IF
C
C*      Get altitude of the upper surface of clouds in hundreds
C*      of meters.
C
        ipt = ipt + 1
        fld2 = lsfrpt ( ipt:ipt+1 )
        IF ( fld2 .ne. '//' ) THEN
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) THEN
                xval = 100. * FLOAT ( ival )
                rivals ( irhct4 ( lvl ) ) = xval
            END IF
        END IF
C
C*      Get description of the top of cloud whose base is
C*      below the level of the station, referencing WMO
C*      Code Table 0552.
C
        ipt = ipt + 2
        fld1 = lsfrpt ( ipt:ipt )
        IF ( fld1 .ne. '/' ) THEN
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rivals ( ircldt ( lvl ) ) =  float ( ival )
            END IF
        END IF
C
C*      Get number of cloud layers.
C
        rivals ( irncl4 ) = lvl
C*
        RETURN
        END
