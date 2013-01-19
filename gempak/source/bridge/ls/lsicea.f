        SUBROUTINE  LS_ICEA ( lsfrpt, ipt, iret)
C************************************************************************
C* LS_ICEA                                                              *
C*                                                                      *
C* This subroutine decodes the ship ice accretion group,                *
C* 6I(s)E(s)E(s)R(s). Ice accretion on ships uses WMO Code Table 1751   *
C* and the rate of ice accretion on ships uses WMO Code Table 3551.     *
C*                                                                      *
C* LS_ICEA  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT          CHAR*           Report array                    *
C*                                                                      *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to 'I' in 6IEER*
C*					group; on output, points to 'R' *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRCOIA)  REAL            Ice accretion on ships;         *
C*      RIVALS(IRIDTH)  REAL            Thickness of ice on ship in m   *
C*      RIVALS(IRROIA)  REAL            Rate of ice accretion on        *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP      1/98    Changes based on MA_ICEA                *
C* A. Hardy/GSC         1/98    Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER     	fld2*2, fld1*1
C------------------------------------------------------------------------
        iret = 0
C
C*      Get ice accretion on ships from WMO Code Table 1751.
C
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 )  rivals ( ircoia ) = FLOAT ( ival )
        END IF
C
C*      Get thickness of ice on ships in meters.
C
        ipt = ipt + 1
        IF ( lsfrpt ( ipt:ipt+1 ) .ne. '//' ) THEN
            fld2 = lsfrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 )  rivals ( iridth ) = .01 * FLOAT ( ival )
        END IF
C
C*      Get rate of ice accretion on ships from WMO Code Table 3551.
C
        ipt = ipt + 2
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = lsfrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irroia ) = FLOAT ( ival )
        END IF
C*
        RETURN
        END
