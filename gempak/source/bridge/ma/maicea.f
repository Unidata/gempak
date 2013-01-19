        SUBROUTINE  MA_ICEA ( marrpt, ipt, iret)
C************************************************************************
C* MA_ICEA                                                              *
C*                                                                      *
C* This subroutine decodes the ship ice accretion group,                *
C* 6I(s)E(s)E(s)R(s).                                                   *
C*                                                                      *
C* MA_ICEA  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
C*                                                                      *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to 'I' in 6IEER*
C*					group; on output, points to 'R' *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRCOIA)  REAL            Ice accretion on ships;         *
C*                                      WMO Code Table 1751 value       *
C*      RIVALS(IRIDTH)  REAL            Thickness of ice on ship in m   *
C*      RIVALS(IRROIA)  REAL            Rate of ice accretion on        *
C*                                      ship; WMO Code Table 3551       *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*				header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER     	fld2*2, fld1*1
C------------------------------------------------------------------------
        iret = 0
C
C*      Get ice accretion on ships from WMO Code Table 1751.
C
        IF ( marrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = marrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 )  rivals ( ircoia ) = FLOAT ( ival )
        END IF
C
C*      Get thickness of ice on ships in meters.
C
        ipt = ipt + 1
        IF ( marrpt ( ipt:ipt+1 ) .ne. '//' ) THEN
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 )  rivals ( iridth ) = .01 * FLOAT ( ival )
        END IF
C
C*      Get rate of ice accretion on ships from WMO Code Table 3551.
C
        ipt = ipt + 2
        IF ( marrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = marrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irroia) = FLOAT ( ival )
        END IF
C*
        RETURN
        END
