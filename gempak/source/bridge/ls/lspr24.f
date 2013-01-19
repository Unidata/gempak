        SUBROUTINE  LS_PR24( lsfrpt, ipt, iret )
C************************************************************************
C* LS_PR24                                                              *
C*                                                                      *
C* This subroutine decodes the group 58PPP or 59PPP, which contain the  *
C* positive or negative amount of surface pressure change during the    *
C* last 24 hours.                                                       *
C*                                                                      *
C* LS_PR24  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         CHAR*             Report array                   *
C*					                                *
C* Input and Output parameters:                                         *
C*      IPT            INTEGER           On input, points to '8' or '9';*
C*                                       on output, points to last P in *
C*                                       the 58PPP or 59PPP group       *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IR24PC) REAL              24 hour pressure change in     *
C*                                       hectopascals                   *
C*	IRET           INTEGER           Return code                    *
C*				   	 0 = normal return 	        *
C*                                       1 = problems                   *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP      12/96   Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP       1/98   Changed interface, cleaned up code      *
C* A. Hardy/GSC          1/98   Added GEMINC                            *
C************************************************************************
        INCLUDE          'GEMPRM.PRM'
        INCLUDE          'lscmn.cmn'
C*
        CHARACTER*(*)    lsfrpt
C
        CHARACTER        fld3*3
C------------------------------------------------------------------------
        iret = 0
C
        IF ( lsfrpt ( ipt+1:ipt+3 ) .eq. '///' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Determine sign of pressure change.
C
        IF ( lsfrpt ( ipt:ipt ) .eq. '8' ) THEN
            xsgn = 1.0
          ELSE
            xsgn = -1.0
        END IF
C
        fld3 = lsfrpt ( ipt+1:ipt+3 )
        CALL  ST_INTG ( fld3, ival, ier )
        ipt = ipt + 3
        IF ( ier .eq. 0 ) THEN
            rval = FLOAT ( ival )
            rivals ( ir24pc ) = .1 * xsgn * rval
        END IF
C*
        RETURN
        END
