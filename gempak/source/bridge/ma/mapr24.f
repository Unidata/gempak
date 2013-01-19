        SUBROUTINE  MA_PR24 ( marrpt, ipt, iret )
C************************************************************************
C* MA_PR24                                                              *
C*                                                                      *
C* This subroutine decodes the group 58PPP or 59PPP, which contains the *
C* positive or negative amount of surface pressure change during the    *
C* last 24 hours.                                                       *
C*                                                                      *
C* MA_PR24  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to '8' or '9'  *
C*					in the 58PPP or 59PPP group; on *
C*					output, points to last P        *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IR24PC)  REAL            24 hour pressure change in hp   *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*				header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER   	fld3*3
C------------------------------------------------------------------------
        iret = 0
C
        IF ( marrpt ( ipt+1:ipt+3 ) .eq. '///' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Determine sign of pressure change.
C
        IF ( marrpt ( ipt:ipt ) .eq. '8' ) THEN
            xsgn = 1.0
          ELSE
            xsgn = -1.0
        END IF
C
        fld3 = marrpt ( ipt+1:ipt+3 )
        CALL  ST_INTG ( fld3, ival, ier )
        ipt = ipt + 3
        IF ( ier .eq. 0 ) rivals ( ir24pc ) = .1 * xsgn * FLOAT ( ival )
C*
        RETURN
        END
