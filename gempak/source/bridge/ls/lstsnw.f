        SUBROUTINE  LS_TSNW( lsfrpt, ipt, iret )
C************************************************************************
C* LS_TSNW                                                              *
C*                                                                      *
C* This routine decodes the 4Esss group.  The E is a code figure from   *
C* WMO Table 975 which indicates the state of the ground with snow or   *
C* measurable ice cover. The sss is a code figure from WMO Table 3889   *
C* which indicates the total depth of snow in centimeters at the        *
C* station.                                                             *
C*                                                                      *
C* LS_TSNW  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         CHARACTER         Report array                   *
C*					                                *
C* Output parameters:                                                   *
C*      IPT            INTEGER           On input, points to 'E' in the *
C*                                       4Esss group; on output, points *
C*                                       to the last 's'                *
C* Output parameters:                                                   *
C*      RIVALS(IRSOGR) REAL              Code figure from Table 975     *
C*      RIVALS(IRSNCM) REAL              Code figure from Table 3889    *
C*      LSOGR          INTEGER           Flag to indicate whether WMO   *
C*                                       Table 901 or 975 used for state*
C*                                       of the ground conditions       *
C*	IRET           INTEGER           Return code                    *
C*				   	 0 = normal return 	        *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP      12/96   Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP       1/98   Changed interface                       *
C* A. Hardy/GSC          1/98   Added GEMINC                            *
C************************************************************************
        INCLUDE          'GEMPRM.PRM'
        INCLUDE          'lscmn.cmn'
C*
        CHARACTER*(*)    lsfrpt
C*
        CHARACTER        fld1*1,   fld3*3
C------------------------------------------------------------------------
        iret = 0
C
        fld1 = lsfrpt ( ipt:ipt )
        CALL  ST_INTG ( fld1, ival, ier )
        ipt = ipt + 1
        IF ( ier .eq. 0 ) THEN
            rivals ( irsogr )  = FLOAT ( ival )
            lsogr = 2
        END IF
C
        fld3 = lsfrpt ( ipt:ipt+2 )
        CALL  ST_INTG ( fld3, ival, ier )
        ipt = ipt + 2
        IF ( ier .eq. 0 ) THEN
C
C*          Code figure from WMO Code Table 3889.
C
            rivals( irsncm ) = FLOAT ( ival )
        END IF
C*
        RETURN
        END
