        SUBROUTINE  LS_SGRD( lsfrpt, ipt, iret )
C************************************************************************
C* LS_SGRD                                                              *
C*                                                                      *
C* This subroutine decodes the 3Ejjj group.  The E is the WMO Table 901 *
C* code figure which indicates the state of the ground without snow or  *
C* measurable ice cover.  The flag LSOGR is used to indicate whether    *
C* WMO Table 901 or 975 was used for state of ground conditions.        *
C*                                                                      *
C* LS_SGRD  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         CHAR*            Report array                    *
C*					                                *
C* Output parameters:                                                   *
C*      IPT            INTEGER          On input, points to 'E' in the  *
C*                                      3Ejjj group; on output points   *
C*                                      to the last 'J'                 *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRSOGR) REAL            WMO Table 901 code figure        *
C*      LSOGR          INTEGER         Flag for ground conditions state *
C*	IRET           INTEGER         Return code                      *
C*				       0 = normal return 	        *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP      12/96   Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP       1/98   Changed interface                       *
C* A. Hardy/GSC          1/98   Added GEMINC, cleaned up prolog         *
C************************************************************************
        INCLUDE          'GEMPRM.PRM'
        INCLUDE          'lscmn.cmn'
C*
        CHARACTER*(*)    lsfrpt
C*
        CHARACTER        fld1*1
C------------------------------------------------------------------------
        iret = 0
C
        fld1 = lsfrpt ( ipt:ipt )
        CALL  ST_INTG ( fld1, ival, ier )
        ipt = ipt + 3
        IF ( ier .eq. 0 ) THEN
            lsogr = 1
            rivals ( irsogr ) = FLOAT ( ival )
        END IF
C*
        RETURN
        END
