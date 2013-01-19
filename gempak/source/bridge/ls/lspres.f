        SUBROUTINE  LS_PRES  ( lsfrpt, iparam, ipt, iret )
C************************************************************************
C* LS_PRES                                                              *
C*                                                                      *
C* This subroutine decodes the station pressure group 3P(0)P(0)P(0)P(0),*
C* if IPARAM is 0, or the mean sea level pressure group 4PPPP, if IPARAM*
C* is 1.                                                                *
C*                                                                      *
C* LS_PRES  ( LSFRPT, IPARAM, IPT, IRET )                               *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT          CHAR*           Report array                    *
C*      IPARAM          INTEGER         Flag value for group type       *
C*					                                *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         On input, points to first P in  *
C*					group nPPPP; on output, points  *
C*					to last P			*
C*					                                *
C* Output parameters:                                                   *
C*	RIVALS(IRPRES)  REAL		Station pressure in mb          *
C*      RIVALS(IRPMSL)	REAL		Mean sea level pressure in mb   *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      4/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP      1/98    Changes based on MA_PRES                *
C* A. Hardy/GSC         1/98    Reordered calling sequence              *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER   	fld4*4
C------------------------------------------------------------------------
        iret = 0
C
C*      Check for missing value.
C
        IF ( lsfrpt ( ipt:ipt+3 ) .eq. '////' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
        IF ( lsfrpt ( ipt+3:ipt+3 ) .eq. '/' ) THEN
            lsfrpt ( ipt+3:ipt+3 ) = '0'
        END IF
C
C*	Get pressure value.
C
        fld4 = lsfrpt ( ipt:ipt+3 )
        CALL  ST_INTG ( fld4, ival, ier )
        IF ( ier .eq. 0 ) THEN
            ipt = ipt + 3
            xp = .1 * FLOAT ( ival )
            IF ( xp .lt. 100. ) xp = 1000. + xp
          ELSE
            ipt = ipt + 1
            RETURN
        END IF
C
C*	Determine which type of pressure value to store.
C
        IF ( iparam .eq. 0 ) THEN
            rivals ( irpres ) = xp
          ELSE IF ( iparam .eq. 1 ) THEN
            rivals ( irpmsl ) = xp
        END IF
C*
        RETURN
        END
