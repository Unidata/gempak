        SUBROUTINE  MA_PRES  ( marrpt, iparam, ipt, iret )
C************************************************************************
C* MA_PRES                                                              *
C*                                                                      *
C* This subroutine decodes the station pressure group 3P(0)P(0)P(0)P(0),*
C* if IPARAM is 0, the mean sea level pressure group 4PPPP, if IPARAM is*
C* 1, or the lowest one-minute pressure group 5PPPP, if IPARAM is 2.    *
C*                                                                      *
C* MA_PRES  ( MARRPT, IPARAM, IPT, IRET )                               *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
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
C*      RIVALS(IRPMN1)	REAL		Lowest 1-min avg pressure in mb *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      4/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*	 			header and comments.                    *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C* D. Kidwell/NCEP      4/05	Added lowest 1-minute average pressure  *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER   	fld4*4
C------------------------------------------------------------------------
        iret = 0
C
C*      Check for missing value.
C
        IF ( marrpt (ipt:ipt+3) .eq. '////' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
        IF ( marrpt (ipt+3:ipt+3) .eq. '/' )
     +       marrpt (ipt+3:ipt+3) = '0'
C
C*	Get pressure value.
C
        fld4 = marrpt (ipt:ipt+3)
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
          ELSE IF ( iparam .eq. 2 ) THEN
            rivals ( irpmn1 ) = xp
        END IF
C*
        RETURN
        END
