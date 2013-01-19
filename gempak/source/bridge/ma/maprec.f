        SUBROUTINE  MA_PREC ( marrpt, iparam, ipt, iret )
C************************************************************************
C* MA_PREC                                                              *
C*                                                                      *
C* This subroutine decodes either the 6RRRt or 7RRRR precipitation      *
C* group.  In the 6RRRt group, RRR and t are WMO code table values which*
C* are converted to the appropriate quantities.  In the 7-group, RRRR is*
C* the precipitation amount in tenths of a millimeter.  A trace value is*
C* stored as -1.                                                        *
C*                                                                      *
C* MA_PREC  ( MARRPT, IPARAM, IPT, IRET )                               *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         	CHAR*           Report array                    *
C*      IPARAM         	INTEGER         Flag set to 1, if decoding      *
C*                                      the 7-group; else set to 0      *
C*      IPREC          	INTEGER         Indicator for inclusion or      *
C*                                      omission of precip data         *
C*      IHOUR          	INTEGER         GMT hour of observation         *
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to first 'R' in*
C*					6RRRt or 7RRRR group; on output,*
C*					points to location of t in 6RRRt*
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRPXXM) 	REAL            Precipitation, unknown duration *
C*	RIVALS(IRP06M)  REAL		06 hour precip, mm              *
C*	RIVALS(IRP12M)  REAL		12 hour precip, mm              *
C*	RIVALS(IRP18M)  REAL		18 hour precip, mm              *
C*	RIVALS(IRP24M)  REAL		24 hour precip, mm              *
C*	RIVALS(IRP01M)  REAL		01 hour precip, mm              *
C*	RIVALS(IRP02M)  REAL		02 hour precip, mm              *
C*	RIVALS(IRP03M)  REAL		03 hour precip, mm              *
C*	RIVALS(IRP09M)  REAL		09 hour precip, mm              *
C*	RIVALS(IRP15M)  REAL		15 hour precip, mm              *
C*	IRET           	INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     11/96  	Added logic to properly decode the      *
C*                             	7-group when precip amount is a trace   *
C* R. Hollern/NCEP     11/96  	Corrected logic error when decoding     *
C*                             	0 precip amount in 6-group              *
C* R. Hollern/NCEP      2/97  	Added code to output the precip amount  *
C*                             	when the duration of the period of the  *
C*                             	precip amount is missing                *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*				header and comments			*
C* D. Kidwell/NCEP     10/97	Changed interface; trace value is -1    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)  	marrpt
C*
        CHARACTER   	fld4*4, fld3*3, fld1*1
C------------------------------------------------------------------------
        iret  = 0
C
        IF ( iparam .eq. 1 ) THEN
            IF ( marrpt ( ipt:ipt+3 ) .ne. '////' ) THEN
C
C*              Decode the 24-hour precip amount in 7RRRR group.
C
                fld4 = marrpt (ipt:ipt+3)
                CALL  ST_INTG ( fld4, ival, ier )
                IF ( ier .eq. 0 ) THEN
                    IF ( ival .eq. 9999 ) THEN
                        xpnnm = -1.
                      ELSE
                        xpnnm = .1 * FLOAT ( ival )
                    END IF
                    rivals ( irp24m ) = xpnnm
                END IF 
            END IF
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Decode the 6RRRt group.
C*      If precipitation indicator flag is set to 4, then no 
C*      precipitation amount is available.
C
        IF ( iprec .eq. 4 ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
C*      If flag is set to 3, then precip amount is 0, but is omitted.
C
        IF ( iprec .eq. 3 ) THEN
            IF ( MOD ( ihour, 6 ) .eq. 0 ) THEN
                rivals ( irp06m ) = 0.
              ELSE IF ( MOD (ihour, 3) .eq. 0 ) THEN
                rivals ( irp03m ) = 0.
            END IF
            RETURN
        END IF
C
C*      Check if precip data is missing.
C
        IF ( marrpt ( ipt:ipt+2 ) .eq. '///' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Get precipitation amount in mm.
C
        fld3 = marrpt ( ipt:ipt+2 )
        CALL  ST_INTG ( fld3, ival, ier )
        IF ( ier .eq. 0 ) THEN
            IF ( ival .ge. 0 .and. ival .lt. 989 ) THEN
                xpnnm = FLOAT ( ival )    
              ELSE IF ( ival .eq. 990 ) THEN
                xpnnm = -1.
              ELSE IF ( ival .gt. 990 .and. ival .le. 999 ) THEN
                ival = ival - 990
                xpnnm = .1 * FLOAT ( ival )
              ELSE IF ( ival .eq. 989 ) THEN
                xpnnm = 989.
	      ELSE
		xpnnm = RMISSD
            END IF
        ELSE
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Get duration of precipitation period.
C
        ipt = ipt + 3
        fld1 = marrpt (ipt:ipt)
C
        IF ( fld1 .eq. '/' .or. fld1 .eq. '0' ) THEN
C
C*          Duration of the precipitation period is missing.
C
            rivals ( irpxxm ) = xpnnm
          ELSE
C
C*	    Store the precip amount for the specified time period.
C
            CALL  ST_INTG(  fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
C
C* 	        See WMO Code table 4019 for ival.
C
                IF ( ival .eq. 1 ) THEN
                    rivals ( irp06m ) = xpnnm
                  ELSE IF ( ival .eq. 2 ) THEN
                    rivals ( irp12m ) = xpnnm
                  ELSE IF ( ival .eq. 3 ) THEN
                    rivals ( irp18m ) = xpnnm
                  ELSE IF ( ival .eq. 4 ) THEN
                    rivals ( irp24m ) = xpnnm
                  ELSE IF ( ival .eq. 5 ) THEN
                    rivals ( irp01m ) = xpnnm
                  ELSE IF ( ival .eq. 6 ) THEN
                    rivals ( irp02m ) = xpnnm
                  ELSE IF ( ival .eq. 7 ) THEN
                    rivals ( irp03m ) = xpnnm
                  ELSE IF ( ival .eq. 8 ) THEN
                    rivals ( irp09m ) = xpnnm
                  ELSE IF ( ival .eq. 9 ) THEN
                    rivals ( irp15m ) = xpnnm
                END IF
	    END IF
        END IF
C*
        RETURN
        END
