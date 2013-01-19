        SUBROUTINE  LS_PREC( lsfrpt, iparam, ipt, iret )
C************************************************************************
C* LS_PREC                                                              *
C*                                                                      *
C* This subroutine decodes the 6RRRt, 7RRRR, or 2RRRR precipitation     *
C* group.  In the 6RRRt group, RRR and t are WMO code table values which*
C* are converted to the appropriate quantities.  In the 7-group, RRRR is*
C* the precipitation amount in tenths of a millimeter. In the 2-group   *
C* from the 555 section, RRRR is the precipitation amount in hundredths *
C* of an inch.  A trace value is stored as -1.                          *
C*                                                                      *
C* LS_PREC  ( LSFRPT, IPARAM, IPT, IRET )                               *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         	CHAR*           Report array                    *
C*      IPARAM         	INTEGER         Flag value                      *
C*                                        0 = no 2 and 7 group to decode*
C*                                        1 = decode 7 group            *
C*                                        2 = decode 2 group            *
C*      IPREC          	INTEGER         Indicator for inclusion or      *
C*                                      omission of precip data         *
C*      IHOUR          	INTEGER         GMT hour of observation         *
C*					                                *
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
C*	IPDPRC          INTEGER         Precip period for precip amount *
C*                                      from India and Sri Lanka        *
C*	IRET           	INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP      11/96   Added logic to properly decode the      *
C*                              7-group when precip amount is a trace   *
C* R. Hollern/NCEP      11/96   Corrected error that occurred when 0    *
C*                              precip amount in 6-group                *
C* R. Hollern/NCEP       1/97   Added the code to decode city precip    *
C* R. Hollern/NCEP       2/97   Added code to output the precip amount  *
C*                              if duration of the period is missing    *
C* R. Hollern/NCEP       3/97   Fixed problem when Indian report has a  *
C*                              precipitation period of 21 hours        *
C* R. Hollern/NCEP       1/98   Changed interface                       *
C* A. Hardy/GSC          1/98   Reordered calling sequence              *
C* S. Chiswell/Unidata	10/07	Added region 3 omitted group 6 P24M=0.0	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE         'lscmn.cmn'
C*
        CHARACTER*(*)  	lsfrpt
C*
        INTEGER   khrs(10)
C*
        CHARACTER   	fld4*4, fld3*3, fld1*1
        CHARACTER       india*2,  srilka*2,  bnglad*2,  sdarb*2
C*
        DATA  india     / 'IN' /
        DATA  srilka    / 'SB' /
        DATA  bnglad    / 'BW' /
        DATA  sdarb     / 'SD' /
        DATA  khrs      / 6,  12,  18,  24,  1,  2,  3,  9, 15,  21 /
C------------------------------------------------------------------------
        iret = 0
C
        IF ( iparam .eq. 1 ) THEN
            IF ( lsfrpt ( ipt:ipt+3 ) .ne. '////' ) THEN
C
C*              Decode the 24-hour precip amount in 7RRRR group.
C
                fld4 = lsfrpt ( ipt:ipt+3 )
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
        IF ( iparam .eq. 2 ) THEN
            IF ( lsfrpt ( ipt:ipt+3 ) .ne. '////' ) THEN
C
C*              Decode the 24-hour city precip amount in 2RRRR group.
C*              The precip amount in this group is .01 inches or more.
C
                fld4 = lsfrpt ( ipt:ipt+3 )
                CALL  ST_INTG( fld4, ival, ier )
                IF ( ier .eq. 0 ) THEN
                    xval = .01 * FLOAT ( ival )
C
C*                  Convert from inches to millimeters.
C
                    rivals ( ircp24 ) = PR_INMM ( xval )
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
C
C*	    Add check for region 3 per Brazil request (Chiswell/Unidata)
C
	    IF ( kwmo .eq. 3 ) rivals ( irp24m ) = 0.
C
            RETURN
        END IF
C
C*      Check if precip data is missing.
C
        IF ( lsfrpt ( ipt:ipt+2 ) .eq. '///' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Get precipitation amount in mm.
C
        fld3 = lsfrpt ( ipt:ipt+2 )
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
        fld1 = lsfrpt (ipt:ipt)
C
        IF ( fld1 .eq. '/' ) THEN
            ier = 0
            IF ( iprec .eq. 2 ) THEN
                IF ( kcoun .eq. india .or. kcoun .eq. srilka ) THEN
C
C*                  Hours since 3Z.
C
                    itm = ihour - 3
                    IF ( itm .lt. 1 ) itm = 24 + itm
C
C*                  Precipitation period.
C
                    ipdprc = itm
C
                    DO i = 1,10
                        If ( khrs(i) .eq. itm ) THEN
                            ival = i
                            ier = 0
                        END IF
                    END DO
                  ELSE IF ( kcoun .eq. bnglad ) THEN
                    ival = 7
                END IF
              ELSE IF ( kcoun .eq. sdarb .and. 
     +                ( ihour .eq. 0 .or. ihour .eq. 12 ) ) THEN
                ival = 2
              ELSE 
C
C*              Set duration of the precip period to missing.
C
                ival = 0
            END IF
          ELSE 
            CALL  ST_INTG( fld1, ival, ier )
        END IF
C
        IF ( ier .eq. 0 ) THEN
C
C* 	    See WMO Code table 4019 for ival.
C
            IF ( ival .eq. 1 ) THEN
                rivals ( irp06m ) = xpnnm
              ElSE IF ( ival .eq. 2 ) THEN
                rivals ( irp12m ) = xpnnm
              ElSE IF ( ival .eq. 3 ) THEN
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
              ELSE IF ( ival .eq. 0 .OR. ival .eq. 10 ) THEN
C
C*              Duration of the precipitation period is missing.
C
                rivals ( irpxxm ) = xpnnm
            END IF
          ELSE
            RETURN
        END IF
C*
        RETURN
        END
