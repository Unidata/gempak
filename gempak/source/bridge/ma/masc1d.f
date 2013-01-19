        SUBROUTINE MA_SC1D  ( marrpt, iret )
C************************************************************************
C* MA_SC1D						                *
C*								        *
C* This subroutine calls the routines which decode the groups in        *
C* section 1 of the drifting buoy report.  The section contains         *
C* meteorological and other non-marine data.                            *
C*                                                                      *
C* MA_SC1D  ( MARRPT, IRET )                                            *
C*                                                                      *
C* Input parameters:						        *
C*      MARRPT          CHAR*           Report array                    *
C*	LSEC1           INTEGER         Length of section 1 in report   *
C*      ISEC1           INTEGER         Pointer to start of section 1   *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRQDS1) 	REAL		Quality control indicator for   *
C*	                                section 1. WMO Code table 3334. *
C*	RIVALS(IRQXS1) 	REAL		Indicator of position of group  *
C*	IRET	 	INTEGER	    	Return code		        *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP      4/97	Changed interface, reorganized header   *
C*				and comments                            *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C* R. Hollern/NCEP      7/99	Decoded quality control data            *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        INTEGER   	jflg(7)
C*
        CHARACTER       fld1*1
C------------------------------------------------------------------------
        iret = 0
        ip = isec1
C
C*      A group should only appear once in a section.  This array will
C*      be used to flag those groups that are decoded in the section.
C
        DO i = 1, 7
            jflg ( i ) = 0
        END DO
C
        ipt = ip
C
C*      Get the quality control indicator for section 1.
C
        fld1 = marrpt (ipt:ipt)
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) rivals ( irqds1 ) = FLOAT ( ival )
C
        ipt = ipt + 1
C
C*      Get the indicator of position of group
C
        fld1 = marrpt (ipt:ipt)
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) rivals ( irqxs1 ) = FLOAT ( ival )
C
        iend = isec1 + lsec1 - 1
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
            IF ( marrpt ( ipt:ipt+1 ) .eq. ' 0' .and.
     +           jflg ( 1 ) .eq. 0 ) THEN
C
C*              Decode the 0ddff group.
C
                ipt = ipt + 2
                CALL  MA_DDFF ( marrpt, ipt, jret)
                jflg (1) = 1
              ELSE IF ( marrpt (ipt:ipt+1) .eq. ' 1' .and.
     +                  jflg (2) .eq. 0 ) THEN
C
C*              Decode air temperature group.
C             
                ipt = ipt + 2
                iparam = 1
                CALL  MA_TEMP ( marrpt, iparam, ipt, jret )
                jflg (2) = 1
              ELSE IF ( marrpt (ipt:ipt+2) .eq. ' 29' .and.
     +                  jflg (3) .eq. 0 ) THEN
C
C*              Decode relative humidity group.    
C             
                ipt = ipt + 3
                CALL  MA_RELH ( marrpt, ipt, jret )
                jflg (3) = 1
              ELSE IF ( marrpt (ipt:ipt+1) .eq. ' 2' .and.
     +                  jflg (4) .eq. 0 ) THEN
C
C*              Decode dew point temperature group.
C             
                ipt = ipt + 2
                iparam = 2
                CALL  MA_TEMP ( marrpt, iparam, ipt, jret )
                jflg (4) = 1
              ELSE IF ( marrpt (ipt:ipt+1) .eq. ' 3' .and.
     +                  jflg (5) .eq. 0 ) THEN
C
C*              Decode the station pressure group. 
C             
                iparam = 0
                ipt = ipt + 2
                CALL  MA_PRES ( marrpt, iparam, ipt, jret )
                jflg (5) = 1
              ELSE IF ( marrpt (ipt:ipt+1) .eq. ' 4' .and.
     +               jflg (6) .eq. 0 ) THEN
C
C*              Decode the mean sea level pressure group.
C             
                iparam = 1
                ipt = ipt + 2
                CALL  MA_PRES ( marrpt, iparam, ipt, jret )
                jflg (6) = 1
              ELSE IF ( marrpt (ipt:ipt+1) .eq. ' 5' .and.
     +                  jflg (7) .eq. 0 ) THEN
C
C*              Decode the station 3 hourly pressure change.
C             
                ipt = ipt + 2
                CALL  MA_PRS3 ( marrpt, ipt, jret )
                jflg (7) = 1
            END IF
        END DO
C*
	RETURN
        END
