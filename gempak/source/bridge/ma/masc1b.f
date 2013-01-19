        SUBROUTINE MA_SC1B ( marrpt, ipt, iret )
C************************************************************************
C* MA_SC1B							        *
C*								        *
C* This subroutine decodes the groups in section 1 of the WMO FM13 or   *
C* CMAN bulletin report.  If the i(r)i(x)hVV or Nddff groups are        *
C* missing or garbled, then the report is considered invalid.  These    *
C* two groups need to be included in the report, but the other          *
C* section 1 groups may or may not be included.                         *
C*								        *
C* MA_SC1B  ( MARRPT, IPT, IRET )   	          		        *
C*							                *
C* Input parameters:						        *
C*      MARRPT          CHAR*           Report array                    *
C*	LSEC1           INTEGER         Length of section 1 in report   *
C*      ISEC1           INTEGER         Pointer to start of section 1   *
C*	IRPTDT (*)	INTEGER		Report date-time                *
C*					(YYYY, MM, DD, HH, MM)          *
C*								        *
C* Input and Output parameters:					        *
C*	IPT		INTEGER		Pointer to groups in report     *
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code		        *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP	4/97	Changed interface, reorganized header   *
C*				and comments				*
C* D. Kidwell/NCEP	4/97	Added check for 0 index value           *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C************************************************************************
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        INTEGER   	jflg (10)
C------------------------------------------------------------------------
        iret = 0
C
C*      A group should only appear once in a section.  This array will
C*      be used to flag those groups that are decoded in the section.
C
        DO i = 1, 10
            jflg ( i ) = 0
        END DO 
C
C*      Decode i(r)i(x)hVV group.
C
        CALL  MA_IIHV ( marrpt, ipt, iret )
C
        IF ( iret .eq. 1 ) RETURN
C
C*      Decode the Nddff group.
C
        CALL  MA_NDDF ( marrpt, ipt, iret )
C
        IF ( iret .eq. 1 ) RETURN
C
        iend = isec1 + lsec1 - 1
        ist = ipt + 1
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
            IF ( marrpt ( ipt:ipt+1 ) .eq. ' 1' .and.
     +           jflg ( 1 ) .eq. 0 ) THEN
C
C*              Decode air temperature group.
C             
                ipt   = ipt + 2
                iparm = 1
                CALL  MA_TEMP ( marrpt, iparm, ipt, jret )
                jflg ( 1 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+2 ) .eq. ' 29' .and.
     +                  jflg ( 2 ) .eq. 0 ) THEN
C
C*              Decode relative humidity group.    
C             
                ipt = ipt + 3
                CALL  MA_RELH ( marrpt, ipt, jret )
                jflg ( 2 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 2' .and.
     +                  jflg ( 3 ) .eq. 0 ) THEN
C
C*              Decode dew point temperature group.
C             
                ipt   = ipt + 2
                iparm = 2
                CALL  MA_TEMP ( marrpt, iparm, ipt, jret )
                jflg ( 3 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 3' .and.
     +                  jflg ( 4 ) .eq. 0 ) THEN
C
C*              Decode the station pressure group. 
C             
                iparm = 0
                ipt   = ipt + 2
                CALL  MA_PRES ( marrpt, iparm, ipt, jret )
                jflg ( 4 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 4' .and.
     +                  jflg ( 5 ) .eq. 0 ) THEN
C
C*              Decode the mean sea level pressure group.
C             
                iparm = 1
                ipt   = ipt + 2
                CALL  MA_PRES ( marrpt, iparm, ipt, jret )
                jflg ( 5 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 5' .and.
     +                  jflg ( 6 ) .eq. 0 ) THEN
C
C*              Decode the station 3 hourly pressure change.
C             
                ipt = ipt + 2
                CALL  MA_PRS3 ( marrpt, ipt, jret )
                jflg ( 6 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 6' .and.
     +                  jflg ( 7 ) .eq. 0 ) THEN
C
C*              Decode the precipitation group.             
C             
                ipt   = ipt + 2
                iparm = 0
                CALL  MA_PREC ( marrpt, iparm, ipt, jret )
                jflg ( 7 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 7' .and.
     +               jflg ( 8 ) .eq. 0 ) THEN
C
C*              Decode the present and past weather group.  
C             
                ipt = ipt + 2
                CALL  MA_MAWX ( marrpt, ipt, jret )
                jflg ( 8 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 8' .and.
     +                  jflg ( 9 ) .eq. 0 ) THEN
C
C*              Decode the cloud group.
C             
                ipt = ipt + 2
                CALL  MA_CLD1 ( marrpt, ipt, jret )
                jflg ( 9 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 9' .and.
     +                  jflg ( 10 ) .eq. 0 ) THEN
C
C*              Decode the obs time group.
C             
                ipt = ipt + 2
                CALL  MA_OBST ( marrpt, ipt, jret )
                jflg ( 10 ) = 1
            END IF
        END DO
C*
	RETURN
        END
