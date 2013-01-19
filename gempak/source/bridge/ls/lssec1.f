        SUBROUTINE LS_SEC1 ( lszrpt, lsfrpt, ipt, iret )
C************************************************************************
C* LS_SEC1							        *
C*								        *
C* This subroutine decodes the groups in section 1 of the WMO FM12      *
C* bulletin report.  If the i(r)i(x)hVV or Nddff groups are missing     *
C* or garbled, then the report is considered invalid.  These two groups *
C* need to be included in the report, but the other section 1 groups    *
C* may or may not be included.                                          *
C*								        *
C* LS_SEC1  ( LSZRPT, LSFRPT, IPT, IRET )   			        *
C*							                *
C* Input parameters:						        *
C*      LSZRPT          INTEGER         Report size                     *
C*      LSFRPT          CHAR*           Report array                    *
C*	LSEC1           INTEGER         Length of section 1 in report   *
C*      ISEC1           INTEGER         Pointer to start of section 1   *
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code		        *
C*				   	 0 = normal return 	        *
C*                                       1 = problems                   *
C* 								        *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP      11/96  Added the CALL to LS_GEOP                *
C* R. Hollern/NCEP       1/98  Changed interface and cleaned up code    *
C* A. Hardy/GSC          1/98  Reordered calling sequence, added GEMINC *
C* R. Hollern/NCEP       2/98  Renamed subr from LS_SC1B to LS_SEC1     *
C* S. Jacobs/NCEP	 4/98  Fixed typo in the a3 DATA statement	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        INTEGER     jflg(10)
        LOGICAL     mslp,   more
        CHARACTER   a3(5)*1,   isurf*1
C*
        DATA  a3 / '1', '2', '5', '7', '8' /
C------------------------------------------------------------------------
        iret = 0
C
C*      A group should only appear once in a section.  This array will
C*      be used to flag those groups that are decoded in the section.
C
        DO i = 1,10
           jflg ( i ) = 0
        END DO
C
C*      Decode i(r)i(x)hVV group.
C
        CALL  LS_IIHV ( lsfrpt, ipt, iret )
C
        IF ( iret .eq. 1 ) RETURN
C
C*      Decode the Nddff group.
C
        CALL  LS_NDDF ( lsfrpt, ipt, iret )
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
            IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 1' .and.
     +           jflg ( 1 ) .eq. 0 ) THEN
C
C*              Decode air temperature group.
C             
                ipt = ipt + 2
                iparam = 1
                CALL  LS_TEMP ( lsfrpt, iparam, ipt, jret )
                jflg ( 1 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+2 ) .eq. ' 29' .and.
     +                 jflg ( 2 ) .eq. 0 ) THEN
C
C*              Decode relative humidity group.
C             
                ipt = ipt + 3
                CALL  LS_RELH ( lsfrpt, ipt, jret )
                jflg ( 2 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 2' .and.
     +                  jflg ( 3 ) .eq. 0 ) THEN
C
C*              Decode dew point temperature group.
C             
                ipt = ipt + 2
                iparam = 2
                CALL  LS_TEMP ( lsfrpt, iparam, ipt, jret )
                jflg ( 3 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 3' .and.
     +                  jflg ( 4 ) .eq. 0 ) THEN
C
C*              Decode the station pressure group.
C             
                iparam = 0
                ipt = ipt + 2
                CALL  LS_PRES ( lsfrpt, iparam, ipt, jret )
                jflg (4) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 4' .and.
     +               jflg ( 5 ) .eq. 0 ) THEN
C
                mslp = .true.
                more = .true.
C
C*              Check if this is a geopotential height
C*              of an agreed standard isobaric surface.
C
                i = 0 
                DO WHILE ( more )
                    i = i + 1
                    IF ( i .eq. 5 ) more = .false.
C
                    IF ( lsfrpt ( ipt+2:ipt+2 ) .eq. a3 ( i ) ) THEN
                        mslp = .false.
                        more = .false.
                        isurf = a3(i)
                        ipt = ipt + 2
                        CALL  LS_GEOP ( lsfrpt, isurf, ipt, jret )
                    END IF
                END DO
C
                IF ( mslp ) THEN
C
C*                  Decode the mean sea level pressure group.
C             
                    iparam = 1
                    ipt = ipt + 2
                    CALL  LS_PRES ( lsfrpt, iparam, ipt, jret )
                END IF
C
                jflg (5) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 5' .and.
     +                  jflg ( 6 ) .eq. 0 ) THEN
C
C*              Decode the station 3 hourly pressure change.
C             
                ipt = ipt + 2
                CALL  LS_PRS3 ( lsfrpt, ipt, jret )
                jflg ( 6 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 6' .and.
     +                 jflg ( 7 ) .eq. 0 ) THEN
C
C*              Decode the precipitation group.
C             
                ipt = ipt + 2
                iparam = 0
                CALL  LS_PREC ( lsfrpt, iparam, ipt, jret )
                jflg ( 7 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 7' .and.
     +                  jflg ( 8 ) .eq. 0 ) THEN
C
C*              Decode the present and past weather group.
C             
                ipt = ipt + 2
                CALL  LS_MAWX ( lsfrpt, ipt, jret )
                jflg ( 8 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 8' .and.
     +                  jflg ( 9 ) .eq. 0 ) THEN
C
C*              Decode the cloud group.
C             
                ipt = ipt + 2
                CALL  LS_CLD1 ( lsfrpt, ipt, jret )
                jflg ( 9 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 9' .and.
     +                  jflg (10) .eq. 0 ) THEN
C
C*              Decode the obs time group.
C             
                ipt = ipt + 2
                CALL  LS_OBST ( lsfrpt, ipt, jret )
                jflg ( 10 ) = 1
            END IF
        END DO
C*
	RETURN
        END
