        SUBROUTINE LS_SEC2( lszrpt, lsfrpt, ipt, iret )
C************************************************************************
C* LS_SEC2							        *
C*								        *
C* This subroutine calls the routines to decode the groups in section 2 *
C* of the WMO FM 12 report.  This section begins with the 222D(s)v(s)   *
C* group.  The number of groups in the section will vary.               *
C*								        *
C* LS_SEC2  ( LSZRPT, LSFRPT, IPT, IRET )   			        *
C*							                *
C* Input parameters:						        *
C*      LSZRPT          INTEGER         Report length                   *
C*      LSFRPT          CHARACTER       Report array                    *
C*	LSEC2           INTEGER         Length of section 2 in report   *
C*      ISEC2           INTEGER         Pointer to start of section 2   *
C*	XWVHGT		REAL		Wave height in meters           *
C*	XSWELL (6)	REAL		Primary and secondary wave      *
C*					direction, period and height    *
C*								        *
C* input and Output parameters:                                         *
C*      IPT             INTEGER         Pointer to groups in report     *
C*								        *
C* Output parameters:						        *
C*      RIVALS(IRWHGT)  REAL            Wave height in meters           *
C*      RIVALS(IRNSWV)  REAL            Number of systems of swell data *
C*      RIVALS(IRDOSW)  REAL            Direction of swell waves        *
C*      RIVALS(IRPOSW)  REAL            Period of swell waves           *
C*      RIVALS(IRHOSW)  REAL            Height of swell waves           *
C*	IRET		INTEGER		Return code		        *
C*				   	 0 = normal return 	        *
C*                                       1 = problems                   *
C* 								        *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       4/96                                           *
C* R. Hollern/NCEP       1/98   Changed interface, cleaned up code      *
C* A. Hardy/GSC          1/98   Reordered calling sequence              *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        INTEGER  jflg(9)
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
        iret = 0
C
C*      A group should only appear once in a section.  This array will
C*      be used to flag those groups that are decoded in the section.
C
        DO i = 1, 9
            jflg ( i ) = 0
        END DO
C
        iend = isec2 + lsec2 - 1
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
            IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 0' .and.
     +           jflg ( 1 ) .eq. 0 ) THEN
C
C*              Decode sea surface temperature group.
C             
                ipt = ipt + 2
                iparam = 3
                CALL  LS_TEMP ( lsfrpt, iparam, ipt, jret )
                jflg ( 1 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 1' .and.
     +                  jflg ( 2 ) .eq. 0 ) THEN
C
C*              Decode the wave period and height group (instruments).
C             
                ipt = ipt + 2
                iparam = 1
                CALL  LS_WVPH ( lsfrpt, iparam, ipt, jret )
                jflg ( 2 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 2' .and.
     +                  jflg ( 3 ) .eq. 0 ) THEN
C
C*              Decode the wind wave period and height group.
C             
                ipt = ipt + 2
                iparam = 2
                CALL  LS_WVPH ( lsfrpt, iparam, ipt, jret )
                jflg( 3 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 3' .and.
     +                  jflg ( 4 ) .eq. 0 ) THEN
C
C*              Decode the swell direction group.
C             
                ipt = ipt + 2
                CALL  LS_SWLD ( lsfrpt, ipt, jret )
                jflg (4) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 4' .and.
     +                  jflg ( 5 ) .eq. 0 ) THEN
C
C*              Decode the primary swell period and height.
C             
                ipt = ipt + 2
                iparam = 3
                CALL  LS_WVPH ( lsfrpt, iparam, ipt, jret )
                jflg ( 5 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 5' .and.
     +                  jflg ( 6 ) .eq. 0 ) THEN
C
C*              Decode the secondary swell period and height.
C             
                ipt = ipt + 2
                iparam = 4
                CALL  LS_WVPH ( lsfrpt, iparam, ipt, jret )
                jflg ( 6 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 6' .and.
     +                  jflg ( 7 ) .eq. 0 ) THEN
C
C*              Decode the ice accretion group.
C             
                ipt = ipt + 2
                CALL  LS_ICEA ( lsfrpt, ipt, jret )
                jflg ( 7 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+2 ) .eq. 'ICI' ) THEN
C
C*              For now skip the icing + remarks data.
C             
                ipt = iend
              ELSE IF ( lsfrpt ( ipt:ipt+2 ) .eq. ' 70' .and.
     +               jflg ( 8 ) .eq. 0 ) THEN
C
C*              Decode the height of waves given to nearest tenth
C*              of meter.
C             
                ipt = ipt + 3
                CALL  LS_WVH1 ( lsfrpt, ipt, jret )
                jflg ( 8 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+1 ) .eq. ' 8' .and.
     +                  jflg ( 9 ) .eq. 0 ) THEN
C
C*              Decode the wet bulb temperature in degrees celsius.
C             
                ipt = ipt + 2
                iparam = 4
                CALL  LS_TEMP( lsfrpt, iparam, ipt, jret )
                jflg ( 9 ) = 1
              ELSE IF ( lsfrpt ( ipt:ipt+2 ) .eq. 'ICE' ) THEN
C
C*              For now skip the ice group.
C             
                ipt = iend
            END IF
        END DO
C
C*	Add wave height to interface array.
C
        IF ( jflg ( 2 ) .eq. 1 ) rivals ( irwhgt ) = xwvhgt
C
C*	Get number of layers of swell wave data.
C
	IF ( .not. ERMISS ( xswell ( 1 ) ) .or.
     +	     .not. ERMISS ( xswell ( 2 ) ) .or.
     +       .not. ERMISS ( xswell ( 3 ) ) )
     +       rivals ( irnswv ) = 1
	IF ( .not. ERMISS ( xswell ( 4 ) ) .or.
     +	     .not. ERMISS ( xswell ( 5 ) ) .or.
     +       .not. ERMISS ( xswell ( 6 ) ) )
     +       rivals ( irnswv ) = 2
C
C*	Add primary and secondary swell data to interface array.
C
	IF ( .not. ERMISS ( rivals (irnswv ) ) ) THEN
	    i = 1
	    DO j = 1, 2
	        rivals ( irdosw ( j ) ) = xswell ( i )
	        rivals ( irposw ( j ) ) = xswell ( i + 1 )
	        rivals ( irhosw ( j ) ) = xswell ( i + 2 )
	        i = 4
	    END DO
	END IF
C*
	RETURN
        END
