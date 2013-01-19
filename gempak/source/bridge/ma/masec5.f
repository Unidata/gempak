        SUBROUTINE MA_SEC5 ( marrpt, ipt, iret )
C************************************************************************
C* MA_SEC5						 	        *
C*								        *
C* This subroutine calls the routines to decode the groups in section 5 *
C* of the CMAN reports, the U.S. fixed buoy reports, and the Canadian   *
C* fixed buoy reports.  This section starts with the 555 group.  The    *
C* number of groups in the section will vary.                           *
C*								        *
C* MA_SEC5  ( MARRPT, IPT, IRET )       			        *
C*							                *
C* Input parameters:						        *
C*      MARRPT          CHAR*           Report array                    *
C*	LSEC5           INTEGER         Length in bytes of section 5    *
C*      ISEC5           INTEGER         Pointer to start of section 5   *
C*      IFBUOY          INTEGER         Set to 0, if fixed buoy         *
C*                                      report; set to 1, if not        *
C*	IBRTYP		INTEGER		Bulletin report type		*
C*	IFLGCO		INTEGER		Flag set to 1, if US or		*
C*					Canadian report; otherwise 0	*
C*								        *
C* Input and Output parameters:				 	        *
C*	IPT		INTEGER		Pointer to groups in report     *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRPMNT)	REAL		Time of lowest pressure - hhmm  *
C*	IRET		INTEGER		Return code		        *
C*				   	  0 = Normal return 	        *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP	4/97	Changed interface, reorganized header   *
C*				and comments				*
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C* D. Kidwell/NCEP      4/05	Added processing for 5 new buoy groups  *
C************************************************************************
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C* 
        INTEGER  	jflg (7)
	CHARACTER	fld4*4
C------------------------------------------------------------------------
        iret = 0
C
        IF ( ibrtyp .ne. 3 ) THEN
C
            IF ( ifbuoy .eq. 0 .and. iflgco .eq. 0 ) THEN
C
C*              Don't try to decode section 5 groups from fixed buoys
C*              other than those from the U.S. or Canada.
C
                RETURN
              ELSE IF ( ifbuoy .eq. 1 ) THEN
C
C*              Don't try to decode section 5 groups in ship report.
C
                RETURN
            END IF
        END IF
C
C*      A group should only appear once in a section.  This array will
C*      be used to flag those groups that are decoded in the section.
C
        DO i = 1, 7
            jflg ( i ) = 0
        END DO
C
        iend = isec5 + lsec5 - 1
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
            IF ( marrpt ( ipt:ipt+2 ) .eq. ' 11' .and.
     +           jflg ( 1 ) .eq. 0 ) THEN
C
C*              Decode the 10 meter extrapolated wind.
C             
                ipt    = ipt + 3
                iparam = 3
                CALL MA_WSPD ( marrpt, iparam, ipt, jret)
                jflg ( 1 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+2 ) .eq. ' 22' .and.
     +                 jflg ( 2 ) .eq. 0 ) THEN
C
C*              Decode the 20 meter extrapolated wind.
C             
                ipt    = ipt + 3
                iparam = 4
                CALL MA_WSPD ( marrpt, iparam, ipt, jret)
                jflg ( 2 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 3' .and.
     +                  jflg ( 3 ) .eq. 0 ) THEN
C
C*              Decode the peak 5-second wind data.
C             
                ipt    = ipt + 2
		iparam = 0
                CALL  MA_PKWD ( marrpt, iparam, ipt, jret )
                jflg ( 3 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 5' .and.
     +                  jflg ( 4 ) .eq. 0 ) THEN
C
C*              Decode the lowest pressure during the previous hour.
C             
                ipt    = ipt + 2
		iparam = 2
                CALL  MA_PRES ( marrpt, iparam, ipt, jret )
                jflg ( 4 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 6' .and.
     +                  jflg ( 5 ) .eq. 0 ) THEN
C
C*              Decode the continuous wind data groups.
C
                ipt = ipt + 2
                CALL MA_CWND ( marrpt, ipt, jret )
                jflg ( 5 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 7' .and.
     +                  jflg ( 6 ) .eq. 0 ) THEN
C
C*              Decode the hour and minute of lowest pressure.
C             
                ipt  = ipt + 2
		fld4 = marrpt ( ipt:ipt+3 )
		CALL ST_INTG ( fld4, ihhmm, ier )
		IF ( ier .eq. 0 ) rivals ( irpmnt ) = FLOAT ( ihhmm )
                jflg ( 6 ) = 1
		ipt        = ipt + 3
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 8' .and.
     +                  jflg ( 7 ) .eq. 0 ) THEN
C
C*              Decode the peak 1-minute wind data.
C             
                ipt    = ipt + 2
		iparam = 1
                CALL  MA_PKWD ( marrpt, iparam, ipt, jret )
                jflg ( 7 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+4 ) .eq. ' TIDE' ) THEN
C
C*              For now skip decoding the tide data.
C
                RETURN
            END IF
        END DO
C*
	RETURN
        END
