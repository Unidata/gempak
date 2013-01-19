        SUBROUTINE MA_SEC3 ( marrpt, ipt, iret )
C************************************************************************
C* MA_SEC3							        *
C*								        *
C* This subroutine calls the routines to decode the groups in section 3 *
C* of the FM13 ship, fixed buoy, or CMAN bulletin report.  This section *
C* begins with the 333 group.  The number of groups in the section      *
C* will vary.                                                           *
C*								        *
C* MA_SEC3  ( MARRPT, IPT, IRET )   			                *
C*							                *
C* Input parameters:						        *
C*      MARRPT          CHAR*           Report array                    *
C*	LSEC3           INTEGER         Length of section 3 in report   *
C*      ISEC3           INTEGER         Pointer to start of section 3   *
C*	XDTFVM		REAL		Duration in minutes of wind     *
C*					gust measurement		*
C* 	GUMS		REAL		Wind gust speed in m/sec	*
C*	XCLDS (*)	REAL		Array containing section 3	*
C*					cloud data for up to 4 layers	*
C*								        *
C* Input and Output parameters:					        *
C*	IPT		INTEGER		Pointer to groups in report	*
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRDTVM)  REAL		Wind gust period                *
C*	RIVALS(IRGUMS)  REAL		Wind gust speed, m/sec          *
C*	RIVALS(IRNCL3)  REAL		Number of layers of cloud data  *
C*	RIVALS(IRVSS3)  REAL		Vertical significance           *
C*	RIVALS(IRCLA3)  REAL		Cloud amount                    *
C*	RIVALS(IRCLT3)  REAL		Cloud type                      *
C*	RIVALS(IRHCB3)  REAL		Height of base of cloud         *
C*	IRET		INTEGER		Return code		        *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      4/96                                            *
C* D. Kidwell/NCEP	4/97		Changed interface, reorganized  *
C*					header and comments		*
C* D. Kidwell/NCEP     10/97		Changed interface, cleaned up   *
C************************************************************************
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C* 
        INTEGER  	jflg(8)
C------------------------------------------------------------------------
        iret = 0
        lvl  = 0
C
C*      A group should only appear once in a section.  This array will
C*      be used to flag those groups that are decoded in the section.
C
        DO i = 1, 8
            jflg ( i ) = 0
        END DO
C
        iend = isec3 + lsec3 - 1
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
            IF ( marrpt ( ipt:ipt+1 ) .eq. ' 1' .and.
     +           jflg ( 1 ) .eq. 0 ) THEN
C
C*              Decode the maximum temperature group.
C             
                ipt    = ipt + 2
                iparam = 5
                CALL  MA_TEMP ( marrpt, iparam, ipt, jret )
                jflg ( 1 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 2' .and.
     +                  jflg ( 2 ) .eq. 0 ) THEN
C
C*              Decode the minimum temperature group.
C             
                ipt    = ipt + 2
                iparam = 6
                CALL  MA_TEMP ( marrpt, iparam, ipt, jret )
                jflg ( 2 ) = 1
              ELSE IF ( ( marrpt ( ipt:ipt+2 ) .eq. ' 58' .or.
     +                    marrpt ( ipt:ipt+2 ) .eq. ' 59' ) .and.
     +                    jflg ( 3 ) .eq. 0 ) THEN
C
C*              Decode the 24 hour pressure change group.
C             
                ipt = ipt + 2
                CALL  MA_PR24 ( marrpt, ipt, jret )
                jflg ( 3 ) = 1
              ELSE IF ( ( marrpt ( ipt:ipt+5 ) .eq. ' 55407' ) 
     +             .or. ( marrpt ( ipt:ipt+5 ) .eq. ' 55408' )
     +             .or. ( marrpt ( ipt:ipt+5 ) .eq. ' 55507' )
     +             .or. ( marrpt ( ipt:ipt+5 ) .eq. ' 55508' ) ) THEN
C
C*              Skip 5-group and the supplementary group which
C*              follows.  Do this to avoid problems.
C             
                ipt = ipt + 8
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 6' .and.
     +                  jflg ( 4 ) .eq. 0 ) THEN
C
C*              Decode the precipitation group.
C             
                ipt    = ipt + 2
                iparam = 0
                CALL  MA_PREC ( marrpt, iparam, ipt, jret )
                jflg ( 4 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 7' .and.
     +                  jflg ( 5 ) .eq. 0 ) THEN
C
C*              Decode the 24-hour precipitation group.
C             
                ipt    = ipt + 2
                iparam = 1
                CALL  MA_PREC ( marrpt, iparam, ipt, jret )
                jflg ( 5 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+5 ) .eq. ' 80000' ) THEN
C
C*              Don't decode the additional data in regional code form.
C             
                RETURN
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 8' ) THEN
C
C*              Decode the cloud groups                
C             
                ipt = ipt + 2
                lvl = lvl + 1
                IF ( lvl .lt. 5 ) THEN
                    CALL  MA_CLDS ( marrpt, lvl, ipt, jret )
                  ELSE
                    ipt = ipt + 3
                END IF
              ELSE IF ( marrpt ( ipt:ipt+3 ) .eq. ' 907' ) THEN
C
C*              Decode the time duration of following phenomena group.
C             
                ipt = ipt + 4
                CALL  MA_TDUR ( marrpt, ipt, jret )
              ELSE IF ( marrpt ( ipt:ipt+3 ) .eq. ' 910' .and.
     +                  jflg ( 6 ) .eq. 0 ) THEN
C
C*              Decode the wind gust from 910ff group.
C             
                ipt    = ipt + 4
                iparam = 1   
                CALL  MA_WSPD ( marrpt, iparam, ipt, jret )
                jflg ( 6 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+3 ) .eq. ' 911' .and.
     +                  jflg ( 8 ) .eq. 0 ) THEN
C
C*              Decode the wind gust from 911ff group.
C             
                ipt    = ipt + 4
                iparam = 5   
                CALL  MA_WSPD ( marrpt, iparam, ipt, jret )
                jflg ( 8 ) = 1
              ELSE IF ( marrpt ( ipt:ipt+3 ) .eq. ' 912' .and.
     +                  jflg ( 7 ) .eq. 0 ) THEN
C
C*              Decode the wind gust from 912ff group.
C             
                ipt    = ipt + 4
                iparam = 2
                CALL  MA_WSPD ( marrpt, iparam, ipt, jret )
                jflg ( 7 ) = 1
            END IF
        END DO
C
        IF ( gums .ge. 0.0 ) THEN
C
C*          Add wind gust period and speed to the interface array.
C
            rivals ( irdtvm ) = xdtfvm
            rivals ( irgums ) = gums
        END IF
C
        IF ( lvl .gt. 0 .and. lvl .lt. 5 ) THEN
C
C*          Add cloud data to interface array.   
C
	    DO j = 1, lvl
		rivals ( irvss3 ( j ) ) = xclds ( 1, j )
		rivals ( ircla3 ( j ) ) = xclds ( 2, j )
		rivals ( irclt3 ( j ) ) = xclds ( 3, j )
		rivals ( irhcb3 ( j ) ) = xclds ( 4, j )
	    END DO
	    rivals ( irncl3 ) = lvl
        END IF
C*
	RETURN
        END
