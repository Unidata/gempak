        SUBROUTINE  MA_TESA  ( marrpt, iend, ipt, iret )
C************************************************************************
C* MA_TESA                                                              *
C*                                                                      *
C* This subroutine decodes the groups zzzz, TTTT, and SSSS following    *
C* the 8887k group in section 3 of the drifting buoy report.  These     *
C* groups contain temperature (TTTT) and salinity (SSSS) data at        *
C* selected or significant depths (zzzz).  The method of salinity/depth *
C* measurement is given by the k value in the 8887k group.              *
C*                                                                      *
C* MA_TESA ( MARRPT, IEND, IPT, IRET )                                  *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT 		CHAR*    	Report array                    *
C*      IEND            INTEGER         Points to the position in       *
C*                                      marrpt of the last section 3    *
C*                                      character                       *
C*					                                *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         On input, points to the         *
C*					position in marrpt of k in the  *
C*					group 8887k; on output, points  *
C*					to the position in marrpt of the*
C*                                      last section 3 character or the *
C*                                      position in marrpt of the second*
C*                                      6 in the group 66k(6)9k(3)      *
C*                                                                      *
C* Output parameters:                                                   *
C*	RIVALS(IRMSDM)	REAL		Method of measurement           *
C*	RIVALS(IRNDTS)	REAL		Number of levels of data        *
C*	RIVALS(IRDBSS)	REAL		Depth in meters                 *
C*	RIVALS(IRSTMP)	REAL		Temperature, hundredths degr C  *
C*	RIVALS(IRSALN)	REAL		Salinity, 1/100ths ppt          *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = No problems               *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      8/96                                            *
C* R. Hollern/NCEP     11/96    Added call to PR_TMCK                   *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP  	4/97	Removed interface calls, reorganized    *
C*				header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface, removed PR_TMCK      *
C* D. Kidwell/NCEP      3/02	Corrected prologue                      *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)  	 marrpt
C*
        CHARACTER     	 fld4*4, fld1*1
C------------------------------------------------------------------------
        iret = 0
        iptsav = 0
C
C*      Get the method of salinity/depth measurement 
C*      (WMO Code Table 2263).
C
        IF ( marrpt ( ipt:ipt ) .ne. '/' ) THEN
            fld1 = marrpt ( ipt:ipt )
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irmsdm ) = FLOAT ( ival )
        END IF
C
        lvl = 0
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
            IF ( marrpt ( ipt:ipt+1 ) .eq. ' 2' ) THEN
C
                lvl = lvl + 1
		IF ( lvl .gt. MXDLYR ) lvl = MXDLYR
C
C*              Get selected and/or significant depths, in meters,
C*              starting with the surface.
C
                ipt = ipt + 2
                fld4 = marrpt ( ipt:ipt+3 )
                CALL  ST_INTG ( fld4, ival, ier )
                IF ( ier .eq. 0 ) 
     +               rivals ( irdbss ( lvl ) ) = FLOAT ( ival )
                ipt = ipt + 3
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 3' ) THEN
C
C*              Get temperatures, in hundredths of a degree Celsius.
C
                ipt = ipt + 2
                fld4 = marrpt ( ipt:ipt+3 )
                CALL  ST_INTG ( fld4, ival, ier )
                IF ( ier .eq. 0 ) THEN
                    rval = FLOAT ( ival )
C
C*                  Check for negative temperature.
C
                    IF ( rval .gt. 4999. ) rval = 5000. - rval   
                    rivals ( irstmp ( lvl ) ) = .01 * rval
                END IF
                ipt = ipt + 3
              ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 4' ) THEN
C
C*              Get salinity, in hundredths of a part per thousand (%).
C
                ipt = ipt + 2
                fld4 = marrpt ( ipt:ipt+3 )
                CALL  ST_INTG ( fld4, ival, ier )
                IF ( ier .eq. 0 )
     +               rivals ( irsaln ( lvl ) ) = .01 * FLOAT ( ival )
                ipt = ipt + 3
              ELSE IF ( marrpt ( ipt:ipt+2 ) .eq. ' 66' ) THEN
                iptsav = ipt - 1
                ipt = iend
            END IF
        END DO
C
        IF ( iptsav .ne. 0 ) ipt = iptsav
	IF ( lvl .gt. 0 ) rivals ( irndts ) = FLOAT ( lvl )
C*
        RETURN
        END
