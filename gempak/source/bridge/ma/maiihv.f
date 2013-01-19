        SUBROUTINE  MA_IIHV ( marrpt, ipt, iret )
C************************************************************************
C* MA_IIHV                                                              *
C*                                                                      *
C* This subroutine decodes the precipitation indicator, the type        *
C* of station (manned or automatic) and WX indicator, the base of the   *
C* lowest cloud, and the horizontal surface visibility.                 *
C*                                                                      *
C* MA_IIHV  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         	CHAR*           Report array                    *
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to start of    *
C*					i(R)i(X)hVV group; on output,   *
C*					points to space before next     *
C*					group in marrpt			*
C*					                                *
C* Output parameters:                                                   *
C*      IPREC          	INTEGER         Indicator for inclusion or      *
C*                                      omission of precip data         *
C*      IXIND          	INTEGER         Indicator for type of station   *
C*                                      operation and WX data           *
C*      HGTLCL         	REAL            Height in hundreds of feet of   *
C*                                      the lowest cloud base           *
C*      HGTLCX         	REAL            Height in meters of the lowest  *
C*                                      cloud base  	                *
C*	RIVALS(IRINPC)  REAL		Precipitation indicator         *
C*	RIVALS(IRITSO)  REAL		Type of station operation       *
C*	RIVALS(IRTOST)  REAL		Manned or automatic station     *
C*	RIVALS(IRCBAS)  REAL		Lowest cloud base height        *
C*	RIVALS(IRVSBK)  REAL		Horizontal visibility, km       *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*				header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C* A. Hardy/GSC		2/98    Added missing condition for low clouds  *
C* R. Hollern/NCEP	7/99	Modification to station indicator       *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        REAL     	hvis(0:99),  cloudh(0:9)
        CHARACTER       fld2*2, fld1*1
C*
C*      Height in meters of the base of the lowest cloud.
C*
        DATA  cloudh /   25.,    75.,   150.,   250.,   450.,   800.,
     +                 1250.,  1750.,  2250.,  2500. /
C*
C*      Horizontal visibility in km at surface. Code Table 4377.
C*
        DATA  hvis / 0.0,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,
     +   0.8,  0.9,  1.0,  1.1,  1.2,  1.3,  1.4,  1.5,  1.6,  1.7,
     +   1.8,  1.9,  2.0,  2.1,  2.2,  2.3,  2.4,  2.5,  2.6,  2.7,
     +   2.8,  2.9,  3.0,  3.1,  3.2,  3.3,  3.4,  3.5,  3.6,  3.7,
     +   3.8,  3.9,  4.0,  4.1,  4.2,  4.3,  4.4,  4.5,  4.6,  4.7,
     +   4.8,  4.9,  5.0,  -9999.,  -9999.,  -9999.,  -9999.,  -9999.,
     +   6.0,  7.0,  8.0,  9.0,  10.,  11.,  12.,  13.,  14.,  15.,
     +   16.,  17.,  18.,  19.,  20.,  21.,  22.,  23.,  24.,  25., 
     +   26.,  27.,  28.,  29.,  30.,  35.,  40.,  45.,  50.,  55.,
     +   60.,  65.,  70.,  71.,  0.0,  .05,  0.2,  0.5,  1.0,  2.0,
     +   4.0,  10.,  20.,  50. /
C------------------------------------------------------------------------
        iret = 0
C
C*      There is at most one space before start of group.
C
        IF ( marrpt (ipt:ipt) .eq. ' ' ) ipt = ipt + 1
C
C*      Check that length of section 1 is large enough.
C         
        IF ( lsec1 .lt. 5 ) THEN
            iret = 1
            RETURN
        END IF
C
C*      Get indicator for inclusion or omission of precipitation data.
C
        fld1 = marrpt ( ipt:ipt )
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) THEN
            iprec = ival
            rivals ( irinpc )  = FLOAT ( iprec )
            IF ( iprec .eq. 3 ) THEN
C
C*              The 6RRRt group is omitted, but precip amount is 0.0.
C
                iparam = 0
                CALL MA_PREC (marrpt, iparam, ipt , jret)
            END IF
        END IF
C
C*      Get indicator for type of station operation (manned or
C*      automatic) and for present and past weather data.
C
        ipt  = ipt + 1
        fld1 = marrpt ( ipt:ipt )
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) THEN
            ixind = ival
            rivals ( iritso ) = FLOAT ( ixind )
C
            IF ( ixind .ge. 1 .and. ixind .le. 3 ) THEN
C
C*		It is a manned station.
C
                rivals ( irtost ) = 1.0
              ELSE IF ( ixind .ge. 4 .and. ixind .le. 7 ) THEN
C
C*              It is an automatic station.
C 
                rivals ( irtost ) = 0.0
            END IF
        END IF
C
C*      Get height in meters of the base of the lowest cloud.
C*      Reference WMO Code Table 1600.
C*
        ipt  = ipt + 1
        fld1 = marrpt ( ipt:ipt )
        IF ( fld1 .ne. '/' ) THEN
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) THEN
                hgtlcx = cloudh ( ival )
C
C*              Convert to hundreds of feet for GEMPAK.
C
                hgtlcl = .0328 * hgtlcx
		rivals ( ircbas ) = FLOAT ( ival )
            END IF
          ELSE
	    rivals ( ircbas ) = -1.
        END IF

C
C*      Get horizontal visibility in kilometers.
C
        ipt = ipt + 1
        IF ( marrpt ( ipt:ipt ) .ne. '/' .and. 
     +       marrpt ( ipt+1:ipt+1 ) .ne. '/' ) THEN
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) THEN
                IF ( .not. ( ival .ge. 51 .and. ival .le. 55 ) )
     +               rivals ( irvsbk ) =  hvis ( ival )
            END IF
        END IF
C
        ipt = ipt + 2
C*
        RETURN
        END
