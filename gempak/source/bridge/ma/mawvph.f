        SUBROUTINE  MA_WVPH ( marrpt, iparm, ipt, iret )
C************************************************************************
C* MA_WVPH                                                              *
C*                                                                      *
C* This subroutine decodes the wave period and height in the 1PPHH,     *
C* 2PPHH, 4P(w1)P(w1)H(w1)H(w2), or 5P(w2)P(w2)H(w2)H(w2) groups.  The  *
C* groups 4 and 5 contain the primary and secondary swell period and    *
C* height, respectively.                                                *
C*                                                                      *
C* MA_WVPH  ( MARRPT, IPARM, IPT, IRET )                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         	CHAR*           Report array                    *
C*      IPARM          	INTEGER         Flag value                      *
C*                                        1 = decoding 1 group          *
C*                                        2 = decoding 2 group          *
C*                                        3 = decoding 4 group          *
C*                                        4 = decoding 5 group          *
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to first 'P' in*
C*					1, 2, 4 or 5 PHHH groups; on    *
C*					output, points to last H        *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRWPER) 	REAL            Wave period in seconds, group 1 *
C*      XWVHGT         	REAL            Wave height in meters, group 1  *
C*      RIVALS(IRPOWW) 	REAL            Wave period in seconds, group 2 *
C*	RIVALS(IRHOWW)	REAL		Wave height in meters, group 2  *
C*      XSWELL (6)     	REAL            Primary and secondary period    *
C*                                      in seconds and height in        *
C*                                      meters of the swell waves       *
C*	IRET           	INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*				header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface, cleaned up code      *
C* R. Hollern/NCEP     12/97	Fixed mismatch of types in IF statement *
C* R. Hollern/NCEP      7/99	Removed check for a wave period greater *
C*                              than 63 seconds                         *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER   	fld2*2
C------------------------------------------------------------------------
        iret = 0
C
C*      Get wave period in seconds.
C
        xx  = RMISSD
        IF ( marrpt ( ipt:ipt+1 ) .ne. '//' ) THEN
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) THEN
                xx = FLOAT ( ival )      
            END IF
        END IF
C
C*      Get wave height in meters.
C
        ipt = ipt + 2
        yy  = RMISSD
        IF ( marrpt ( ipt:ipt+1 ) .ne. '//' ) THEN
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 )  yy = .5 * FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
        IF ( iparm .eq. 1 ) THEN
            rivals ( irwper ) = xx
            xwvhgt = yy
          ELSE IF ( iparm .eq. 2 ) THEN
            rivals ( irpoww ) = xx
            rivals ( irhoww ) = yy
          ELSE IF ( iparm .eq. 3 ) THEN
            xswell ( 2 ) = xx
            xswell ( 3 ) = yy
          ELSE IF ( iparm .eq. 4 ) THEN
            xswell ( 5 ) = xx
            xswell ( 6 ) = yy
        END IF
C*
        RETURN
        END
