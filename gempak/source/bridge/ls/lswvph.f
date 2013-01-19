        SUBROUTINE  LS_WVPH ( lsfrpt, iparam, ipt, iret )
C************************************************************************
C* LS_WVPH                                                              *
C*                                                                      *
C* This subroutine decodes the wave period and height in the            *
C* 1P(wa)P(wa)H(wa)H(wa), 2P(w)P(w)H(w)H(w), 4P(w1)P(w1)H(w1)H(w1), or  *
C* 5P(w2)P(w2)H(w2)H(w2) groups.  The groups 4 and 5 contain the        *
C* primary and secondary swell period and height, respectively.         *
C*                                                                      *
C* LS_WVPH  ( LSFRPT, IPARAM, IPT, IRET )                               *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         	CHAR*           Report array                    *
C*      IPARAM         	INTEGER         Flag value                      *
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
C* R. Hollern/NCEP      1/98    Changes based on MA_WVPH                *
C* A. Hardy/GSC         1/98    Reorder calling sequence                *
C* R. Hollern/NCEP      8/99    Removed check for a wave period greater *
C*                              than 63 seconds                         *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER   	fld2*2
C------------------------------------------------------------------------
        iret = 0
C
C*      Get wave period in seconds.
C
        xx  = RMISSD
        IF ( lsfrpt ( ipt:ipt+1 ) .ne. '//' ) THEN
            fld2 = lsfrpt ( ipt:ipt+1 )
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
        IF ( lsfrpt ( ipt:ipt+1 ) .ne. '//' ) THEN
            fld2 = lsfrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 )  yy = .5 * FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
        IF ( iparam .eq. 1 ) THEN
            rivals ( irwper ) = xx
            xwvhgt = yy
          ELSE IF ( iparam .eq. 2 ) THEN
            rivals ( irpoww ) = xx
            rivals ( irhoww ) = yy
          ELSE IF ( iparam .eq. 3 ) THEN
            xswell ( 2 ) = xx
            xswell ( 3 ) = yy
          ELSE IF ( iparam .eq. 4 ) THEN
            xswell ( 5 ) = xx
            xswell ( 6 ) = yy
        END IF
C*
        RETURN
        END
