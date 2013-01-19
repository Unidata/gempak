        SUBROUTINE  LS_SWLD ( lsfrpt, ipt, iret )
C************************************************************************
C* LS_SWLD                                                              *
C*                                                                      *
C* This subroutine decodes the group 3d(w1)d(w1)d(w2)d(w2), which       *
C* contains the primary and secondary directions from which the swell   *
C* waves are coming.  The direction is determined from the WMO          *
C* Table 0877 value.                                                    *
C*                                                                      *
C* LS_SWLD  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT          CHAR*           Report array                    *
C*					                                *
C* Input and output parameters:                                         *
C*	IPT		INTEGER		On input, points to first 'd' in*
C*					3d(w1)d(w1)d(w2)d(w2); on       *
C*					output, point to last d         *
C*					                                *
C* Output parameters:                                                   *
C*      XSWELL (6)      REAL            Primary and secondary           *
C*                                      direction in degrees from       *
C*                                      which swell waves are coming    *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP      1/98    Changes based on MA_SWLD                *
C* A. Hardy/GSC         1/98    Added GEMINC                            *
C************************************************************************
        INCLUDE  	'GEMPRM.PRM'
        INCLUDE  	'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER     	fld2*2
C------------------------------------------------------------------------
        iret = 0
C
C*      Get primary direction from which swell waves are coming.
C
        IF ( lsfrpt ( ipt:ipt+1 ) .eq. '//' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
        fld2 = lsfrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )
        ipt = ipt + 2
        IF ( ier .eq. 0 ) THEN
            IF ( ival .ge. 0 .and. ival .le. 36 ) THEN
                xswell ( 1 ) = FLOAT ( 10 * ival )
              ELSE
                RETURN
            END IF
          ELSE
            RETURN
        END IF
C
C*      Get secondary direction from which swell waves are coming.
C
        IF ( lsfrpt ( ipt:ipt+1 ) .eq. '//' ) THEN
            ipt = ipt + 1
            RETURN
        END IF
C
        fld2 = lsfrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )
        ipt = ipt + 1
        IF ( ier .eq. 0 ) THEN
            IF ( ival .ge. 0 .and. ival .le. 36 ) 
     +           xswell ( 4 ) = FLOAT ( 10 * ival )
        END IF
C*
        RETURN
        END
