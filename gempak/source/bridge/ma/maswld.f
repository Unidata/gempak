        SUBROUTINE  MA_SWLD ( marrpt, ipt, iret )
C************************************************************************
C* MA_SWLD                                                              *
C*                                                                      *
C* This routine decodes the group 3d(w1)d(w1)d(w2)d(w2), which contains *
C* the primary and secondary directions from which the swell waves are  *
C* coming.  The direction is determined from the WMO TABLE 0877 value.  *
C*                                                                      *
C* MA_SWLD  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
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
C* R. Hollern/NCEP      8/96    Set swell wave direction to missing if  *
C*                              code figure is 99                       *
C* D. Kidwell/NCEP	4/97	Reorganized header and comments, cleaned*
C*				up code					*
C* D. Kidwell/NCEP     10/97	Cleaned up                              *
C* R. Hollern/NCEP     12/97	Corrected secondary wave dir problem    *
C************************************************************************
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER     	fld2*2
C------------------------------------------------------------------------
        iret = 0
C
C*      Get primary direction from which swell waves are coming.
C
        IF ( marrpt ( ipt:ipt+1 ) .eq. '//' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
        fld2 = marrpt ( ipt:ipt+1 )
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
        IF ( marrpt ( ipt:ipt+1 ) .eq. '//' ) THEN
            ipt = ipt + 1
            RETURN
        END IF
C
        fld2 = marrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )
        ipt = ipt + 1
        IF ( ier .eq. 0 ) THEN
            IF ( ival .ge. 0 .and. ival .le. 36 ) 
     +           xswell ( 4 ) = FLOAT ( 10 * ival )
        END IF
C*
        RETURN
        END
