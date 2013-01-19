        SUBROUTINE  MA_DSVS ( marrpt, ipt, iret )
C************************************************************************
C* MA_DSVS                                                              *
C*                                                                      *
C* This subroutine decodes the 222D(s)v(s) group at start of section 2. *
C* The D(s) is the WMO Code Table 0700 value for the true direction of  *
C* resultant displacement of the ship during the 3 hours preceding the  *
C* time of observation.  The v(s) is the WMO Code Table 4451 value for  *
C* the ship's average speed during this period.                         *
C*                                                                      *
C* MA_DSVS  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to 'D' in      *
C*					222D(s)v(s); on output, points  *
C*					to v                            *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRTDMP)  REAL            Direction of ship's movement    *
C*      RIVALS(IRASMP)  REAL            Ship's average speed in m/sec   *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*				header and comments			*
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE		'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C------------------------------------------------------------------------
        iret = 0
C
C*      Get direction of ship's movement from WMO Code Table 700.
C
        IF ( marrpt ( ipt:ipt ) .ne. '/' ) THEN
            CALL  ST_INTG ( marrpt ( ipt:ipt ), ival, ier )
            IF ( ier .eq. 0 ) rivals ( irtdmp ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
C*      Get ship's average speed during the 3 hours preceding time of
C*      obs from WMO Code Table 4451.  
C
        IF ( marrpt ( ipt:ipt ) .ne. '/' ) THEN
            CALL  ST_INTG ( marrpt ( ipt:ipt ), ival, ier )
            IF ( ier .eq. 0 ) rivals ( irasmp )  = FLOAT ( ival )    
        END IF
C*
        RETURN
        END
