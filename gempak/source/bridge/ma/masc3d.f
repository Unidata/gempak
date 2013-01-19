        SUBROUTINE MA_SC3D ( marrpt, iret )
C************************************************************************
C* MA_SC3D			 				        *
C*								        *
C* This subroutine calls the routine which decodes the groups in        *
C* section 3 of the drifting buoy report. This section contains         *
C* temperatures, salinity and current (when available) at selected      *
C* depths.                                                              *
C*                                                                      *
C* MA_SC3D  ( MARRPT, IRET )                                            *
C*                                                                      *
C* Input parameters:						        *
C*      MARRPT          CHAR*           Report array                    *
C*	LSEC3           INTEGER         Length of section 3             *
C*      ISEC3           INTEGER         Pointer to start of section 3   *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRQ3D1) 	REAL		Quality control indicator for   *
C*	                                temperature/salinity profile.   *
C*	                                WMO Code Table 3334.            *
C*	RIVALS(IRQ3D2) 	REAL		Quality control indicator for   *
C*	                                current profile.  WMO Code      *
C*	                                Table 3334.                     *
C*	IRET	 	INTEGER	    	Return code		        *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       8/96                                           *
C* D. Kidwell/NCEP       4/97	Changed interface, reorganized header   *
C*				and comments				*
C* D. Kidwell/NCEP      10/97	Changed interface                       *
C* R. Hollern/NCEP       7/99   Decoded the quality control indicators  *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER       fld1*1
C------------------------------------------------------------------------
        iret = 0
        ipt = isec3
        iend = isec3 + lsec3 - 1
C
C*      Get the quality control indicator for temperature/salinity
C*      profile.
C
        fld1 = marrpt (ipt:ipt)
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) rivals ( irq3d1 ) = FLOAT ( ival )
C
C*      Get the quality control indicator for current profile.    
C
        ipt = ipt + 1
        fld1 = marrpt (ipt:ipt)
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) rivals ( irq3d2 ) = FLOAT ( ival )
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
            IF ( marrpt ( ipt:ipt+4 ) .eq. ' 8887' ) THEN
C
C*              Decode the depth, temperature and salinity data groups.
C
                ipt = ipt + 5
C
                CALL MA_TESA( marrpt, iend, ipt, jret )
C
              ELSE IF ( marrpt ( ipt:ipt+2 ) .eq. ' 66' ) THEN
C
C*              Will add routines later to decode these groups.
C
            END IF
        END DO
C*
	RETURN
        END
