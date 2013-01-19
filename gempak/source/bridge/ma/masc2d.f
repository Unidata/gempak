        SUBROUTINE MA_SC2D  ( marrpt, iret )
C************************************************************************
C* MA_SC2D							        *
C*								        *
C* This subroutine calls the routines which decode the groups in        *
C* section 2 of the drifting buoy report.  This section contains surface*
C* marine data.                                                         *
C*                                                                      *
C* MA_SC2D  ( MARRPT, IRET )                                            *
C*                                                                      *
C* Input parameters:						        *
C*      MARRPT          CHAR*           Report array                    *
C*	LSEC2           INTEGER         Length of section 2             *
C*      ISEC2           INTEGER         Pointer to start of section 2   *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRQDS2) 	REAL		Quality control indicator for   *
C*	                                section 2. WMO Code table 3334. *
C*	RIVALS(IRQXS2) 	REAL		Indicator of position of group  *
C*	IRET	 	INTEGER	    	Return code		        *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP      12/96   Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP	 4/97	Changed interface, reorganized header   *
C*				and comments                            *
C* D. Kidwell/NCEP	10/97	Changed interface                       *
C* R. Hollern/NCEP       7/99   Decoded the wave period and height      *
C*                              groups and the quality control data     *
C* R. Hollern/NCEP       9/99   Bug fix if only wave data in section 222*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER       fld1*1
C------------------------------------------------------------------------
        iret = 0
        ip = isec2
        jflg = 0
        ipt = ip
C
C*      Get the quality control indicator for section 2.
C
        fld1 = marrpt (ipt:ipt)
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) rivals ( irqds2 ) = FLOAT ( ival )
C
C*      Get the indicator of position of group
C
        ipt = ipt + 1
        fld1 = marrpt (ipt:ipt)
        CALL  ST_INTG ( fld1, ival, ier )
        IF ( ier .eq. 0 ) rivals ( irqxs2 ) = FLOAT ( ival )
C
        iend = isec2 + lsec2 - 1
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
            IF ( marrpt (ipt:ipt+1) .eq. ' 0' .and.
     +           jflg .eq. 0 ) THEN
C
C*              Decode the 0STTT sea surface temperature group.
C
                ipt = ipt + 2
                iparam = 3
                CALL  MA_TEMP ( marrpt, iparam, ipt, jret )
                ipt = ipt + 1
            END IF
C
            IF ( marrpt (ipt:ipt+1) .eq. ' 1' .and.
     +           jflg .eq. 0 ) THEN
C
C*              Decode the wave period and height data groups.
C
                ipt = ipt + 2
                CALL  MA_DWPH ( marrpt, ipt, jret )
                jflg = 1
            END IF
        END DO
C*
	RETURN
        END
