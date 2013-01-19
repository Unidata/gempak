        SUBROUTINE MA_SC4D  ( marrpt, iret )
C************************************************************************
C* MA_SC4D							        *
C*								        *
C* This subroutine calls the routines which decode the groups in        *
C* section 4 of the drifting buoy report.  This section contains        *
C* quality control data and information on engineering and technical    *
C* parameters.                                                          *
C*                                                                      *
C* MA_SC4D  ( MARRPT, IRET )                                            *
C*                                                                      *
C* Input parameters:						        *
C*      MARRPT          CHAR*           Report array                    *
C*	LSEC4           INTEGER         Length of section 4 in report   *
C*      ISEC4           INTEGER         Pointer to start of section 4   *
C*								        *
C* Output parameters:						        *
C*	IRET	 	INTEGER	    	Return code		        *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP	4/97	Changed interface, reorganized header   *
C*				and comments                            *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C* R. Hollern/NCEP      9/99	Decoded engineering and technical parms *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C------------------------------------------------------------------------
        iret = 0
        iql = 0
C
        ip = isec4
        jflg1 = 0
        jflg2 = 0
        jflg3 = 0
C
        iend = isec4 + lsec4 - 1
        ipt = ip
C
        DO WHILE ( ipt .lt. iend )
C
            ipt = ipt + 1
C
           IF ( marrpt (ipt:ipt+1) .eq. ' 9' ) THEN
C
C*             Decode the drogue data.
C
               CALL MA_S4DG ( marrpt, ipt, jret )
               RETURN
             ELSE IF ( marrpt (ipt:ipt+1) .eq. ' 8' .and.
     +                 jflg1 .eq. 0 ) THEN
C
C*             Decode the engineering data.
C
               CALL MA_S4EG ( marrpt, ipt, jret )
               jflg1 = 1
             ELSE IF ( marrpt ( ipt:ipt+1 ) .eq. ' 1' .and.
     +                 jflg2 .eq. 0 ) THEN
C
C*             Decode the first set of quality control indicators.
C
               ipt = ipt + 2
               CALL  MA_DS4Q  ( marrpt, ipt, jret )
               jflg2 = 1
             ELSE IF ( marrpt (ipt:ipt+1) .eq. ' 2' .and.
     +                 jflg3 .eq. 0 ) THEN
C
C*             Decode the second set of quality control indicators.
C
               ipt = ipt + 2
               CALL  MA_DS4R  ( marrpt, ipt, jret )
               jflg3 = 1
C
               iql = rivals ( irqcil ) + .05
C
               IF ( iql .eq. 1 ) THEN
C
C*                 Decode the time and location of the last known
C*                 position of the buoy.
C
                   CALL MA_S4TP ( marrpt, ipt, jret )
               END IF
C
               IF ( iql .eq. 2 ) THEN
C
C*                 Decode the latitude and longitude location of the
C*                 second possible solution (symmetrical to the
C*                 satellite subtrack).
C
                   CALL MA_S4LL ( marrpt, ipt, jret )
                END IF
            END IF
        END DO
C*
	RETURN
        END
