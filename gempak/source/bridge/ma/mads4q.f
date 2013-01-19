        SUBROUTINE  MA_DS4Q  ( marrpt, ipt, iret )
C************************************************************************
C* MA_DS4Q 						                *
C*								        *
C* This subroutine decodes the drifting buoy section 4 group            *
C* 1Q(P)Q(2)Q(TW)Q(4), which contains quality control data.             *
C*                                                                      *
C* MA_DS4Q  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:						        *
C*      MARRPT		CHAR*    	Report array                    *
C*								        *
C* Input and Output parameters:						*
C*      IPT             INTEGER         On input, points to first Q in  *
C*					group; on output, to last Q     *
C* Output parameters:						        *
C*	RIVALS(IRQOPM) 	REAL		Quality of pressure measurement *
C*	RIVALS(IRQCBH) 	REAL		Quality of ARGOS housekeepg parm*
C*	RIVALS(IRQWTM) 	REAL		Quality of water sfc temperature*
C*	RIVALS(IRQATM) 	REAL		Quality of air temperature      *
C*	IRET	 	INTEGER	    	Return code		        *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP	4/97	Removed interface calls, reorganized    *
C*				header and comments                     *
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE 	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER  	fld1*1
C------------------------------------------------------------------------
        iret = 0
C
C*      Get the quality of the pressure measurement.
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irqopm ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
C*      Get the quality of the ARGOS housekeeping parameter.
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irqcbh ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
C*      Get the quality of the water surface temperature.   
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irqwtm ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
C*      Get the quality of the air temperature.   
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irqatm ) = FLOAT ( ival )
        END IF
C*
	RETURN
        END
