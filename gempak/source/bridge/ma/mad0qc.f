        SUBROUTINE  MA_D0QC  ( marrpt, ipt, iret )
C************************************************************************
C* MA_D0QC 						                *
C*								        *
C* This subroutine decodes the drifting buoy section 0 group            *
C* 6Q(l)Q(t)Q(A)/, which contains quality control indicators.           *
C*                                                                      *
C* MA_D0QC  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:						        *
C*      MARRPT		CHAR*    	Report array                    *
C*								        *
C* Input and output parameters:						*
C*      IPT             INTEGER         On input, points to first Q in  *
C*					group; on output, to last /     *
C* Output parameters:						        *
C*	RIVALS(IRQPOS) 	REAL		Quality control indicator for   *
C*	                                position - WMO Code table 3334  *
C*	RIVALS(IRQTIM) 	REAL		Quality control indicator for   *
C*	                                time - WMO Code table 3334      *
C*	RIVALS(IRQCLS) 	REAL		Quality control indicator for   *
C*	                                location of quality class -     *
C*	                                WMO Code table 3302             *
C*	IRET	 	INTEGER	    	Return code		        *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      7/99                                            *
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
C*      Get the quality control indicator for postion.
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irqpos ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
C*      Get the quality control indicator for time.
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irqtim ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
C*      Get the quality control indicator for location qualilty class.
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irqcls ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C*
	RETURN
        END
