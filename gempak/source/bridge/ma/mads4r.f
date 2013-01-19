        SUBROUTINE  MA_DS4R  ( marrpt, ipt, iret )
C************************************************************************
C* MA_DS4R 				         	                *
C*								        *
C* This subroutine decodes the drifting buoy section 4 group            *
C* 2Q(N)Q(L)Q(A)/, which contains the quality of the buoy satellite     *
C* transmission and location.                                           *
C*                                                                      *
C* MA_DS4R  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:						        *
C*      MARRPT          CHAR*          	Report array                    *
C*								        *
C* Input and Output parameters:					        *
C*      IPT             INTEGER        	On input, points to first Q in  *
C*					group; on output, points to     *
C*					second Q			*
C*									*
C* Output parameters:						        *
C*	RIVALS(IRQBST)	REAL		Quality of buoy satellite trans.*
C*	RIVALS(IRQCIL)	REAL		Quality of location             *
C*	RIVALS(IRQ4CL)	REAL		Location quality class          *
C*	IRET	 	INTEGER	    	Return code		        *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C**								        *
C* Log:								        *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* D. Kidwell/NCEP  	4/97	Removed interface calls, reorganized    *
C*				header and comments      		*
C* D. Kidwell/NCEP     10/97	Changed interface                       *
C* R. Hollern/NCEP      7/99	Decoded location quality class parameter*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)    marrpt
C*
        CHARACTER        fld1*1
C------------------------------------------------------------------------
        iret = 0
C
C*      Get the quality of the buoy satellite transmission - reference 
C*      WMO Code Table 3313.
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irqbst ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
C*      Get the quality of the location - reference WMO Code Table 3311.
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irqcil ) = FLOAT ( ival )
        END IF
C 
        ipt = ipt + 1
C
C*      Get the location quality class - reference WMO Code Table 3302.
C
        IF ( marrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = marrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( irq4cl ) = FLOAT ( ival )
        END IF
C*
	RETURN
        END
