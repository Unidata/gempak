        SUBROUTINE  MA_DWPH ( marrpt, ipt, iret )
C************************************************************************
C* MA_DWPH                                                              *
C*                                                                      *
C* This subroutine decodes the wave period and wave height data groups  *
C* 1P(wa)P(wa)H(wa)H(wa), 20P(wa)P(wa)P(wa), and 21H(wa)H(wa)H(wa) in   *
C* the WMO FM18 drifting buoy report.  The data in these groups are     *
C* obtained by instrumental methods.  The wave data in the 20-group and *
C* the 21-group contain the wave period and height, repectively, to a   *
C* greater degree of accuracy than that given in the 1-group.           *
C*                                                                      *
C* MA_DWPH  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         	CHAR*           Report array                    *
C*					                                *
C* Input and output parameters:                                         *
C*	IPT		INTEGER		On input, points to first 'P' in*
C*					1-group; on output, points to   *
C*					last character of group decoded *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRWPER) 	REAL            Wave period in seconds          *
C*      RIVALS(IRWHGT) 	REAL            Wave height in meters           *
C*	IRET           	INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      7/99                                            *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER   	fld2*2,   fld3*3
C------------------------------------------------------------------------
        iret = 0
C
C*      Get wave period in seconds.
C
        IF ( marrpt ( ipt:ipt+1 ) .ne. '//' ) THEN
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rivals (irwper) = FLOAT ( ival )      
            END IF
        END IF
C
C*      Get wave height in half-meters.
C
        ipt = ipt + 2
C 
        IF ( marrpt ( ipt:ipt+1 ) .ne. '//' ) THEN
            fld2 = marrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rivals (irwhgt) = .5 * FLOAT ( ival )      
            END IF
        END IF
C
        ipt = ipt + 3
C
C*      Get wave period in tenths of a second.
C
        IF ( marrpt ( ipt:ipt+1 ) .eq. '20' ) THEN
            ipt = ipt + 2
            fld3 = marrpt ( ipt:ipt+2 )
            CALL  ST_INTG ( fld3, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rivals (irwper) = .1 * FLOAT ( ival )      
            END IF
        END IF
C
        ipt = ipt + 4
C
C*      Get wave height in tenths of a meter.
C
        IF ( marrpt ( ipt:ipt+1 ) .eq. '21' ) THEN
            ipt = ipt + 2
            fld3 = marrpt ( ipt:ipt+2 )
            CALL  ST_INTG ( fld3, ival, ier )
            IF ( ier .eq. 0 ) THEN
                rivals (irwhgt) = .1 * FLOAT ( ival )      
            END IF
            ipt = ipt + 3
        END IF
C*
        RETURN
        END
