        SUBROUTINE  MA_S4EG ( marrpt, ipt, iret )
C************************************************************************
C* MA_S4EG                                                              *
C*                                                                      *
C* This subroutine decodes the first of the drifting buoy section 4     *
C* groups 8V(i)V(i)V(i)V(i), which contains information on the          *
C* engineering status of the buoy.                                      *
C*                                                                      *
C* MA_S4EG ( MARRPT, IPT, IRET )                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         CHAR*       Report array                         *
C*                                                                      *
C* Input and output parameters:                                         *
C*	IPT	       INTEGER	   On input, points to the space before *
C*                                 group 8VVVV; on output, to the last  *
C*                                 V in group                           *
C* Output parameters:                                                   *
C*      RIVALS(IRBENG) REAL        Engineering status info on buoy      *
C*      IRET           INTEGER     Return code                          *
C*                                   0 = normal return                  *
C*                                   1 = problems                       *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP       9/99                                           *
C************************************************************************
        INCLUDE         'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER       fld4*4
C------------------------------------------------------------------------
        iret = 0
        ipt  = ipt + 2
        fld4 = marrpt ( ipt:ipt+3 )
        CALL  ST_INTG ( fld4, ival, ier )   
        ipt  = ipt + 3
        IF ( ier .eq. 0 ) THEN
            rivals ( irbeng )  =  FLOAT ( ival ) 
        END IF
C*
	RETURN
	END
