        SUBROUTINE  MA_S4DG ( marrpt, ipt, iret )
C************************************************************************
C* MA_S4ED                                                              *
C*                                                                      *
C* This subroutine decodes the drifting buoy section 4 group            *
C* 9i(d)Z(d)Z(d)Z(d), which contains the drogue type and the length of  *
C* the cable at which the drogue is attached.                           *
C*                                                                      *
C* MA_S4ED ( MARRPT, IPT, IRET )                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT         CHAR*             Report array                   *
C*	IPT	       INTEGER	       	 Points to the space preceding  *
C*                                       the group 9iZZZZ               *
C* Output parameters:                                                   *
C*      RIVALS(IRDROT) REAL              Drogue type                    *
C*      RIVALS(IRCALN) REAL              Cable length in meters         *
C*      IRET           INTEGER           Return code                    *
C*                                         0 = normal return            *
C*                                         1 = problems                 *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* R. Hollern/NCEP       9/99                                           *
C************************************************************************
        INCLUDE         'macmn.cmn'
C*
        CHARACTER*(*)   marrpt
C*
        CHARACTER       fld1*1,  fld3*3
C------------------------------------------------------------------------
        iret  = 0
C
        ipt  = ipt + 2
        fld1 = marrpt ( ipt:ipt )
        CALL  ST_INTG ( fld1, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            rivals ( irdrot )  =  FLOAT ( ival ) 
          ELSE
            iret = 1
            RETURN
        END IF
C
        ipt  = ipt + 1
        fld3 = marrpt ( ipt:ipt+2 )
        CALL  ST_INTG ( fld3, ival, ier )   
        IF ( ier .eq. 0 ) THEN
            rivals ( ircaln )  =  FLOAT ( ival ) 
        END IF
C*
	RETURN
	END
