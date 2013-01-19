        SUBROUTINE  LS_PRS3  ( lsfrpt, ipt, iret )
C************************************************************************
C* LS_PRS3                                                              *
C*                                                                      *
C* This subroutine decodes the group 5appp containing the 3-hour        *
C* pressure tendency and change.  The tendency is the WMO Table 200     *
C* pressure tendency value.                                             *
C*                                                                      *
C* LS_PRS3  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT          CHAR*           Report array                    *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         On input, points to a in group  *
C*					5appp; on output, points to     *
C*					last p in group                 *
C*					                                *
C* Output parameters:                                                   *
C*	RIVALS(IRP03D)	REAL		appp group                      *
C*      RIVALS(IRCHPT)  REAL            WMO Code Table 200 value        *
C*      RIVALS(IR3HPC)  REAL            3-hour pressure change in Pa    *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     12/96    Replaced ST_C2R with ST_INTG            *
C* R. Hollern/NCEP      1/98    Changes based on MA_PRS3                *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'lscmn.cmn'
C*
        CHARACTER*(*)   lsfrpt
C*
        CHARACTER       fld4*4, fld3*3, fld1*1
C------------------------------------------------------------------------
        iret = 0
C
C*	Check for missing value.
C
        IF ( lsfrpt ( ipt:ipt+3 ) .eq. '////' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
C*      Get appp group.
C
        fld4 = lsfrpt ( ipt:ipt+3 )
        CALL  ST_INTG ( fld4, ival, ier )
        IF ( ier .eq. 0 ) rivals ( irp03d ) = FLOAT ( ival )
C
C*      Get the characteristic of pressure tendency. 
C
        IF ( lsfrpt ( ipt:ipt ) .eq. '/' ) THEN
            ipt = ipt + 3
            RETURN
        END IF
C
        fld1 = lsfrpt ( ipt:ipt )
        CALL  ST_INTG( fld1, ival, ier )
        IF ( ier .ne. 0 .or. ival .eq. 9 ) THEN
            ipt = ipt + 3
            RETURN
          ELSE
            ipt = ipt + 1
            ipch = ival
            rivals ( irchpt ) = ival
        END IF
C
C*      Get the pressure change.
C
        fld3 = lsfrpt ( ipt:ipt+2 )
        CALL  ST_INTG ( fld3, ival, ier )
        IF ( ier .eq. 0 ) THEN
            ipt = ipt + 2
            rivals ( ir3hpc ) = .1 * FLOAT ( ival )
            IF ( ipch .ge. 5 .and. ipch .le. 8 ) 
     +	         rivals ( ir3hpc ) = -rivals ( ir3hpc )
          ELSE
            ipt = ipt + 1
        END IF
C*
        RETURN
        END
