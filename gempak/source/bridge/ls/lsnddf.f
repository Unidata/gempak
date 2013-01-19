        SUBROUTINE  LS_NDDF ( lsfrpt, ipt, iret )
C************************************************************************
C* LS_NDDF                                                              *
C*                                                                      *
C* This subroutine decodes the Nddff group.  This group contains the    *
C* total cloud cover and wind direction and speed parameters.           *
C*                                                                      *
C* LS_NDDF  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT         	CHAR*           Report array                    *
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER 	On input, points to start of    *
C*					Nddff group; on output, to end  *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRCFRT) 	REAL            Total cloud cover in oktas      *
C*      RIVALS(IRDRCT)  REAL            Wind direction in degrees       *
C*      RIVALS(IRSPED) 	REAL            Wind speed in meters/second     *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP      8/96    Set wind direction to missing, if code  *
C*                              figure is 99                            *
C* R. Hollern/NCEP      1/98    Changes based on MA_NDDF                *
C* A. Hardy/GSC         1/98    Reordered calling sequence              *
C* R. Hollern/NCEP      2/98    Removed check that Nddff group exists   *
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'lscmn.cmn'
C
        CHARACTER*(*)    lsfrpt
C
        CHARACTER        fld2*2, fld1*1
C------------------------------------------------------------------------
        iret   = 0
        wdrct  = RMISSD
	ival   = IMISSD
C
C*      There is at most one space before start of group.
C
        IF ( lsfrpt ( ipt:ipt ) .eq. ' ' ) ipt = ipt + 1
C
C*      Get total cloud cover.  WMO Table 2700 value.
C
        IF ( lsfrpt (ipt:ipt) .ne. '/' ) THEN
            fld1 = lsfrpt (ipt:ipt)
            CALL  ST_INTG ( fld1, ival, ier )
            IF ( ier .eq. 0 ) rivals ( ircfrt ) = FLOAT ( ival )
        END IF
C
        ipt = ipt + 1
C
C*      Get wind direction in degrees.
C
        IF ( lsfrpt ( ipt:ipt ) .ne. '/' .and.
     +       lsfrpt ( ipt:ipt+1 ) .ne. '/' ) THEN
            fld2 = lsfrpt ( ipt:ipt+1 )
            CALL  ST_INTG ( fld2, ival, ier )
            IF ( ier .eq. 0 ) THEN
                IF ( ival .ge. 0 .and. ival .le. 36 ) THEN
                    wdrct = 10. * FLOAT ( ival ) 
                  ELSE IF ( ival .eq. 99 ) THEN
C
C*		    Direction is variable, or all directions (WMO Code
C*		    Table 0877).
C
		    wdrct = 99.
                END IF
            END IF
        END IF
C
C*      Get wind speed in meters/second.
C
        ipt    = ipt + 2
        iparam = 0
        CALL  LS_WSPD  ( lsfrpt, iparam, ipt, iret )
C
C*	If direction is variable but speed is 0, set direction missing.
C
	IF ( ival .eq. 99 .and. rivals ( irsped ) .eq. 0.0 )
     +       wdrct = RMISSD
C
	rivals ( irdrct ) = wdrct
C*
        RETURN
        END
