        SUBROUTINE  MA_OBST ( marrpt, ipt, iret )
C************************************************************************
C* MA_OBST                                                              *
C*                                                                      *
C* This subroutine decodes the time of observation group 9GGgg.         *
C* Adjustments to the date/time data will be made if the 9-group obs    *
C* hour differs from the obs hour in the group YYGGi.  The obs hour/mins*
C* in the 9-group will have priority over the obs hour in the YYGGi     *
C* group.                                                               *
C*                                                                      *
C* MA_OBST  ( MARRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      MARRPT          CHAR*           Report array                    *
C*					                                *
C* Input and Output parameters:                                         *
C*	IPT		INTEGER		On input, points to first 'G'   *
C*					in 9GGgg; on output, points to  *
C*					last g                          *
C*					                                *
C* Output parameters:                                                   *
C*      IRPTDT (*)      INTEGER         Report date-time                *
C*                                      (YYYY, MM, DD, HH, MM)          *
C*	IRET            INTEGER         Return code                     *
C*				   	  0 = Normal return 	        *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* D. Kidwell/NCEP	4/97		Reorganized header and cleaned  *
C*					up code				*
C* D. Kidwell/NCEP     10/97		Documentation                   *
C* D. Kidwell/NCEP      1/00		Checked for valid hour, minutes *
C************************************************************************
        INCLUDE  	'macmn.cmn'
C*
	CHARACTER*(*)	marrpt
C*
        INTEGER   	jrptdt(5)
        CHARACTER  	fld2*2
C------------------------------------------------------------------------
        iret = 0
C
        fld2 = marrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )
        ipt = ipt + 2
        IF ( ( ier .eq. 0 ) .and. ( ival .ge. 0 ) .and. 
     +       ( ival .le. 23 ) ) THEN
            iobshr = ival     
          ELSE
            ipt = ipt + 2
            RETURN
        END IF
C
        fld2 = marrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )
        ipt = ipt + 2
        IF ( ( ier .eq. 0 ) .and. ( ival .ge. 0 ) .and. 
     +       ( ival .le. 59 ) ) THEN
            iobmin = ival     
          ELSE
            RETURN
        END IF
C
        IF ( iobshr .gt. irptdt ( 4 ) ) THEN
C
C*          Observation occurred in previous day.  Backdate 1 day.
C
            CALL  TI_SUBD ( irptdt, jrptdt, jret )
C
            irptdt ( 1 ) = jrptdt ( 1 )
            irptdt ( 2 ) = jrptdt ( 2 )
            irptdt ( 3 ) = jrptdt ( 3 )
            irptdt ( 4 ) = iobshr
            irptdt ( 5 ) = iobmin
          ELSE
            irptdt ( 4 ) = iobshr
            irptdt ( 5 ) = iobmin
        END IF
C*
        RETURN
        END
