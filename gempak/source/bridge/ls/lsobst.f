        SUBROUTINE  LS_OBST ( lsfrpt, ipt, iret )
C************************************************************************
C* LS_OBST                                                              *
C*                                                                      *
C* This subroutine decodes the time of observation group 9GGgg.         *
C* Adjustments to the date/time data will be made if the 9-group obs    *
C* hour differs from the obs hour in the group YYGGi.  The obs hour/mins*
C* in the 9-group will have priority over the obs hour in the YYGGi     *
C* group.                                                               *
C*                                                                      *
C* LS_OBST  ( LSFRPT, IPT, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      LSFRPT          CHAR*           Report array                    *
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
C*				   	  0 = normal return 	        *
C*                                        1 = problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP      6/96                                            *
C* R. Hollern/NCEP     10/96    Added range check on time values        *
C* R. Hollern/NCEP      1/98    Changes based on MA_WSPD                *
C* A. Hardy/GSC         1/98    Added GEMINC                            *
C************************************************************************
        INCLUDE  	'GEMPRM.PRM'
        INCLUDE  	'lscmn.cmn'
C*
	CHARACTER*(*)	lsfrpt
C*
        INTEGER   	jrptdt(5)
        CHARACTER  	fld2*2
C------------------------------------------------------------------------
        iret = 0
C
        fld2 = lsfrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )
        ipt = ipt + 2
        IF ( ier .eq. 0 .and.
     +       ( ival .ge. 0 .and. ival .le. 23 ) ) THEN
            iobshr = ival     
          ELSE
            ipt = ipt + 2
            RETURN
        END IF
C
        fld2 = lsfrpt ( ipt:ipt+1 )
        CALL  ST_INTG ( fld2, ival, ier )
        ipt = ipt + 2
        IF ( ier .eq. 0  .and.
     +       ( ival .ge. 0 .and. ival .le. 59 ) ) THEN
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
