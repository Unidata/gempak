	SUBROUTINE TG_CTOI  ( gdattm, intdtf, iret )
C************************************************************************
C* TG_CTOI								*
C*									*
C* This subroutine converts a full grid date/time string into the	*
C* three integers for date, time and forecast time.			*
C*									*
C* TG_CTOI  ( GDATTM, INTDTF, IRET )					*
C*									*
C* Input parameters:							*
C*	GDATTM		CHAR*		GEMPAK grid date/time		*
C*									*
C* Output parameters:							*
C*	INTDTF (3)	INTEGER		Grid date, time, forecast	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = invalid forecast type	*
C*					 -3 = invalid forecast time	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C* L. Sager/NMC		 5/93		Use saved values if available	* 
C************************************************************************
	CHARACTER*(*)	gdattm
	INTEGER		intdtf (*)
C*
	CHARACTER	time*20, ftype*1, ftime*8, tame*20
	LOGICAL		vtype
C*
        CHARACTER	gtime*20
	INTEGER         jntdtf (3) 
C*
	SAVE		gtime, jntdtf 
C*
	DATA		gtime /' '/, jntdtf /3*-1/
C------------------------------------------------------------------------
	iret  = 0
	IF  ( gdattm  .eq. gtime ) THEN
	    intdtf (1) = jntdtf (1)
	    intdtf (2) = jntdtf (2)
            intdtf (3) = jntdtf (3)
	    return
	END IF
	time  = gdattm
	vtype = .false.
	intdtf (1) = 0
	intdtf (2) = 0
	intdtf (3) = 0
C
C*	If the string is blank, encode all zeros.
C
	IF  ( time .eq. ' ' )  THEN
	    RETURN
	END IF
C
C*	Since this must be a full GEMPAK grid time, the forecast
C*	information is in the 12th character followed by the forecast
C*	time.  Translate these into the integer time.
C
	ftype = time (12:12)
	CALL ST_LCUC  ( ftype, ftype, ier )
	ftime = time (13:  )
C
C*	If the forecast type is V, set type to F and set flag to
C*	translate time later.
C
	IF  ( ftype .eq. 'V' )  THEN
	    ftype = 'F'
	    vtype = .true.
	END IF
C
C*	Compute the integer forecast time.
C
	CALL TG_IFTM  ( ftype, ftime, intdtf (3), iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Translate the GEMPAK time into two integers.
C
	tame = time ( : 11 )
	CALL TI_IDTM  ( tame, intdtf (1), intdtf (2), iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	If the time was a V time, change the date and time to
C*	a forecast time.
C
	IF  ( vtype )  CALL TG_VTOF  ( intdtf, intdtf, ier )
C
C*    	Save new values
C
	gtime = time
	jntdtf (1) = intdtf (1)
	jntdtf (2) = intdtf (2)
	jntdtf (3) = intdtf (3)
C*
	RETURN
	END
