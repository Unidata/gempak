	SUBROUTINE POLY_SZONE ( zone, dattim, iret )
C************************************************************************
C* POLY_SZONE  								*
C*									*
C* This subroutine sets zone index/names, grid time and table flag	*
C* to common.								*
C*									*
C* POLY_SZONE ( ZONE, DATTIM, IRET )					*
C*									*
C* Input parameters:							*
C*	ZONE		Zone name					*
C*	DATTIM		Grid time					*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-2 = cannot find the zone	*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 5/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	CHARACTER*(*)	zone, dattim
	CHARACTER*32	time, stime
C-----------------------------------------------------------------------
	iret = 0
	tbread = .false.
C
	CALL ST_LCUC ( zone, zone, ier )
	IF ( INDEX ( zone, 'OPC_PAC' ) .ne. 0 )  THEN
	    idxzon = 1
	    cdxzon = 'pac'
	  ELSE IF ( INDEX ( zone, 'OPC_ATL' ) .ne. 0 )  THEN
	    idxzon = 2
	    cdxzon = 'atl'
	  ELSE IF ( INDEX ( zone, 'TPC_ATL' ) .ne. 0 )  THEN
	    idxzon = 3
	    cdxzon = 'tpc'
	  ELSE 
	    iret = -2
	END IF
C
	fwarn = .true.
	time = dattim
	CALL ST_LCUC ( time, time, ier )
C
C*	Check watch or warning.
C
	IF ( ( INDEX ( time, 'F12'  ) .ne. 0 ) .or.
     +	     ( INDEX ( time, 'F012' ) .ne. 0 ) .or.
     +	     ( INDEX ( time, 'F15'  ) .ne. 0 ) .or.
     +	     ( INDEX ( time, 'F015' ) .ne. 0 ) )  THEN
C
	  ELSE
	    fwarn = .false.
	END IF
C
	idt = INDEX ( time, 'F' )	
	time = time ( : idt - 1 )
	itype = 1
	CALL CSS_GTIM ( itype, stime, ier )
	CALL TI_STAN ( time, stime, time, ier )
	idt = INDEX ( time, '/' )	
	gdattm = time ( : idt - 1 ) // time ( idt + 1 : idt + 2 )
C*
	RETURN
	END
