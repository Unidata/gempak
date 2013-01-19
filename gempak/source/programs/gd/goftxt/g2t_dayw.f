	SUBROUTINE G2T_DAYW ( dayw, iret )
C************************************************************************
C* G2T_DAYW								*
C*									*
C* This subroutine gets days of the week for the forecast periods using *
C* the initial time of the forecast cycle.				*
C*									*
C* G2T_DAYW ( DAYW, IRET )						*
C*									*
C* Input parameters:							*
C* Ouput parameters:							*
C*	DAYW(NGRDTM)	CHAR*		Day of the week for G2T output	*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		10/06	Created					*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	dayw(*)
	INTEGER		idtarr(5)
	CHARACTER	time*20, fdate(5)*14, ddd(7)*3
	CHARACTER*2	cyc_00(2), cyc_06(2), cyc_12(2), cyc_18(2)
	CHARACTER*2	f12, hrs
	LOGICAL		done, more
C*
	DATA		ddd /  'SUN', 'MON', 'TUE', 'WED', 'THU',
     +				'FRI', 'SAT' /
	DATA		cyc_00 / '12', '15' /, cyc_06 / '18', '21' /
	DATA		cyc_12 / '00', '03' /, cyc_18 / '06', '09' /
	DATA		fdate /	'TODAY', 'THIS AFTERNOON', 'TONIGHT',
     +				 'OVERNIGHT', ' NIGHT'/
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize the output.
C
	DO kk = 1, ngrdtm
	    dayw ( kk ) = ' '
	END DO
C
	nt = 1
	done = .false.
	DO WHILE ( .not. done )
C
C*	    Add days of the week.
C
	    CALL TG_VALD ( grdtm ( nt ), time, ier )
	    CALL TI_CTOI ( time, idtarr, ier )
	    CALL TI_DAYW ( idtarr, idaywk, ier )
	    IF ( nt .eq. 1 )   THEN
		f12 = time ( 8 : 9 )
	      ELSE 
		hrs = time ( 8 : 9 )
	    END IF
C
C*	    Get day/night. 
C
	    more = .true.	
	    IF ( f12 .eq. cyc_12 ( 1 ) .or. f12 .eq. cyc_12 (2) )  THEN
		IF  ( nt .eq. 1 )  THEN
		    dayw ( nt ) = fdate ( 3 )
		    more = .false.
		END IF
	      ELSE IF ( f12 .eq. cyc_18 ( 1 ) .or.
     +		    	f12 .eq. cyc_18 ( 2 ) )  THEN
		IF  ( nt .eq. 1 )  THEN
		    dayw ( nt ) = fdate ( 4 )
		    more = .false.
		END IF
	      ELSE IF ( f12 .eq. cyc_00 ( 1 ) .or.
     +			f12 .eq. cyc_00 ( 2 ) )  THEN
		IF  ( nt .eq. 1 )  THEN
		    dayw ( nt ) = fdate ( 1 )
		    more = .false.
		  ELSE IF ( nt .eq. 3 )  THEN
		    dayw ( nt ) = fdate ( 3 )
		    more = .false.
		END IF
	      ELSE IF ( f12 .eq. cyc_06 ( 1 ) .or.
     +			f12 .eq. cyc_06 ( 2 ) )  THEN
		IF  ( nt .eq. 1 )  THEN
		    IF  ( f12 .eq. cyc_06 ( 1 ) )  THEN
			dayw ( nt ) = fdate ( 2 )
			more = .false.
		      ELSE IF ( f12 .eq. cyc_06 ( 2 ) )  THEN
			dayw ( nt ) = fdate ( 1 )
			more = .false.
		    END IF
		  ELSE IF ( nt .eq. 3 )  THEN
		    dayw ( nt ) = fdate ( 3 )
		    more = .false.
		END IF
	    END IF
C
	    IF ( more )  THEN
		IF ( hrs .eq. '00' .or.  hrs .eq. '03' )  THEN
		    idaywk = idaywk - 1
		    IF ( idaywk .eq. 0 ) idaywk = 7
		    dayw ( nt ) = ddd ( idaywk ) // fdate ( 5 )
		  ELSE
		    dayw ( nt ) = ddd ( idaywk )
		END IF
	    END IF
	    nt = nt + 2
	    IF ( nt .gt. ngrdtm ) done = .true.
	END DO
C*
	RETURN
	END
