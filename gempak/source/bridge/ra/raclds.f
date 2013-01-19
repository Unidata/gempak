	SUBROUTINE RA_CLDS  ( irpntr, iptbef, maxcld, ipcaft, cldtyp,
     +			      cldhgt, ncld, iret )
C************************************************************************
C* RA_CLDS								*
C*									*
C* This subroutine gets the cloud information from an airways report.	*
C*									*
C* RA_CLDS  ( IRPNTR, IPTBEF, MAXCLD, IPCAFT, CLDTYP, CLDHGT, NCLD,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	IRPNTR		INTEGER		First field after header	*
C*	IPTBEF		INTEGER		First field before PTD		*
C*	MAXCLD		INTEGER		Maximum number of cloud levels	*
C*									*
C* Output parameters:							*
C*	IPCAFT		INTEGER		First field after clouds	*
C*	CLDTYP (*)	REAL		GEMPAK cloud type		*
C*	CLDHGT (*)	REAL		Cloud height			*
C*	NCLD		INTEGER		Number of cloud levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* B. Doty/RDS		11/87						*
C* I. Graffman/RDS	 4/88						*
C* M. desJardins/GSFC	 9/89	GEMPAK 5				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*
	REAL		cldtyp (*), cldhgt (*)
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize arrays to missing data value.
C
	ncld = 0
	DO  i = 1, maxcld
	    cldtyp (i) = RMISSD
	    cldhgt (i) = RMISSD
	END DO
	ipcaft = irpntr
C
C*	Loop through all fields from end of header to start of 
C*	temperature field.
C
	DO  i = irpntr, iptbef
C
C*	    Check for a valid cloud cover code.
C
	    cldnum = PT_CCNM  ( cfield (i) )
C
C*	    If this was not a valid code, the value 0 was returned.
C
	    IF  ( ( cldnum .gt. 0. ) .and. ( ncld .lt. maxcld ) )  THEN
		ncld = ncld + 1
		cldtyp (ncld) = cldnum
C
C*		See if the previous field was a number.  If so, this
C*		is the cloud height.  First, check and eliminate V.
C
		IF  ( cfield (i-1) .eq. 'V' )  THEN
		    ii = i - 2
		  ELSE
		    ii = i - 1
		END IF
		IF  ( ( ii .ge. irpntr ) .and. 
     +		      ( iftype (ii) .eq. 2 ) .and.
     +		      ( cldtyp (ncld) .ne. 1. ) )  THEN
		    cldhgt (ncld) = ifintg (ii)
		END IF
	    END IF
C
C*	    Set pointer after clouds.
C
	    IF  ( cldnum .gt. 0. )  ipcaft = i + 1
	END DO
C
C*	Now check for "CLR BLO XXX".  Eliminate CLR and move past XXX.
C*	We cannot handle this since it is not necessarily CLR.
C
	IF  ( ( ncld .ge. 1 ) .and. ( cldtyp (ncld) .eq. 1. ) .and.
     +	      ( cfield (ipcaft) .eq. 'BLO' ) )  THEN
	    cldtyp (ncld) = RMISSD
	    ncld = ncld - 1
	    ipcaft = ipcaft + 1
	    IF  ( iftype (ipcaft) .eq. 2 )  ipcaft = ipcaft + 1
	END IF
C*
	RETURN
	END
