	SUBROUTINE MR_MAND  ( datman, nman, datamn, namn, nlev, 
     +			      ipt, stndat, idtype, iret )
C************************************************************************
C* MR_MAND								*
C*									*
C* This subroutine adds the mandatory data to the STNDAT array.  The	*
C* surface data should be put in STNDAT ( i, 1 ) before this 		*
C* subroutine is called.						*
C*									*
C* MR_MAND  ( DATMAN, NMAN, DATAMN, NAMN, NLEV, IPT, STNDAT, IDTYPE,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	DATMAN (6,NMAN)	REAL		Mandatory data below 100 mb	*
C*	NMAN		INTEGER		Number of man lev below 100 mb	*
C*	DATAMN (6,NAMN)	REAL		Mandatory data above 100 mb	*
C*	NAMN		INTEGER		Number of man lev above 100 mb	*
C*									*
C* Input and output parameters:						*
C*	NLEV		INTEGER		Number of levels		*
C*	IPT    ( NLEV )	INTEGER		Pointers to ordered data	*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*	IDTYPE (NLEV)	INTEGER		Data type flags			*
C*					  1 = mandatory			*
C*					  2 = sig temperature		*
C*					  3 = sig wind			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/86						*
C* M. desJardins/GSFC	 9/87	Eliminated rejection for miss t,z	*
C* M. desJardins/GSFC	10/87	Add rejection for miss t,wind		*
C* M. desJardins/GSFC	 1/89	Added IDTYPE				*
C* K. Brill/NMC		01/92	Hght is in 6 not 4		  	*
C* S. Jacobs/NMC	 4/95	Check for PRES, TEMP, HGHT to add data	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	REAL		datman (6,*), datamn (6,*), stndat (6,*)
	INTEGER		ipt (*), idtype (*)
C
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Save the surface pressure.  If missing, make a large number.
C
	IF  ( ERMISS ( stndat ( 1, 1 ) ) )  THEN
	    plast = 2000.
	  ELSE
	    plast = stndat ( 1, 1 )
	END IF
C
C*	Move the mandatory data below 100 mb to the output array.
C*	Check that pressure is not missing and is decreasing.
C
	DO  i = 2, nman
	    IF  ( (  datman ( 1, i ) .lt. plast ) .and. 
     +	          ( .not. ERMISS ( datman ( 1, i ) ) ) .and.
     +		  ( .not. ERMISS ( datman ( 2, i ) ) ) .and.
     +		  ( .not. ERMISS ( datman ( 6, i ) ) ) )  THEN
		nlev = nlev + 1
		DO  j = 1, 6
		    stndat ( j, nlev ) = datman ( j, i )
		END DO
		idtype ( nlev ) = 1
		ipt ( nlev ) = nlev
		plast = stndat ( 1, nlev )
	    END IF
	END DO
C
C*	Move the mandatory data above 100 mb to the output array.
C
	DO  i = 1, namn
	    IF  ( (  datamn ( 1, i ) .lt. plast ) .and. 
     +		  ( .not. ERMISS ( datamn ( 1, i ) ) ) .and.
     +		  ( .not. ERMISS ( datamn ( 2, i ) ) ) .and.
     +		  ( .not. ERMISS ( datamn ( 6, i ) ) ) )  THEN
		nlev = nlev + 1
		DO  j = 1, 6
		    stndat ( j, nlev ) = datamn ( j, i )
		END DO
		idtype ( nlev ) = 1
		ipt ( nlev ) = nlev
		plast = stndat ( 1, nlev )
	    END IF
	END DO
C*
	RETURN
	END
