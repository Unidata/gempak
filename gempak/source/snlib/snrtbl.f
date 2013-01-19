	SUBROUTINE SN_RTBL  ( lun, nparms, data, nlev, iret )
C************************************************************************
C* SN_RTBL								*
C*									*
C* This subroutine reads the data for one station.			*
C*									*
C* SN_RTBL  ( LUN, NPARMS, DATA, NLEV, IRET )				*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		LUN for sounding table		*
C*	NPARMS		INTEGER		Number of parameters		*
C*									*
C* Output parameters:							*
C*	DATA		REAL		Station data			*
C*	  (NPARMS,NLEV)							*
C*	NLEV		INTEGER		Number of levels		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*                                      -29 = invalid station data	*
C**									*
C* Log:									*
C* T. Lee/GSC		12/99	Adapted from SNEDTA			*
C* T. Lee/GSC		 4/00	Backspaced				*
C* D. Kidwell/NCEP	 4/05	Replaced 40 with MMPARM                 *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		data ( NPARMS, * )
C*
	CHARACTER	record*132
	REAL		rarr (MMPARM)
	LOGICAL		start
C------------------------------------------------------------------------
	iret = 0
	nlev = 0
C
C*	Loop through records looking for the first record containing 
C*	data.
C
	iostat = 0
	start  = .false.
	DO WHILE  ( iostat .eq. 0 )
	    READ   ( lun, 10, IOSTAT = iostat )  record
10	    FORMAT ( A )
	    CALL ST_LCUC  ( record, record, ier )
	    CALL ST_LDSP  ( record, record, n, ier )
	    IF  ( ( n .gt. 0 ) .and. ( ( record (1:1) .lt. 'A' ) .or.
     +		  ( record (1:1) .gt. 'z' ) ) )  THEN
		iostat = -1
		start  = .true.
		CALL FL_BKSP ( lun, ier )
	    END IF
	END DO
C
C*	Make sure there is some data.
C
	IF  ( .not. start )  THEN
	    iret = -29
	    RETURN
	END IF
C
C*	Read in data.
C
	iostat = 0
	iparms = 0
	nlev   = 1
	DO WHILE  ( iostat .eq. 0 )
	    READ   ( lun, 10, IOSTAT = iostat )  record
	    CALL ST_LCUC  ( record, record, ier )
	    CALL ST_LDSP  ( record, record, n, ier )
C
C*	    Skip blank records.
C
	    IF  ( ( iostat .eq. 0 ) .and. ( n .gt. 0 ) )  THEN
C
C*		Decode record into real numbers.
C
		CALL ST_C2R  ( record, MMPARM, rarr, narr, ier )
		IF  ( ier .ne. 0 )  THEN
		    iostat = -1
		    CALL FL_BKSP ( lun, ier )
		  ELSE
C
C*		    Move data into output sounding data array.
C
		    id = 1
		    DO WHILE  ( id .le. narr )
			IF  ( iparms .le. nparms )  THEN
			    iparms = iparms + 1
			    data ( iparms, nlev ) = rarr ( id )
			END IF
			id = id + 1
		    END DO
C
C*		    Check to see if all data for level has been found.
C
		    IF  ( iparms .ge. nparms )  THEN
			iparms = 0
			nlev   = nlev + 1
		    END IF
		END IF
	    END IF
	END DO
C
C*	Decrement nlev counter.
C
	nlev = nlev - 1
C*
	RETURN
	END
