	SUBROUTINE SN_GCLM  ( lun, nparms, dattim, rlat, tmpk, data,
     +			      hght, pres, iret )
C************************************************************************
C* SN_GCLM								*
C*									*
C* This subroutine reads data from a climatology sounding table	and	*
C* interpolates a cloud temperature, TMPK, to the data. The sounding	*
C* data is made of ten soundings, representing the summer and winter	*
C* atmosphere in every 15 degree from 15 to 75 latitudes.  The routine	*
C* finds the closest soundings to an input location, RLAT, and		*
C* interpolates the data to the location and time of the year.  A	*
C* negative COS curve is used to represent the seasonal height		*
C* variations.								*
C*									*
C* SN_GCLM  ( LUN, NPARMS, DATTIM, RLAT, TMPK, DATA, HGHT, PRES, IRET )	*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		LUN for sounding table		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	DATTIM		CHAR*		Data time			*
C*	RLAT		REAL		Latitude			*
C*	TMPK		REAL		Temperature in Kelvin		*
C*									*
C* Output parameters:							*
C*	DATA		REAL		Sounding data			*
C*	  (NPARMS,NLEV)							*
C*	HGHT		REAL		Pressure			*
C*	PRES		REAL		Height				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*                                      -29 = invalid sounding data	*
C*                                      -32 = latitude out of range	*
C**									*
C* Log:									*
C* T. Lee/GSC		 5/00	Initial coding				*
C* D. Kidwell/NCEP	 4/05	Replaced 40 with MMPARM                 *
C* T. Lee/NCWCP		10/13	Fixed interpolation bugs		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( RDIFF = 15., NOD = 365 )
	CHARACTER*(*)	dattim
C*
	REAL		data ( nparms, * )
	CHARACTER	record*132, clat*8
	INTEGER		idtarr (5)
	REAL		rarr (MMPARM), rloc (4), pp (4), hh (4)
	LOGICAL		start, done, good
C------------------------------------------------------------------------
	iret = 0
C
C*	Check the input latitude.
C
	plat = ABS ( rlat )
	pres = RMISSD
	hght = RMISSD
C
C*	Check latitude value.
C
	IF  ( plat .gt. 90. )  THEN
	    iret = -32
	    RETURN
	  ELSE
	    IF  ( plat .eq. 0.  )  plat = plat + .1
	    IF  ( plat .eq. 90. )  plat = plat - .1
	END IF
C
C*	Convert input time to Julian day.
C
	CALL TI_CTOI ( dattim, idtarr, ier )
	CALL TI_ITOJ ( idtarr, jyear, jday, iret )
	IF  ( rlat .lt. 0 )  jday = jday + 180
C
C*	Convert temperature to Celsius.
C
	tmpc = PR_TMKC ( tmpk )
C
C*	Loop through records looking for the needed data.
C
	k = 1
	done = .false. 
	DO WHILE  (  .not. done )
	    iostat = 0
	    start  = .false.
	    good = .false.
	    DO WHILE  ( iostat .eq. 0 )
		READ   ( lun, 10, IOSTAT = iostat )  record
10		FORMAT ( A )
		CALL ST_LCUC  ( record, record, ier )
		CALL ST_LDSP  ( record, record, n, ier )
		ipos = index ( record, 'SLAT' )
C
C*		Decode latitude into real numbers.
C
		IF  ( ipos .ne. 0 )  THEN
		    clat = record (9:15)
		    CALL ST_C2R ( clat, 1, rarr, num, ier ) 
		    dist = ABS ( rarr (1) - plat )
		    IF  ( dist .lt. RDIFF )  THEN
			rloc (k) = rarr (1) 
			good = .true.
		    END IF
		END IF
C
C*		Check valid data.
C
		IF  ( ( n .gt. 0 ) .and. ( ( record (1:1) .lt. 'A' ) 
     +		  .or.  ( record (1:1) .gt. 'z' ) ) .and. good )  THEN
		    iostat = -1
		    start  = .true.
		    CALL FL_BKSP ( lun, ier )
		END IF
	    END DO
C
C*	    Make sure there is some data.
C
	    IF  ( .not. start )  THEN
		iret = -29
		RETURN
	    END IF
C
C*	    Read climatology data.
C
	    iostat = 0
	    iparms = 0
	    nlev   = 1
	    DO WHILE  ( iostat .eq. 0 )
		READ   ( lun, 10, IOSTAT = iostat )  record
		CALL ST_LCUC  ( record, record, ier )
		CALL ST_LDSP  ( record, record, n, ier )
C
C*		Skip blank records.
C
		IF  ( ( iostat .eq. 0 ) .and. ( n .gt. 0 ) )  THEN
C
C*		    Decode record into real numbers.
C
		    CALL ST_C2R  ( record, MMPARM, rarr, narr, ier )
		    IF  ( ier .ne. 0 )  THEN
			iostat = -1
			CALL FL_BKSP ( lun, ier )
		      ELSE
C
C*			Move data into output sounding data array.
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
C*			Check to see if all data for level has 
C*			been found.
C
			IF  ( iparms .ge. nparms )  THEN
			    iparms = 0
			    nlev   = nlev + 1
			END IF
		    END IF
		END IF
	    END DO
C
C*	    Decrement nlev counter.
C
	    nlev = nlev - 1
C
C*	    Interpolate the data.
C
	    CALL SN_INTC ( data, nparms, nlev, tmpc, pr, zz, iret )
	    IF  ( iret .ne. 0 )  THEN
		pres = pr
		hght = zz
		RETURN
	      ELSE
		pp ( k ) = pr
		hh ( k ) = zz
		k = k + 1
	    END IF
C
C*	    If the pixel is located over the tropical or poleward of
C*	    75 degree, use the near latitudinal data.
C
	    IF  ( ( dist .eq. 0. ) .or. ( plat .le. 15. ) 
     +		.or. ( plat .ge. 75. ) )  THEN
		pp ( k ) = pr
		hh ( k ) = zz
		rloc ( k ) = rloc ( k - 1 )  
		k = k + 1
	    END IF
	    IF  ( k .gt. 4 )  done = .true.
C
	END DO
C
C*	Interpolate to the data location.
C
	plog1  = ALOG ( pp ( 1 ) )
	plog2  = ALOG ( pp ( 2 ) )
	plog15 = ALOG ( 15. )

	zjan =  hh ( 1 ) + ( hh ( 2 ) - hh ( 1 ) ) / 15.
     +		* ( plat - rloc ( 1 ) )
	pjan =  pp ( 1 ) * EXP ( ( plog2 - plog1 ) / plog15
     +		* ( ALOG ( plat ) - ALOG ( rloc ( 1 ) ) ) )
C
	plog3 = ALOG ( pp (3) )
	plog4 = ALOG ( pp (4) )
	zjul =  hh ( 3 ) + ( hh ( 4 ) - hh ( 3 ) ) / 15.
     +		* ( plat - rloc ( 3 ) )
	pjul =  pp ( 3 ) * EXP ( ( plog4 - plog3 ) / plog15
     +		* ( ALOG ( plat ) - ALOG ( rloc ( 3 ) ) ) )
C
C*	Interpolate to the data time. 
C
	hght = ( ( zjul - zjan ) * ( 1. - COS ( jday * TWOPI / NOD ) ) )
     +		/ 2. + zjan
	pres = ( ( pjul - pjan ) * ( 1. - COS ( jday * TWOPI / NOD ) ) ) 
     +		/ 2. + pjan
C*
	RETURN
	END
