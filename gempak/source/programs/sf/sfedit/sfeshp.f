	SUBROUTINE SFESHP  ( lunedt, isffln, nparm, iploc, iret )
C************************************************************************
C* SFESHP								*
C*									*
C* This subroutine reads data from an edit text file and writes it	*
C* to a non-standard surface file.					*
C*									*
C* SFESHP  ( LUNEDT, ISFFLN, NPARM, IPLOC, IRET )			*
C*									*
C* Input parameters:							*
C*	LUNEDT		INTEGER		LUN for edit file		*
C*	ISFFLN		INTEGER		Surface file number		*
C*	NPARM		INTEGER		Number of parameters		*
C*	IPLOC(*)	INTEGER		Location of data in output file	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C** Log:								*
C* S. Jacobs/NMC	10/94		Copied from SFEDTA		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		iploc (*)
C*
	CHARACTER	record*132, tim*20, stn*8, stid*8
	CHARACTER	curtim*20, cdata (MMPARM)*20
	REAL		rdata ( MMPARM ), data ( MMPARM )
	LOGICAL		first
C------------------------------------------------------------------------
	iret = 0
C
C*	Read all the records in the file.
C
	ier    = 0
	first  = .true.
	knt    = 0
	curtim = ' '
C*
	DO WHILE  ( ier .eq. 0 )
C
C*	    Read the next record.
C
	    READ   ( lunedt, 10, IOSTAT = ier )  record
10	    FORMAT ( A )
C
C*	    Skip blank records.
C
	    iskp = INDEX ( record, 'YYMMDD' )
	    IF  ( iskp .ne. 0 )  record = ' '
	    IF  ( first )  THEN
		iskp = INDEX ( record, '/' )
		IF  ( iskp .eq. 0 )  record = ' '
	    END IF
	    IF  ( ( ier .eq. 0 ) .and. ( record .ne. ' ' ) )  THEN
C
C*		Break record into character strings.
C
		CALL ST_C2C  ( record, MMPARM, cdata, ndat, ier )
C
C*		Get station and time from first data record.
C
		IF  ( first )  THEN
		    stn   = cdata (1)
		    tim   = cdata (2) (1:6) // '/' // cdata (3) (1:4)
		    first = .false.
		    i     = 4
		  ELSE
		    i     = 1
		END IF
C
C*		Convert rest of variables into real numbers.
C
		DO WHILE  ( ( i .le. ndat ) .and. ( knt .lt. nparm ) )
		    knt = knt + 1
		    CALL ST_CRNM  ( cdata (i), rdata (knt), ier )
		    i = i + 1
		END DO
C
C*		If all the variables are found, write to file.
C
		IF  ( knt .ge. nparm )  THEN
C
C*		    Reset counters.
C
		    first = .true.
		    knt   = 0
C
C*		    Set the lat/lon from the edit file data array.
C
		    rlat = rdata ( iploc (1) )
		    rlon = rdata ( iploc (2) )
C
C*		    Fill the rest of the data array.
C
		    DO  i = 3, nparm
			data ( iploc (i) ) = rdata (i)
		    END DO
C
C*		    Write the data to the file.
C
		    CALL ST_NUMB ( stn, isnum, ierr )
		    IF  ( ierr .eq. 0 )  THEN
			stid  = ' '
		    ELSE
			stid  = stn
			isnum = RMISSD
		    END IF
		    CALL SF_WSDD  ( isffln, tim, stn, isnum,
     +				    rlat, rlon, 0, ' ', ' ',
     +				    ihhmm, data, ier )
		END IF
	    END IF
	END DO
C*
	RETURN
	END
