	SUBROUTINE SFEDTA  ( lunedt, isffln, nparm, iploc, iret )
C************************************************************************
C* SFEDTA								*
C*									*
C* This subroutine reads data from an edit text file and writes it	*
C* to a surface file.							*
C*									*
C* SFEDTA  ( LUNEDT, ISFFLN, NPARM, IPLOC, IRET )			*
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
C* M. desJardins/GSFC	 6/88						*
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized record		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		iploc (*)
C*
	CHARACTER	record*132, tim*20, stn*8
	CHARACTER	curtim*20, cdata (MMPARM)*20
	REAL		rdata ( MMPARM ), data ( MMPARM )
	LOGICAL		first
C------------------------------------------------------------------------
	iret = 0
	record = ' '
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
C*		    Set the proper station.
C
		    CALL SFESTN  ( isffln, stn, tim, curtim, data, 
     +				   ihhmm, iret )
		    IF  ( iret .lt. 0 )  THEN
			RETURN
		      ELSE IF  ( iret .eq. 0 )  THEN
			DO  i = 1, nparm
			    data ( iploc (i) ) = rdata (i)
			END DO
			CALL SF_WDAT  ( isffln, ihhmm, data, ier )
		    END IF
		END IF
	    END IF
	END DO
C*
	RETURN
	END
