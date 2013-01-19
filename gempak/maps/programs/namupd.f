	PROGRAM NAMUPD
C************************************************************************
C* NAMUPD								*
C*									*
C* This program updates the record numbers in the CIA.NAM file and	*
C* creates a separate file containing the names of all the records.	*
C* The files created are CIA.NAM_NEW and REC.NAM_NEW.  The file		*
C* REC.NAM_TMP may be deleted.						*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/91						*
C************************************************************************
	REAL		rlat (100), rlon (100)
	CHARACTER	name*64, namold*60
	INTEGER		kblk (10)
	CHARACTER	region (10)*24
C*
	DATA	region / 'Continental borders', 'Islands', 'Lakes', 
     +			 'State borders', 'National borders',
     +			 'Rivers', 'Tectonic plates', 'Miscellaneous',
     +			  2 * ' ' /
C------------------------------------------------------------------------
	CALL IN_BDTA  ( ier )
C
C*	Open existing name file and new file.
C
	CALL FL_SOPN  ( 'cia.nam', iflold, ier )
	IF  ( ier .ne. 0 )  THEN
	    WRITE (6,*) 'Cannot open existing map file' 
	    STOP
	END IF
	CALL FL_SWOP  ( 'cia.nam_new', iflnew, ier )
	IF  ( ier .ne. 0 )  THEN
	    WRITE (6,*) 'Cannot open new map file'
	    STOP
	END IF
C
C*	Open temporary record file and permanent record file.
C
	CALL FL_SWOP  ( 'rec.nam_tmp', ifltmp, ier1 )
	CALL FL_SWOP  ( 'rec.nam_new', iflrec, ier2 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
	    WRITE (6,*) 'Cannot open record files'
	    STOP
	END IF
C
C*	Read through map file.  Create new map file with updated record numbers.
C*	Create temporary record file and save block information for final
C*	record file.
C
	irec = 0
	iblk = 1
	kblk (iblk) = 1
	name = ' '
	namold = ' '
	iostat = 0
	ibstrt = 0
C*
	DO WHILE  ( iostat .eq. 0 )
C
C*	    Read in next record information.
C
	    READ  ( iflold, 1100, IOSTAT = iostat )  npts, nrec, name
1100	    FORMAT ( I3, 2X, I5, 4X, A )
C
C*	    Write out to new file.
C
	    IF  ( iostat .eq. 0 )  THEN
		irec = irec + 1
		WRITE  ( iflnew, 1100, IOSTAT = iostat )  npts, irec, name
C
C*		Read and write the rest of the record.
C
		mpts = ABS ( npts )
		READ  (iflold, 1200) (rlat (i), rlon (i), i = 1, mpts)
		WRITE (iflnew, 1200) (rlat (i), rlon (i), i = 1, mpts)
1200		FORMAT ( 8F10.5 )
	    END IF
C
C*	    Check for end of block information.
C
	    IF  ( iostat .eq. 0 )  THEN
		iend = INDEX ( name, '(End of' )
		IF  ( iend .ne. 0 )  THEN
		    iblk = iblk + 1
		    kblk (iblk) = irec
		    name ( iend: ) = ' '
		END IF
C
C*		If this is a new name, write out the old name.
C
		IF  (( name .ne. ' ' ) .and. ( ibstrt .ne. 0 )) THEN
		    ibend = irec - 1
		    WRITE  ( ifltmp, 1400 )  ibstrt, ibend, namold
1400		    FORMAT ( I5, ' to', I5, 7X, A )
		    ibstrt = irec
		    namold = name
		  ELSE IF  ( name .ne. ' ' )  THEN
		    ibstrt = irec
		    namold = name
		END IF
C
C*		If this is the end of a block, write out record.
C
		IF  ( iend .ne. 0 )  THEN
		    ibend = irec
		    WRITE  ( ifltmp, 1400 )  ibstrt, ibend, namold
		    ibstrt = 0
		END IF
	    END IF
	END DO
C
C*	Write message at the end of this phase.
C
	WRITE (6,*) 'New map file has been written with', 
     +					irec, ' records.'
	WRITE (6,*) 'blocks: ', ( kblk (i), i = 1, iblk )
C
C*	Close new and old map files.
C
	CALL FL_CLOS  ( iflold, ier )
	CALL FL_CLOS  ( iflnew, ier )
C
C*	Make new record file from temporary file.
C
	CALL FL_REWD  ( ifltmp, ier )
C
C*	Write out first record.
C
	WRITE  ( iflrec, 1500 )
1500	FORMAT ( '*****************************************************' )
	WRITE  ( iflrec, 1400 )  kblk (1), kblk (2), region (1)
	WRITE  ( iflrec, 1500 )
C
C*	Set block counters.
C
	iblk = 2
	inxt = kblk (iblk)
C
C*	Read and write out records.
C
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
C*
	    READ   ( ifltmp, 1400, IOSTAT = iostat ) ibstrt, ibend, namold
C
C*	    Check for end of block.
C
	  IF  ( iostat .eq. 0 )  THEN
	    IF  ( ibstrt .eq. inxt + 1 )  THEN
		WRITE  ( iflrec, 1500 )
		isblk = kblk (iblk) + 1
		ieblk = kblk (iblk+1)
		WRITE  ( iflrec, 1400 )  isblk, ieblk, region (iblk)
		WRITE  ( iflrec, 1500 )
		iblk = iblk + 1
		inxt = kblk (iblk)
	    END IF
	    WRITE  ( iflrec, 1400 ) ibstrt, ibend, namold
	  END IF
	END DO
C
C*	Close all files.
C
	CALL FL_CLOS  ( ifltmp, ier )
	CALL FL_CLOS  ( iflrec, ier )
C*
	END
