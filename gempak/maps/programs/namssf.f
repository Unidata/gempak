	PROGRAM NAMSSF
C************************************************************************
C* NAMSSF								*
C*									*
C* This program converts a NAME file into a GEMPAK Standard Sequential	*
C* Format (SSF) file.  The NAME file was originally created by Joseph 	*
C* Fulson-Woytek at GSFC from a subset of the CIA map database. 	*
C* Comments identifying line segments in the file have been added.	*
C* identify the line segments in the file.  This program takes as	*
C* input either a name or a range of records to be used in creating	*
C* the SSF file.  Also, the regions to accept must be entered.		*
C*									*
C* NOTE: The records corresponding to regions are hard-coded and MUST	*
C*       be edited if the map name file is changed.			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	10/91						*
C* M. desJardins/NMC	10/91						*
C************************************************************************
	REAL		rlat (100), rlon (100)
	CHARACTER	name*64, namold*64, infile*80, outfil*80,
     +			area*72, region*12
	LOGICAL		range, done, good
	INTEGER		iarr (2)
C------------------------------------------------------------------------
C*	Initialize GEMPAK.
C
	CALL IN_BDTA ( ier )
        WRITE  ( 6, * ) 'Enter NAME map file to be converted: '
	READ   ( 5, 2 )  infile
2	FORMAT ( A ) 
        WRITE  ( 6, * ) 'Enter name of SSF map file to be created: '
	READ   ( 5, 2 )  outfil
C
C*	Open the files and check for errors.
C
	CALL FL_SOPN  ( infile, lunin, ier1 )
	CALL FL_SWOP  ( outfil, lunot, ier2 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
            CALL ER_WMSG  ( 'FL', ier1, infile, ier )
            CALL ER_WMSG  ( 'FL', ier2, outfil, ier )
            CALL FL_CLOS  ( lunin, ier )
            CALL FL_CLOS  ( lunot, ier )
            STOP
        END IF
C
C*	Get the input for data selection.
C
	WRITE  ( 6, * ) 'Enter data selection as name or record range:'
	READ   ( 5, 2 )  area
C
C*	Check to see if this is an area or a range.
C
	ipos = INDEX  ( area, '-' )
	IF  ( ipos .ne. 0 )  THEN
	    CALL ST_ILST  ( area, '-', IMISSD, 2, iarr, num, ier )
	    IF  ( ( num .eq. 2 ) .and. ( iarr (1) .gt. 0 ) .and.
     +		  ( iarr (2) .gt. 0 ) )  THEN
		range = .true.
		ifirst = iarr (1)
		ilast  = iarr (2)
	    END IF
	  ELSE IF  ( area .eq. ' ' )  THEN
	    range  = .true.
	    ifirst = 1
	    ilast  = 1000000
	  ELSE
	    range  = .false.
	END IF
C
C*	Check for sections to use.
C
	WRITE  ( 6, * ) 'The file has the following sections: '
	WRITE  ( 6, * ) '   C  -  Continental borders'
	WRITE  ( 6, * ) '   I  -  Islands'
	WRITE  ( 6, * ) '   L  -  Lakes'
	WRITE  ( 6, * ) '   S  -  State borders'
	WRITE  ( 6, * ) '   N  -  National borders'
	WRITE  ( 6, * ) '   R  -  Rivers'
	WRITE  ( 6, * ) '   T  -  Tectonic plates'
	WRITE  ( 6, * ) '   M  -  Miscellaneous'
	WRITE  ( 6, * ) 
     +		'Enter characters corresponding to sections to search:'
	READ   ( 5, 2 )  region
	CALL ST_LCUC  ( region, region, ier )
	IF  ( ( region (1:1) .eq. 'A' ) .or. ( region .eq. ' ' ) )  
     +			region = 'CILSNRTM'
C
C*	Get valid regions.
C
	CALL ST_LCUC  ( region, region, ier )
	iposc = INDEX ( region, 'C' )
	iposi = INDEX ( region, 'I' )
	iposl = INDEX ( region, 'L' )
	iposs = INDEX ( region, 'S' )
	iposn = INDEX ( region, 'N' )
	iposr = INDEX ( region, 'R' )
	ipost = INDEX ( region, 'T' )
	iposm = INDEX ( region, 'M' )
C
C*	Capitalize name of area.
C
	IF  ( .not. range )  THEN
	    CALL ST_LCUC  ( area, area, ier )
	    CALL ST_LSTR  ( area, lenare, ier )
	END IF
C
C*	Loop through data in file.
C
	namold = ' '
	nptold = 0
	done   = .false.
	DO WHILE  ( .not. done )
C
C*	  Read header record for this buffer.
C
	  READ  ( lunin, 1100, IOSTAT = iostat )  npts, nrec, name
1100	  FORMAT ( I3, 2X, I5, 4X, A )
	  IF  ( iostat .ne. 0 )  THEN
	   done = .true.
	  ELSE 
C
C*	    If name is missing, last name is valid.
C
	    IF  ( name .eq. ' ' )  name = namold
C
C*	    If npts is negative, this is a continuation.  Therefore,
C*	    we must get the last point from the last record.
C
	    IF  ( npts .lt. 0 )  THEN
		npts = -npts
		ipt1 = 2
		ipt2 = npts + 1
		rlat (1) = rlat ( nptold )
		rlon (1) = rlon ( nptold )
	      ELSE
		ipt1 = 1
		ipt2 = npts
	    END IF
C
C*	    Read in the lat/lon pairs.
C
	    READ   ( lunin, 1200 ) ( rlat (i), rlon (i), i = ipt1, ipt2 )
1200	    FORMAT ( 8F10.5 )
C
C*	    Check selection criteria.
C
	    good = .false.
	    IF  ( .not. range )  THEN
		CALL ST_LCUC  ( name, name, ier )
		ipos = INDEX  ( name, area (1:lenare) )
		IF  ( ipos .ne. 0 )  good = .true.
	      ELSE
		IF  ( ( nrec .ge. ifirst ) .and. 
     +		      ( nrec .le. ilast ) )  THEN
		    good = .true.
		  ELSE IF  ( ilast .lt. nrec )  THEN
		    done = .true.
		END IF
	    END IF
C
C*	    Now check to see if this is in correct region.
C
	    IF  ( good )  THEN
		IF  ( nrec .le. 1686 )  THEN
		    IF  ( iposc .eq. 0 )  good = .false.
C*
		  ELSE IF  ( nrec .le. 2726 )  THEN
		    IF  ( iposi .eq. 0 )  good = .false.
C*
		  ELSE IF  ( nrec .le. 3029 )  THEN
		    IF  ( iposl .eq. 0 )  good = .false.
C*
		  ELSE IF  ( nrec .le. 3160 )  THEN
		    IF  ( iposs .eq. 0 )  good = .false.
C*
		  ELSE IF  ( nrec .le. 3828 )  THEN
		    IF  ( iposn .eq. 0 )  good = .false.
C*
		  ELSE IF  ( nrec .le. 4531 )  THEN
		    IF  ( iposr .eq. 0 )  good = .false.
C*
		  ELSE IF  ( nrec .le. 4615 )  THEN
		    IF  ( ipost .eq. 0 )  good = .false.
C*
		  ELSE IF  ( iposm .eq. 0 )  THEN
		    good = .false.
		END IF
	    END IF
C
C*	    If this record is within the subset chosen, add to SSF file.
C
	    IF  ( good )  THEN
		rlnmx = -200.
		rlnmn =  200.
		rltmx = -100.
		rltmn =  100.
C
C*		Find the max/min values.
C
		DO  k = 1, ipt2
		    rltmx = MAX ( rlat (k), rltmx )
		    rlnmx = MAX ( rlon (k), rlnmx )
		    rltmn = MIN ( rlat (k), rltmn )
		    rlnmn = MIN ( rlon (k), rlnmn )
		END DO
C
C*		Write the data to the new SSF file.
C
		iout = ipt2 * 2
	    	WRITE  ( lunot, 1400 ) iout, rltmx, rltmn, rlnmx, rlnmn,
     +				       ( rlat (k), rlon (k), k = 1, ipt2 )
1400		FORMAT ( I4, 14X, 6F9.3, 8X, / ( 8F9.3, 8X ) )
	    END IF
C
C*	    Save variable names.
C
	    namold = name
	    nptold = ipt2
	  END IF
	END DO
C*
	END
