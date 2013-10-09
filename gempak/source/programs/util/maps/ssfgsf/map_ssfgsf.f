	SUBROUTINE MAP_SSFGSF ( infile, outfil, window, iret )
C************************************************************************
C* MAP_SSFGSF								*
C*									*
C* This program converts a map file in Sequential Standard Format 	*
C* (SSF) to a GEMPAK Standard Format (GSF).  The GSF file is a packed	*
C* binary file which is used for rapid access to the map segments.	*
C* The formats of these files are documented in the GEMPAK Map Files	*
C* Document.								*
C**									*
C* Log: 								*
C* G. Chatters/RDS	 1981	Written for PDP 11/70			*
C* M. desJardins/GSFC	 1985	Converted to VAX			*
C* G. Huffman/GSC	12/88	GEMPAK4.1 upgrade; FL, remove BYTE, doc	*
C* M. desJardins/GSFC	 3/89	Recoded and rewrote documentation	*
C* G. Krueger/EAI	 9/96	Fixed broken lines on block boundaries	*
C* G. Krueger/EAI	12/98	Corrected intermediate maps; Simplified	*
C* G. Krueger/EAI	 3/99	Increased record size for county map	*
C* G. Krueger/EAI	 6/99	Increased record size for highways maps	*
C* G. Krueger/EAI	 1/00	Implemented file byte swapping		*
C* G. Krueger/EAI	 2/00	Correction to byte swapping		*
C* S. Jacobs/NCEP	 2/01	Added machine type MTLNUX		*
C* R. Tian/SAIC		 2/06	Changed from program to subroutine	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	infile, outfil, window
C*
	PARAMETER	( MAPLEN = 128 )
	PARAMETER	( IBLKLN = 256 )
	PARAMETER	( MXRCSZ = 9999 )
	PARAMETER	( MXRECS = 120000 )
C*
	CHARACTER	dafil*80, intseq*80
	REAL		ultln    ( 4 )
	LOGICAL		bounds
C*
	REAL		rmapbf ( 256), rpts  (MXRCSZ), opts   (MXRCSZ)
	REAL		dlatu  (MXRECS)
	INTEGER		imapbf ( 512), inptr (MXRECS), iotptr (MXRECS)
	INTEGER*2	imapb2 ( 512), latptr  ( 180)
C*
	EQUIVALENCE	( rmapbf, imapbf )
	EQUIVALENCE	( imapbf, imapb2 )
C*
	DATA		dafil  / 'DAFIL.INT' /, 
     +			intseq / 'SEQFIL.INT' /
C
C*	This statement function checks to see if a point is within
C*	the user selected bounds.
C
	bounds (j) = ( ( rpts (j) .ge. umnlt ) .and. 
     +		       ( rpts (j) .le. umxlt ) .and.
     +		       ( rpts (j+1) .ge. umnln ) .and.
     +		       ( rpts (j+1) .le. umxln ) )
C-----------------------------------------------------------------------
	iret = 0
C
C*	Reset subsetting window values of -9999. to limits.
C
	CALL ST_RLST  ( window, ';', -9999., 4, ultln, ndum, ier )
	IF  ( ultln (1) .eq. -9999. )  THEN
	    umnlt =  -90.
	  ELSE
	    umnlt = ultln (1)
	END IF
	IF  ( ultln (2) .eq. -9999. )  THEN
	    umnln = -180.
	  ELSE
	    umnln = ultln (2)
	END IF
	IF  ( ultln (3) .eq. -9999. )  THEN
	    umxlt =  90.
	  ELSE
	    umxlt = ultln (3)
	END IF
	IF  ( ultln (4) .eq. -9999. )  THEN
	    umxln =  180.
	  ELSE
	    umxln = ultln (4)
	END IF
C	WRITE  (6,*) 'Data subset area: ', umnlt, umnln, umxlt, umxln
C
C*	Read input sequential file and make an intermediate sequential
C*	file that has no records with more than 124 points.
C
	CALL FL_SOPN  ( infile, lunin, ier1 )
	CALL FL_SWOP  ( intseq, lunis, ier2 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
	    CALL ER_WMSG  ( 'FL', ier1, infile, ier )
	    CALL ER_WMSG  ( 'FL', ier2, intseq, ier )
	    CALL FL_CLOS  ( lunin, ier )
	    CALL FL_CLOS  ( lunis, ier )
	    STOP
	END IF
C*
	ninrec = 0
124	READ   ( lunin, 3000, END = 128 ) npts, rmxlt, rmnlt, 
     +	       rmxln, rmnln, ( rpts (m), m = 1, npts )
3000	FORMAT ( I4, 14X, 6F9.3, 8X / ( 8F9.3, 8X ) )
	IF ( npts .gt. MXRCSZ ) THEN
	    WRITE (*, *) npts, ' element input record exceeds ',
     +			 MXRCSZ, ' maximum.'
	    STOP
	END IF
C
C*	Write Intermediate SSF map file.
C
	jiptr = 1
	DO WHILE  ( jiptr .le. npts )
C
C*	    Collect points within bounds.
C
	    nout = 0
	    DO WHILE ( (.not. bounds ( jiptr )) .and.
     +		       (jiptr .lt. npts) )
		jiptr = jiptr + 2
	    END DO
	    DO WHILE  ( (nout .le. 247) .and. (jiptr .le. npts) .and.
     +			bounds (jiptr) )
		nout = nout + 1
		opts ( nout ) = rpts ( jiptr )
		nout = nout + 1
		opts ( nout ) = rpts ( jiptr + 1 )
		IF  ( nout .le. 247 ) jiptr = jiptr + 2
	    END DO
C
C*	    Calculate the bounding box.  Ignore bounding box from input
C*	    file.
C
	    rmnlt  = 90.
	    rmnln  = 180.
	    rmxlt  = -90.
	    rmxln  = -180.
	    DO joptr = 1, nout, 2
		rmnlt = MIN ( rmnlt, opts ( joptr ) )
		rmnln = MIN ( rmnln, opts ( joptr + 1 ) )
		rmxlt = MAX ( rmxlt, opts ( joptr ) )
		rmxln = MAX ( rmxln, opts ( joptr + 1 ) )
	    END DO
C
C*	    Write data to output file if any points remain.
C
	    IF  ( nout .gt. 3 )  THEN
		WRITE  ( lunis, 3000 ) nout, rmxlt, rmnlt, rmxln, rmnln,
     +				       ( opts (m), m = 1, nout )
		ninrec = ninrec + 1
		IF ( ninrec .gt. MXRECS ) THEN
		    WRITE (*, *) 'Input records exceed ', MXRECS,
     +				 ' maximum.'
		    STOP
		END IF
		inptr (ninrec) = ninrec
		dlatu (ninrec) = rmxlt
	    END IF
	END DO
	GOTO 124
C*
128	CALL FL_CLOS  ( lunin, ier )
C
C*	Sort the upper-latitude array (DLATU) and swap the corresponding
C*	pointer array (INPTR).
C
	DO  i1 = 1, ninrec - 1
	  DO  i2 = i1 + 1, ninrec
            IF  ( dlatu (i1) .lt. dlatu (i2) )  THEN
		tempst = dlatu (i1)
		dlatu (i1) = dlatu (i2)
		dlatu (i2) = tempst
		itemp = inptr (i1)
		inptr (i1) = inptr (i2)
		inptr (i2) = itemp
	    END IF
	  END DO
	END DO
C
C*	Let IOTPTR be the inverse of the sorted array.  As the
C*	records are read in, this will determine the order in the
C*	output file.
C
	DO  i = 1, ninrec
	    ip = inptr (i)
	    iotptr (ip) = i
	END DO
C	WRITE (6,*) 'Intermediate SSF map file created'
C------------------------------------------------------------------------
C*	Create an intermediate sorted, unpacked, direct-access file.
C
	CALL FL_DCRE  ( dafil, MAPLEN, lunid, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, dafil, ier1 )
	    CALL FL_CLOS  ( lunis, ier1 )
	    STOP
	END IF
C
C*	Rewind the intermediate SSF file.  Then read it and store
C*	records in an intermediate direct access file.
C
	CALL FL_REWD  ( lunis, ier )
C
C*	Read in all the records.
C
	DO  inrec = 1, ninrec
	    READ  ( lunis, 3000 ) npts, rmxlt, rmnlt, rmxln, rmnln,
     +				  ( rmapbf (m), m = 7, npts + 6 )
C
C*	    Save the record length in the inptr array.
C
	    inptr ( iotptr (inrec) ) = 6 + npts
C
C*	    Move header into buffer.
C
	    imapbf (1) = npts / 2
	    rmapbf (2) = rmnlt
	    rmapbf (3) = rmnln
	    rmapbf (4) = rmxlt
	    rmapbf (5) = rmxln
C
C*	    Set block to write.  Write out new record.  This file does
C*	    not need byte swapping, because it is internal to this
C*	    program.
C
	    iorec = 2 * iotptr (inrec) + 1
	    CALL FL_WRIT  ( lunid, iorec, MAPLEN, imapbf, ier1 )
	    iorec = iorec + 1
	    CALL FL_WRIT  ( lunid, iorec, MAPLEN, imapbf (MAPLEN+1),
     +			    ier2 )
	    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
		CALL ER_WMSG  ( 'FL', ier1, dafil, ier )
		CALL ER_WMSG  ( 'FL', ier2, dafil, ier )
		CALL FL_CLOS  ( lunis, ier )
		CALL FL_CLOS  ( lunid, ier )
		STOP
	    END IF
	END DO
C
C*	Finished with intermediate SSF file; close it.
C
	CALL FL_CLOS  ( lunis, ier )
C	WRITE (6,*) 'Intermediate direct access file created.'
C------------------------------------------------------------------------
C*	Create a GSF file which is a direct access file with the records
C*	blocked.
C
	CALL FL_DCRE  ( outfil, MAPLEN, lungsf, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, outfil, ier1 )
	    CALL FL_CLOS  ( lunid, ier1 )
	    STOP
	END IF
C
C*	Block the direct access records.  
C*	First, initialize LATPTR, the array of latitude pointers.
C
	DO  ilat = 1, 180
	    latptr (ilat) = 0
	END DO
C
C*	Initialize block counter (IBLK), length of block (NLBLK) and
C*	the entire buffer.
C
	iblk   = 2
	nlblk  = 2
	irec   = 2
	irecin = 2
	imapbf (1) = 0
	imapbf (2) = 0
C
C*	Loop through all the records.
C
	DO  inrec = 1, ninrec
C
C*	    See if this record can be added to this block.
C
	    lenr = inptr (inrec)
	    IF  ( ( lenr + nlblk ) .gt. IBLKLN )  THEN
C
C*		Write out current block.  First zero out rest of
C*		the block.  Swap bytes before writing.
C
		DO  i = nlblk + 1, 256
		    imapbf (i) = 0
		END DO
		IF ( MTMACH .eq. MTIGPH .or. MTMACH .eq. MTULTX .or.
     +		     MTMACH .eq. MTALPH .or. MTMACH .eq. MTLNUX )
     +		    CALL MV_SWP4 ( MAPLEN * 2, imapbf(1), imapbf(1) )
		irec = irec + 1
		CALL FL_WRIT  ( lungsf, irec, MAPLEN, imapbf, ier1 )
		irec = irec + 1
		CALL FL_WRIT ( lungsf, irec, MAPLEN, imapbf (MAPLEN+1),
     +			      ier2 )
		IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
		    CALL ER_WMSG  ( 'FL', ier1, outfil, ier )
		    CALL ER_WMSG  ( 'FL', ier2, outfil, ier )
		    CALL FL_CLOS  ( lunid, ier )
		    STOP
		END IF
C
C*		Reset initial variables.
C
		iblk  = iblk + 1
		nlblk = 2
		imapbf (1) = 0
		imapbf (2) = 0
	    END IF
C
C*	    Read in this record and add to block.  This file does
C*	    not need byte swapping, because it is internal to this
C*	    program.
C
	    imapbf (1) = imapbf (1) + 1
	    irecin = irecin + 1
	    CALL FL_READ  ( lunid, irecin, MAPLEN, imapbf ( nlblk+1 ),
     +			    ier1 )
	    irecin = irecin + 1
	    CALL FL_READ  ( lunid, irecin, MAPLEN,
     +			    imapbf ( nlblk+MAPLEN+1 ), ier2 )
	    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
		CALL ER_WMSG  ( 'FL', ier1, dafil, ier )
		CALL ER_WMSG  ( 'FL', ier2, dafil, ier )
		CALL FL_CLOS  ( lunid, ier )
		STOP
	    END IF
C
C*	    Get minimum latitude.
C
	    rmnlat = rmapbf ( nlblk + 2 )
C
C*	    Compute length of block.
C
	    nlblk = nlblk + lenr
C
C*	    Set up latitude block pointer array.
C
	    DO  ilat = 1, 180
		lat = 91 - ilat
		IF  ( ( rmnlat .le. FLOAT (lat) ) .and. 
     +		      ( latptr (ilat) .eq. 0 ) )  latptr (ilat) = iblk
	    END DO
	END DO
C
C*	Write out last block.  Swap bytes before writing.
C
	IF ( MTMACH .eq. MTIGPH .or. MTMACH .eq. MTULTX .or.
     +	     MTMACH .eq. MTALPH .or. MTMACH .eq. MTLNUX )
     +		CALL MV_SWP4 ( MAPLEN * 2, imapbf(1), imapbf(1) )
	irec = irec + 1
	CALL FL_WRIT  ( lungsf, irec, MAPLEN, imapbf, ier1 )
	irec = irec + 1
	CALL FL_WRIT  ( lungsf, irec, MAPLEN, imapbf (MAPLEN+1), ier2 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
	    CALL ER_WMSG  ( 'FL', ier1, outfil, ier )
	    CALL ER_WMSG  ( 'FL', ier2, outfil, ier )
	    CALL FL_CLOS  ( lungsf, ier )
	    STOP
	END IF
C
C*	Assemble the header record and write it out.
C*	Note that the header record is written as INTEGER*2 words.
C*	This may be changed here, but comparable changes must be
C*	made in GSFSSF and in GDRMAP.  If necessary, swap bytes for the
C*	final output file.
C
	imapb2 (1) = iblk
	DO  ilat = 1, 180
	    imapb2 ( ilat + 1 ) = latptr ( ilat )
	END DO
	DO  ilat = 181, IBLKLN * 2
	    imapb2 (ilat) = 0
	END DO
C*
	IF ( MTMACH .eq. MTIGPH .or. MTMACH .eq. MTULTX .or.
     +	     MTMACH .eq. MTALPH .or. MTMACH .eq. MTLNUX )
     +		CALL MV_SWP2 ( 4 * MAPLEN, imapb2, imapb2 )
	CALL FL_WRIT  ( lungsf, 1, MAPLEN, imapbf, ier1 )
	CALL FL_WRIT  ( lungsf, 2, MAPLEN, imapbf (MAPLEN+1), ier2 )
	IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
	    CALL ER_WMSG  ( 'FL', ier1, outfil, ier )
	    CALL ER_WMSG  ( 'FL', ier2, outfil, ier )
	    CALL FL_CLOS  ( lunid,ier )
	    CALL FL_CLOS  ( lungsf, ier )
	    STOP
	END IF
C	WRITE (6,*)  iblk, ' blocks written to new GSF file.'
	CALL FL_CLOS  ( lunid, ier )
	CALL FL_CLOS  ( lungsf, ier )
C------------------------------------------------------------------------
C*
	RETURN
	END
