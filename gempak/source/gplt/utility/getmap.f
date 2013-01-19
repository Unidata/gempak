	SUBROUTINE GETMAP  ( maptyp, mxelts, mxpts,  nelts, npts, ielts,
     +			     xlats, ylons, iret )
C************************************************************************
C* GETMAP								*
C* 									*
C* This subroutine gets the latitude and longitude data points for a	*
C* map.  An index that points to the start of each map element is also	*
C* returned.  The map elements may be returned as lines or as polygons.	*
C* The map file to be used may be specified in GSMFIL.			*
C* 									*
C* GETMAP  ( MAPTYP, MXELTS, MXPTS,  NELTS, NPTS, IELTS, XLATS, YLONS,	*
C*	     IRET )							*
C* 									*
C* Input parameters:                                                    *
C*	MAPTYP		INTEGER		Map type			*
C*					  0 = lines			*
C*					  1 = polygons			*
C*	MXELTS		INTEGER		Size of IELTS array		*
C*	MXPTS		INTEGER		Size of XLATS and YLONS arrays	*
C*									*
C* Output parameters:							*
C*	NELTS		INTEGER		Number of map elements		*
C*	NPTS		INTEGER		Number of lats/lons		*
C*	IELTS (NELTS)	INTEGER		Index to map elements		*
C*	XLATS (NPTS)	REAL		Map latitudes			*
C*	YLONS (NPTS)	REAL		Map longitudes			*
C* 	IRET		INTEGER		Return code			*
C**                                                                     *
C* Log:									*
C* G. Krueger/EAI	 7/97	Derived from GDRMAP			*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
C*
	INTEGER		ielts (*)
	REAL		xlats (*), ylons (*)
C*
	CHARACTER	mapnam*80, ucmap*80, mptmp*80, newfil*132
	LOGICAL		points, done, skip, idl, exist, skpall
C*
	REAL		rmapbf ( 256 )
	INTEGER		imapbf ( 256 )
	INTEGER*2	imapb2 ( 512 )
C*
	PARAMETER 	( MAPLEN = 128, IBLKFC = 2 )
C*
	EQUIVALENCE	( rmapbf , imapbf )
	EQUIVALENCE	( imapbf , imapb2 )
C-----------------------------------------------------------------------
	iret   = NORMAL 
	npts   = 0
	nelts  = 0
C
        points = .false. 
	skpall = .false.
C
C*	Check if map projection has been set.
C
	IF  ( .not. mset )  THEN
	    iret = NIPROJ
	    RETURN
	END IF
C
C*	Check for mapping across international date line.
C
	IF  ( ( ( blonw .lt. blone ) .and. ( blonw .gt. 0. )
     +	     .and. ( blonw .lt. 180. ) .and. ( blone .gt. 180. ) )
     +      .or.
     +        ( ( blonw .lt. blone ) .and. ( blonw .lt. 0. )
     +       .and. ( blonw .lt. -180. ) .and. ( blone .gt. -180. ) )
     +      ) THEN
	    idl = .true.
	  ELSE
	    idl = .false.
	END IF
C
C*	Select the map files to be used.
C
	CALL GQMFIL   ( mapnam, iret )
C
C*	Check for default map file, select a faster one if possible.
C
	CALL ST_LCUC ( mapnam, ucmap, ier )
	CALL ST_LSTR ( ucmap, lenmnm, ier )
	IF ( ucmap (1:4) .eq. 'HIPO' .and.
     +	     ucmap (lenmnm-2:lenmnm) .eq. 'CIA' ) THEN
	    mapnam (5:6) = 'wo'
C
C*	    If map is not centered over the pole, and does not cross
C*	    the dateline, select the smallest map file that contains
C*	    the specified area.
C
	    IF ( blone .gt. blonw ) THEN
		IF ( blats .ge. 0 ) THEN
		    IF ( blone .le. 0 ) mapnam (5:6) = 'nw'
		    IF ( blonw .ge. 0 ) mapnam (5:6) = 'ne'
		END IF
		IF ( blatn .le. 0 ) THEN
		    IF ( blone .le. 0 ) mapnam (5:6) = 'sw'
		    IF ( blonw .ge. 0 ) mapnam (5:6) = 'se'
		END IF
	    END IF
	END IF
C
C*	Determine if map file exists. Look at file name in local
C*	and $GEMMAPS.
C
	CALL FL_INQR ( mapnam, exist, newfil, ier )
	IF  ( .not. exist )  THEN
C
C*	    Look for file in $GEMMAPS.
C
	    mptmp = '$GEMMAPS/' // mapnam
	    CALL FL_INQR ( mptmp, exist, newfil, ier )
	    IF  ( exist )  mapnam = mptmp
	END IF
C
C*	Open the map files to be used.
C*	Return if the file cannot be opened.
C
	CALL FL_DOPN  ( mapnam, maplen, .false., lunmap, istat )
	IF  ( istat .ne. 0 )  THEN
	    iret = NOMFIL
	    RETURN
	END IF
C
C*	Read header block.  It is not necessary to read the second
C*	record since there is nothing in it.
C
	CALL FL_READ  ( lunmap, 1, MAPLEN, imapbf ( 1 ), ier1 )
	IF  ( ier1 .ne. 0 )  THEN
	    CALL FL_CLOS  ( lunmap, ier )
	    iret = NMAPFR
	    RETURN
	END IF
C
C*	Get number of blocks and start block.  Note that nmblk and
C*	LATPTR are stored in the file as INTEGER*2 words.
C
	nmblk = imapb2 ( 1 )
	ixlat = MAX0 ( 1, MIN0 ( 180, IFIX( 91. - blatn ) ) )
	isblk = imapb2 ( 1 + ixlat )
	irec  = 2 * ( isblk - 1 )
C
C*	Loop through reading blocks.
C
	IF  ( isblk .gt. 0 )  THEN
C
C*	    Loop through the blocks in the map file.
C                             
	    iblk = isblk
	    done = .false.
	    DO WHILE ( ( .not. done ) .and. ( iblk .le. nmblk ) )
C
C*		Increment block counter for next time through loop.
C
		iblk = iblk + 1
C
C*		Read in block of data.  Note that each record is
C*		contained in two blocks.
C
		irec = irec + 1
		CALL FL_READ ( lunmap, irec, maplen, imapbf ( 1 ), ier1)
		irec = irec + 1
		CALL FL_READ ( lunmap, irec, maplen, imapbf (129), ier2)
		IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  THEN
		    CALL FL_CLOS  ( lunmap, ier )
		    iret = NMAPFR
		    RETURN
		END IF
C
C*	        Pick up number of segments in current block.
C
	        nmseg = imapbf ( 1 )
		ipt   = 3
C
C*		Loop through all the segments.
C
	        DO  jseg = 1, nmseg
C
C*		  Get the number of points and the lat/lon range.
C
                  nmp    = imapbf ( ipt )
C
C*		  Check for the case (only in old files) where
C*		  the I*2 word following nmp had a value of 1.
C
		  IF  ( nmp .gt. 16000 )  nmp = MOD ( nmp, 65536 )
		  alatmn = rmapbf ( ipt + 1 )
		  alonmn = rmapbf ( ipt + 2 )
		  alatmx = rmapbf ( ipt + 3 )
		  alonmx = rmapbf ( ipt + 4 )
		  icblk  = imapbf ( ipt + 5 )
		  ipt    = ipt + 5
		  skip   = .false.
C
C*		  If this is not a continuation block, it is the start
C*		  of a new map element.
C
		  IF  ( icblk .eq. 0 ) THEN
C
C*		      Throw out the entire previous map element if it
C*		      was entirely outside of the lat/lon range.
C
		      IF  ( skpall ) THEN
			  npts = ielts ( nelts ) - 1
			  nelts = nelts - 1
		      END IF
		      nelts = nelts + 1
C
C*		      No more space for map elements.
C
		      IF  ( nelts .gt. mxelts ) THEN
			  npts = ielts ( nelts ) - 1
			  nelts = nelts - 1
			  IF ( nelts .lt. 1 ) nelts = 0
			  iret = NNMAPPL
			  RETURN
		      END IF
C
C*		      Add the current map element.
C
		      ielts ( nelts ) = npts + 1
		      skpall = .true.
		  END IF
C
C*	          Check for ending latitude, then check all longitude 
C*		  limits.
C
                  IF  ( blats .gt. alatmx )  THEN
		    done = .true.
		    skip = .true.
		  END IF
C
		  IF  ( blatn .lt. alatmn )  skip = .true.
C
C*		  Check longitude range in this record to see if they
C*		  should be plotted.  Check is only performed if the
C*		  International Date Line is not crossed.
C
	          IF ( ( blonw .lt. blone ) .and. ( .not. idl ) ) THEN
	                IF ( ( blonw .gt. alonmx ) .or.
     +	                     ( blone .lt. alonmn ) )   skip = .true.
	          END IF
C
C*		  Stuff all the latitudes and longitudes into separate
C*		  arrays.
C
		  IF  ( .not. skip .or. mtype .ne. 0 )  THEN
		    IF ( .not. skip ) skpall = .false.
	            DO  i = 1, 2 * nmp - 2, 2
			npts = npts + 1
			IF  ( npts .gt. mxpts ) THEN
			    npts = ielts ( nelts ) - 1
			    nelts = nelts - 1
			    IF ( nelts .lt. 1 ) nelts = 0
			    iret = NNMAPPT
			    RETURN
			END IF
C
C*			Add point to buffer.
C
			xlats (npts) = rmapbf ( ipt + i )
			ylons (npts) = rmapbf ( ipt + i + 1 )
	            END DO
C
C*		    Add the last point into the array.
C
		    npts = npts + 1
		    IF  ( npts .gt. mxpts ) THEN
			npts = ielts ( nelts ) - 1
			nelts = nelts - 1
			IF ( nelts .lt. 1 ) nelts = 0
			iret = NNMAPPT
			RETURN
		    END IF
		    xlats (npts) = rmapbf ( ipt + (nmp-1)*2 + 1 )
		    ylons (npts) = rmapbf ( ipt + (nmp-1)*2 + 2 )
C*
                    points = .true.
		  END IF
C
C*	          Increment offset values for next record.
C
		  ipt = ipt + 2 * nmp + 1
	        END DO
	    END DO
	END IF
C
C*	Close the map file.
C
        CALL FL_CLOS  ( lunmap, ier )
C
C*	Check that some points were plotted.
C
        IF  ( .not. points )  iret = NOMAPP
C*
	RETURN
	END
