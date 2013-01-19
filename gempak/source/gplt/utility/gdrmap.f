	SUBROUTINE GDRMAP  ( iret )
C************************************************************************
C* GDRMAP								*
C* 									*
C* This subroutine draws a map.  A map projection must be defined	*
C* before it is called.  The current color and line attributes will	*
C* be used.  The map file to be used may be specified in GSMFIL.	*
C* 									*
C* GDRMAP  ( IRET )							*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**                                                                     *
C* Log:									*
C* G. Chatters/RDS	 9/81						*
C* M. Goodman/RDS 	 9/85	Recoded from accidental purge		*
C* B. Doty/RDS		 5/87	Fixed loop limits so last block plots;	*
C*				Fixed file close bug; added NOMAPP err	*
C* M. desJardins/GSFC	 7/87	Replaced FSTVID with FL routines	*
C* M. desJardins/GSFC	 8/87	Added check for crossing IDL		*
C* M. desJardins/GSFC	 5/88	Eliminated call to GFLUSH		*
C* M. desJardins/GSFC	 3/89	Standardized				*
C* K. Brill/GSC          1/90   Extended check for setting dateline flag*
C* K. Brill/GSC          1/90   Check for blank region in case of conic *
C* M. desJardins/GSFC	 8/90	Elim check for ier2 when not set	*
C* K. Brill/EMC		 3/96	CALL PRNLON				*
C* G. Krueger/EAI	 8/96	Select quadrant for default map files	*
C* S. Jacobs/NCEP	 8/96	Added check for location of map files	*
C* G. Krueger/EAI	 9/96	Only perform quadrant check on CIA maps	*
C* J. Cowie/COMET	 9/96	Break lines > 2 deg lat for sat proj's  *
C* S. Jacobs/NCEP	 3/97	Added check for UTF and VG drivers	*
C* A. Hardy/GSC          9/98   Added check for RBK driver              *
C* G. Krueger/EAI	12/99	Byte swap for Little-Endian platforms	*
C* S. Jacobs/NCEP	 2/01	Added check for LINUX machine type	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'XYDEF.CMN'
	INCLUDE 	'DEVCHR.CMN'
C*
	CHARACTER	mapnam*80, ucmap*80, mptmp*80, newfil*132
	LOGICAL		points, done, skip, idl, notcut, exist
C*
	REAL		xlat (512), ylon (512)
C*
	REAL		rmapbf ( 256 )
	INTEGER		imapbf ( 256 )
	INTEGER*2	imapb2 ( 512 )
C*
	PARAMETER 	( MAPLEN = 128, IBLKFC = 2, DELMAX  = 2. )
C*
	EQUIVALENCE	( rmapbf , imapbf )
	EQUIVALENCE	( imapbf , imapb2 )
C-----------------------------------------------------------------------
	iret   = NORMAL 
C
C*	If the device is UTF, VG or RBK, do not draw the map.
C
	IF  ( ( ddev .eq. 'UTF' ) .or. ( ddev .eq. 'VG' ) .or.
     +        ( ddev .eq. 'RBK' ) )  RETURN
C
        points = .false. 
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
	IF ( MTMACH .eq. MTIGPH .or. MTMACH .eq. MTULTX .or.
     +	     MTMACH .eq. MTALPH .or. MTMACH .eq. MTLNUX )
     +		CALL MV_SWP2 ( MAPLEN, imapbf, imapbf )
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
		IF ( MTMACH .eq. MTIGPH .or. MTMACH .eq. MTULTX .or.
     +		     MTMACH .eq. MTALPH .or. MTMACH .eq. MTLNUX )
     +			CALL MV_SWP4 ( 2 * maplen, imapbf, imapbf )
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
		  ipt    = ipt + 5
		  skip   = .false.
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
C*		    Stuff all the latitudes and longitudes into separate
C*		    arrays.  Check if two points span a longitudinal 
C*		    area greater than 2.0 degrees.  If so, then break 
C*		    the longitudinal span into smaller distances. For
C*		    satellite projections, impose the same constraint
C*		    over the latitudinal spans.
C
		  IF  ( .not. skip )  THEN
		    jpts = 0
	            DO  i = 1, 2 * nmp - 2, 2
			jpts = jpts + 1
			dellat =  rmapbf ( ipt + i + 2 ) - 
     +				  rmapbf ( ipt + i )
			dellon =  rmapbf ( ipt + i + 3 ) -
     +				  rmapbf ( ipt + i + 1 )
C
C*			Check number of segments to break line into.
C
			nseg = IFIX (ABS (dellon) / delmax + 1. )

			IF ( mtype .eq. 3 ) THEN
			    nseg2 = IFIX (ABS (dellat) / delmax + 1. )
			    nseg  = MAX0 ( nseg, nseg2 )
			END IF

			delx = dellat / FLOAT ( nseg )
			dely = dellon / FLOAT ( nseg )
C
C*			Add start point to buffer.
C
			xlat (jpts) = rmapbf ( ipt + i )
			ylon (jpts) = rmapbf ( ipt + i + 1 )
C
C*			Add rest of points in segment.
C
			IF  ( nseg .gt. 1 )  THEN
			    DO  n = 2, nseg
				jpts = jpts + 1
				xlat (jpts) = xlat (jpts-1) + delx
				ylon (jpts) = ylon (jpts-1) + dely
	                    END DO
			END IF
	            END DO
C
C*		    Add the last point into the array.
C
		    jpts = jpts + 1
		    xlat (jpts) = rmapbf ( ipt + (nmp-1)*2 + 1 )
		    ylon (jpts) = rmapbf ( ipt + (nmp-1)*2 + 2 )
C
C*		    Draw the line segment.
C
		    IF ( mclass .ne. 3 ) THEN
C
C*		      Line will be continuous.
C
 		      CALL GLINE  ( 'M', jpts, xlat, ylon, iret )
C*
		    ELSE
C
C*                    If the line crosses the conic cut, it must be
C*                    done in pieces.  A segment is never drawn across
C*                    the cutting longitude.  Furthermore, a segment
C*                    cannot begin or end exactly on the cutting
C*                    longitude to avoid complicated checks to see
C*                    whether or not such segments cross the void.
C
		      cutang = angle2 + 180.
		      CALL PRNLON ( 1, cutang, ier )
                      istrt = 1
	              indx  = 1
		      DO WHILE ( indx .lt. jpts )
	                notcut = .true.
			icnt = 0
	                DO WHILE ( notcut )
	                  icnt = icnt + 1
	                  inm1 = indx
	                  indx = indx + 1
			  pxll = ylon (inm1) * ylon (indx)
	                  IF ( ylon (indx) .eq. cutang   .or.
     +			     ( ylon (inm1) .gt. cutang .and.
     +                         ylon (indx) .lt. cutang ) .or.
     +                       ( ylon (inm1) .lt. cutang .and.
     +                         ylon (indx) .gt. cutang ) .or.
     +                       ( cutang .eq. 180.  .and.
     +                         pxll .lt. 0. )            .or.
     +                       ( cutang .eq. -180. .and.
     +                         pxll .lt. 0. )         )  THEN
C*
                             IF ( ylon (istrt) .eq. cutang ) THEN
                                  istrt = istrt + 1
                                  icnt = icnt - 1
                             END IF
                             IF ( ylon (istrt + icnt - 1 ) .eq.
     +                            cutang ) icnt = icnt - 1
	                     IF ( icnt .gt. 1 )
     +                         CALL GLINE ( 'M', icnt, xlat (istrt),
     +                                      ylon (istrt), iret )
C*
	                     notcut = .false.
			     istrt = indx
	                   ELSE IF ( indx .eq. jpts ) THEN
	                     icnt = icnt + 1
                             IF ( ylon (istrt) .eq. cutang ) THEN
                                  istrt = istrt + 1
                                  icnt = icnt - 1
                             END IF
                             IF ( ylon (istrt + icnt - 1 ) .eq.
     +                            cutang ) icnt = icnt - 1
			     IF ( icnt .gt. 1 )
     +                           CALL GLINE ( 'M', icnt, xlat (istrt),
     +                                      ylon (istrt), iret )
	                     notcut = .false.
                           END IF
	                 END DO
                      END DO                             
                    END IF
C*
                    points = .true.
C
C*	            Increment offset values for next record.
C
		  END IF
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
