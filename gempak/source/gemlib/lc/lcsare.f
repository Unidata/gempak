	SUBROUTINE LC_SARE  ( area, iflno, stn, iret )
C************************************************************************
C* LC_SARE								*
C*									*
C* This subroutine sets the search criteria in a DM file using the	*
C* value for AREA input by the user.  The area may be composed of 	*
C* subareas which are separated by slashes (/).  The DM file must be	*
C* opened before this subroutine is called.  If an invalid subarea is	*
C* encountered, an error message is printed and an error is returned.	*
C* If any subarea is centered on a station, that station is returned	*
C* in STN.  Note that any subroutine which defines a search, such as	*
C* SF_SSTN, will eliminate the search set by this subroutine.  		*
C*									*
C* LC_SARE  ( AREA, IFLNO, STN, IRET )					*
C*									*
C* Input parameters:							*
C*	AREA		CHAR*		Area to be defined		*
C*	IFLNO		INTEGER		File number for DM file		*
C*									*
C* Output parameters:							*
C*	STN		CHAR*		Center station name		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return 		*
C*					  -1 = invalid area name	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/87						*
C* M. desJardins/GSFC	10/87	Fixed to make 1st area positive		*
C* I. Graffman/RDS	11/87	Added :C for country			*
C* M. desJardins/GSFC	 1/88	Fixed errors from 11/87			*
C* M. desJardins/GSFC	 6/88	Reordered variables			*
C* G. Huffman/GSC	 4/89	Convert ARIN to upper case		*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* J. Nielsen/SUNYA	 4/91	Require 5 digits for station number	*
C* K. Brill/NMC		 8/93	Change for 8-char ID			*
C* G. Krueger/EAI	 6/96	Add default projection			*
C************************************************************************
	CHARACTER*(*)	area, stn
C*
	LOGICAL		addtyp, cnflag
	INTEGER		iloval (2), ihival (2), istid (2)
	REAL  		rltln (4)
	CHARACTER	stns (40)*8, stcn*4, anam*72, keynam (2)*4
	CHARACTER	arin*72, aaa*72, cdproj*30
C------------------------------------------------------------------------
	iret = 0
	stn  = ' '
C
C*	Remove blanks and find length of area name, then convert to
C*	upper case.
C
	CALL ST_RMBL  ( area, arin, length, ier )
	IF  ( length .le. 0 )  THEN
	    iret = -1
	    CALL ER_WMSG ( 'LC', iret, area, ier )
	    RETURN
	END IF
	CALL ST_LCUC  ( arin, arin, ier )
C
C*	Fix to make first subarea positive.  This takes care of the
C*	condition where a negative latitude is entered and the
C*	minus sign is stripped, leaving only the positive value.
C*	This will not fix that case for later subareas which must be
C*	explicitly set.
C
	IF  ( arin (1:1) .ne. '+' )  THEN
	    aaa    = arin
	    arin   = '+' // aaa
	    length = length + 1
	END IF
C
C*	Reset all conditional searches in the DM file.
C
	CALL DM_DCSR  ( iflno, ier )
C
C*	Loop through the area name breaking it into parts.
C
	iend = 0
	DO WHILE  ( iend .lt. length )
C
C*	    Find next slash, which is a separator.
C
	    istart = iend + 1
	    iend   = INDEX  ( arin ( istart: ), '/' )
	    IF  ( iend .eq. 0 )  THEN
		iend  = length
		istop = length
	      ELSE
		iend  = iend + istart - 1
		istop = iend - 1
	    END IF
C
C*	    Get area name and check for type of search.
C
	    anam   = arin ( istart:istop )
	    lenare = istop - istart + 1
	    IF  ( anam (1:1) .eq. '-' )  THEN
		addtyp = .false.
		anam   = anam ( 2: )
	        lenare = lenare - 1
	      ELSE IF  ( anam (1:1) .eq. '+' )  THEN
		addtyp = .true.
		anam   = anam ( 2: )
	        lenare = lenare - 1
	      ELSE
		addtyp = .true.
	    END IF
C
C*	    Decode the area.  If there is a colon in the area name,
C*	    set iartyp to 8.
C
	    icoln = INDEX ( anam, ':' )
	    IF  (( icoln .ne. 0 )  .and. 
     +	         ( anam (lenare - 1:lenare) .ne. ':C')) THEN
		iartyp = 8
	      ELSE
		CALL LC_ABND ( anam, iartyp, rltln (1), rltln (2), 
     +	                       rltln (3), rltln (4), stns, nstn, stcn,
     +			       cdproj, cenlat, cenlon, iret )
		IF  ( ( iartyp .eq. 2 ) .and. ( stn .eq. ' ' ) )
     +				stn = stns (1)
	    END IF
C
C*	    Set the search depending on the area type.  When area is
C*	    DSET (iartyp=4), no condition needs to be set on the search.
C
	    IF  ( ( iartyp .ge. 1 ) .and. ( iartyp .le. 3 ) )  THEN
C
C*		Set search with area bounds.
C
		keynam (1) = 'SLAT'
		keynam (2) = 'SLON'
		iloval (1) = rltln (1) * 100.
		iloval (2) = rltln (2) * 100.
		ihival (1) = rltln (3) * 100.
		ihival (2) = rltln (4) * 100.
		CALL DM_CSRC ( iflno, addtyp, 2, keynam, iloval, ihival,
     +			       ier )
C
	      ELSE IF  ( iartyp .eq. 6 ) THEN
C
C*		Set station list.
C
		ier = 0
		DO  i = 1, nstn
C
C*		    Check to see if this is a character or number id.
C
		    CALL ST_NUMB  ( stns (i), istid (1), ier1 )
		    IF  ( ier1 .ne. 0 )  THEN
			CALL ST_STOI  ( stns (i), 8, ni, istid, ier )
			keynam (1) = 'STID'
			keynam (2) = 'STD2'
			nkey = 2
		      ELSE
			keynam (1) = 'STNM'
			nkey = 1
		    END IF
		    CALL DM_CSRC ( iflno, addtyp, nkey, keynam, istid,
     +				   istid, ier1 )
C
C*		    Check for case when STD2 does not exist.
C
		    IF  ( ier1 .ne. 0 .and. nkey .eq. 2 )
     +			CALL DM_CSRC ( iflno, addtyp, 1, keynam, istid,
     +				       istid, ier1 )
		    IF ( ier1 .ne. 0 ) ier = ier1
		END DO
C
	      ELSE IF  ( iartyp .eq. 5 )  THEN
C
C*		Set state or country.
C
		CALL LC_COUN  ( stcn, cnflag, ier )
		IF  ( cnflag )  THEN
		    keynam (1) = 'COUN'
		  ELSE
		    keynam (1) = 'STAT'
		END IF
		CALL ST_CTOI ( stcn, 1, istcn, ier )
		CALL DM_CSRC ( iflno, addtyp, 1, keynam, istcn, istcn,
     +			       ier )
	      ELSE IF ( iartyp .eq. 7 )  THEN
C
C*	        Set country.
C
	        keynam (1) = 'COUN'
	        CALL ST_CTOI  ( stcn, 1, istcn, ier )
	        CALL DM_CSRC  ( iflno, addtyp, 1, keynam, istcn, istcn,
     +				ier )
	      ELSE IF  ( iartyp .eq. 8 )  THEN
C
C*		Since the area could not be decoded, check whether 
C*		range for header has been specified.
C
		CALL ST_LCUC  ( anam, anam, ier )
		keynam (1) = anam ( : icoln-1 )
		CALL ST_ILST  ( anam ( icoln+1: ), ':', IMISSD, 
     +				2, iloval, num, ier1 )
		IF  ( num .eq. 1 )  THEN
		    num = 2
		    ihival (1) = iloval (1)
		  ELSE IF  ( num .eq. 2 )  THEN
		    ihival (1) = iloval (2)
		END IF
		IF  ( num .eq. 2 )  THEN
		    CALL DM_CSRC  ( iflno, addtyp, 1, keynam,
     +				    iloval, ihival, ier )
		    iret = 0
		END IF
	    END IF
C
C*	    Check error code.
C
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'DM', ier, ' ', ier1 )
		iret = -1
	    END IF
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'LC', iret, anam, ier )
		RETURN
 	    END IF
	END DO
C*
	RETURN
	END
