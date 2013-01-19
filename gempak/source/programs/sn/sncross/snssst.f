	SUBROUTINE SNSSST  ( cxstns, iflno, nprms, msdsiz, nstn, stns, 
     +			     ipsdat, nlvls, stndat, idtype, sloc, xmin,
     +			     xmax, iret )
C************************************************************************
C* SNSSST								*
C*									*
C* This subroutine gets the stations to analyze for the cross section	*
C* program.								*
C*									*
C* SNSSST  ( CXSTNS, IFLNO, NPRMS, MSDSIZ, NSTN, STNS, IPSDAT,		*
C*           NLVLS, STNDAT, IDTYPE, SLOC, XMIN, XMAX, IRET )		*
C*									*
C* Input parameters:							*
C*	CXSTNS		CHAR*		User input station list		*
C*	IFLNO		INTEGER		File number			*
C*	NPRMS		INTEGER		Number of parameters in file	*
C*	MSDSIZ		INTEGER		Size of station data buffer	*
C*									*
C* Output parameters:							*
C*	NSTN		INTEGER		Number of stations		*
C*	STNS   (NSTN)	CHAR*8		Station ids			*
C*	IPSDAT (NSTN)	INTEGER		Pointer to data in STNDAT	*
C*	NLVLS  (NSTN)	INTEGER		Number of levels at station	*
C*	STNDAT (MSDSIZ)	REAL		Station data buffer		*
C*      IDTYPE (*,*)	INTEGER		Type of level data		*
C*	SLOC   (NSTN)	REAL		Station locations on axis	*
C*	XMIN		REAL		Minimum value on x axis		*
C*	XMAX		REAL		Maximum value on x axis		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = fewer than four stations	*
C*					 -7 = data buffer too small	*
C*					 -9 = stn ... can't be found	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* I. Graffman/RDS	 7/86	Fixed $STNLST, error msg for no stn	*
C* M. desJardins/GSFC	 9/86	Changed numlev in SN library.		*
C* G. Huffman/GSC	11/88	GEMPAK4; removed defunct STNLST option;	*
C* M. desJardins/GSFC	11/89	Changes for station time;		*
C*				changed name from SNSSTN to SNSSST	*
C* M. desJardins/GSFC	 7/90	Eliminate cross section table		*
C* S. Schotz/GSC	 8/90	Changes to pass out idtype		*
C* M. desJardins/GSFC	 3/91	Added LLTMCX				*
C* K. Brill/NMC		 8/93	Change for 8-char Id			*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	cxstns, stns (*), badstn (LLTMCX)*8
	REAL		stndat (*)
	INTEGER		ipsdat (*), nlvls (*), idtype (LLMXLV, *)
C*
	REAL		sltln (2,20)
	CHARACTER	stnin (20)*8, cid*8, stnum*8
	LOGICAL		found (20), done
C------------------------------------------------------------------------
C*	Break CXSTNS into station array.
C
	CALL ST_LCUC ( cxstns, cxstns, ier )
	CALL ST_CLST ( cxstns, ';', ' ', 20, stnin, nstin, ier )
C
C*	Read in the data from the stations.
C
	done = .false.
	nstn = 0
	nbadsn = 0
	iptr = 1
	DO i = 1, nstin
	    found (i) = .false.
	END DO
C
C*	Set the pointer to the beginning.
C
	CALL SN_BEGS  ( iflno, ier )
	DO WHILE ( .not. done )
C
C*	  Read in the next stations.
C
	  CALL SN_SNXT ( iflno, cid, idnum, rlat, rlon, el, ier)
	  IF  ( ier .ne. 0 ) THEN
	    done = .true.
	   ELSE
C
C*	    Check for the station in list of stations.  
C
	    istn = 0
	    CALL ST_INCH ( idnum, stnum, ier )
	    DO  i = 1, nstin
		IF ((cid .eq. stnin(i)) .or.
     +                 (stnum .eq. stnin(i)))istn = i
	    END DO
C
C*	    If station is in list, save data and headers.
C
	    IF  ( istn .ne. 0 )  THEN
		CALL SN_RDAT ( iflno, numlev, stndat (iptr), ihhmm,
     +                          ier )
		IF  ( (ier .ne. 0) .or. (numlev .le. 0) ) istn = 0
	    END IF
C*
	    IF  ( istn .ne. 0 ) THEN
	        CALL SN_RTYP ( iflno, numlev, idtype (1, istn), ier )
		nstn = nstn + 1
		stns (istn) = stnin (istn)
		sltln (1, istn) = rlat
		sltln (2, istn) = rlon
		nlvls (istn) = numlev
		ipsdat (istn) = iptr
		iptr = iptr + numlev * nprms
		found (istn) = .true.
		IF  ( iptr .gt. MSDSIZ )  THEN
		    iret = -7
		    CALL ER_WMSG ('SNCROSS', iret, ' ', ier )
		    RETURN
		END IF
		IF  ( nstn .eq. nstin )  done = .true.
	    END IF
	  END IF
	END DO
C
C*	Check for stations that were not found.
C
	j = 0
	DO i = 1, nstin
	    IF  (.not. found(i) ) THEN
		nbadsn = nbadsn + 1
		badstn (nbadsn) = stnin (i)
	        CALL ER_WMSG ('SNCROSS', -9, stnin (i), iret)
	      ELSE
		j = j + 1
		IF  ( i .ne. j ) THEN
		    stns (j)    = stns (i)
		    sltln (1,j) = sltln (1,i)
		    sltln (2,j) = sltln (2,i)
		    nlvls (j)   = nlvls (i)
		    ipsdat (j)  = ipsdat (i)
		END IF
	    END IF
	END DO
C
C*	Check that at least four valid stations were found.
C
	IF  ( nstn .lt. 4 ) THEN
	    iret = -6
	    CALL ER_WMSG ( 'SNCROSS', iret , ' ', ier )
	    RETURN
	END IF
C
C*	Set up x axis.
C
	CALL SNSXAX  ( nstn, stns, sltln, ipsdat, nlvls, sloc,
     +		       xmin, xmax, iret )
C*
	RETURN
	END
