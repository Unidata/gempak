	SUBROUTINE WN_GHDR  ( bultin, icor, wtype, wfrom, cnties, 
     +			      edttim, nchar, etn, iret )
C************************************************************************
C* WN_GHDR								*
C*									*
C* This subroutine gets the header information from a WMO bulletin.	*
C*									*
C* The bulletin head consists of the the lines before the first         *
C* asterisk in the warning report.  The bulletin should still have      *
C* all control characters present for this routine to function properly.*
C*									*
C* WN_GHDR  ( BULTIN, ICOR, WTYPE, WFROM, CNTIES, EDTTIM, NCHAR, ETN,	*
C*	      IRET )							*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		WMO bulletin w/ control chars	*
C*									*
C* Output parameters:							*
C*	ICOR		INTEGER		Correction indicator		*
C*	WTYPE		CHAR*		Warning type                    *
C*	WFROM		CHAR*		Issuing station id		*
C*	CNTIES		CHAR*		Counties in the warning		*
C*	EDTTIM		CHAR*		Expiration time                 *
C*	NCHAR		INTEGER		VTEC indicator          	*
C*	ETN		CHAR*		Event Tracking Number           *
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal return		*
C*					 -1 = 'BULLETIN' not found      *
C*					 -2 = Not a valid report type   *
C*					 -3 = Invalid county/date string*
C**									*
C* Log:									*
C* A. Hardy/GSC		 5/99						*
C* A. Hardy/GSC		 5/99	Added indx check for date/time string 	*
C* A. Hardy/GSC		 5/99	Added logical and check for 8 hdr lines *
C* A. Hardy/GSC		 5/99	Reworked, get all information from hdr  *
C* A. Hardy/GSC		 2/00   Modified to allow for longer cnty ids   *
C* A. Hardy/GSC          2/00   Added 'BULLETIN' check                  *
C* A. Hardy/GSC          3/00   Added length to 'btin' in INDEX         *
C* A. Hardy/GSC          6/00   Reworked using new decoding logic       *
C* A. Hardy/GSC         11/00   Modified storm type decoding		*
C* D. Kidwell/NCEP      12/00   Added check for VTEC, fixed wtype length*
C* A. Hardy/SAIC        12/01   Modified finding county string		*
C* F. J. Yen/NCEP        3/02	Reworked stop time string and checked	*
C*			 	length.  Renumbered err codes.		*
C* F. J. Yen/NCEP	 3/08	Added Event Tracking Number (CSC)	*
C* F. J. Yen/NCEP	 4/08	Added call to BR_VTEC			*
C* S. Guan/NCEP         11/17   Modified to add snow squall warn (SQW)  *
C* S. Guan/NCEP         05/18   Fixed producing spurious warns bug      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	bultin, wtype, wfrom, edttim, cnties, etn
        INTEGER		icor
C*
	CHARACTER	btin*(DCMXBF), vtec*124
        CHARACTER       vact*4, vorig*4, vtype*2, vstrt*11, vsig*2,
     +                  vend*11, vnum*5, vprdc*5, vtecp1*2

C------------------------------------------------------------------------
        iret = 0
        icor = 0
        wtype = ' '
C
C*	Find the length of the header string.
C
	CALL ST_LSTR ( bultin, lenbul, ier )
        CALL ST_UNPR ( bultin, lenbul, btin, lenb, ier)  
        ibuldex = INDEX ( btin(:lenb), 'BULLETIN')
C
C        IF ( ibuldex .eq. 0 ) THEN
C            iret = -1  
C	    RETURN
C        END IF
C
C*	Check for a VTEC line beginning with '/' and adjust
C*	header string length accordingly.  Also get the VTEC string.
C
	ierv = 1
	vtec = ' '
	etn = ' '
	islash = INDEX ( btin ( : ibuldex ), '/' )
        IF ( islash .eq. 0 ) THEN
           islash = INDEX ( btin ( : lenb ), '/' )
        END IF
	IF ( islash .gt. 0 ) THEN
	    vtecp1 = btin ( islash + 1: islash + 2 )
      	    IF ( vtecp1 .eq. 'O.' .or. vtecp1 .eq. 'T.' .or.
     +		   vtecp1 .eq. 'X.' .or. vtecp1 .eq. 'E.' ) THEN
      	        IF ( btin (islash+46:islash+47) .eq. 'Z/' ) THEN
                    vtec = btin (islash+1:islash+46)
		    CALL BR_VTEC ( vtec, vact, vorig, vtype, vsig, vnum,
     +                     vstrt, vend, vprdc, ierv)
		  ELSE
C*
C*		    Bad vtec, but set vtec for DC_WLOG only
C*
		    vtec = btin (islash+1:islash+40)
		END IF
	      ELSE
C*
C*	        Bad vtec, but set vtec for DC_WLOG only
C*
		vtec = btin (islash+1:islash+40)
	    END IF
	    ibuldex = islash 
	END IF
        IF ( vtec .eq. ' ' .or. ierv .ne. 0 ) THEN 
            iret = 3
            CALL DC_WLOG ( 2, 'DCWARN', iret, vtec(:40), ierrr )
	  ELSE
	    CALL ST_LSTR ( vnum, len, ier )
            IF ( len .gt. 4 ) THEN
                iret = 3
                CALL DC_WLOG ( 2, 'DCWARN', iret, vtec(:40), ierrr )
	      ELSE
	        etn = vnum (:len)
	    END IF
        END IF
C
C*      Retrieve the storm type and the issuing station.
C
        isvridx = INDEX ( btin(:ibuldex), 'SVR')
        itoridx = INDEX ( btin(:ibuldex), 'TOR')
        iffwidx = INDEX ( btin(:ibuldex), 'FFW')
        isqwidx = INDEX ( btin(:ibuldex), 'SQW')
        IF ( isvridx .ne. 0 )  THEN 
            itypidx = isvridx
          ELSE IF ( itoridx .ne. 0 )  THEN 
            itypidx = itoridx
          ELSE IF ( iffwidx .ne. 0 ) THEN
            itypidx = iffwidx
          ELSE IF ( isqwidx .ne. 0 ) THEN
            itypidx = isqwidx
	  ELSE
	    itypidx = 0
        END IF
C
        IF ( (itypidx .ne. 0) .or. 
     +    ( INDEX (btin(:ibuldex+20), '.SQ.W.') .ne. 0) ) THEN
            IF ( itypidx .ne. 0 ) THEN 
                wtype = btin ( itypidx:itypidx+2 )
                wfrom = btin ( itypidx+3:itypidx+5 )
                ictybeg = itypidx+7
            ELSE
                wtype = 'SQW'
                ictybeg = 1
            END IF
C
C*	    Find the counties and ending time.
C
            IF ( btin (ibuldex-2:ibuldex-2).eq. '-' ) THEN
		ietnd = ibuldex - 3
	      ELSE
C
C*		The ending '-' has been omitted
C
		ietnd = ibuldex - 2
	    END IF
	    ietbg = ietnd
	    ibg = max ( ictybeg, ietnd - 10 )
	    ied = ietnd
	    DO WHILE ( ied .gt. ibg )
		ied = ied - 1
		IF ( btin ( ied:ied ) .eq. '-' ) THEN
		    ietbg = ied
		    ied = ibg
		END IF
	    END DO
	    IF ( ietbg .eq. ietnd ) THEN
		iret = -3
		RETURN
	      ELSE IF ( ietnd - ietbg .ne. 6 ) THEN
		edttim = '999999'
	      ELSE
		edttim = btin ( ietbg + 1:ietnd )
            END IF
            cnties = btin ( ictybeg:ietbg )
C
C*          Check for a correction indicator
C
 	    indx = INDEX ( btin(itypidx:lenbul), 'COR')
 	    IF (  indx .gt. 0  ) icor = 1
          ELSE
            iret = -2
        END IF 
        nchar = INDEX ( btin (islash + 2 : lenb ), '/O.' )
C*
	RETURN
	END
