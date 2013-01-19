	SUBROUTINE GH_BKPP ( ibkpt, iarea, iret )
C************************************************************************
C* GH_BKPP								*
C*									*
C* This subroutine draws breakpoints which are given as a series of     *
C* pairs.                                                               *
C*									*
C* GH_BKPP ( IBKPT, IAREA, IRET ) 					*
C*									*
C* Input parameters:							*
C*	IBKPT(2)	INTEGER		Array of breakpoint numbers     *
C*      IAREA(2)	INTEGER         Geographic area designator	*
C*                                        IUSGEC = U.S.                 *
C*                                        IMXCSA = Mex., Cn. Am., S. Am.*
C*                                        IPACIF = East Pacific coast   *
C*                                        IKEYS  = Florida Keys         *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/03	Replaces GH_BKCM, GH_BKPC, part GH_BKUS *
C* D. Kidwell/NCEP	 1/04	Change GLINE arguments for Matagorda I. *
C* m.gamazaychikov/SAIC	04/04	From GH_BKPR with modifications		*
C* m.gamazaychikov/SAIC	07/04	Exclude intermediate supplemental	*
C*				breakpoints from plotting, change GLINE	*
C* 				arguments for Matagorda Island 		*
C* D. Kidwell/NCEP	 2/05	Mod to connect points across TX/MX brdr *
C* D. Kidwell/NCEP	 4/05	Add pri 99 check for Keys, do not allow *
C*				plotting across geog areas except TX/MX *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C*
	INTEGER		ibkpt (*), iarea(*)
C*
        REAL            drlat (MAXBK), drlon (MAXBK), bdrlat (2),
     +			bdrlon (2)
C------------------------------------------------------------------------
	iret = 0
C
        IF ( iarea (1) .eq. iarea (2) ) THEN
            ipass = 1
          ELSE
            ipass = 2
        END IF
C
	IF ( ipass .eq. 2 ) THEN
	    IF ( ( ( iarea ( 1 ) .eq. IUSGEC ) .and.
     +		   ( iarea ( 2 ) .eq. IMXCSA ) ) .or.
     +		 ( ( iarea ( 1 ) .eq. IMXCSA ) .and.
     +		   ( iarea ( 2 ) .eq. IUSGEC ) ) ) THEN
	      ELSE
	        RETURN
	    END IF
	END IF
C
	IF ( iarea ( 1 ) .eq. IKEYS ) THEN
	    isupp = 99
	  ELSE
	    isupp = 19
	END IF
C
	DO ip = 1, ipass
	    ibeg = indxbk ( iarea (ip) )
	    iend = indxbk ( iarea (ip)  + 1 ) - 1
            IF ( ipass .eq. 1) THEN
	        ilo  = MIN0 ( ibkpt (1), ibkpt (2) )
	        ihi  = MAX0 ( ibkpt (1), ibkpt (2) )
              ELSE
	        ilo  = 1
	        ihi  = ibkpt (ip)
            END IF
	    bdrlat ( ip ) = RMISSD
	    bdrlon ( ip ) = RMISSD
C
C*          Find the beginning and ending indices in the breakpoint
C*	    sequence array.
C
	    indxb = 0
	    indxe = 0
	    DO kk = ibeg, iend
	        IF ( ibkseq ( kk ) .eq. ilo ) indxb = kk
	        IF ( ibkseq ( kk ) .eq. ihi ) indxe = kk
            END DO
	    IF ( ( indxb * indxe ) .gt. 0 ) THEN
	        np = indxe - indxb + 1
C
C*		Check the priority number of a breakpoint.
C        
                inpri = 1
                DO inp = indxb, indxe
                    IF ( ( ibkpri ( inp ) .ne. isupp ) .or.
     +                   ( ( ibkpri ( inp ) .eq. isupp ) .and. 
     +                     ( ( inp .eq. indxb ) .or. 
     +			     ( inp .eq. indxe ) ) ) ) THEN
                        drlat ( inpri ) = bklat ( inp )
                        drlon ( inpri ) = bklon ( inp )
                        inpri = inpri + 1
                    END IF
C
C*		    Save points to plot across TX/MX border.
C
		    IF ( ( ipass .eq. 2 ) .and. ( inp .eq. indxb )) THEN
			bdrlat ( ip ) = bklat ( inp )
			bdrlon ( ip ) = bklon ( inp )
		    END IF
                END DO
                npri = inpri - 1
C
C*	        Draw the coastline connecting the points in the pair.
C
	        CALL GLINE ( 'M', npri, drlat, drlon, ier )
	    END IF
C
C*	    Check for inclusion of Matagorda Island for U.S.
C*	    These two points MUST be the last entries in the "U. S. 
C*	    sounds, bays, etc" section of the breakpoint plotting table.
C
            IF ( iarea (ip) .eq. IUSGEC ) THEN
                IF ( ( ilo .le. isnd ( 1, nsnds ) ) .and.
     +	             ( ihi .ge. isnd ( 2, nsnds ) ) ) THEN
                    CALL GLINE ( 'M', 2,
     +		                 bklat ( indxbk (IWATER+1) - 2 ),
     +			         bklon ( indxbk (IWATER+1) - 2 ), ier )
                END IF
            END IF
        END DO
C
C*	Plot across TX/MX border if required.
C
	IF ( ipass .eq. 2 ) CALL GLINE ( 'M', 2, bdrlat, bdrlon, ier )
C*
	RETURN
	END
