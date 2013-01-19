	SUBROUTINE BG_PARM ( buftbl, cvntbl, nbp, ngp, bparm, ireps,
     +			     gparm, logscl, iofset, nbits,
     +			     cfac, ctrm, iret )
C************************************************************************
C* BG_PARM								*
C*									*
C* This subroutine uses the information in a Jack Woollen style BUFR	*
C* table file and the information in a second table to create a list	*
C* of GEMPAK parameters names and packing information.  The BUFR table	*
C* file name is BUFTBL.  The second table is a conversion table in the	*
C* file CVNTBL.  The columns of CVNTBL contain the following:		*
C*									*
C*  BUFR name	GEMPAK name   conversion factor   conversion term	*
C*									*
C* The BUFR name is used to find the packing information in the		*
C* BUFR file.  The conversion factor and term are used to determine	*
C* the GEMPAK packing information returned in LOGSCL, IOFSET, and	*
C* NBITS.  The conversion factor and term for each parameter along	*
C* with its GEMPAK name are also returned.  The conversion factor and	*
C* term are to be applied to the data values according to:		*
C*									*
C*  GEMPAK value = BUFR value * CFAC + CTRM				*
C*									*
C* The BUFR packing information and this equation are used to compute	*
C* the GEMPAK packing information.					*
C*									*
C* If replication occurs for a BUFR parameter other than a sounding	*
C* parameter, enter a line in the table for each replicated element	*
C* for the parameter.  The BUFR name entry will be the same on each	*
C* line; the GEMPAK name must be different.  Only one replicated	*
C* parameter name will be stored in a BPARM element.  The number of	*
C* replications will be stored in the corresponding IREPS element.	*
C* If a BPARM element contains a list of non-replicated parameters,	*
C* then IREPS for that element is zero.  Note that a parameter occurs	*
C* IREPS + 1 times in the data set.					*
C*									*
C* If a record in the conversion table begins with \, then a new	*
C* grouping of parameter names follows in the next element of BPARM.	*
C* This forces the parameters preceding the \ to be read in a call	*
C* to the BUFR reader separate from that call to the BUFR reader for	*
C* parameters following the \.						*
C*									*
C* BG_PARM ( BUFTBL, CVNTBL, NBP, NGP, BPARM, IREPS, GPARM, LOGSCL,	*
C*	     IOFSET, NBITS, CFAC, CTRM, IRET )				*
C*									*
C* Input parameters:							*
C*	BUFTBL		CHAR*		BUFR table file name		*
C*	CVNTBL		CHAR*		Conversion table file name	*
C*									*
C* Input and output parameters:						*
C*	NBP		INTEGER		Input:  Max # of parm strings	*
C*					Output: Actual number		*
C*	NGP		INTEGER		Input:  Max # of GEMPAK parms	*
C*					Output: Actual number		*
C*									*
C* Output parameters:							*
C*	BPARM  (NBP)	CHAR*80(*)	Strings of BUFR parm names	*
C*	IREPS  (NBP)	INTEGER		Replication #s for BPARM names  *
C*	GPARM  (NGP)	CHAR*(*)	GEMPAK parameter names		*
C*	LOGSCL (NGP)	INTEGER		LOG10 of GEMPAK scaling factor  *
C*	IOFSET (NGP)	INTEGER		GEMPAK data offset value	*
C*	NBITS  (NGP)	INTEGER		GEMPAK packing bit count	*
C*	CFAC   (NGP)	REAL		Conversion factor		*
C*	CTRM   (NGP)	REAL		Conversion term			*
C*	IRET		INTEGER		Return code			*
C*					 +1 = not all BUFR parms found	*
C*					  0 = normal return		*
C*					 -1 = Improper BUFR table file	*
C*					 -2 = No BUFR parameters	*
C*					 -3 = cannot open CVNTBL	*
C*					 -4 = improper CVNTBL file	*
C*					 -5 = invalid factor		*
C*					 -6 = invalid term		*
C*					 -7 = not enough BUFR parms	*
C*					 -8 = no valid GEMPAK packing	*
C*					 -9 = too many strings needed	*
C*					-10 = too many GEMPAK parms	*
C**									*
C* Log:									*
C* K. Brill/EMC		12/96						*
C* K. Brill/EMC		 2/97	No error for nbits > 32			*
C* K. Brill/EMC		 3/97	Take into account CFAC < 0		*
C* K. Brill/EMC		 7/98	Added code for \			*
C************************************************************************
	PARAMETER	( NMXBUF = 512 )
	PARAMETER	( IBKSLH = 92 )
C*
	CHARACTER*(*)	buftbl, cvntbl, gparm (*)
	CHARACTER*80	bparm (*)
	REAL		cfac (*), ctrm (*)
	INTEGER		logscl (*), iofset (*), nbits (*), ireps (*)
C*
	CHARACTER	sbuf*128, prmbuf*512, bcks*1
	CHARACTER*32	colum (4)
	CHARACTER*12	plist (128), lastpn
	LOGICAL		newgrp
C-----------------------------------------------------------------------
	iret = 0
	bcks = CHAR ( IBKSLH )
	maxnbp = nbp
	maxngp = ngp
C
C*	Open the input table file.
C
	CALL FL_SOPN ( cvntbl, luncvn, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -3
	    RETURN
	END IF
C
C*	Read past comments.
C
	CALL FL_TDAT ( luncvn, ier )
C
C*	Read the conversion file contents.
C
	irep = 0
	lastpn = ' '
	nbp = 0
	ngp = 0
	indx = 0
	indxpb = 1
	iostat = 0
	newgrp = .false.
	ipass = 0
	DO WHILE ( iostat .eq. 0 )
	    READ ( luncvn, '(A)', IOSTAT=iostat ) sbuf
	    iq = INDEX ( sbuf, '!' )
	    islash = INDEX ( sbuf, bcks )
	    IF ( islash .ne. 0 ) THEN
		newgrp = .true.
		ipass = 1
	    END IF
	    IF ( iostat .eq. 0 .and. iq .eq. 0 .and.
     +		 islash .eq. 0 ) THEN
	    	IF ( nbp .ge. maxnbp ) THEN
		    iret = -9
		    RETURN
	    	END IF
	    	IF ( ngp .ge. maxngp ) THEN
		    iret = -10
		    RETURN
	    	END IF
		CALL ST_CLST ( sbuf, ' ', ' ', 4, colum, num, ier )
		IF ( num .lt. 4 .or. ier .ne. 0 ) THEN
		    iret = -4
		    RETURN
		END IF
		CALL ST_LSTR ( colum (1), lng, ier )
		CALL ST_LSTR ( lastpn, lpn, ier )
		IF ( indx .eq. 0 ) THEN
		    lastpn = colum (1) (1:lng)
		    indx = 1
		ELSE IF ( lastpn (1:lpn) .eq. colum (1) (1:lng) .and.
     +			  irep .eq. 0 ) THEN
		    nbp = nbp + 1
		    indx = 1
		    bparm (nbp) = colum (1) (1:lng)
		    irep = 1
		ELSE IF ( lastpn (1:lpn) .ne. colum (1) (1:lng) ) THEN
		    IF ( irep .ne. 0 ) THEN
			ireps (nbp) = irep
		        irep = 0
		    ELSE IF ( indx + lng + 1 .ge. 80 .or. indx .eq. 1
     +			      .or. ( newgrp .and. ipass .eq. 2 ) ) THEN
			nbp = nbp + 1
			indx = lpn
			bparm (nbp) = lastpn (1:lpn)
			newgrp = .false.
		    ELSE
 			bparm (nbp) = bparm (nbp) (1:indx) // ' ' //
     +					lastpn (1:lpn)
			indx = indx + 1 + lpn
		    END IF
		    lastpn = colum (1) (1:lng)
		ELSE
		    irep = irep + 1
		END IF
		IF ( indxpb .eq. 1 ) THEN
		    prmbuf = colum (1) (1:lng)
		    indxpb = lng
	        ELSE IF ( indxpb + 1 + lng .le. NMXBUF ) THEN
		    prmbuf = prmbuf (1:indxpb) // ' ' //
     +				colum (1) (1:lng)
		    indxpb = indxpb + 1 + lng
		ELSE
		    iret = -10
		    RETURN
		END IF
		CALL ST_LSTR ( colum (2), lng, ier )
		ngp = ngp + 1
		gparm ( ngp ) = colum (2) (1:lng)
		CALL ST_CRNM ( colum (3), cfac (ngp), ier )
		IF ( ier .ne. 0 ) THEN
		    iret = -5
		    RETURN
		END IF
		CALL ST_CRNM ( colum (4), ctrm (ngp), ier )
		IF ( ier .ne. 0 ) THEN
		    iret = -6
		    RETURN
		END IF
		IF ( newgrp ) ipass = 2
	    END IF
	END DO
C
C*	Load last parameter onto 80-byte Jack record.
C
	IF ( irep .eq. 0 ) THEN
	    IF ( indx + lng + 1 .ge. 80 .or. indx .eq. 1
     +		 .or. newgrp ) THEN
		nbp = nbp + 1
		IF ( nbp .gt. maxnbp ) THEN
		    iret = -9
		    RETURN
		END IF
		bparm (nbp) = lastpn (1:lpn)
	    ELSE
		bparm (nbp) = bparm (nbp) (1:indx) // ' ' //
     +				lastpn (1:lpn)
	    END IF
	ELSE
	    ireps (nbp) = irep
	END IF

	CALL FL_CLOS ( luncvn, ier )
C
C*	Now open the BUFR table file and get the packing information
C*	for all the parms in  PRMBUF.
C
	CALL FL_SOPN ( buftbl, luntbl, ier )
C*
	CALL BG_RDPK ( luntbl, prmbuf, logscl, iofset, nbits, plist,
     + 		       np, iret )
	CALL FL_CLOS ( luntbl, ier )
	IF ( iret .ne. 0 ) RETURN
	IF ( np .ne. ngp ) THEN
	    iret = -7
	    RETURN
	END IF
C
C*	Compute GEMPAK packing parameters.
C
	DO ip = 1, ngp
	    ixbit = nbits (ip)
	    ref = FLOAT ( iofset (ip) )
	    scale = 10. ** logscl (ip)
	    bin = 2. ** nbits (ip) - 1.
	    gmin = ( ref / scale ) * cfac (ip) + ctrm (ip)
	    gmax = ( ( ref + bin ) / scale ) * cfac (ip) + ctrm (ip)
	    ires = -logscl (ip) + INT ( ALOG10 ( ABS ( cfac (ip) ) ) )
	    gres = 10. ** ires
	    IF ( gmin .gt. gmax ) THEN
		save = gmax
		gmax = gmin
		gmin = save
	    END IF
	    CALL DP_TERM ( gmin, gmax, gres, logscl (ip), iofset (ip),
     +			   nbits (ip), ier )
	    IF ( ier .ne. 0 .and. ixbit .lt. 32 ) THEN
		iret = -8
		RETURN
	    ELSE IF ( ier .ne. 0 ) THEN
		nbits (ip) = 32
	    END IF
	END DO
C*
	RETURN
	END
