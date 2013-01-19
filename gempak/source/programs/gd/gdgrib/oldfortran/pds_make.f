	SUBROUTINE PDS_MAKE  ( noptv, idoc, idngp, idosc, rnvblk,
     +			       gparm, oparm, gvcrd, ovcrd, level, olevl,
     +			       havbms, ipsc10, lasttm, flgdtm, gbtbls,
     +			       igpds, nbyts, cpds, idhm, iret )
C************************************************************************
C* PDS_MAKE								*
C*									*
C* This subroutine combines user input, grid header information, and	*
C* information from other section builders to make the full GRIB PDS.	*
C*									*
C* If an overide parameter is blank, then the corresponding GEMPAK	*
C* parameter is used to look up the GRIB number in the appropriate	*
C* GRIB table.  OPARM and OVCRD must translate into a numeric value.	*
C* OLEVL must represent a single numeric value or two values separated	*
C* by a colon.								*
C*									*
C* PDS_MAKE  ( NOPTV, IDOC, IDNGP, IDOSC, RNVBLK, 			*
C*	       GPARM, OPARM, GVCRD, OVCRD, LEVEL, OLEVL,		*
C*	       HAVBMS, IPSC10, LASTTM, FLGDTM, GBTBLS, IGPDS,		*
C*	       NBYTS, CPDS, IDHM, IRET )				*
C*									*
C* Input parameters:							*
C*	NOPTV		INTEGER		Number of parm table version	*
C*	IDOC		INTEGER		Identification of Center	*
C*	IDNGP		INTEGER		ID number of generating process *
C*	IDOSC		INTEGER		ID number of sub center		*
C*	RNVBLK (*)	REAL		Grid navigation block		*
C*	GPARM		CHAR*		GEMPAK parameter name string	*
C*	OPARM		CHAR*		User overide GRIB parameter #	*
C*	GVCRD		CHAR*		GEMPAK VCORD name string	*
C*	OVCRD		CHAR*		User overide GRIB ver cord #	*
C*	LEVEL (2)	INTEGER		GEMPAK vert level values	*
C*	OLEVL 		CHAR*		User overide of level values	*
C*	HAVBMS		LOGICAL		Flag for existence of a BMS	*
C*	IPSC10		INTEGER		Power of 10 scale factor	*
C*	LASTTM		CHAR*		Last grid time			*
C*	FLGDTM (2)	CHAR*20		Complete single GEMPAK time	*
C*	GBTBLS		CHAR*		User input for GRIB table files *
C*	IGPDS		INTEGER		Grid navigation # from CPYFIL	*
C*									*
C* Input and output parameter:						*
C*	NBYTS		INTEGER		Input:  maximum # of bytes for	*
C*						the PDS			*
C*					Output:  actual # of bytes in	*
C*						 the PDS		*
C*									*
C* Output parameters:							*
C*	CPDS (NBYTS)	CHAR*1		GRIB PDS octets			*
C*	IDHM (3)	INTEGER		Day, hour, minute for WMO hdr	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-92 = PDS allocation too small	*
C*					-93 = IPSC10 too large		*
C*					-95 = CPYFIL grid # mismatch	*
C*					-96 = CPYFIL # not valid for PDS*
C**									*
C* Log:									*
C* K. Brill/HPC		 8/99						*
C* K. Brill/HPC		 2/00	Added IGPDS; return if its > 255;	*
C*				Allow override names for OPARM & OVCRD  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rnvblk (*)
	INTEGER		level (2), idhm (3)
	CHARACTER*(*)	gparm, oparm, gvcrd, ovcrd, olevl, gbtbls,
     +			lasttm, flgdtm (*)
	CHARACTER*1	cpds (*)
	LOGICAL		havbms
C*
	CHARACTER*128	tbls (4)
	CHARACTER*4	cvrnm
	INTEGER		lvls (2), i13_25 (13), ibyts (2)
C------------------------------------------------------------------------
	iret = 0
	IF ( igpds .gt. 255 .or. igpds .le. 0 ) THEN
	    iret = -96
	    RETURN
	END IF
C
C*	Get the GRIB table file names from gbtbls.
C
	CALL ST_INCH ( noptv, cvrnm, ier )
	CALL ST_LSTR ( cvrnm, lng, ier )
	CALL ST_CLST ( gbtbls, ';', '?', 4, tbls, num, iret )
	IF ( tbls (1) .eq. '?' ) THEN
	    tbls (1) = 'wmogrib' // cvrnm (1:lng) // '.tbl'
	END IF
	IF ( tbls (2) .eq. '?' ) THEN
	    tbls (2) = 'ncepgrib' // cvrnm (1:lng) // '.tbl'
	END IF
	IF ( tbls (3) .eq. '?' ) THEN
	    tbls (3) = 'vcrdgrib1.tbl'
	END IF
	IF ( tbls (4) .eq. '?' ) THEN
	    tbls (4) = ' '
	END IF
		    
C
C*	PDS length = 28.
C
	IF ( nbyts .lt. 28 ) THEN
	    iret = -92
	    RETURN
	ELSE
	    nbyts = 28
	END IF
	cpds (1) = CHAR (0)
	cpds (2) = CHAR (0)
	cpds (3) = CHAR (28)
C
C*	Set the fixed identifiers.
C
	cpds (4) = CHAR (noptv)
	cpds (5) = CHAR (idoc)
	cpds (6) = CHAR (idngp)	
C
C*	Set the grid number.
C
	CALL PDS_BYT7  ( rnvblk, 16, cpds (7), ibyt7, iret )
	IF ( ibyt7 .ne. igpds ) THEN
	    IF ( igpds .ne. 255 ) THEN
		iret = -95
	        CALL ER_WMSG ( 'GDGRIB', iret, ' ', ier )
		RETURN
	    ELSE
		ibyt7 = 255
		cpds (7) = CHAR (ibyt7)
	    END IF
	END IF
	IF ( iret .ne. 0 ) THEN
C
C*	    Write a warning.  GDS will be made anyway.
C
	    CALL ER_WMSG ( 'GDGRIB', iret, ' ', ier )
	    iret = 0
	END IF
C
C*	Set the GDS (always present) and BMS flags.
C
	ibyt8 = 128
	IF ( havbms ) ibyt8 = ibyt8 +  64
	cpds (8) = CHAR (ibyt8)
C
C*	Check for override parameter number.
C	
	idt = 0
	IF ( oparm .ne. ' ' ) THEN
	    CALL ST_NUMB ( oparm, ibyt9, ier )
	    IF ( ier .eq. 0 .and. ibyt9 .gt. 0 .and.
     +		 ibyt9 .lt. 255 ) THEN
		cpds (9) = CHAR ( ibyt9 )
	    ELSE
		CALL PDS_BYT9 ( oparm, tbls (1), tbls (2),
     +				cpds (9), ibyt9, idt, iret )
		IF ( iret .ne. 0 ) THEN
		    WRITE (6,*) ' User parm input is invalid---',
     +			        ' trying GRIB table.'
		    CALL PDS_BYT9 ( gparm, tbls (1), tbls (2),
     +				    cpds (9), ibyt9, idt, iret )
		END IF
	    END IF
	ELSE
	    CALL PDS_BYT9 ( gparm, tbls (1), tbls (2),
     +			    cpds (9), ibyt9, idt, iret )
	END IF
	IF ( iret .ne. 0 ) RETURN
C
C*	Check for override vertical coordinate number.
C
	IF ( ovcrd .ne. ' ' ) THEN
	    CALL ST_NUMB ( ovcrd, ibyt10, ier )
	    IF ( ier .eq. 0 .and. ibyt10 .gt. 0 .and.
     +		 ibyt10 .lt. 255 ) THEN
		cpds (10) = CHAR ( ibyt10 )
		ivscal = 0
	    ELSE
		CALL PDS_BY10 ( ovcrd, tbls (3), ' ',
     +				cpds (10), ibyt10, ivscal, iret )
		IF ( iret .ne. 0 ) THEN
		    WRITE (6,*)
     +		    ' User vertical coordinate input is invalid---',
     +		    ' trying GRIB table.'
		    CALL PDS_BY10 ( gvcrd, tbls (3), ' ',
     +				    cpds (10), ibyt10, ivscal, iret )
		END IF
	    END IF
	ELSE
	    CALL PDS_BY10 ( gvcrd, tbls (3), ' ',
     +			    cpds (10), ibyt10, ivscal, iret )
	END IF
	IF ( iret .ne. 0 ) RETURN
C
C*	Check for override for vertical coordinate values.
C*	Fill bytes 11-12.  Adjust byte 10 for layers.
C
	IF ( olevl .ne. ' ' ) THEN
	    CALL ST_ILST ( olevl, ':', -1, 2, lvls, num, ier )
	    IF ( ier .ne. 0 ) THEN
		lvls (1) = level (1)
		lvls (2) = level (2)
	    END IF
	ELSE
	    lvls (1) = level (1)
	    lvls (2) = level (2)
	END IF
	CALL PDS_BY11  ( lvls, ivscal, cpds (10), ibyt10,
     +		          cpds (11), ibyt11,
     +		          cpds (12), ibyt12, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Process time information to fill bytes 13-25.
C
	CALL PDS_VLDT  ( flgdtm, lasttm, idt,
     +			 cpds (13), i13_25, iret )
	IF ( iret .ne. 0 ) RETURN
	idhm (1) = i13_25 (3)
	idhm (2) = i13_25 (4)
	idhm (3) = i13_25 (5)

C
C*	Set the subcenter number.
C
	cpds (26) = CHAR ( idosc )
C
C*	Store the decimal scale factor.
C
	ii = 26
	nb = 2
	CALL GDIGIT ( ipsc10, 256, nb, ibyts, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -93
	    RETURN
	END IF
	IF ( ipsc10 .lt. 0 ) ibyts (2) = ibyts (2) + 128
	DO i = 2, 1, -1
	    ii = ii + 1
	    cpds (ii) = CHAR ( ibyts (i) )
	END DO
C*
	RETURN
	END
