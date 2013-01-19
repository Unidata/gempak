        SUBROUTINE NAMPRP ( bufrfl, lunbuf, tblde, nde, degree, iret )
C************************************************************************
C* NAMPRP								*
C*									*
C* This subroutine prepares for accessing a MODEL BUFR file and its	*
C* associated table stored in the first BUFR message.			*
C*									*
C*									*
C* If this is an old BUFR file, the SLAT, SLON, were stored as radians	*
C* in the BUFR file.  In that case, the values in the table file will	*
C* be scaled by five and have the reference -158000 for latitude and	*
C* -315000 for longitude.  For this case, DEGREE must be set to FALSE.	*
C*									*
C* The TBLDE array contains the list of table D entries in the order	*
C* of occurrence in the file.						*
C*									*
C* NAMPRP  ( BUFRFL, LUNBUF, TBLDE, NDE, DEGREE, IRET )			*
C*									*
C* Input parameters:							*
C*	BUFRFL		CHAR*		BUFR file name			*
C*									*
C* Output parameters:							*
C*	LUNBUF		INTEGER		Unit number of the BUFR file	*
C*	TBLDE (NDE)	CHAR*		TABLE D entries from BUFR table	*
C*	NDE		INTEGER		# of TABLE D entries		*
C*	DEGREE		LOGICAL		Flag for lat/lon in degrees	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -1 = error opening bufr table  *
C*					 -2 = no LAT in HEADR sequence  *
C*					 -3 = sequence name not found   *
C*					 -4 = error opening bufr file   *
C*					-34 = error getting LUNBUF	*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	8/98						*
C* D. Kidwell/NCEP	9/98	Added calls to ER_WMSG                  *
C* K. Brill/EMC		9/98	CALL JTB_ instead of BTB_ routines	*
C* D. Kidwell/NCEP     12/98	SNMPRP -> NAMPRP, SNMODL -> NAMSND      *
C************************************************************************
	PARAMETER	( MXTBLA=1 )
	CHARACTER*(*)	bufrfl, tblde (*)
	LOGICAL		degree
C*
	CHARACTER*8	tblas (MXTBLA), seqs (80), pnams (80), pseq
	CHARACTER*40	descr (MXTBLA)
	CHARACTER*12	unit
	LOGICAL		found, header
C-----------------------------------------------------------------------
	iret = 0
C*
	CALL FL_GLUN ( lunbuf, ier )
	IF ( ier .ne. 0 ) THEN
     	    CALL ER_WMSG ( 'FL', ier, ' ', ire )
	    iret = -34
	    RETURN
	END IF
C
C*	CALL JB_OPEN to open the BUFR file.
C
  	CALL JB_OPEN ( bufrfl, lunbuf, ' ', ier )
	IF ( ier .ne. 0 ) THEN
	    ier = ier - 50
     	    CALL ER_WMSG ( 'NAMSND', ier, bufrfl, iret )
	    iret = -4
	END IF
C*
	np = MXTBLA
C
C*	Open the BUFR table file and get all TABLE A entries.
C
  	CALL JTB_INIT ( lunbuf, np, tblas, ier )
	IF ( ier .ne. 0 ) THEN
	    ier = ier - 40
 	    CALL ER_WMSG ( 'NAMSND', ier, ' ', iret )
	    iret = -1
	    RETURN
	END IF
C
C*	Get the TABLE D names for the TABLE A entry - there should
C*	only be one TABLE A entry.
C
	nseq = 1
	seqs ( 1 ) = tblas ( 1 )
  	CALL JTB_SPLT ( seqs, nseq, pseq, pnams, np, iret )
	IF ( iret .ne. 0 ) THEN
	    iermsg = iret - 40
 	    CALL ER_WMSG ( 'NAMSND', iermsg, pseq, ier )
	    RETURN
	END IF
	IF ( np .eq. 0 ) THEN
	    nde = nseq
	    DO i = 1, nseq
		tblde ( i ) = seqs ( i )
	    END DO
	END IF
C
	found  = .false.
	header = .false.
	DO WHILE ( .not. found )
	    CALL JTB_SPLT ( seqs, nseq, pseq, pnams, np, iret )
	    IF ( iret .ne. 0 ) THEN
		iermsg = iret - 40
 	        CALL ER_WMSG ( 'NAMSND', iermsg, pseq, ier )
	        RETURN
	    END IF
	    IF ( pseq (1:5) .eq. 'HEADR' ) THEN
		found  = .true.
		header = .true.
	      ELSE IF ( nseq .eq. 0 ) THEN
		found = .true.
	    END IF
	END DO
C
C*	Search for a parameter containing LAT in the list of HEADR
C*	parameters, and use its scale value to determine whether
C*	degrees or radians.
C
	found = .false.
	IF ( header ) THEN
	    indx = 1
	    DO WHILE ( indx .le. np )
	        IF ( pnams ( indx ) ( 2:4 ) .eq. 'LAT' ) THEN
		    indx = np + 1
		    found = .true.
	            CALL JTB_QPRM ( pnams ( indx ), 1, descr, iscl, 
     +			            iref, ibit, unit, iret )
		    IF ( iscl .eq. 5 ) THEN
		        degree = .false.
		      ELSE
		        degree = .true.
		    END IF
	          ELSE
		    indx = indx + 1
	        END IF
	    END DO
	END IF
	IF ( .not. found ) THEN
 	    CALL ER_WMSG ( 'NAMSND', -50, 'LAT', iret )
	    iret = -2
	    RETURN
	END IF
C*
	RETURN
	END
