       SUBROUTINE FF_DECD ( bultin, orig, istart, numlin, cntyid, ff01,
     +			    ff03, ff06, ff12, ff24, irec, iret )
C************************************************************************
C* FF_DECD                                                              *
C*                                                                      *
C* This routine decodes a single Flash Flood Guidance bulletin.		*
C*                                                                      *
C* FF_DECD  ( BULTIN, ORIG, ISTART, NUMLIN, CNTYID, FF01, FF03, FF06,	*
C*            FF12, FF24, IREC, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      BULTIN          CHAR*           Flash Flood Guidance Bulletin   *
C*      ORIG		CHAR*           Originating station	   	*
C*      ISTART (*)      INTEGER		Start of each line of bulletin  *
C*      NUMLIN		INTEGER		Number of lines in bulletin	*
C*                                                                      *
C* Output parameters:                                                   *
C*	CNTYID(*)	CHAR*		County ID			*
C*	FF01(*)		REAL		 1-hr FFG value			*
C*	FF03(*)		REAL		 3-hr FFG value 		*
C*	FF06(*)		REAL		 6-hr FFG value			*
C*	FF12(*)		REAL		12-hr FFG value			*
C*	FF24(*)		REAL		24-hr FFG value			*
C*	IREC		INTEGER		Number of county/zones returned *
C*      IRET            INTEGER         Return code                     *
C*					  0 = normal return		*
C*					 -8 = no stations decoded 	*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* G. Taylor/NCEP	 2/95						*
C* L. Sager/NCEP         5/96 		Converted for GEMPAK  	  	*
C* Chiz/Unidata		 1/97		Fixed for bulletin format	*
C* K. Tyle/GSC		 3/97		Generalized for all RFC's	*
C* K. Tyle/GSC		 4/97		Decode KSAC bulletins; check	*
C*					missing CNTYID in report; add	*
C*					orig to calling sequence	*
C* D. Kidwell           12/97           Changed orig station checks     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*(*)   bultin, orig, cntyid(*)
        REAL            ff01(*), ff03(*), ff06(*), ff12(*), ff24(*)
C*
	CHARACTER*128	tmpstr
        INTEGER         istart (*)
	REAL    	rarr (5)
	LOGICAL		alph
C-----------------------------------------------------------------------
	iret = 0
	DO k = 1, 5
	    rarr (k) = RMISSD
	END DO
C
C*	Loop through the lines of Flash Flood Guidance bulletins. 
C 
	irec = 0
C 
	DO  k = 1, numlin
	    ibeg = istart (k) 
	    iend = istart (k+1) - 1 
	    idif = iend - ibeg
	    tmpstr = bultin (ibeg:iend)
C
C*	    Check for a blank line and do not attempt to decode it.
C
	    CALL ST_UNPR  ( tmpstr, idif, tmpstr, lens, ier ) 
	    CALL ST_RMBL  ( tmpstr, tmpstr, lens, ier )
C
C*	    Decode the FFG values if present.
C 
	    IF ( ( bultin (ibeg:ibeg) .ne. ':' ) .and. 
     +		 ( lens .gt. 1 ) ) THEN
		nexp = 5
		icom = INDEX ( bultin (ibeg:iend),':' )
		ilf  = INDEX ( bultin (ibeg:iend),CHLF )
C
C*		Check for lines containing "COUNTIES":  ignore them.
C
		icnt = INDEX ( bultin (ibeg:iend),'COUNTIES')
C
C*		Check for alphabetical characters at the beginning
C*		of the line.  If missing, probably no CNTYID.
C
		alph = .false.
		DO ilet = 0,2
		    ipos = ibeg + ilet
		    CALL ST_ALNM ( bultin(ipos:ipos), ityp, ier2 )
		    IF ( ityp .eq. 2 ) alph = .true.
		END DO
		IF ( icom .gt. 0 ) THEN
C
C*		    All stations but KSTR have a ':' at the end of
C*		    the list of FFG values.
C
		    iend = ibeg + icom - 2
		  ELSE IF ( orig .eq. 'KSTR' ) THEN
C
C*		    KSTR bulletins:  data not terminated with a ':'.
C*		    Back up before the two CHCR's and the CHLF.
C*		    Ignore lines which only contain a line feed.
C
		    IF ( ilf .gt. 1 ) iend = iend - 3
		  ELSE IF ( orig. eq. 'KSLR' ) THEN
C
C*		    KSLR (AFOS) bulletins:  data not terminated with ':'.
C*		    Back up before the one CHCR and the CHLF.
C*		    Ignore lines which only contain a line feed.
C
		    IF ( ilf .gt. 1 ) iend = iend - 2
		END IF

		IF ( ( (ibeg+8) .lt. iend ) .and. ( ilf .gt. 1 ) .and. 
     +		     ( icnt .eq. 0 ) .and. (alph) ) THEN
		    CALL ST_RLST ( bultin ((ibeg+8):iend), '/', RMISSD,
     +				   nexp, rarr, inum, ier )
		    IF ( inum .le. 1 ) THEN
			IF ( ( inum .eq. 1 ) .and. ( orig .eq. 'KRSA' ))
     +			    THEN
C
C*			    KRSA bulletins contain only FF06 values,
C*			    which are in rarr(1).
C
			    rarr(3) = rarr(1)
			    rarr(1) = RMISSD
			  ELSE
			    CALL DC_WLOG ( 2, 'DCFFG', 7, 
     +					   bultin(ibeg:iend), ierr )
			END IF
		      ELSE
C
C*		    	Check number of values returned.  Some bulletins
C*			include an extra '/' at the end of the report.
C
		    	IF (( inum .ne. 3 ) .and. ( inum .ne. 5 ))
     +			    CALL DC_WLOG ( 2, 'DCFFG', 6, 
     +					   bultin(ibeg:iend), ierr )
		    END IF
C
		    IF ( ier .ne. 0 ) THEN
			CALL DC_WLOG ( 2, 'DCFFG', 7, 
     +				       bultin(ibeg:iend), ierr)
		      ELSE
C
C*			Store the county ID and FFG values.
C
			irec = irec + 1
			cntyid (irec) = bultin (ibeg:(ibeg+8))
			ff01 (irec) = rarr (1)
			ff03 (irec) = rarr (2)
			ff06 (irec) = rarr (3)   
			ff12 (irec) = rarr (4)
			ff24 (irec) = rarr (5)
		    END IF
		END IF
	    END IF
	END DO
	IF ( irec .eq. 0 ) iret = -8
C*
	RETURN
	END

