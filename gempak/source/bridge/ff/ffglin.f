	SUBROUTINE FF_GLIN ( bultin, lenbul, orig, istart, numlin,
     +			     rpttim, iret) 
C************************************************************************
C* FF_GLIN								*
C*									*
C* This subroutine determines the indicies of the beginning and end of 	*
C* each line in the main body of a Flash Flood Guidance Bulletin.	*
C* It also will get the issuance time of the report.			*
C*									*
C* FF_GLIN ( BULTIN, LENBUL, ORIG, ISTART, NUMLIN, RPTTIM, IRET)        *
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		FFG Bulletin  			*
C*	LENBUL		INTEGER		Length of the bulletin		*
C*	ORIG		CHAR*		Originating station		*
C*									*
C* Output parameters:							*
C*	ISTART(*)	INTEGER		Indicies of line beginnings	*
C*	NUMLIN		INTEGER		Number of lines			*
C*	RPTTIM		CHAR*		Report time			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = no beginning/end 		*
C*					 +8 = no report time		*
C**									*
C* Log:									*
C* L. Sager/NCEP         5/96		Converted MM_GLIN for FFG    	*
C* Chiz/Unidata		 1/97		Fixed for buls with 4 CHCR /line*
C* K. Tyle/GSC		 3/97		Simplified do-while loop	*
C* K. Tyle/GSC		 5/97		Added report time		*
C* D. Kidwell/NCEP      12/97           Changed orig station check      *
C* D. Kidwell/NCEP      12/99           Allowed 4-digit year            *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bultin, orig, rpttim
	INTEGER		istart(*)
C*	
	CHARACTER	timstr*12
	LOGICAL		more
C-----------------------------------------------------------------------
	iret = 0
	numlin = 0
	rpttim = ' '
C
C*	Locate the beginning of each line between the .B and
C*	.END indicators.
C 
	ibeg = INDEX ( bultin(1:lenbul), '.B ' )
	IF ( ibeg .eq. 0 ) 
     +	    ibeg = INDEX ( bultin(1:lenbul), ':' )
	iend = INDEX ( bultin(ibeg:lenbul), '.END' )
	IF (( ibeg .eq. 0 ) .or. ( iend .eq. 0 )) THEN 
 	    iret = -7 
	    RETURN
	END IF	
	iend = iend + ibeg - 1
	more = .true.
C
C*	Write out the first line to the log.
C
	ilook = INDEX ( bultin (ibeg:iend),CHLF)
	IF ( ilook .ne. 0 ) 
     +	    CALL DC_WLOG ( 2, 'DCFFG', 2, bultin(ibeg:ibeg+ilook),ier)
C
C*	Search for the report time on the .B line.  This
C*	appears in all bulletins save KSTR/KSLR.
C
	itime  = INDEX ( bultin (ibeg:iend), '/DC' )
	IF ( itime .gt. 0 ) THEN
	    itime  = ibeg + itime + 2
C
C*	    Check for 4-digit year.
C
	    IF ( bultin (itime+10:itime+10) .ne. CHSPAC )
     +		 itime = itime + 2
	    timstr = bultin (itime:itime+9)
	    rpttim = timstr(:6) // '/' // timstr(7:)
	  ELSE IF ( ( orig .eq. 'KSTR' ) .or. ( orig .eq. 'KSLR' )) THEN
C
C*	    Translate the plain English report time to a GEMPAK time.
C
	    CALL FF_GTIM ( bultin, lenbul, rpttim, ier )
	    IF ( ier .ne. 0 ) iret = 8
	  ELSE 
	    iret = 8
	END IF
C
C*	Find the index that identifies the start of the line.
C

	DO WHILE ( more ) 
            ilook = INDEX ( bultin ( ibeg:iend ),CHLF) 
	    IF ( ilook .eq. 0 ) THEN
		more = .false.
	      ELSE
		numlin = numlin + 1
		istart ( numlin ) = ibeg + ilook
		ibeg = istart ( numlin )
		IF ( ibeg .ge. iend ) THEN
		    numlin = numlin - 1
		    more = .false.
		END IF
	    END IF
	END DO
C*
	RETURN
	END

