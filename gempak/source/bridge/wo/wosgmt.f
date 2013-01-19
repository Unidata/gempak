	SUBROUTINE WO_SGMT ( segment, lens, dattim, lcltim, strtim, 
     +			     stptim, cnties, ncnty, icancl, iret )
C************************************************************************
C* WO_SGMT 								*
C*									*
C* This subroutine decodes a single watch outline update segment.     	*
C*                                                                      *
C* WO_SGMT ( SEGMENT, LENS, DATTIM, LCLTIM, STRTIM, STPTIM, CNTIES,     *
C*           NCNTY, ICANCL, IRET )					*
C*									*
C* Input parameters:	                                                *
C*  	SEGMENT	  	CHAR* 	  	Bulletin segment		*
C*	LENS  	  	INTEGER	  	Length of segment		*
C*	DATTIM		CHAR*		Default start time, GEMPAK frmt	*
C*	LCLTIM		CHAR*		Local time string		*
C*									*
C* Output parameters:							*
C*	STRTIM	  	CHAR  	  	Report start time, GEMPAK frmt	*
C*	STPTIM	  	CHAR* 	  	Report stop time, GEMPAK frmt	*
C*	CNTIES(NCNTY)   CHAR*6	  	County names in watch area	*
C*	NCNTY 	  	INTEGER	  	Number of counties in area	*
C*	ICANCL 	  	INTEGER	  	Cancelation flag		*
C*	IRET  	  	INTEGER	  	Return code			*
C*				 	  2 = could not get start time  *
C*				  	  0 = normal return		*
C*				 	 -1 = no county info. found     *
C*				 	 -2 = could not get end time    *
C*				 	 -9 = could get start & end time*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 2/03	Remove blanks in county string		*
C* A. Hardy/NCEP	 7/03	Added decoding of cancel msgs   	*
C* A. Hardy/NCEP	 8/04	Fixed decode of cancel w/VTEC		*
C* F. J. Yen/NCEP	10/06	Checked return code for WO_CNTY; checked*
C*				for valid start/end time; fixed strtim;	*
C*				fixed subscript out of range for iend	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	segment, dattim, strtim, stptim, lcltim,
     +			cnties(*)
C
	CHARACTER	strbuf*700, carr(7)*4, purge*6, ttt*700,  
     +			c*1, sss*700
	INTEGER		idtarr (5), irdtar (5), iend (700)
	LOGICAL		done, good, ending
C------------------------------------------------------------------------
	iret  = 0
        ending = .false.
C
C*	Ensure that segment is upper case.
C
	CALL ST_LCUC ( segment, segment, ier )
C
C*	Look for county information string and decode it
C
	strbuf = segment ( :MIN(700, lens) )

        CALL ST_LSTR ( strbuf,  len, ier )
        CALL ST_UNPR ( strbuf, len, strbuf, len1, ier )
C
C*      Have the below in from ST_RMBL because of 
C*      160 char. string limit.
C
        ttt    = ' '
        length = 0
        sss = strbuf
        DO  ii = 1, len1
            c = sss (ii:ii)
            IF (( c .ne. CHSPAC ) .and. ( c .ne. CHTAB )) THEN
                length = length + 1
                ttt ( length : length ) = c
            END IF
        END DO
        strbuf = ttt
        lens = length
C
	done = .false.
	ix = 1
	good = .true.
        idx = INDEX ( strbuf, 'NOLONGER')  
	DO WHILE ( .not. done )
	    CALL ST_NOCC ( strbuf, '-', ix, iend(ix), ier )
	    IF (ix .eq. 1) THEN
                iendpr = 0
              ELSE
                iendpr = iend(ix-1)
            END IF
	    IF ( (ier .eq. 0) .and. ( iend(ix) - iendpr .eq. 7 ) )THEN
		inum = 1
		DO ii = 1, 6
		    CALL ST_ALNM ( strbuf(iend(ix)-ii:iend(ix)-ii),
     +				   kk, ier )
		    inum = inum * kk 
		END DO
	  	IF ( inum .eq. 1 ) THEN
		    icnt = iend (ix)
		    done = .true.
		  ELSE
		    ix = ix + 1
		    IF ( ix .ge. MIN (700, lens) ) THEN
			done = .true.
			good = .false.
		    END IF
		END IF
	      ELSE 
	  	ix = ix + 1
		IF ( ix .ge. MIN (700, lens) ) THEN
		    done = .true.
		    good = .false.
		END IF
	    END IF 
	END DO
C
C*	WOU cancel message.
C
        IF (idx .gt. 0 ) THEN
            ncnty  = 0
            icancl = 1
            ending = .true.
            good   = .false.
            purge  = dattim(5:6) // dattim(8:11)
        END IF
C
	IF ( good ) THEN
	    CALL WO_CNTY ( strbuf (:icnt), icnt, cnties, ncnty, purge,
     +			   ier)
	    IF ( ier .ne. 0 ) THEN
		iret = -1
		good = .false.
	    END IF
	  ELSE
	    iret = -1
	    good = .false.
	END IF
C
	IF ( ( good ) .or. ( ending ) ) THEN
            iret = 0
C*	    Convert the local time group and get the 
C*	    start time, if given.
C
       	    CALL ST_CLST ( lcltim, ' ', ' ', 7, carr, numb, ierr )
       	    CALL ST_LSTR ( carr(1), lens, ier )
C
            IF ( lens .eq. 3 ) THEN
       	        CALL ST_LSTR ( lcltim, lent, ier )
                lcltim = lcltim ( :lent)
            END IF
 	    CALL TI_LOCL (lcltim, strtim, ier) 
C
C*	    Bad local time conversion.
C
            IF ( ier .ne. 0 ) THEN
 	        strtim = dattim
		iret = 2
 	    END IF
C
C*          Compute the end time.
C
	    CALL TI_CTOI ( strtim, idtarr, ier1 )
	    CALL ST_INTG ( purge, ipurge, ier2 )
	    IF ( ( ier1 .eq. 0 ) .and. ( ier2 .eq. 0 ) ) THEN
	        iday = ipurge / 10000
	        ihour = ipurge /100 - iday * 100
	        imin = ipurge - iday * 10000 - ihour * 100
	        CALL DC_ITIM ( idtarr, iday, ihour, imin, irdtar, ier ) 
	        CALL TI_ITOC ( irdtar, stptim, ier )
		IF ( ier .ne. 0 ) THEN
		    IF ( iret .eq. 2 ) THEN
			iret = -9
		      ELSE
		        iret = -2
		    RETURN
		    END IF
		END IF
	      ELSE
		IF ( iret .eq. 2 ) THEN
		    iret = -9
		  ELSE
		    iret = -2
		END IF
		RETURN
	    END IF
        END IF
C*
	RETURN
	END
