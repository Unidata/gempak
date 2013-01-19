	SUBROUTINE G2T_OUTPUT ( lunt, lunb, nz, iret )
C************************************************************************
C* G2T_OUTPUT								*
C*									*
C* This subroutine creates stepwise G2T offshore text.			*
C*									*
C* G2T_OUTPUT ( LUNT, LUNB, NZ, IRET )					*
C*									*
C* Input parameters:							*
C*	LUNT		INTEGER		LUN for offshore text file	*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C* 	NZ		INTEGER		Nth zone area			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		10/06	Created					*
C* T. Lee/SAIC		11/07	Combining periods			*
C* T. Lee/SAIC		12/07	Set nthzon in goftxt.cmn		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER	ooo*256, ftime*40
	CHARACTER	eol*2, spac*2
	CHARACTER	dayw(ngrdtm)*20
	LOGICAL		proces, done
C------------------------------------------------------------------------
	iret = 0
	nthzon = nz
C
C*	Get days of the week of the forecast period.
C
	CALL G2T_DAYW ( dayw, ier )
C
C*	Create gale/storm warning.
C
	CALL G2T_HEADLN ( 1, dayw, lunt, iret )
C
C*	Perform statistical analysis of the data.
C
	DO kt = 1, ngrdtm
	    CALL G2T_STAT ( kt, nz, ier )
	    CALL G2T_EXCEPT ( 1, kt, nz, ier )
	    CALL G2T_EXCEPT ( 2, kt, nz, ier )
	END DO
C
C*	Create text.
C
	nt = 1
	DO WHILE ( nt .lt. ngrdtm )  
	    nt1 = nt + 1
	    nt2 = nt + 2
C
C*	    Add days of the week.
C
	    ooo = dayw ( nt )
	    CALL ST_LSTR ( ooo, looo, ier )	
	    offtxt ( nt ) = '.' // ooo (:looo) // '...'
C
C*	    Statistical analysis of the grids.
C
	    IF ( ier .eq. 0 )  proces = .true.
C
C*	    Add wind information. If wind speed less than or equal to 10
C*	    ->  VARIABLE WINDS 10 KT OR LESS.
C
	    IF ( proces )  THEN
C
C*		Add wind trending.
C
		CALL G2T_DSDT ( lunb, nt, iret )
C
C*		Add wave trending.
C*		Single wave height, str (2) = ' '.
C
		CALL G2T_DWDT ( lunb, nt, iret )
		IF ( iret .ne. 0 )  THEN
		    CALL ER_WMSG ( 'GOFTXT', iret,  ' ', ier )
		END IF
	    END IF
	    nt = nt + 2
	END DO
C
C*	Add line feeds and send text to a file.
C
	nt = 1
	eol = CHLF // CHNULL
	spac = ' ' // CHNULL
	DO WHILE ( nt .lt. ngrdtm )
	    istart = nt
	    ipos = INDEX ( offtxt ( nt ), '...' )
	    CALL ST_LSTR ( offtxt ( nt ), jpos, ier )
	    nt = nt + 2
	    IF ( nt .lt. ngrdtm )  THEN
		kpos = INDEX ( offtxt ( nt ), '...' )
	    	CALL ST_LSTR ( offtxt ( nt ), lpos, ier )
		proces = .true.
	      ELSE
		proces = .false.
	    END IF
C
C*	    Combine periods.
C
	    IF ( proces )  THEN
		IF ( offtxt ( istart  ) ( ipos : jpos ) .eq. 
     +		     offtxt ( nt ) ( kpos : lpos ) )  THEN
		    icount = 2
		    done = .false.
		    iend  = nt
		    DO WHILE ( nt .lt. ngrdtm .and. .not. done ) 
			nt = nt + 2
			IF ( nt .lt. ngrdtm )  THEN
			    kpos = INDEX ( offtxt ( nt ), '...' )
	    		    CALL ST_LSTR ( offtxt ( nt ), lpos, ier )
			    IF ( offtxt ( istart ) ( ipos : jpos ) .eq. 
     +				 offtxt ( nt ) ( kpos : lpos ) )  THEN
				icount = icount + 1
				iend  = nt
			      ELSE
				done = .true.
			    END IF
			  ELSE
			    done = .true.
			END IF
		    END DO
		    CALL ST_NULL ( offtxt ( istart ), offtxt ( istart ),
     +				   len, ier )
		    CALL ST_LSTR ( dayw ( istart ), list, ier )
		    CALL ST_LSTR ( dayw ( iend ), lien, ier )
		    IF ( icount .le. 2 )  THEN
			ftime = '.' // dayw ( istart ) ( : list ) // 
     +				' AND ' //
     +				dayw ( iend ) ( : lien )
		      ELSE
			ftime = '.' // dayw ( istart ) ( : list ) //
     +				' THROUGH ' // dayw ( iend ) ( : lien )
		    END IF
C
		    CALL ST_LSTR ( ftime, lftime, ier )
		    offtxt ( istart ) = ftime ( : lftime ) // 
     +					offtxt ( istart ) ( ipos : )
		    CALL ST_LSTR  ( offtxt ( istart ), len, ier )
		    IF  ( len .gt. MXCHAR )  THEN
			CALL CST_WRAP ( offtxt ( istart ), spac, MXCHAR,
     +					eol, CHNULL, offtxt ( istart ), 
     +					ier )
		    END IF
		    WRITE ( lunt, 10 ) offtxt ( istart ) ( : len )
		  ELSE
		    CALL ST_NULL ( offtxt ( istart ), offtxt ( istart ),
     +				   len, ier )
		    CALL ST_LSTR  ( offtxt ( istart ), len, ier )
		    IF  ( len .gt. MXCHAR )  THEN
			CALL CST_WRAP ( offtxt ( istart ), spac, MXCHAR,
     +				        eol, CHNULL, offtxt ( istart ), 
     +					ier )
		    END IF
		    WRITE ( lunt, 10 ) offtxt ( istart ) ( : len )
		END IF
	      ELSE
		CALL ST_NULL ( offtxt ( istart ), offtxt ( istart ),
     +			       len, ier )
		CALL ST_LSTR  ( offtxt ( istart ), len, ier )
		IF  ( len .gt. MXCHAR )  THEN
		    CALL CST_WRAP ( offtxt ( istart ), spac, MXCHAR,
     +				    eol, CHNULL, offtxt ( istart ), 
     +				    ier )
		END IF
		WRITE ( lunt, 10 ) offtxt ( istart ) ( : len )
	    END IF
	END DO
10	FORMAT ( A )
C*
	RETURN
	END
