	SUBROUTINE G2T_WRN2 ( ntype, dayw, warn, iostr, iret )
C************************************************************************
C* G2T_WRN2								*
C*									*
C* This subroutine appends headline message from the 5th period and	*
C* thereafter for OFF text.						*
C*									*
C* G2T_WRN2 ( NTYPE, DAYW, WARN, IOSTR, IRET )				*
C*									*
C* Input parameters:							*
C*	NTYPE		INTEGER		Storm type			*
C*					  1: Hurricane			*
C*					  2. Storm			*
C*					  3. Gale			*
C*	DAYW(*)		CHAR*		Day of the week			*
C*	WARN		CHAR*		Warning message			*
C*									*
C* Iput and output parameters:						*
C*	IOSTR		CHAR*		I/P and O/P headline string	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		12/06						*
C* T. Lee/SAIC		 2/06	Added THROUGH versus INTO text		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	warn, iostr,  dayw(*)
	LOGICAL		wflag(ngrdtm)
	LOGICAL		contin, done, first
	INTEGER		isped(3)
	CHARACTER	ooo*128, dot*3, and*5, sp*1
	CHARACTER	trend*9, thru*9, into*6
	DATA		dot / '...' /, and /' AND '/, sp / ' ' /,
     +			thru /' THROUGH '/, into /' INTO '/
	DATA		isped / 65, 50, 35 /
C-----------------------------------------------------------------------
	iret = 0
C
C*	Fill appropriate storm flags.
C
	IF  ( ntype .eq. 1 )  THEN
	    DO ii = 1, ngrdtm
		wflag ( ii ) = fhurr ( ii )
	    END DO
	  ELSE IF ( ntype .eq. 2 )  THEN
	    DO ii = 1, ngrdtm
		wflag ( ii ) = fstorm ( ii )
	    END DO
	  ELSE IF ( ntype .eq. 3 )  THEN
	    DO ii = 1, ngrdtm
		wflag ( ii ) = fgale ( ii )
	    END DO
	END IF
C
C*	Append the 5th period and beyond if necessary.		
C
	CALL ST_LSTR ( warn, iw, ier )
	nt = 9
	done = .false.
	first = .true.
	DO  WHILE ( .not. done )
	    IF ( wflag ( nt ) )  THEN
		contin = .true.
		kk = 0
		DO WHILE ( contin )
		    kk = kk  + 1
		    IF  ( kk .eq. 1 )  THEN
			is = nt
		      ELSE
			ie = nt
		    END IF
		    nt = nt + 2
		    IF  ( nt .lt. ngrdtm )  THEN
			contin = wflag ( nt )
		      ELSE
			contin  = .false.
		    END IF
		END DO
C
		IF  ( kk .eq. 1 )  THEN
		    CALL ST_LSTR ( dayw ( is ), nis, ier )
		    IF ( iostr .eq. ' ' )  THEN
			iostr = warn ( : iw ) // sp
     +				// dayw ( is ) ( : nis ) // dot
			first = .false.
		      ELSE
			ooo = iostr
			CALL ST_LSTR ( ooo, lo, ier )
			IF ( first )  THEN
			    iostr = ooo ( : lo - 3 ) // and
     +                              // warn ( 4 : iw ) // sp
     +				    // dayw ( is ) ( : nis ) // dot
			    first = .false.
			  ELSE
			    iostr  = ooo ( : lo - 3 ) // and
     +                               //  dayw ( is ) ( : nis ) // dot
			END IF
		    END IF
		  ELSE IF ( kk .ge. 2 )  THEN
C
C*		    THROUGH versus INTO.
C
		    IF ( ie .lt. ngrdtm )  THEN
			kt = ie + 2
		      ELSE
			kt = ie
		    END IF
C
		    IF ( mxval_s (2,1,kt) .lt. isped ( ntype ) )  THEN
			trend = into
		      ELSE
			trend = thru
		    END IF
		    CALL ST_LSTR ( trend, it, ier )	
C
		    CALL ST_LSTR ( dayw ( is ), nis, ier )
		    CALL ST_LSTR ( dayw ( ie ), nie, ier )
		    IF ( iostr  .eq. ' ' )  THEN
			iostr  = warn ( : iw ) // sp
     +				 // dayw ( is ) ( : nis ) 
     +				 // trend ( : it + 1 )
     +				 // dayw ( ie ) ( : nie ) // dot
			first = .false.
		      ELSE
			ooo = iostr
			CALL ST_LSTR ( ooo, lo, ier )
			IF  ( first )  THEN
			    iostr = ooo ( : lo - 3 ) // and
     +                              //  warn ( 4 : iw ) // sp
     +				    // dayw ( is ) ( : nis )  
     +				    // trend ( : it + 1 )
     +				    // dayw ( ie ) ( : nie ) // dot
			    first = .false.
			  ELSE
			    iostr = ooo ( : lo - 3 ) // and
     +				     // dayw ( is ) ( : nis )  
     +				     // trend ( : it + 1 )
     +				     // dayw ( ie ) ( : nie ) // dot
			END IF
		    END IF
		END IF
	    END IF
	    nt = nt + 2
	    IF ( nt .ge. ngrdtm )  done = .true.
	END DO
C*
	RETURN
	END
