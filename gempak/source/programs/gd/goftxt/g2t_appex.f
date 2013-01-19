	SUBROUTINE G2T_APPEX ( ktype, lunb, nt, itrnd, end, iret )
C************************************************************************
C* G2T_APPEX								*
C*									*
C* This subroutine appends the EXCEPT portion of the wind/wave text 	*
C* after the RANGE text.  For wind text, wind directions will not 	*
C* repeat themselves (DIRFLG = .FALSE.) if they are same as the RANGE	*
C* part.  Likewise, wind speed will not repeat itself (SPDFLG = .FALSE.)*
C* if it is the same as the RANGE part.					*
C*									*
C* G2T_APPEX ( KTYPE, LUNB, NT, ITRND, END, IRET )			*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Grid parameter type		*
C*					 1 = wave			*
C*					 2 = wind			*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	NT		INTEGER		Nth time step			*
C*	ITRND		INTEGER		Index for trending block	*
C*	END		INTEGER		Appending text, 'EARLY'/'LATE'	*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/07						*
C* T. Lee/SAIC		08/07	Removed "OVER" before "WITHIN"		*
C* T. Lee/SAIC		11/07	Added NT for combining period		*
C* T. Lee/SAIC		11/07	Added LGHTS wind and PORTION for TPC	*
C* T. Lee/SAIC		12/07	Used "PORTION" flag, pflag_d		*
C* T. Lee/SAIC		06/08	Use 1 FT or less for 0 FT wave		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	end
	CHARACTER	ooo*72, woo*72, str(2)*5, id*(MAXIDS)
	CHARACTER	wd_r(2)*2, wd_e(2)*2
	LOGICAL		dirflg, spdflg, spread
	INCLUDE		'EQUAL.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Return if nothing for EXCEPTION.
C
	IF ( .not. eflag_d ( ktype, itrnd ) )  RETURN
C
	mine = mxn_de ( ktype, itrnd, 1 )
	maxe = mxn_de ( ktype, itrnd, 2 )
C
	CALL ST_INCH ( mine, str ( 1 ), ier )
	CALL ST_INCH ( maxe, str ( 2 ), ier )
C
	IF  ( ktype .eq. 1 )  THEN
	    IF ( INDEX ( end, 'THROUGH ' ) .ne. 0 )  THEN
		IF ( mine .eq. 0 .or. maxe .eq. 0 )  THEN
		    CALL G2T_GTEXT ( lunb, 52, str, ooo, ier )
		  ELSE
		    CALL G2T_GTEXT ( lunb, 5, str, ooo, ier )
		END IF
	      ELSE
		IF ( mine .eq. 0 .or. maxe .eq. 0 )  THEN
		    CALL G2T_GTEXT ( lunb, 55, str, ooo, ier )
		  ELSE
		    CALL G2T_GTEXT ( lunb, 37, str, ooo, ier )
		END IF
	    END IF
	  ELSE 
	    IF ( maxe .le. LGHTS )  THEN
		str ( 1 ) = ' '	
		CALL ST_INCH ( LGHTS, str ( 2 ), ier )
		CALL G2T_GTEXT ( lunb, 24, str, ooo, ier )
	      ELSE
		min = mxn_d ( ktype, itrnd, 1 )
		max = mxn_d ( ktype, itrnd, 2 )
		wd_r ( 1 ) =  wdir_d ( 1, itrnd )
		wd_r ( 2 ) =  wdir_d ( 2, itrnd )
		wd_e ( 1 ) =  wdir_de ( 1, itrnd )
		wd_e ( 2 ) =  wdir_de ( 2, itrnd )
		spdflg = ( max .ne. maxe ) .or. ( min .ne. mine )
C
		dirflg =  ( mxn_d ( ktype, itrnd, 2 ) .le. LGHTS ) .or.
     +			  ( CNE ( wd_r ( 1 ), wd_e ( 1 ) ) .and.
     +		 	    CNE ( wd_r ( 1 ), wd_e ( 2 ) ) ) .or.
     +			  ( CNE ( wd_r ( 2 ), wd_e ( 1 ) ) .and.
     +			    CNE ( wd_r ( 2 ), wd_e ( 2 ) ) )
C
C*		The following is the first attempt trying to resolve the
C*		wind spread issues.  If wind directions spread more 
C*		than one cardinal point, use the most prevalent wind 
C*		direction only.  Similar code can be found in the 
C*		G2T_AWSTXT module.
C
		CALL G2T_WSPREAD ( wd_e, spread, ier )
C
C*		If wind directions are the same as the "Range" part, no
C*		repeat except when the "Range" spells "VARIABLE WINDS".
C
		IF  ( dirflg )  THEN
		    CALL G2T_GTEXT ( lunb, 14, wd_e, woo, ier )
		    CALL ST_LSTR ( woo, iwoo, ier )
C
C*		    If wind speed is same, show directions only.
C
		    IF  ( spdflg )  THEN
			CALL G2T_GTEXT ( lunb, 15, str, ooo, ier )
		      ELSE
			ooo = '.'
		    END IF
		    ooo = woo ( : iwoo ) // ooo
	          ELSE
		    CALL G2T_GTEXT ( lunb, 13, str, ooo, ier )
		END IF
	    END IF
	END IF
	CALL ST_LSTR ( ooo, looo, ier )
	CALL ST_LSTR ( end, len, ier )
	IF ( len .gt. 0 )  looo = looo - 1
	CALL ST_LSTR ( offtxt ( nt ), lout, ier )
	id = id_d ( ktype, itrnd )
	CALL ST_LSTR ( id, lid, ier )
	ipos = INDEX ( ooo, '!AS' )
C
C*	Add "PORTION" if needed.
C
	IF ( pflag_d ( ktype, itrnd ) )  THEN
	    offtxt ( nt ) = offtxt ( nt ) ( : lout - 1 ) // 
     +			    ooo ( : ipos - 1 ) // id  ( : lid ) // 
     +			    ' PORTION' // ooo ( ipos + 3 : looo ) // 
     +			    end ( : len ) 
	  ELSE
	    offtxt ( nt ) = offtxt ( nt ) ( : lout - 1 ) // 
     +			    ooo ( : ipos - 1 ) // id  ( : lid ) // 
     +			    ooo ( ipos + 3 : looo ) // end ( : len )
	END IF
C*
	RETURN
	END
