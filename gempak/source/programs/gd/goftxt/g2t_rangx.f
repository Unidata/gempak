	SUBROUTINE G2T_RANGX ( ktype, lunb, nt, itrnd, iret )
C************************************************************************
C* G2T_RANGX								*
C*									*
C* This subroutine adds wind/wave RANGE info from the trending buffer	*
C* to offshore text, OFFTXT.						*
C*									*
C* G2T_RANGX ( KTYPE, LUNB, NT, ITRND, IRET )				*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Grid parameter type		*
C*					 1 = wave			*
C*					 2 = wind			*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	NT		INTEGER		Nth time step			*
C*	ITRND		INTEGER		Index for trending buffer	*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/07						*
C* T. Lee/SAIC		11/07	Added NT for combining periods		*
C* T. Lee/SAIC		06/08	Use 1 FT or less for 0 FT wave		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER	ooo*72, outstr*72, str(2)*5
	LOGICAL		spread
C-----------------------------------------------------------------------
	iret = 0
C
	DO i = 1, 2
	    str ( i ) = ' '
	END DO
C
	min = mxn_d ( ktype, itrnd, 1 )
	max = mxn_d ( ktype, itrnd, 2 )
	CALL ST_INCH ( min, str ( 1 ), ier )
	CALL ST_INCH ( max, str ( 2 ), ier )
	IF  ( ktype .eq. 1 )  THEN
	    IF ( min .eq. 0 .or. max .eq. 0 )  THEN
		CALL G2T_GTEXT ( lunb, 54, str, ooo, ier )
	      ELSE
		CALL G2T_GTEXT ( lunb, 36, str, ooo, ier )
	    END IF
C
	  ELSE IF ( ktype .eq. 2 )  THEN
	    IF ( max .le. LGHTS )  THEN
		str ( 1 ) = ' '
		CALL ST_INCH ( LGHTS, str ( 2 ), ier )
		CALL G2T_GTEXT ( lunb, 8, str, ooo, ier )
	      ELSE
		IF  ( min .lt. 5 ) THEN
		    str ( 1 ) = ' '
		    CALL G2T_GTEXT ( lunb, 10, str, ooo, ier )
		  ELSE
		    CALL G2T_GTEXT ( lunb, 11, str, ooo, ier )
		END IF
		CALL ST_LSTR ( ooo, looo, ier )
C
		str ( 1 ) = wdir_d ( 1, itrnd )
		str ( 2 ) = wdir_d ( 2, itrnd )
C
C*		The following is the first attempt trying to resolve the
C*		wind spread.  If wind directions spread more than one 
C*		cardinal point, use the most prevalent wind direction 
C*		only.  Similar code can be found in the G2T_APPEX module.
C
		CALL G2T_WSPREAD ( str, spread, ier )
C
		CALL G2T_GTEXT ( lunb, 9, str, outstr, ier )
		CALL ST_LSTR ( outstr, lout, ier )
		ooo = outstr ( : lout ) // ooo ( : looo )
	    END IF
	END IF
	CALL ST_LSTR ( ooo, looo, ier )
	CALL ST_LSTR ( offtxt ( nt ), lout, ier )
	offtxt ( nt ) = offtxt ( nt ) ( : lout ) // ooo ( : looo )
C
C*      Add location if needed.
C
        CALL ST_LSTR ( id_d ( ktype, itrnd ), lid, ier )
        IF ( idflag .and. ktype .eq. 1 .and. lid .ne. 0 )  THEN
            CALL ST_LSTR ( offtxt ( nt ), loff, ier )
            offtxt ( nt ) = offtxt ( nt ) ( : loff - 1 ) // ' ' //
     +			    id_d ( ktype, itrnd ) ( : lid ) // '.'
        END IF
C*
	RETURN
	END
