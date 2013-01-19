	SUBROUTINE G2T_CASE8 ( ktype, lunb, nt, index, iret )
C************************************************************************
C* G2T_CASE8								*
C*									*
C* This subroutine creates G2T trending for Case 8, Ra >> Rb << Rc.	*
C*									*
C* G2T_CASE8 ( KTYPE, LUNB, NT, INDEX, IRET )				*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type			*
C*					 1 = Wave			*
C*					 2 = Wind			*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	NT		INTEGER		Nth time step			*
C*	INDEX		INTEGER		Index for exception		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C* T. Lee/SAIC		11/07	Added NT for combining periods		*
C* T. Lee/SAC		06/08	Use 1 FT or less for 0 FT wave		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER	ooo*256, str(2)*5, end*50
	LOGICAL		E1, E3
C------------------------------------------------------------------------
	iret = 0
C
	nt1 = nt + 1
	nt2 = nt + 2
	ooo = ' '
	DO ii = 1, 2
	    str ( ii ) = ' '
	END DO
C
C*	For Case 1.8 ( index = 1 )
C
C*	Wording: RA THROUGHOUT EA BECOMING EB EARLY THEN BECOMING EC LATE
C
	IF ( index .eq. 1 )  THEN
	    itrnd = 1
	    CALL G2T_ABC ( ktype, nt, itrnd, ier )
C
	    CALL G2T_RANGX ( ktype, lunb, nt, itrnd, ier )
	    CALL ST_LSTR ( offtxt ( nt ), loff, ier )
	    offtxt ( nt ) = offtxt ( nt ) ( : loff -  1 ) // 
     +			    ' THROUGHOUT.'
	    CALL G2T_APPEX ( ktype, lunb, nt, itrnd, ' ', ier )
C
	    itrnd = 2
	    CALL G2T_ABC ( ktype, nt1, itrnd, ier )
C
	    mine_2  = mxn_de  ( ktype, 2, 1 )
	    maxe_2  = mxn_de  ( ktype, 2, 2 )
C
	    CALL ST_INCH ( mine_2, str ( 1 ), ier )
	    CALL ST_INCH ( maxe_2, str ( 2 ), ier )
	    IF ( ktype .eq. 1 )  THEN
		IF ( mine_2 .eq. 0 .or. maxe_2 .eq. 0 )  THEN
		    CALL G2T_GTEXT ( lunb, 53, str, ooo, ier )
		  ELSE
		    CALL G2T_GTEXT ( lunb, 6, str, ooo, ier )
		END IF
	      ELSE
		CALL G2T_GTEXT ( lunb, 7, str, ooo, ier )
	    END IF
	    CALL G2T_APPTXT ( ktype, nt, ooo, ier )
C
	    CALL G2T_ABC ( ktype, nt2, itrnd, ier )
	    mine_3  = mxn_de  ( ktype, 2, 1 )
	    maxe_3  = mxn_de  ( ktype, 2, 2 )
	    CALL ST_INCH ( mine_3, str ( 1 ), ier )
	    CALL ST_INCH ( maxe_3, str ( 2 ), ier )
	    IF ( ktype .eq. 1 )  THEN
		CALL G2T_GTEXT ( lunb, 42, str, ooo, ier )
	      ELSE
		CALL G2T_GTEXT ( lunb, 22, str, ooo, ier )
	    END IF
C
	    CALL G2T_APPTXT ( ktype, nt, ooo, ier )
C
C*	  For Cases 8.1. 
C
C*	  Wording: RA BECOMING RB EARLY THEN BECOMING RC LATE...EA
C*	  THROUGH PERIOD.
C
	  ELSE IF ( index .eq. 2 )  THEN
	    itrnd = 1
	    CALL G2T_ABC ( ktype, nt, itrnd, ier )
	    eflag_d ( ktype, itrnd ) = .false.
	    CALL G2T_RANGX ( ktype, lunb, nt, itrnd, ier )
C
	    itrnd = 2
	    CALL G2T_ABC ( ktype, nt1, itrnd, ier )
	    eflag_d ( ktype, itrnd ) = .false.
	    mnw_2  = mxn_d  ( ktype, 2, 1 )
	    mxw_2  = mxn_d  ( ktype, 2, 2 )
	    CALL ST_INCH ( mnw_2, str ( 1 ), ier )
	    CALL ST_INCH ( mxw_2, str ( 2 ), ier )
	    IF ( ktype .eq. 1 )  THEN
		IF ( mnw_2 .eq. 0 .or. mxw_2 .eq. 0 )  THEN
		    CALL G2T_GTEXT ( lunb, 57, str, ooo, ier )
		  ELSE
		    CALL G2T_GTEXT ( lunb, 46, str, ooo, ier )
		END IF
	      ELSE
		CALL G2T_GTEXT ( lunb, 26, str, ooo, ier )
	    END IF
	    CALL  G2T_APPTXT ( ktype, nt, ooo, ier )
C
	    CALL G2T_ABC ( ktype, nt2, itrnd, ier )
	    mnw_3  = mxn_d  ( ktype, 2, 1 )
	    mxw_3  = mxn_d  ( ktype, 2, 2 )
	    CALL ST_INCH ( mnw_3, str ( 1 ), ier )
	    CALL ST_INCH ( mxw_3, str ( 2 ), ier )
	    IF ( ktype .eq. 1 )  THEN
		CALL G2T_GTEXT ( lunb, 42, str, ooo, ier )
	      ELSE
		CALL G2T_GTEXT ( lunb, 22, str, ooo, ier )
	    END IF
	    CALL G2T_APPTXT ( ktype, nt, ooo, ier )
C
	    eflag_d ( ktype, 1 ) = .true.
	    end = ' THROUGH PERIOD.'
	    itrnd = 1
	    CALL G2T_APPEX ( ktype, lunb, nt, itrnd, end, ier )
C
C*	  For Cases 8.2 to 8.8 ( index = 3 ) and Cases 2.8 to 8.8
C
C*	  Wording: PERIOD A BECOMING B EARLY THEN BECOMING C LATE.
C
	  ELSE IF ( index .ge. 3 )  THEN
	    end = ' EARLY.'
	    CALL G2T_A2RBEB ( ktype, lunb, nt, end, ier )
	    mxwe_2  = mxn_de  ( ktype, 2, 2 )
	    E1 = eflag_d ( ktype, 2 )
C
	    itrnd = 2
	    CALL G2T_ABC ( ktype, nt2, itrnd, ier )
	    mnw_3  = mxn_d  ( ktype, 2, 1 )
	    mxw_3  = mxn_d  ( ktype, 2, 2 )
	    E3 = eflag_d ( ktype, 2 )
	    CALL ST_INCH ( mnw_3, str ( 1 ), ier )
	    CALL ST_INCH ( mxw_3, str ( 2 ), ier )
	    IF ( ktype .eq. 1 )  THEN
		IF ( E1 .and. E3 )  THEN
		    IF ( mxwe_2 .ge. mxw_3 )  THEN
			CALL G2T_GTEXT ( lunb, 50, str, ooo, ier )
		      ELSE
		    	CALL G2T_GTEXT ( lunb, 42, str, ooo, ier )
		    END IF
		  ELSE IF ( E1 .and. .not. E3 ) THEN
		    IF ( mxwe_2 .ge. mxw_3 )  THEN
			CALL G2T_GTEXT ( lunb, 51, str, ooo, ier )
		      ELSE
		    	CALL G2T_GTEXT ( lunb, 43, str, ooo, ier )
		    END IF
		  ELSE 
		    CALL G2T_GTEXT ( lunb, 42, str, ooo, ier )
		END IF
	      ELSE
		IF ( E1 .and. E3 )  THEN
		    IF ( mxwe_2 .ge. mxw_3 )  THEN
			CALL G2T_GTEXT ( lunb, 34, str, ooo, ier )
		      ELSE
		    	CALL G2T_GTEXT ( lunb, 22, str, ooo, ier )
		    END IF
		  ELSE IF ( E1 .and. .not. E3 ) THEN
		    IF ( mxwe_2 .ge. mxw_3 )  THEN
			CALL G2T_GTEXT ( lunb, 35, str, ooo, ier )
		      ELSE
		    	CALL G2T_GTEXT ( lunb, 23, str, ooo, ier )
		    END IF
		  ELSE 
		    CALL G2T_GTEXT ( lunb, 33, str, ooo, ier )
		END IF
	    END IF
	    eflag_d ( ktype, 1 ) = .true.
	    CALL  G2T_APPTXT ( ktype, nt, ooo, ier )
	END IF
C*
	RETURN
	END 
