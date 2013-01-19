	SUBROUTINE G2T_DSDT ( lunb, nt, iret )
C************************************************************************
C* G2T_DSDT								*
C*									*
C* This subroutine creates OFF text wind trending.			*
C*									*
C* G2T_DSDT ( LUNB, NT, IRET )						*
C*									*
C* Input parameters:							*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	NT		INTEGER		Nth time step			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C* T. Lee/SAIC		03/08	Removed case numbers			*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	PARAMETER	( ktype = 2 )
	CHARACTER	sid(3)*(MAXIDS)
	CHARACTER	end*20
	CHARACTER	d1(3)*2, d2(3)*2, de1(3)*2, de2(3)*2
	LOGICAL		Rcase_8, Ecase_8
	INTEGER		X(3), N(3), E(3)
	INCLUDE		'EQUAL.FNC'
C------------------------------------------------------------------------
	iret = 0
C
	Rcase_8 = .false.
	Ecase_8 = .false.
C
	end = ' '
	nt1 = nt + 1
	nt2 = nt + 2
C
C*	For 06/18Z cycle, omit "EARLY" and "LATE" for the 1st period.
C
	CALL G2T_SYNOP ( nt, ier )

C
C*	Get data. X(3): stores max wind from nth to (n+2)th time step.
C*		  N(3): stores min wind
C*		  E(3): stores max wind in the EXCEPT portion.
C
	CALL G2T_GET ( ktype, nt, D1, D2, DE1, DE2, X, N, E, sid, iret )
C
	Rcase_8 = .false.
	IF  ( X ( 2 ) .gt. 10 )  THEN
	    IF ( X ( 1 ) .ge. 2 * X ( 2 ) .and.
     +		 X ( 3 ) .ge. 2 * X ( 2 )  )  Rcase_8 = .true.
	END IF
C
	Ecase_8 = .false.
	IF  ( E ( 2 ) .ne. IMISSD .and. E ( 2 ) .gt. 10 )  THEN
	    IF ( E ( 1 ) .ge. 2 * E ( 2 ) .and.
     +	         E ( 3 ) .ge. 2 * E ( 2 )  )  Ecase_8 = .true.
	END IF
C
C*	Add wave trend (see Trending Matrix for wave).
C
C	Case 8: Ra >> Rb << Rc
C
	IF ( Rcase_8 )  THEN
C
C*	    Case 8: First 4 periods.
C
	    IF ( nt .le. 7 )  THEN
C
C*		All equals -> Ra -> Rb early -> Rc late...Ea through period.
C
		IF  ( C1 ( E (1), E (2), E (3) ) )  THEN
		    CALL G2T_CASE8 ( ktype, lunb, nt, 2, ier )
		  ELSE
C
C*		    For Cases 8.2 to 8.7, same text output.
C
		    CALL G2T_CASE8 ( ktype, lunb, nt, 3, ier )
		END IF
	      ELSE
		CALL G2T_OUTLOOK ( ktype, lunb, nt, ier )
	    END IF
C
C*	  Case 1: Ra = Rb = Rc.
C

	  ELSE IF ( C1 ( X (1), X (2), X (3) ) )  THEN
C
C*	    Case 1: First 4 periods.
C
	    IF ( nt .le. 7 )  THEN
C
C*		Case 1.8
C
C*		Wording: Period A BECOMING B EARLY THEN BECOMING C LATE.
C
		IF ( Ecase_8 ) THEN
		    CALL G2T_CASE8 ( ktype, lunb, nt, 1, ier )
C
C*		  All equals -> Period A.
C
		  ELSE IF ( C1 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .and.
     +		     CEQ ( SID ( 1 ), SID ( 2 ), SID ( 3 ) ) .or.
     +		     C6 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
		    CALL G2T_A ( ktype, lunb, nt, ier )
C
C*		  Period A BECOMING B.
C
		  ELSE IF ( C2 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
		    IF ( synop )  end = ' EARLY.'
		    CALL G2T_A2RBEB ( ktype, lunb, nt, end, ier )
C
C*		  Period A BECOMING C.
C
		  ELSE IF ( C3 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +			    C4 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +			  ( C7 ( E ( 1 ), E ( 2 ), E ( 3 ) ) ) )  THEN
		    IF ( C3 ( E (1), E (2), E (3) ) )  THEN
			IF ( synop )  end = ' LATE.' 
		    END IF
		    CALL G2T_A2RCEC ( ktype, lunb, nt, end, ier )
C
C*		  RAEB.
C

		  ELSE IF ( C5 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
		    CALL G2T_RAEB ( ktype, lunb, nt, end, ier )
		  ELSE IF ( SID ( 1 ) .ne. SID ( 2 ) )  THEN
C
C*		  Period A BECOMING B. Location changes only.
C
		    CALL G2T_A2RBEB ( ktype, lunb, nt, end, ier )	
		  ELSE IF ( SID ( 1 ) .ne. SID ( 3 ) )  THEN
C
C*		  Period A BECOMING C. Location changes only.
C
		    CALL G2T_A2RCEC ( ktype, lunb, nt, end, ier )	
		  ELSE
		    iret = -12
cc
		END IF
	      ELSE
C
C*		Case 1: Outlook.
C
		CALL G2T_OUTLOOK ( ktype, lunb, nt, iret )	
	    END IF
C
C*	  Case 2: Ra < (or >) Rb = Rc.
C
	  ELSE IF ( C2 ( X ( 1 ), X ( 2 ), X ( 3 ) ) )  THEN
C
C*	    Case 2: First 4 periods.
C
	    IF ( nt .le. 7 )  THEN
C
C*		Case 2.8
C
C*		Wording: Period A BECOMING B EARLY THEN BECOMING C LATE.
C
		IF ( Ecase_8 ) THEN
		    CALL G2T_CASE8 ( ktype, lunb, nt, 3, ier )
C
C*		Case 2.1/2.2/2.5.
C
C*		Wording: Period A BECOMING B.	
C

		  ELSE IF ( C1 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     	    C2 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     	    C5 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
		    IF ( synop )  end = ' EARLY.'
		    CALL G2T_A2RBEB ( ktype, lunb, nt, end, ier )
C
C*		Case 2.3/2.4/2.6/2.7.
C
C*		Wording: Period A BECOMING RBEC.
C
		  ELSE IF ( C3 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     	    C4 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     	    C6 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     	    C7 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RBEC ( ktype, lunb, nt, end, ier )
		  ELSE
		    iret = -12
		END IF
	      ELSE
C
C*		Case 2: Outlook.
C
		CALL G2T_OUTLOOK ( ktype, lunb, nt, iret )
	    END IF
C
C*	  Case 3.
C
	  ELSE IF ( C3 ( X ( 1 ), X ( 2 ), X ( 3 ) ) ) THEN
	    IF ( nt .le. 7 )  THEN
C
C*		Case 3.8
C
C*		Wording: Period A BECOMING B EARLY THEN BECOMING C LATE.
C
		IF ( Ecase_8 ) THEN
		    CALL G2T_CASE8 ( ktype, lunb, nt, 3, ier )
C
C*		Case 3.1/3.2/3.3/3.4/3.6/3.7.
C
C*		Wording: Period A BECOMING C.
C
		  ELSE IF ( C1 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C2 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C3 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C4 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C6 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C7 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    IF ( synop )  THEN
			IF ( C1 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +			     C2 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +			     C3 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
			    IF ( synop )  end = ' LATE.'
			END IF
		    END IF
                    CALL G2T_A2RCEC ( ktype, lunb, nt, end, ier )
C
C*		Case 3.5
C
C*		Wording: Period A BECOMING RCEB.
C
		  ELSE IF ( C5 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RCEB ( ktype, lunb, nt, end, ier )
		  ELSE
		    iret = -12
		END IF
	      ELSE
C
C*              Case 3: Outlook.
C
                CALL G2T_OUTLOOK ( ktype, lunb, nt, iret )
            END IF
C
C*	  Case 4.
C
	  ELSE IF ( C4 ( X ( 1 ), X ( 2 ), X ( 3 ) ) )  THEN
	    IF ( nt .le. 7 )  THEN
C
C*		Case 4.8
C
C*		Wording: Period A BECOMING B EARLY THEN BECOMING C LATE.
C
		IF ( Ecase_8 ) THEN
		    CALL G2T_CASE8 ( ktype, lunb, nt, 3, ier )
C
C*		Case 4.1/4.2/4.3/4.4/4.6/4.7.
C
C*		Wording: Period A BECOMING C.
C
		  ELSE IF ( C1 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C2 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C3 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C4 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C6 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C7 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RCEC ( ktype, lunb, nt, end, ier )
C
C*		Case 4.5.
C
C*		Wording: Period A BECOMING RCEB.
C
		  ELSE IF ( C5 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RCEB ( ktype, lunb, nt, end, ier )
		  ELSE
		    iret = -12
		END IF
	      ELSE
C
C*              Case 4: Outlook.
C
                CALL G2T_OUTLOOK ( ktype, lunb, nt, iret )
            END IF
C
C*	  Case 5.
C
	  ELSE IF ( C5 ( X ( 1 ), X ( 2 ), X ( 3 ) ) )  THEN
	    IF ( nt .le. 7 )  THEN
C
C*		Case 5.8
C
C*		Wording: Period A BECOMING B EARLY THEN BECOMING C LATE.
C
		IF ( Ecase_8 ) THEN
		    CALL G2T_CASE8 ( ktype, lunb, nt, 3, ier )
C
C*		Case 5.1/5.2/5.5
C
C*		Wording: Period A BECOMING B.
C
		  ELSE IF ( C1 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C2 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C5 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RBEB ( ktype, lunb, nt, end, ier )
C
C*		Case 5.3/5.4/5.6/5.7
C
C*		Wording: Period A BECOMING RBEC.
C
		  ELSE IF ( C3 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +			    C4 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +			    C6 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +			    C7 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RBEC ( ktype, lunb, nt, end, ier )
		  ELSE
		    iret = -12
		END IF
	      ELSE
C
C*              Case 5: Outlook.
C
                CALL G2T_OUTLOOK ( ktype, lunb, nt, iret )
            END IF
C
C*	  Case 6.
C
	  ELSE IF ( C6 ( X ( 1 ), X ( 2 ), X ( 3 ) ) .and.
     +		    GE ( X ( 1 ), X ( 3 ) ) )  THEN
	    IF ( nt .le. 7 )  THEN
C
C*		Case 6.8
C
C*		Wording: Period A BECOMING B EARLY THEN BECOMING C LATE.
C
		IF ( Ecase_8 ) THEN
		    CALL G2T_CASE8 ( ktype, lunb, nt, 3, ier )
C
C*		Case 6.1/6.6.
C
C*		Wording: Period A.
C
		ELSE IF ( C1 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C6 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A ( ktype, lunb, nt, ier )
C
C*		Case 6.2.
C
C*		Wording: Period A BECOMING RAEB.
C
		  ELSE IF ( C2 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RAEB ( ktype, lunb, nt, end, ier )
C
C*		Case 6.3/6.4/6.7
C
C*		Wording: Period A BECOMING RAEC.
C
		  ELSE IF ( C3 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +			    C4 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +			    C7 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RAEC ( ktype, lunb, nt, end, ier )
C
C*		Case 6.5.
C
C*		Wording: RAEB.
C
		  ELSE IF ( C5 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_RAEB ( ktype, lunb, nt, end, ier )
		  ELSE
		    iret = -12
		END IF
	      ELSE
C
C*              Case 6: Outlook.
C
                CALL G2T_OUTLOOK ( ktype, lunb, nt, iret )
            END IF
C
C*	  Case 7.
C
	  ELSE IF ( C7 ( X ( 1 ), X ( 2 ), X ( 3 ) ) .and.
     +		    LT ( X ( 1 ), X ( 3) ) )  THEN
	    IF ( nt .le. 7 )  THEN
C
C*		Case 7.8
C
C*		Wording: Period A BECOMING B EARLY THEN BECOMING C LATE.
C
		IF ( Ecase_8 ) THEN
		    CALL G2T_CASE8 ( ktype, lunb, nt, 3, ier )
C
C*		Case 7.1/7.2/7.3/7.4/7.6/7.7.
C
C*		Wording: Period A BECOMING C.
C
		ELSE IF ( C1 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C2 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C3 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C4 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C6 ( E ( 1 ), E ( 2 ), E ( 3 ) ) .or.
     +		     C7 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RCEC ( ktype, lunb, nt, end, ier )
C
C*		Case 7.5.
C
C*		Wording: Period A BECOMING RCEB.
C
		  ELSE IF ( C5 ( E ( 1 ), E ( 2 ), E ( 3 ) ) )  THEN
                    CALL G2T_A2RCEB ( ktype, lunb, nt, end, ier )
		  ELSE
		    iret = -12
		END IF
	      ELSE
C
C*              Case 7: Outlook.
C
                CALL G2T_OUTLOOK ( ktype, lunb, nt, iret )
            END IF
	END IF
C*
	RETURN
	END
