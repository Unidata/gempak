	SUBROUTINE G2T_GTEXT ( lunb, numg2t, str, outmsg, iret )
C************************************************************************
C* G2T_GTEXT								*
C*									*
C* This subroutine gets offshore text message.  The strings, STR(2),	*
C* replace !WDx !SPx !WHx in the text message table, G2T_TXT.TBL.	*
C*									*
C* G2T_GTEXT  ( LUNB, NUMG2T, STR, OUTMSG, IRET )			*
C*									*
C* Input parameters:							*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	NUMEG2T		INTEGER		Text number 			*
C*	STR(2)		CHAR*		Input wind/wave strings		*
C*									*
C* Output parameters:							*
C*	OUTMSG		CHAR*		Output message			*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					+1 = text number not found 	*
C*					+4 = key word not found 	*
C**									*
C* Log:									*
C* T. Lee/SAIC		10/06	Created					*
C* T. Lee/SAIC		04/07	NUMG2T = 5 -> Light winds		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	outmsg, str(*)
	CHARACTER*128 	text, ooo
	LOGICAL		done
C*
C------------------------------------------------------------------------
	iret = 0
	outmsg = ' '
C
C*	Search for the text number.
C
	done = .false.
	REWIND ( lunb )
	DO WHILE  ( .not. done )
	    READ   ( lunb, 1000, iostat = ier ) text
1000	    FORMAT ( A )
	    IF  ( ier .ne. 0 )  THEN
		iret = +1
		CALL ER_WMSG ( 'GOFTXT', iret, ' ', ier )
		RETURN
	      ELSE
		CALL ST_ILST  ( text, '!', 0, 1, num, nnum, iret )
		IF ( ( nnum .eq. 1 ) .and. ( num .eq. numg2t ) )  THEN
		    iret = 0
		    ipos = INDEX ( text, '!' )
		    IF  ( ipos .gt. 0 )  THEN
			text = text ( ipos+1: )
		      ELSE
			text = ' '
		    ENDIF
		    done = .true.
		END IF
	    END IF
	END DO
C
	outmsg = text
	IF  ( str ( 1 ) .eq. ' ' .and. str ( 2 ) .eq. ' ' )  THEN
	    IF  ( numg2t .eq. 5 )  outmsg = text
	    RETURN
 	  ELSE IF ( str ( 1 ) .eq. str ( 2 ) )  THEN
	    str ( 2 ) = ' '
	END IF
C
C*      Replace wind directions if any !WDx in the text.
C
	iwd = INDEX ( outmsg, '!WD' )
	isp = INDEX ( outmsg, '!SP' )
	iwh = INDEX ( outmsg, '!WH' )
	IF  ( iwd .ne. 0 )  THEN
	    IF  ( str ( 2 ) .eq. ' ' )  THEN
		ipos = INDEX ( outmsg, '!WD1' )
		IF  ( ipos .ne. 0 )  THEN
		    CALL ST_LSTR ( str ( 1 ), lens, ier )
		    ooo = outmsg
		    IF  ( lens .gt. 0 )  THEN
		        outmsg = ooo ( :ipos - 1 ) // str(1) ( :lens) //
     +				 ooo ( ipos + 12 : )
		      ELSE
			outmsg = ooo ( :ipos - 1 ) // '...' //
     +				 ooo ( ipos + 12 : )
		    END IF
		END IF
	      ELSE
		DO ii = 1, 2
		    ipos = INDEX ( outmsg, '!WD' )
		    IF  ( ipos .ne. 0 )  THEN
			CALL ST_LSTR ( str ( ii ), lens, ier )
			ooo = outmsg
			IF  ( lens .gt. 0 )  THEN
			    outmsg = ooo (:ipos-1) // str (ii) (:lens)
     +				     // ooo ( ipos + 4 : )
			  ELSE
			    outmsg = ooo ( : ipos-1 ) // '...' //
     +				     ooo ( ipos + 4 : )
			END IF
		    END IF
		END DO
	    END IF
C
C*	    Replace wind speed,
C
C	    Case 1: str ( 1 ) = ' ' -> Variable winds !SP2 KT or less.
C
	  ELSE IF ( isp .ne. 0 )  THEN
	    IF  ( str ( 1 )  .eq. ' ' )  THEN
		ipos = INDEX ( outmsg, '!SP2' )
		IF  ( ipos .ne. 0 )  THEN
		    CALL ST_LSTR ( str ( 2 ), lens, ier )
		    ooo =  outmsg 
		    IF ( lens .gt. 0 )  THEN
			outmsg = ooo ( :ipos - 1 ) //
     +				 str ( 2 ) ( : lens ) //
     +				 ooo ( ipos + 4 : )
		      ELSE
			outmsg = ooo ( :ipos - 1 ) // '...' //
     +				 ooo ( ipos + 4 : )
		    END IF
		END IF
C
C	          Case 2: str ( 2 ) = ' ' -> Single wind speed.
C
	      ELSE IF  ( str ( 2 )  .eq. ' ' )  THEN
		ipos = INDEX ( outmsg, '!SP1' )
		IF  ( ipos .ne. 0 )  THEN
		    CALL ST_LSTR ( str ( 1 ), lens, ier )
		    ooo = outmsg
		    IF  ( lens .gt. 0 )  THEN
			outmsg = ooo ( : ipos - 1 ) // str (1) ( :lens )
     +				 // ooo ( ipos + 12 : )
		      ELSE
			outmsg = ooo ( : ipos - 1 ) // '...' //
     +				 ooo ( ipos + 12 : )
                        END IF
                    END IF
                  ELSE
C
C*		Case 3: SP1 to SP2
C
		DO jj = 1, 2
		    ipos = INDEX ( outmsg, '!SP' )
		    IF  ( ipos .ne. 0 )  THEN
			CALL ST_LSTR ( str ( jj ), lens, ier )
			ooo = outmsg
			IF  ( lens .gt. 0 )  THEN
			    outmsg = ooo ( : ipos - 1 ) // 
     +				     str (jj) ( : lens ) //
     +				     ooo ( ipos + 4 : )
			  ELSE 
			    outmsg = ooo ( : ipos - 1 ) // '...' //
     +				     ooo ( ipos + 4 : )
			END IF 
		    END IF
		END DO
	    END IF
C
C*	  Wave information.
C
	
	  ELSE IF ( iwh .ne. 0 )  THEN
	    IF  ( str ( 2 ) .eq. ' ' )  THEN
		ipos = INDEX ( outmsg, '!WH1' )
		IF  ( ipos .ne. 0 )  THEN
		    CALL ST_LSTR ( str ( 1 ), lens, ier )
		    ooo = outmsg
 		    IF  ( lens .gt. 0 )  THEN
			outmsg = ooo ( : ipos - 1 ) // str (1) ( :lens )
     +				 // ooo ( ipos + 12 : )
                      ELSE
			outmsg = ooo ( : ipos - 1 ) // '...' //
     +				 ooo ( ipos + 12 : )
                    END IF
                END IF
	      ELSE
		DO kk = 1, 2
		    ipos = INDEX ( outmsg, '!WH' )
		    IF  ( ipos .ne. 0 )  THEN
		        CALL ST_LSTR ( str ( kk ), lens, ier )
		        ooo = outmsg
		        IF  ( lens .gt. 0 )  THEN
			    outmsg = ooo (:ipos-1) //  str (kk) (:lens)
     +					           // ooo ( ipos+4 : )
			  ELSE
			    outmsg = ooo ( : ipos-1 ) // '...' //
     +				     ooo ( ipos + 4 : )
			END IF
		    END IF
		END DO
	    END IF
	  ELSE
	    iret = +4
	END IF
C*
	RETURN
	END
