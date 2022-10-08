	SUBROUTINE IG_IFPT  ( rimnem, iret )
C************************************************************************
C* IG_IFPT								*
C*									*
C* This subroutine prints the interface values arrays to the decoder	*
C* log if the verbosity level is 3 or higher.				*
C*									*
C* IG_IFPT  ( RIMNEM, IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP 06/05                                           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'igcmn.cmn'
C*
	CHARACTER*(*)	rimnem (*)               
C-----------------------------------------------------------------------
	iret = 0
        loglvl = 3
C
C*      Print all of the single-level real interface values
C*      that are not missing.
C
	CALL DC_IFPR  ( 3, rimnem, rivals, NRSLMN, ierfpr )
C
C*      Print all of the multi-level interface values.
C*      Print backscatter and related parameters.                     
C
        IF  ( rivals ( irnbks ) .gt. 0 )  THEN
C
            WRITE ( UNIT = logmsg, FMT = '( 6A10 )' )
     +          rimnem ( irnbks ),     rimnem ( irtobd (1) ),
     +          rimnem ( irqcb1 (1) ), rimnem ( irbks1 (1) ),
     +          rimnem ( irfos1 (1) ), rimnem ( irbks2 (1) )
            CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:60), ierwlg )
            WRITE ( UNIT = logmsg, FMT = '( 6A10 )' )
     +          rimnem ( irqcb2 (1) ), rimnem ( irelb1 (1) ),
     +          rimnem ( irhinb (1) ), rimnem ( irfos2 (1) ),
     +          rimnem ( irelb2 (1) ), rimnem ( irnvpb (1) ) 
            CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:60), ierwlg )
            WRITE ( UNIT = logmsg, FMT = '( 6F10.2 )' )
     +          rivals ( irnbks ),     rivals ( irtobd (1) ),
     +          rivals ( irqcb1 (1) ), rivals ( irbks1 (1) ),
     +          rivals ( irfos1 (1) ), rivals ( irbks2 (1) ) 
            CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:60), ierwlg )
            WRITE ( UNIT = logmsg, FMT = '( 6F10.2 )' )
     +          rivals ( irqcb2 (1) ), rivals ( irelb1 (1) ),
     +          rivals ( irhinb (1) ), rivals ( irfos2 (1) ),
     +          rivals ( irelb2 (1) ), rivals ( irnvpb (1) )  
            CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:60), ierwlg )
C
            IF  ( rivals ( irnbks ) .gt. 1 )  THEN
                 DO jj = 2, rivals ( irnbks )
                     WRITE ( UNIT = logmsg,
     +                      FMT = '( 10X, 5F10.2 )' )
     +               rivals ( irtobd ( jj ) ), rivals ( irqcb1 ( jj ) ),
     +               rivals ( irbks1 ( jj ) ), rivals ( irfos1 ( jj ) ),
     +               rivals ( irbks2 ( jj ) ) 
                     CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:60),
     +                               ierwlg )
                     WRITE ( UNIT = logmsg,
     +                      FMT = '( 6F10.2 )' )
     +               rivals ( irqcb2 ( jj ) ), rivals ( irelb1 ( jj ) ),
     +               rivals ( irhinb ( jj ) ), rivals ( irfos2 ( jj ) ),
     +               rivals ( irelb2 ( jj ) ), rivals ( irnvpb ( jj ) )
                     CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:60),
     +                               ierwlg )
                 END DO
            END IF
C
        END IF
C
C*      Print mean frequency, brightness temperature, and assoc. parameters.
C
        IF  ( rivals ( irnmef ) .gt. 0 )  THEN
C
             WRITE ( UNIT = logmsg, FMT = '( 4A10 )' )
     +          rimnem ( irnmef ),
     +          rimnem ( irmefr (1) ),
     +          rimnem ( irqctm (1) ), rimnem ( irtmbr (1) )
             CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:40), ierwlg )
             WRITE ( UNIT = logmsg, FMT = '( 4F10.2 )' )
     +          rivals ( irnmef ),
     +          rivals ( irmefr (1) ),
     +          rivals ( irqctm (1) ), rivals ( irtmbr (1) )
            CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:40), ierwlg )
C
            IF  ( rivals ( irnmef ) .gt. 1 )  THEN
                 DO jj = 2, rivals ( irnmef )
                     WRITE ( UNIT = logmsg,
     +                      FMT = '( 10X, 3F10.2 )' )
     +               rivals ( irmefr ( jj ) ),
     +               rivals ( irqctm ( jj ) ), rivals ( irtmbr ( jj ) )
                    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:40),
     +                              ierwlg )
                 END DO
            END IF
C
        END IF
C
C*      Print satellite derived wind calculation method and 
C*      10 meter wind speed.                                                   
C
        IF  ( rivals ( irnsww ) .gt. 0 )  THEN
C
             WRITE ( UNIT = logmsg, FMT = '( 3A10 )' )
     +          rimnem ( irnsww ),
     +          rimnem ( irswcm (1) ), rimnem ( irws10 (1) )
             CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:30), ierwlg )
             WRITE ( UNIT = logmsg, FMT = '( 3F10.2 )' )
     +          rivals ( irnsww ),
     +          rivals ( irswcm (1) ), rivals ( irws10 (1) )
            CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:30), ierwlg )
C
            IF  ( rivals ( irnsww ) .gt. 1 )  THEN
                 DO jj = 2, rivals ( irnsww )
                     WRITE ( UNIT = logmsg,
     +                      FMT = '( 10X, 2F10.2 )' )
     +               rivals ( irswcm ( jj ) ), rivals ( irws10 ( jj ) )
                    CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:30),
     +                              ierwlg )
                 END DO
            END IF
C
        END IF
C
	RETURN
	END
