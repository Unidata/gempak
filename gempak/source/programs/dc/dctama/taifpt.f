	SUBROUTINE TA_IFPT  ( rimnem, cimnem, iret )
C************************************************************************
C* TA_IFPT								*
C*									*
C* This subroutine prints the interface values arrays to the decoder	*
C* log if the verbosity level is 3 or higher.				*
C*									*
C* TA_IFPT  ( RIMNEM, CIMNEM, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*	RIMNEM (*)	CHAR*		Interface mnemonics for reals   *
C*      CIMNEM (*)      CHAR*           Interface mnemonics for chars   *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* C. Caruso Magee/NCEP 08/06                                           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'tacmn.cmn'
C*
	CHARACTER*(*)	rimnem (*), cimnem (*)   
C-----------------------------------------------------------------------
	iret = 0
        loglvl = 3
C
C*      Print all of the character interface values
C*      that are not missing.
C
	CALL DC_IFPC  ( 3, cimnem, civals, NCIMN, ierfpc )
C
C*      Print all of the single-level real interface values
C*      that are not missing.
C
	CALL DC_IFPR  ( 3, rimnem, rivals, NRSLMN, ierfpr )
C
C*      Print all of the multi-level interface values.
C
        IF  ( rivals ( irntqc ) .gt. 0 )  THEN
C
            WRITE ( UNIT = logmsg, FMT = '( 2A10 )' )
     +          rimnem ( irntqc ),     rimnem ( irtpqc (1) )
            CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:60), ierwlg )
            WRITE ( UNIT = logmsg, FMT = '( 2F10.2 )' )
     +          rivals ( irntqc ),     rivals ( irtpqc (1) ) 
            CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:60), ierwlg )
C
            IF  ( rivals ( irntqc ) .gt. 1 )  THEN
                 DO jj = 2, rivals ( irntqc )
                     WRITE ( UNIT = logmsg,
     +                      FMT = '( 10X, F10.2 )' )
     +               rivals ( irtpqc ( jj ) )
                     CALL DC_WLOG  ( loglvl, 'DC', 2, logmsg (1:60),
     +                               ierwlg )
                 END DO
            END IF
C
        END IF
C
	RETURN
	END
