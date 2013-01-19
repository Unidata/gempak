       SUBROUTINE IM_CHTB  ( chanl, itype, iret )
C************************************************************************
C* IM_CHTB                                                              *
C*                                                                      *
C* This subroutine translates the McIDAS channel to a channel number.   *
C*                                                                      *
C* IM_CHTB  ( CHANL, ITYPE, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      CHANL           CHAR*           McIDAS channel string		*
C*                                                                      *
C* Output parameters:                                                   *
C*      ITYPE           INTEGER         Channel number			*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/GSC		 7/01	Created					*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER*(*)   chanl
C*
        CHARACTER       mchnl*60, buffer*80
	LOGICAL		found, eof
C------------------------------------------------------------------------
        iret = 0
	found = .false.
	eof = .false.
C
C*	Get length of input channel string.
C
	CALL ST_LSTR ( chanl, lenc, ier )
C
C*	Open and read McIDAS channel string table.
C
        CALL FL_TBOP  ( 'channel.tbl', 'sat', lun, ier )
	IF  ( ier .ne. 0 )  THEN	    
	    iret = -13
	    RETURN
	END IF
C
C*	Compare input string to table string for a match.
C
	DO WHILE ( .not. ( eof .or. found ) )

C
C*	    Read the next record.
C
	    READ  ( lun, '(A)', IOSTAT = iostat ) buffer
C
	    IF ( iostat .ne. 0 ) THEN
C
		eof = .true.
C
	      ELSE IF ( buffer(1:1) .ne. '!' ) THEN
C
                READ ( buffer, '(A,1X,I3)') mchnl, itype
	        CALL ST_LSTR ( mchnl, lens, ier )
                IF ( chanl(:lenc) .eq. mchnl(:lens) ) THEN
                    found = .true.
                END IF
            END IF
        END DO
C
	CALL FL_CLOS ( lun, ier )
C
C*	Match was not found; return default channel number.
C
	IF ( .not. found) THEN
	    itype = 2
            iret = 4
	    CALL ER_WMSG ( 'IM', iret, ' ', ierr )
	END IF
C*
        RETURN
        END
