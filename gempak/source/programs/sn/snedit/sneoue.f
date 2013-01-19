	SUBROUTINE SNEOUE ( snefil, lunedt, iret )
C************************************************************************
C* SNEOUE								*
C*									*
C* This routine attempts to open the unmerged edit file, and checks	*
C* its validity.  It returns to the beginning of the edit file		*
C* if sucessful.							*
C*									*
C* SNEOUE ( SNEFIL, LUNEDT, IRET )					*
C*									*
C* Input Parameters:							*
C* 	SNEFIL		CHAR*		Edit file name			*
C*									*
C* Output Parameters:							*
C*	LUNEDT		INTEGER		Lun for Edit File		*
C*	IRET		INTEGER		Return code			*
C*						  0 = normal            *
C*						 -3 = file not opened 	*
C*						 -18 = Improper format  * 
C**									*
C* Log:									*
C* S. Schotz/GSC	12/89						*
C************************************************************************
	CHARACTER	snefil*(*)
C*
	LOGICAL		kntflg, ddpflg, zwind, done
	CHARACTER	ptnam*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Attempt to open edit file
C
	CALL FL_SOPN ( snefil, lunedt, ier)
	IF ( ier .ne. 0 ) THEN
	    CALL ER_WMSG ( 'FL', ier, snefil, iret)
	    iret = -3
	    lunedt = 0
	    RETURN
	END IF
C
C*	Check to see if station header appears first
C
	CALL SNECPT ( lunedt, ptnam, ihhmm, nparm, kntflg, ddpflg, 
     +                zwind, ier )
	IF ( ier .ne. 2 ) THEN
            iret = - 18
	    CALL FL_CLOS ( lunedt, ier )
	    lunedt = 0
	    RETURN
	END IF
C
C*      Read past station header check validity of first part in file 
C
        done = .false.
	DO WHILE ( .NOT. done)
            CALL SNECPT ( lunedt, ptnam, ihhmm, nparm, kntflg, ddpflg, 
     +                zwind, ier )
	    IF ( ier .ne. 0 ) THEN
               IF ( ier .ne. 2) THEN
                  done = .true.
                  iret = - 18
	          CALL FL_CLOS ( lunedt, ier )
	          lunedt = 0
               END IF
	    ELSE
               done = .true.
	       CALL FL_REWD ( lunedt, ier )    
	    END IF
	END DO
C*
	RETURN
	END
