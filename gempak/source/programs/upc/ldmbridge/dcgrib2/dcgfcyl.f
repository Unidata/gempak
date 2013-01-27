	SUBROUTINE DCGFCYL (filnam, iflno, iret)
C************************************************************************
C* DCGFCYL								*
C*									*
C* This routine checks a list of open files for the requested file.     *
C* If the file is already open, the unit number is returned, otherwise	*
C* a return code -1 is provided. If the file is not already open, and	*
C* the file list is full, the oldest is closed and the order is cycled	*
C* for the remaining files. 						*
C* 									*
C* DCGFCYL (filnam, iflno, iret)					*
C*									*
C* Input parameters:							*
C*	filnam		CHAR*		Requested file name		*
C*									*
C* Output parameters:							*
C*	iflno		INTEGER		Unit number for requested file	*
C*	iret		INTEGER		Return Code			*
C*					   0 = file open/iflno returned	*
C*					  -1 = file not open		* 
C*									*
C**									*
C* Log									*
C* Chiz/Unidata		04/2000		Created				*
C************************************************************************
        INCLUDE         'dccmn.cmn'
C*

	CHARACTER*(*)   filnam
	INTEGER		iflno
	INTEGER		iret

C
C*      Return 0 if the file is already open, otherwise -1
C
        iret = -1

C
C*      Check for an existing match for the file name.
C
        DO  i = 1, maxfil
            IF  ( filnam .eq. filopn (i) )  THEN
                iflno = lunopn (i)
                iret = 0
                RETURN
            END IF
        END DO

C
C*      see if we need to close anything
C
        DO  i = 1, maxfil
            IF  ( numopn (i) .eq. 0 )  THEN
                RETURN
            END IF
        END DO
C
C*      Close the oldest file and open the new file it its place.
C*      Cycle the order of the other open files.
C
	DO  i = 1, maxfil
            IF  ( numopn (i) .eq. 1 )  THEN
C               close the grid file 
		call gd_clos(lunopn (i),ier)
		call dc_wlog(1, 'DCGRIB', ier, 'Closing '//filopn(i), ierr )
                filopn (i) = ' '
                lunopn (i) = 0
                numopn (i) = 0
            ELSE
                numopn (i) = numopn (i) - 1
            END IF
        END DO

	RETURN
	END


        SUBROUTINE DCGFINT ( numfil )
C************************************************************************
C* DCGFINT                                                              *
C*									*
C* This routine initializes the common block structure for the number 	*
C* of cyclical file names desired.					*
C*                                                                      *
C* DCGFINT (numfil)							*
C*                                                                      *
C* Input parameters:                                                    *
C*	numfil		INTEGER		Number of files to allow	*
C*                                                                      *
C* Output parameters:                                                   *
C*	NONE								*
C*                                                                      *
C**                                                                     *
C* Log                                                                  *
C* Chiz/Unidata         04/2000         Created                         *
C************************************************************************
        INCLUDE         'dccmn.cmn'
C*
	INTEGER		numfil

        IF  ( ( numfil .gt. 0 ) .and. ( numfil .le. MXFILE ) )  THEN
            maxfil = numfil
          ELSE
            maxfil = MXFILE
            CALL DC_WLOG ( 0, 'DC', 4, ' ', ier )
        END IF
C
C*      Initialize the open file arrays.
C
        DO  i = 1, maxfil
            filopn (i) = ' '
            lunopn (i) = 0
            numopn (i) = 0
        END DO

	RETURN
	END

	SUBROUTINE DCGFILES (filnam, iflno, iret)
C************************************************************************
C* DCGFILES                                                             *
C*                                                                      *
C* This routine adds a file name and unit number into the cyclical	*
C* file list.								*
C*                                                                      *
C* DCGFILES (filnam,iflno,iret)                                         *
C*                                                                      *
C* Input parameters:                                                    *
C*	filnam		CHAR*		file name			*
C*      iflno		INTEGER		file unit number		*
C*                                                                      *
C* Output parameters:                                                   *
C*      iret		INTEGER		Return code			*
C*					   0 = Normal Return		*
C*                                                                      *
C**                                                                     *
C* Log                                                                  *
C* Chiz/Unidata         04/2000         Created                         *
C************************************************************************
        INCLUDE         'dccmn.cmn'
C*

        CHARACTER*(*)   filnam
        INTEGER         iflno
        INTEGER         iret

	iret = 0

	DO  i = 1, maxfil
            IF  ( numopn (i) .eq. 0 )  THEN
	        filopn (i) = filnam
                lunopn (i) = iflno
                DO  j = 1, maxfil
                    numopn (i) = MAX ( numopn (i), numopn (j) )
                END DO
                numopn (i) = numopn (i) + 1
                RETURN
            END IF
        END DO


	RETURN
        END


	SUBROUTINE DCGFCLS ( iret )
C************************************************************************
C* DCGFCLS                                                              *
C*                                                                      *
C* This routine closes all open files and clears the cyclical structure.*
C*                                                                      *
C* DCGFCLS (iret)                                         		*
C*                                                                      *
C* Input parameters:                                                    *
C*	NONE								*
C*                                                                      *
C* Output parameters:                                                   *
C*      iret		INTEGER		Return code			*
C*					   0 = Normal Return		*
C*                                                                      *
C**                                                                     *
C* Log                                                                  *
C* Chiz/Unidata         04/2000         Created                         *
C************************************************************************
        INCLUDE         'dccmn.cmn'
C*
	iret = 0
C
C*      Close all open files.
C
        DO  i = 1, maxfil
            IF  ( lunopn (i) .ne. 0 )  THEN
		call gd_clos(lunopn (i), ier)
		call dc_wlog(1, 'DCGRIB', ier, 
     +                       'Closing '//filopn(i), ierr )
	    END IF
	    filopn (i) = ' '
	    lunopn (i) = 0
	    numopn (i) = 0
	END DO
C*
        RETURN
        END
