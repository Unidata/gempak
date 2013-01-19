	SUBROUTINE DC_OSNM ( filnam, iflsrc, stnfil, iadstn, maxtim,
     +			     iflno, nparm, parms, iret )
C************************************************************************
C* DC_OSNM								*
C*									*
C* This routine opens a merged sounding file.				*
C*									*
C* DC_OSNM ( FILNAM, IFLSRC, STNFIL, IADSTN, MAXTIM, IFLNO, NPARM, 	*
C*	     PARMS, IRET )						*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File to be opened or created	*
C*	IFLSRC		INTEGER		File source			*
C*	STNFIL		CHAR*		Station table			*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Max number of times		*
C*									*
C* Output parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	NPARM		INTEGER		Number of parameters		*
C*	PARMS (NPARM)	CHAR*4		Parameter list			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 8/95						*
C* D. Keiser/GSC	 4/96	Added iflsrc to calling sequence	*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C* J. Whistler/AWC	 8/96	Declared parms				*
C* D. Keiser/GSC	 9/96	Fix spelling error in SN_ASTN call seq. *
C* A. Hardy/GSC		 3/99	Added priority parameter to DC_STNS     *
C* F. J. Yen/NCEP	 1/01	Updated call to DC_STNS			*
C************************************************************************
	INCLUDE		'dccmn.cmn'
C*
	CHARACTER*(*)	filnam, stnfil, parms (*)
C*
	CHARACTER	newfil*132
	LOGICAL		exist, mrgdat, pkflg
C*
	CHARACTER	stid ( LLSTFL )*8, coun ( LLSTFL )*2,
     +			stat ( LLSTFL )*2, stnnam ( LLSTFL )*32,
     +			tbchrs ( LLSTFL )*20
	INTEGER		isnm ( LLSTFL ), ispri ( LLSTFL )
	REAL		slat ( LLSTFL ), slon ( LLSTFL ),
     +			selv ( LLSTFL )
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for the requested upper air data file.
C
	CALL FL_INQR ( filnam, exist, newfil, ier )
	IF  ( exist )  THEN
C
C*	    Open the unmerged upper air data file.
C
	    CALL SN_OPNR ( filnam, iflno, ifsc, nparm, parms, ivert,
     +			   mrgdat, iret )
C
C*	    If the file is merged check the parameter list.
C*	    If the file is unmerged or the parameter list doesn't match,
C*	    create a new file with an 'A' at the end of the file name.
C
	    IF  ( mrgdat )  THEN
		RETURN
	      ELSE
		CALL SN_CLOS ( iflno, ier )
		CALL ST_LSTR ( filnam, lenf, ier )
		filnam(lenf+1:lenf+1) = 'A'
	    END IF
	ELSE
C
C*	    Read the station file.
C
	    CALL DC_STNS ( stnfil, stid, isnm, stnnam, stat, coun,
     +			   slat, slon, selv, ispri, tbchrs, nstn, ierr )
	    IF  ( ierr .ne. 0 )  THEN
		CALL DC_WLOG ( 0, 'DC', ierr, ' ', ier )
		nstn = 300
	    END IF
C
C*	    Create the merged upper air data file. Use the number of
C*	    stations found in the table + IADSTN extra stations.
C
	    maxstn = nstn + iadstn
	    CALL SN_CRFP ( filnam, prmfil, iflsrc, maxstn, maxtim,
     +			   .true., iflno, nparm, parms, pkflg, iret )
C
C*	    Add all of the stations to the data file.
C
	    IF  ( ( ierr .eq. 0 ) .and. ( iret .eq. 0 ) )  THEN
		CALL SN_ASTN  ( iflno, nstn, stid, isnm, slat, slon,
     +				selv, stat, coun, nadd, ier )
	    END IF
	END IF
C*
	RETURN
	END
