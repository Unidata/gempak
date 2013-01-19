	SUBROUTINE DC_OSFC ( filnam, iflsrc, stnfil, iadstn, maxtim, 
     +			     iflno, nparm, parms, iret )
C************************************************************************
C* DC_OSFC								*
C*									*
C* This routine opens a surface land file.				*
C*									*
C* DC_OSFC ( FILNAM, IFLSRC, STNFIL, IADSTN, MAXTIM, IFLNO, NPARM,	*
C*	     PARMS, IRET )					*
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
C* S. Jacobs/NCEP	10/95	Added missing variable declarations	*
C* D. Keiser/GSC	 4/96	Added iflsrc to calling sequence	*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C* A. Hardy/GSC		 3/99	Added priority parameter to DC_STNS     *
C* F. J. Yen/NCEP	 1/01	Updated call to DC_STNS			*	
C************************************************************************
	INCLUDE		'dccmn.cmn'
C*
	CHARACTER*(*)	filnam, stnfil, parms (*)
C*
	CHARACTER	newfil*132
	LOGICAL		exist, pkflg
C*
	CHARACTER	stid ( LLSTFL )*8, stat ( LLSTFL )*2,
     +			coun ( LLSTFL )*2, stnnam ( LLSTFL )*32,
     +			tbchrs ( LLSTFL )*20
	INTEGER		isnm ( LLSTFL ), ispri ( LLSTFL )
	REAL		slat ( LLSTFL ), slon ( LLSTFL ),
     +			selv ( LLSTFL )
C------------------------------------------------------------------------
	iret = 0
C*
	CALL FL_INQR ( filnam, exist, newfil, ier )
	IF  ( exist )  THEN
C
C*	    Open the file if it exists.
C
	    CALL SF_OPNR ( filnam, iflno, ifsc, nparm, parms, iret )
	ELSE
C
C*	    Otherwise, create the file.
C*
C*	    Read the station file.
C
	    CALL DC_STNS ( stnfil, stid, isnm, stnnam, stat, coun,
     +			   slat, slon, selv, ispri, tbchrs, nstn, ierr )
	    IF  ( ierr .ne. 0 )  THEN
		CALL DC_WLOG ( 0, 'DC', ierr, ' ', ier )
		nstn = 1990
	    END IF
C
C*	    Create the land surface file. Use the number of stations
C*	    found in the table + IADSTN extra.
C
	    maxstn = nstn + iadstn
	    CALL SF_CRFP ( filnam, prmfil, iflsrc, maxstn, maxtim,
     +			   .true., iflno, nparm, parms, pkflg, iret )
C
C*	    Add all of the stations to the data file.
C
	    IF  ( ( ierr .eq. 0 ) .and. ( iret .eq. 0 ) )  THEN
		CALL SF_ASTN  ( iflno, nstn, stid, isnm, slat, slon,
     +				selv, stat, coun, ispri, nadd, ier )
	    END IF
	END IF
C*
	RETURN
	END
