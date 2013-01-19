	SUBROUTINE GR_OPEN  ( gdfile, wrtflg, gdcur, igdfln, lasttm,
     +			       anl, rnav, numgrd, maxgrd, newfil, iret )
C************************************************************************
C* GR_OPEN								*
C*									*
C* This subroutine opens a grid file.  The input file name is first 	*
C* compared to the name of the current open grid file.  If it is a new	*
C* file, the old file is closed and the new file is opened.  If the new	*
C* open is successful, GDCUR is updated.				*
C*									*
C* Note that this subroutine does not set the navigation information	*
C* in GEMPLT or initialize the DG package.				*
C*									*
C* GR_OPEN  ( GDFILE, WRTFLG, GDCUR, IGDFLN, LASTTM, ANL, RNAV,		*
C*            NUMGRD, MAXGRD, NEWFIL, IRET )				*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name 			*
C*	WRTFLG		LOGICAL		Write access flag		*
C*									*
C* Input and output parameters:						*
C*	GDCUR		CHAR*		Current file name		*
C*	IGDFLN		INTEGER		Grid file number 		*
C*									*
C* Output parameters:							*
C*	LASTTM		CHAR*		Last time in file		*
C*	ANL  (*)	REAL		Analysis block			*
C*	RNAV (*)	REAL		Navigation block		*
C*	NUMGRD		INTEGER		Number of grids in file		*
C*	MAXGRD		INTEGER		Maximum number of grids		*
C*	NEWFIL		LOGICAL		New file flag			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file open error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85						*
C* M. desJardins/GSFC	 7/87	Changed to GEMPAK4 GD library		*
C* M. desJardins/GSFC	 7/87	Corrected error handling		*
C* M. desJardins/GSFC	 1/88	Added grid forecast time		*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	11/88	Added NEWFIL				*
C* K. Brill/GSC		09/89   Documentation for multiple files	*
C* S. Jacobs/NCEP	10/95	Changed len to lens			*
C************************************************************************
	CHARACTER*(*)	gdfile, gdcur, lasttm
	REAL		rnav (*), anl (*) 
	LOGICAL		wrtflg, newfil
C*
	CHARACTER	fff*20
C------------------------------------------------------------------------
C*	Check to see if this is the same file.
C
	iret   = 0
	newfil = .false.
	CALL ST_LSTR  ( gdcur, lens, ier )
	IF  ( ( gdfile .ne. gdcur ) .or. ( lens .eq. 0 ) )  THEN
	    newfil = .true.
C
C*	    Close old file, if necessary.
C
	    IF  ( lens .ne. 0 )  CALL GD_CLOS  ( igdfln, ier )
C
C*	    Open new file.
C
	    CALL GD_OPNF  ( gdfile, wrtflg, igdfln, navsz, rnav, iasz, 
     +			    anl, ihdrsz, maxgrd, ier )
C
C*	    Check error code.
C
	    IF  ( ier .ne. 0 )  THEN
		iret   = -4
		igdfln = 0
		gdcur  = ' '
	      ELSE
C
C*	      Get the last time in the grid file.
C
		CALL GD_NGRD  ( igdfln, numgrd, fff, lasttm, ier )
C
C*		Update current grid file name.
C
		gdcur = gdfile
	    END IF
	END IF
C*
	RETURN
	END
