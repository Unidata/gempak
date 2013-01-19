	SUBROUTINE SC_WEA3  ( string, iwea, strwea, iret ) 
C************************************************************************
C* SC_WEA3                                                              *
C*                                                                      *
C* This subroutine identifies a valid weather phenomena group.  The     *
C* group is later translated into a GEMPAK weather number by PT_WNMT.   *
C* 								        *
C* SC_WEA3  ( STRING, IWEA, STRWEA, IRET )                              *
C*								        *
C* Input parameters: 						        *
C*      STRING		CHAR*		Possible weather phenomena group*
C*	IWEA		INTEGER		Index for output array          *
C*								        *
C* Output parameters:						        *
C*	STRWEA (*)	CHAR*		Present weather string array    *
C*      RIVALS(IRNPWX)  REAL            Number of present weathr strings*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return       	*
C*	                                 32 = unrecognized WX code 	*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP 	 3/97   Based on MT_WEA3                        *
C* A. Hardy/GSC         12/97   Added sccmn.cmn for interface           *
C* A. Hardy/GSC          5/98   Extracted code and added DC_WTHR call   *
C* D. Kidwell/NCEP 	 6/98   Corrected prologue                      *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE         'sccmn.cmn'
C*
	CHARACTER*(*) 	string, strwea (*)
C*
	CHARACTER       outstr*9
	LOGICAL         wxfnd
C*
C------------------------------------------------------------------------
	iret  = 0
	wxfnd = .false.
C
C*      Check for length of weather group string.	
C
	CALL ST_LSTR ( string, lens, iret )
C
C*      Send string to be validated as a weather code.
C
	CALL DC_WTHR ( string, lens, wxfnd, outstr, iret )
C
C*      If a code was found, then save the group in the array.
C
        IF ( wxfnd ) THEN
	    strwea ( iwea ) = outstr
	    rivals ( irnpwx ) = iwea
        END IF 
C*
	RETURN
	END
