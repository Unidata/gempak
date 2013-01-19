	SUBROUTINE SC_GEMP  ( lunf, ihhmm, cprms, parms, nparm, numprm, 
     +                        imnem, iret )
C************************************************************************
C* SC_GEMP                                                            	*
C*                                                                      *
C* This subroutine writes a decoded SCD report to a GEMPAK file.	*
C* 								        *
C* SC_GEMP  ( LUNF, IHHMM, CPRMS, PARMS, NPARM, NUMPRM, IMNEM, IRET )   *
C*								        *
C* Input parameters:						        *
C*	LUNF		INTEGER  	Surface file number             *
C*	IHHMM		INTEGER		Station time (HHMM)	        *
C*	CPRMS (*)	CHAR*		GEMPAK parms chosen for output  *
C*	PARMS (*)       CHAR*		Parameter list                  *
C*	NPARM		INTEGER		Number of parameters            *
C*	NUMPRM		INTEGER		Count of chosen GEMPAK parms    *
C*	IMNEM (*)	INTEGER		Mnemonic positions in intf tbl  *
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code                     *
C*					  0 = normal return             *
C*					 -3 = file not open	        *
C*					 -7 = location not set	        *
C*					-12 = DM error	 	        *
C**								        *
C* Log:							        	*
C* D. Kidwell/NCEP	 3/97	Based on MT_GEMP                        *
C* A. Hardy/GSC         12/97   Added interface and reworked output     *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE         'sccmn.cmn'
C*
	INTEGER 	imnem (*)
	CHARACTER*(*)	cprms (*), parms (*)
C*
	REAL		rdata (MMPARM)
	CHARACTER 	wchr*32
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize the GEMPAK array first.
C
	DO ii = 1, MMPARM
	    rdata ( ii ) = RMISSD
	END DO
C
C*      Locate the GEMPAK parameters within the packing table.
C
	DO  k = 1, numprm

C
C*          Check for the parameter in the list.
C
	    CALL ST_FIND  ( cprms ( k ), parms, nparm, ilc, ier )
C
	    IF ( ilc .eq. 0 )  THEN
		CALL DC_WLOG ( 2, 'DCSCD', -3, cprms(k), ier )
	      ELSE
C
C*              Retrieve the requested data and store it into the
C*              GEMPAK array.
C
         	IF ( ( imnem ( k ) .ne. 0 ) .and.
     +               ( cprms ( k ) .ne. 'WNUM' ) ) THEN
C
C*		   Store real value from interface to GEMPAK
C
         	   rdata ( ilc ) = rivals ( imnem ( k ) )
C
		 ELSE IF ( imnem ( k ) .ne. 0 ) THEN
C
C*                 Build the weather code from up to 3 weather 
C*		   character strings.
C
		   wchr = civals ( icwcod (1) ) // 
     +                    civals ( icwcod (2) ) // 
     +                    civals ( icwcod (3) )
                   CALL ST_RMBL ( wchr, wchr, lenr, ier )
                   IF ( wchr .ne. ' ' ) rdata ( ilc ) = PT_WNMT ( wchr )
C
C*                 Weather code is not valid.
C
	           IF ( rdata ( ilc ) .eq. 0 ) rdata ( ilc ) = RMISSD
		 END IF
	     END IF
	END DO
C
C*	Write the data to a surface data file.
C
	CALL SF_WDAT ( lunf, ihhmm, rdata, iret )
	IF ( iret .ne. 0 ) THEN
	    CALL DC_WLOG ( 2, 'SF', iret, ' ', ierwlg )
	END IF
C*
	RETURN
	END
