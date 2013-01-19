        SUBROUTINE GG_WFPS ( inum, dattim, systim, nmwcn, wcnty, iret)
C************************************************************************
C* GG_WFPS                                                              *
C*                                                                      *
C* This subroutine returns an array of WCN FIPS codes			*
C*                                                                      *
C* GG_WFPS ( INUM, DATTIM, SYSTIM, NMWCN, WCNTY, IRET )      		*
C*                                                                      *
C* Input parameters:                                                    *
C*        INUM          INTEGER         Current watch number            *
C*        DATTIM        CHAR*           User specified time             *
C*        SYSTIM        CHAR*           System time                     *
C*                                                                      *
C* Output parameters:                                                   *
C*        NMCNTY        INTEGER         Number of WCN FIPS in the array	*
C*        WCNTY (NCNTY) INTEGER		WCN FIPS codes array           	*
C*        IRET          INTEGER         Return code                     *
C*                                       8 = Dim of cnties/wfocty exceed*
C*                                       7 = Dim. of sacod exceeded     *       
C*                                       2 = User time=end of watch time*
C*                                       0 = Normal return              *
C*                                      -1 = No active counties         *
C*                                      -3 = Bad input string in TI_STAN*
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/SAIC 10/04                                           *
C* A. Hardy/NCEP	03/05		Added action code array	GG_WACT *
C* A. Hardy/NCEP	03/05		Added ircntm to GG_WACT		*
C* S. Schotz/NCEP	01/06		Corrected extraction of FIPs 	* 
C*					for marine zones		*
C* F. J. Yen/NCEP	03/07		Replaced dimension LLSTFL with	*
C*					MAX_CNTY. (fix for #44 and #46)	*
C************************************************************************
       INCLUDE         'GEMPRM.PRM'
C*
       CHARACTER*(*)   dattim, systim 
       INTEGER         wcnty(*)
C* -------------------------------------------------------------------
       CHARACTER       strtim*20, stptim*20, type*3, sacod(100)*4,
     +                 cnties(MAX_CNTY)*128, wfocty(MAX_CNTY)*128,
     +		       ircntm*20
       INTEGER         numarr
       LOGICAL         etmchg
C---------------------------------------------------------------------
       systim =' '
       iret   = 0
       strtim = ' '
       stptim = ' '
       ircntm = ' '
       etmchg   = .false.
C
       CALL GG_WACT ( inum, dattim, systim, type, strtim, stptim, 
     +                ncnty, cnties, wfocty, knt, etmchg, itest, 
     +                sacod, numarr, ircntm, iret )
C
       nmwcn = ncnty
C
C*  Extract county/marine zone FIPS code from the data file record 
C*  contained in the cnties array.  Note that FIPS is contained in the 
C*  characters 10-15.
C 
       DO ii = 1, nmwcn
          CALL ST_NUMB  ( cnties(ii) (10:15), wcnty(ii), ier )
       END DO 
C*
       RETURN
       END        
