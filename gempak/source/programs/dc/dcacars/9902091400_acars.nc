CDF  	    
      recNum         enLen         rptStationLen         AirportIdLen      dateLen             title         /ACARS data, encrypted tail numbers, version 2.1    version       @ ÌÌÌÌÌÍ      %   missingInputMinutes              	long_name         missing minutes of input data      units         minutes         L   minDate                	long_name         minimum observation date   units         human-readable min date/time         P   maxDate                	long_name         maximum observation date   units         human-readable max date/time         p   minSecs              	long_name         minimum observation time   units         seconds since 1970-1-1 00:00:00            maxSecs              	long_name         maximum observation time   units         seconds since 1970-1-1 00:00:00            latitude                units         degree_N   valid_range           B´     
_FillValue        GÃO            	longitude                   units         degree_E   valid_range       Ã4  C4     
_FillValue        GÃO        ¤   altitude                units         meter (pressure altitude, msl)     
_FillValue        GÃO   	valid_min         ÂÈ          ¨   timeObs                 	long_name         time of observation    units         seconds since 1970-1-1 00:00:00    
_FillValue                        ¬   temperature                 units         Kelvin     
_FillValue        GÃO        ´   windDir                 	long_name         Wind Direction     units         degree_true    
_FillValue        GÃO   valid_range           C³þ¸        ¸   	windSpeed                   	long_name         
Wind Speed     units         	meter/sec      
_FillValue        GÃO   	valid_min                     ¼   heading                 	long_name         "heading of flight path over ground     units         degree_true    
_FillValue        GÃO   valid_range           C³þ¸        À   mach                	long_name         mach number (0-1)           Ä   waterVaporMR                	long_name         water vapor mixing ratio   units         none   
_FillValue        ¿          È   dewpoint                	long_name         Dew Point (Celsius)    units         degrees Celsius    
_FillValue        GÃO        Ì   rh_probe                	long_name         0RH in probe (0-1), w/ Vaisala correction un-done   
_FillValue                    Ð   medTurbulence                   	long_name         Median eddy dissipation rate   units         m^(2/3) s^-1   
_FillValue        Á×
        Ô   maxTurbulence                   	long_name         Maximum eddy dissipation rate      units         m^(2/3) s^-1   
_FillValue        Á×
        Ø   	vertAccel                   	long_name         peak vertical acceleration     units         meters/sec/sec     valid_range       ÀXù@Xù   
_FillValue        GÃO        Ü   en_tailNumber                      	long_name         FSL-encrypted tail number           à   dataDescriptor                  	long_name         AWIPS-type data descriptor     units           good= R  or  T , bad= X           ì   	errorType                   units           temp=T, wind=W, both=B            ð   rollFlag                	long_name         Aircraft roll angle flag       units         "  G = < 5 degrees, B = > 5 degrees          ô   waterVaporQC                	long_name         %water vapor mixing ratio QC character           ø   interpolatedTime                	long_name         /UPS ascent/descent time interpolation indicator    units         "r for raw data, i for interpolated          ü   interpolatedLL                  	long_name         2UPS ascent/descent lat&lon interpolation indicator     units         "r for raw data, i for interpolated              	tempError                   units         "good= p ,bad= W  or  C ,unknown= -             windDirError                units          good= p ,bad= B ,unknown= -           windSpeedError                  units         $  good= p ,bad= F  or  S ,unknown= -           
speedError                  	long_name         Aircraft ground speed error    units         $  good= p ,bad= F  or  S ,unknown= -           bounceError                 	long_name          Aircraft altitude variance error   units         "good= p ,bad= H  or  L ,unknown= -             correctedFlag                   	long_name         Corrected data indicator   units         r,l,T,f,or t   value_r       raw data   value_l       -lat/lon correction (other than interpolation)      value_T       temperature correction     value_f       $longitude and wind direction flipped   value_t       0obs time has been set to the report receipt time           
rptStation                     	long_name         Station reporting through      
_FillValue                       timeReceived                	long_name         (time data was received at ground station   units         seconds since 1970-1-1 00:00:00    
_FillValue                            origAirport                    	long_name         Originating Airport    	valueUNKN                	FillValue                     (   destAirport                    	long_name         Destination Airport    	valueUNKN                	FillValue                     0    Tue 09-Feb-1999 1400:00 UTC     Tue 09-Feb-1999 1459:57 UTC     AË`°   AË`&¶  B4©?ÂëÃûFAË`%   CV&fC_  B;AþGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00417    R.N-rrppp--rBOI AË`%                   B\ÂÏ\F9¼ÍAË`#ß  CZ&fC  B+GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00001    R.N-rrpppp-rGSO AË`#Ê   KPH     LKA     B(öÂù,F9BáAË`$v   C[YC BíÌCnôzGÃO¿  GÃOGÃOÁ×
Á×
    FSL00001    T.N-rrppppprGSO AË`$`   KPH     LKA     B¿&ÂF9BáAË`%  CY&fC B$JCnk¿GÃO¿  GÃOGÃOÁ×
Á×
    FSL00001    T.N-rrppppprGSO AË`$ö   KPH     LKA     B.Â1~F9BáAË`%£   C[&fC B+CiÈjGÃO¿  GÃOGÃOÁ×
Á×
    FSL00001    T.N-rrppppprRDU AË`%   KPH     LKA     B§Â DDF9¼ÍAË`&9  CYÙC  BüChÖPGÃO¿  GÃOGÃOÁ×
Á×
    FSL00001    T.N-rrppppprCLT AË`&"   KPH     LKA     A¯¼Bã7ÜEçAË` 
   C¬ÍC  A(öC|ÿGÃO¿  GÃOGÃOÁ×
Á×
    FSL00002    T.N-rrppppprSZX AË` d   VIDP    VHHH    A®"ÑBãWÜE½AË` (   CÓ3C A^=qCR~?)ä ¿  GÃOGÃOÁ×
Á×
    FSL00002    T.N-rrppppprSZX AË` d   VIDP    VHHH    A¬úxBä·Ec	´AË`     CS3C  @æxBâæî>Nc¿  GÃOGÃOÁ×
Á×
    FSL00002    T.N-rrppppprSZX AË`!   VIDP    VHHH    A­V¡BäA E<µAË` Ü   CS3Cp  @öîïB|!> à¿  GÃOGÃOÁ×
Á×
    FSL00002    T.N-rrppppprSZX AË`!   VIDP    VHHH    A®íBäròEyAË` ú   C¹CP  @ÅòB3nB?	Ô¿  GÃOGÃOÁ×
Á×
    FSL00002    T.N-rrppppprSZX AË`!Ì   VIDP    VHHH    A¯ûJBãÜDà|6AË`!   CS3C%  AíÌC#¿>¿  GÃOGÃOÁ×
Á×
    FSL00002    R.N-rrpppp-rSZX AË`!Ì   VIDP    VHHH    Aù,`Â·èöF9¼ÍAË` %   C\YC  A¨¼ßCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00003    T.N-rrppppprIAH AË` (   KMC     OKD     Aú33Â¹:áF9BáAË` »  C]YC  A¬ÚtC´GÃO¿  GÃOGÃOÁ×
Á×
    FSL00003    T.N-rrppppprDFW AË` ¾   KMC     OKD     AüåÂºy,F9¼ÍAË`!R   C\&fC A¹33CkGÃO¿  GÃOGÃOÁ×
Á×
    FSL00003    T.N-rrppppprDFW AË`!T   KMC     OKD     Aý0Â»ÃjF9BáAË`!è   C\ÙC AµCNGÃO¿  GÃOGÃOÁ×
Á×
    FSL00003    T.N-rrppppprDFW AË`!ê   KMC     OKD     Aþ»¼Â½F9¼ÍAË`"~  C\&fC  A µC¤GÃO¿  GÃOGÃOÁ×
Á×
    FSL00003    R.N-rrpppp-rIAH AË`"   KMC     OKD     Aq_BÎúF&®ÈAË`"b   CdÙC;  A^=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    R.N-rrpppp-rKKC AË`"b   VHHH    VTBD    AlxBÌ=IF«þAË`#¬   CjYC   AíÌCÄâGÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprKKC AË`$   VHHH    VTBD    Aj6ãBÌ'ãFfAË`#Ê   Co¦fC  @(öCHGÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprKKC AË`$   VHHH    VTBD    AiXBËÿ¾FÇAË`#Ê   CuYBÎ  @ÖGCk<¸GÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprKKC AË`$   VHHH    VTBD    AhöBËÂEýáAË`#è   Cz¦fC  @æxCx¹ GÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprKKC AË`$   VHHH    VTBD    AgÇzBËEêÏpAË`$   C~¦fC,  @µCtg¶GÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprTKH AË`$`   VHHH    VTBD    AgÎBË`OE×þAË`$$   C,ÍBú  @fxCs^ÞGÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprTKH AË`$`   VHHH    VTBD    AfsBË5ÝEÄ4AË`$B   C,ÍB  @ÖGCs×ÆGÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprTKH AË`$`   VHHH    VTBD    Ae®æBË:E°ÙÆAË`$`   C,ÍBÞ  A(öCt&.GÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprTKH AË`$`   VHHH    VTBD    AdæBÊÏÒEtAË`$~   CÓ3B°  @æxCtGÃO¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprTKH AË`$ö   VHHH    VTBD    AdYKBÊ«QEõÉAË`$   CyBø  @öîïCt+[>~¯¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprTKH AË`%2   VHHH    VTBD    AcÌdBÊ_Em\îAË`$   CÓ3Bö  @ÖGCsã?	]Ñ¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprTKH AË`%2   VHHH    VTBD    Ad"hBÊ0¤EG&AË`$Ø   C3BÀ  @¤JCÈ>lh	¿  GÃOGÃOÁ×
Á×
    FSL00257    T.N-rrppppprTKH AË`%2   VHHH    VTBD    AejBÉâhE øöAË`%   C¹B  @öîïCÐ¡>æ¿  GÃOGÃOÁ×
Á×
    FSL00257    R.N-rrpppp-rTKH AË`%2   VHHH    VTBD    Bm:Â°®F&° AË`%m   CWÙC B+GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00006    R.N-rrpppp-rSTL AË`%n   KCV     GKL     B'AÂ±æfF&° AË`&  CXÙC B(öC%êGÃO¿  GÃOGÃOÁ×
Á×
    FSL00006    T.N-rrppppprSDF AË`&   KCV     GKL     BáHÂ³DDF&° AË`&   CYÙC BíÌC$&GÃO¿  GÃOGÃOÁ×
Á×
    FSL00006    T.N-rrppppprSTL AË`&   KCV     GKL     B	´èÂ¯wwF&6AË`$!   CX¦fC AaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00008    R.N-rrpppp-rATL AË`$$   KAT     LKS     B
kÂ°Ø¿F&° AË`$·   CX¦fC  AvîïC>(GÃO¿  GÃOGÃOÁ×
Á×
    FSL00008    T.N-rrppppprBNA AË`$º   KAT     LKS     B
Ü)Â²DDF&° AË`%M  CXÙC Ad CfGÃO¿  GÃOGÃOÁ×
Á×
    FSL00008    T.N-rrppppprBNA AË`%P   KAT     LKS     BDDÂ³¨öF&° AË`%ä   CZYC  A¨¼ßC%öGÃO¿  GÃOGÃOÁ×
Á×
    FSL00008    T.N-rrppppprBNA AË`%æ   KAT     LKS     BzáÂ°\)F9BáAË`ü   CZ&fC  AæxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00794    R.N-rrppp--rATL AË`ì   KAT     LKM     B|Â±UF9BáAË`   C\&fC  AÞ=qCwåGÃO¿  GÃOGÃOÁ×
Á×
    FSL00794    R.N-rrppp--rMEM AË`     KAT     LKM     A÷iÐÂúF/¼{AË`    CTÙC A*ApGÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprMCO AË` 
   KFL     LKL     AüQìÂÂF/¼{AË` ­  CTÙC Ad A IGÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprCHS AË`     KFL     LKL     B ¸ÂcF/¼{AË`!D   CT&fC Ad A$ªàGÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprRDU AË`!6   KFL     LKL     B0ÂO\F/¼{AË`!Ú  CSÙC  A°ø	A&¢GÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprRDU AË`!Ì   KFL     LKL     B§Â0F/¼{AË`"p  CT&fC AÅòA$;XGÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprRDU AË`"b   KFL     LKL     BÂÝF/¼{AË`#   CSÙC AÖGA$=çGÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprGSO AË`"ø   KFL     LKL     B
|Â»¼F/¼{AË`#  CS¦fC Aê0@À­GÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprGSO AË`#   KFL     LKL     BòÂ¶F&6AË`$w  CX¦fC  Aû?$Ø«GÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprRDU AË`$`   KFL     LKL     BÂ³3F&6AË`%   CX¦fC B²¢?U<GÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprGSO AË`$ö   KFL     LKL     BüÂS F&6AË`%¤  CXÙC  B(öAnGÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprRDU AË`%   KFL     LKL     BkÂÅùF&6AË`&:  CX&fC  B"AÃ¨ÍGÃO¿  GÃOGÃOÁ×
Á×
    FSL00545    T.N-rrppppprGSO AË`&"   KFL     LKL     BÑìÂÜ¿F06fAË`$_   CT¦fC  Aê0GÃO?KC¿  GÃOGÃOÁ×
Á×
    FSL00856    R.N-rrpppp-rWMS AË`$B   KLA     SKA     BòYÂÚUUF06fAË`$õ  CTÙC Aê0B¯æ*?MOß¿  GÃOGÃOÁ×
Á×
    FSL00856    T.N-rrppppprWMS AË`$Ø   KLA     SKA     B§ÂØF06fAË`%   CTYC AòÑZB°?LD¿  GÃOGÃOÁ×
Á×
    FSL00856    T.N-rrppppprGNR AË`%n   KLA     SKA     B mÂÖÌÍF06fAË`&"  CT¦fC AæxB±¢?LD¿  GÃOGÃOÁ×
Á×
    FSL00856    T.N-rrppppprALS AË`&   KLA     SKA     BÂ±æfFBÉHAË`Á   CZ&fC A µBÅGGÃO¿  GÃOGÃOÁ×
Á×
    FSL00009    T.N-rrppppprMEM AË`Î   KDF     WKA     BÎÂ°G®FBÉHAË` W  CZÙC  A²¢B¬¶?N{¿  GÃOGÃOÁ×
Á×
    FSL00009    T.N-rrppppprMOB AË` F   KDF     WKA     BÂ¬¿EäAË`!é   CuYCq  AÐ7B¤.GÃO¿  GÃOGÃOÁ×
Á×
    FSL00009    R.N-rrpppp-rATL AË`!ê   KDF     WKA     BCiÃYÛF08×AË`°   C[ÙC  B$JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00958    R.N-rrppp--rXXC AË`°   RJAA    KSEA    AðèÂ¨6F/¼{AË`ê   CV&fC AaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00011    R.N-rrppp--rMCO AË`Î   KAT     LKR     AçbýÂ¦®ïF/¼{AË`!  CWÙC  AíÌCFGÃO¿  GÃOGÃOÁ×
Á×
    FSL00011    R.N-rrppp--rMCO AË` ú   KAT     LKR     B_¸RÂ$£×F&° AË`%:  CU&fC&  AûGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00012    R.N-rrppp--rXXD AË`%2                   B0¸Ás33F&° AË`!?   CT&fC£ B"GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00013    R.N-rrppp--rXXD AË`!6                   B4  ÁÂF&° AË`$Ã   CU&fC AÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00013    R.N-rrppp--rXXD AË`$º                   Aý(öÂº
=F9¼ÍAË` :   C[ÙC  AÅòCàGÃO¿  GÃOGÃOÁ×
Á×
    FSL00682    T.N-rrppppprDFW AË` (   KTP     AKD     Aþ0Â»LÍF9¼ÍAË` Ð   C[ÙC  A°ø	CæXGÃO¿  GÃOGÃOÁ×
Á×
    FSL00682    T.N-rrppppprDFW AË` ¾   KTP     AKD     AÿwwÂ¼­:F9BáAË`!f  C\YC AFCfGÃO¿  GÃOGÃOÁ×
Á×
    FSL00682    R.N-rrpppp-rIAH AË`!T   KTP     AKD     BÂÂ°þKF9¼ÍAË`#&   C`YC B(¼ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00772    R.N-rrpppp-rSTL AË`#   KCV     GKS     B6Â²^¸F9¼ÍAË`#¼  CaYC B*ËªC¬GÃO¿  GÃOGÃOÁ×
Á×
    FSL00772    T.N-rrppppprSTL AË`#¬   KCV     GKS     BÂ³À F9BáAË`$S   C]¦fC  B&®CüGÃO¿  GÃOGÃOÁ×
Á×
    FSL00772    T.N-rrppppprSTL AË`$B   KCV     GKS     BÜ)Âµ#×F9¼ÍAË`$é  C[¦fC  B0ø	C«GÃO¿  GÃOGÃOÁ×
Á×
    FSL00772    T.N-rrppppprSTL AË`$Ø   KCV     GKS     B OÂ¶F9¼ÍAË`%   C\&fC  B=PÈC¬GÃO¿  GÃOGÃOÁ×
Á×
    FSL00772    T.N-rrppppprSTL AË`%n   KCV     GKS     B UUÂ·ÝÞF9¼ÍAË`&   C[&fC  B0ø	CbfGÃO¿  GÃOGÃOÁ×
Á×
    FSL00772    T.N-rrppppprSTL AË`&   KCV     GKS     B òÂ¹<F9BáAË`&¬  C[YC B5C9GÃO¿  GÃOGÃOÁ×
Á×
    FSL00772    T.N-rrppppprSTL AË`&   KCV     GKS     B0Â ÆF9¼ÍAË` 
  CYÙC  BÁlGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00790    R.N-rrpppp-rGSO AË` 
   KLG     AKA     BzáÂ¡¢ýF9BáAË` ¡   CXYC AòÑZChëGÃO¿  GÃOGÃOÁ×
Á×
    FSL00790    T.N-rrppppprGSO AË`     KLG     AKA     BØ¿Â¢³3F9¼ÍAË`!7   CY¦fC  Aî³ÅChwGÃO¿  GÃOGÃOÁ×
Á×
    FSL00790    T.N-rrppppprRDU AË`!6   KLG     AKA     B33Â£ÂF9BáAË`!Í  CX&fC  AæxCh'(GÃO¿  GÃOGÃOÁ×
Á×
    FSL00790    R.N-rrpppp-rATL AË`!Ì   KLG     AKA     AÞ(öÂ¤ÚF¯®AË`&Q   C_ÙC£  @¤JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00521    R.N-rrppp--rMIA AË`&@   KAT     LKF     B»¼ÂÙF9¼ÍAË`"0   C\YC B£×GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00260    R.N-rrpppp-rALB AË`"&   KBO     SKP     Bð¤Â×åF9¼ÍAË`"Æ  C[&fC B²¢Ccë
GÃO¿  GÃOGÃOÁ×
Á×
    FSL00260    T.N-rrppppprALB AË`"¼   KBO     SKP     B¸ÂÕUF9¼ÍAË`#]   CZ¦fC  Aÿ*CcfGÃO¿  GÃOGÃOÁ×
Á×
    FSL00260    T.N-rrppppprALB AË`#R   KBO     SKP     BBÂÔ{F9¼ÍAË`#ó  CZÙC  B²¢CbøßGÃO¿  GÃOGÃOÁ×
Á×
    FSL00260    T.N-rrppppprIAD AË`#è   KBO     SKP     BaHÂÐ7F9¼ÍAË`$   C\YC Aâ[CbGöGÃO¿  GÃOGÃOÁ×
Á×
    FSL00260    T.N-rrppppprBWI AË`$~   KBO     SKP     B8RÂ
F9¼ÍAË`%   CY¦fC B²¢CWµÓGÃO¿  GÃOGÃOÁ×
Á×
    FSL00260    T.N-rrppppprGSO AË`%   KBO     SKP     B÷wÂ<F9BáAË`%¶  CXÙC  AòÑZCQ×ÚGÃO¿  GÃOGÃOÁ×
Á×
    FSL00260    T.N-rrppppprGSO AË`%ª   KBO     SKP     BÀÚÂÜ)F9¼ÍAË`&M   CZYC  Aî³ÅCQa+GÃO¿  GÃOGÃOÁ×
Á×
    FSL00260    T.N-rrppppprRDU AË`&@   KBO     SKP     AôwwÂ§¸RF&° AË`%i   CXÙC  A^=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00649    R.N-rrpppp-rATL AË`%P   KTP     AKA     Aø±~Â¨záF&° AË`%ÿ  CXÙC AvîïC¢"ÚGÃO¿  GÃOGÃOÁ×
Á×
    FSL00649    T.N-rrppppprATL AË`&   KTP     AKA     AüîïÂ©<F&6AË`&  CX&fC  A¬ÚtC¢=GÃO¿  GÃOGÃOÁ×
Á×
    FSL00649    R.N-rrpppp-rATL AË`&   KTP     AKA     BX¿Â¯[OF&° AË`"e  CX¦fC  AÖGGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00016    R.N-rrpppp-rATL AË`"b   KAT     LKL     BµÂ°¹,F&6AË`"ü   CYYC AµCs}GÃO¿  GÃOGÃOÁ×
Á×
    FSL00016    T.N-rrppppprBNA AË`"ø   KAT     LKL     BâýÂ²!HF&6AË`#   CY¦fC A¨¼ßCÆGÃO¿  GÃOGÃOÁ×
Á×
    FSL00016    T.N-rrppppprBNA AË`#   KAT     LKL     BÓ Â³\F&° AË`$(  CZ&fC  A(öCeFGÃO¿  GÃOGÃOÁ×
Á×
    FSL00016    T.N-rrppppprDFW AË`$$   KAT     LKL     BÎÂµùF&6AË`$¿   CY&fC  A^=qCÍGÃO¿  GÃOGÃOÁ×
Á×     FSL00016    T.N-rrppppprDFW AË`$º   KAT     LKL     BÝÞÂ¶jF&6AË`%U  CYÙC  A*CÌGÃO¿  GÃOGÃOÁ×
Á×
    FSL00016    T.N-rrppppprDFW AË`%P   KAT     LKL     Bí:Â·û¼F&6AË`%ì   CY¦fC AvîïCÎGÃO¿  GÃOGÃOÁ×
Á×
    FSL00016    T.N-rrppppprMEM AË`%æ   KAT     LKL     B÷wÂ¹q~F&° AË`&  CYYC  Ad CcGÃO¿  GÃOGÃOÁ×
Á×
    FSL00016    T.N-rrppppprDFW AË`&|   KAT     LKL     BòYÂ¶J=F9¼ÍAË`è  CZYC A¹33Cx4GÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprMEM AË`ì   KMC     OKS     B mÂ·F9BáAË`    C[¦fC  AÅòCæäGÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprMEM AË`    KMC     OKS     B	?&Â¸¼F9¼ÍAË`!  C[¦fC AÉ©CD;GÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprTUL AË`!   KMC     OKS     B
ZtÂ¹öF9¼ÍAË`!¬   CZ&fC  AÅòC#"GÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprDFW AË`!®   KMC     OKS     BtÂ»6F9BáAË`"B   CY¦fC  AÁn]CßSGÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprDFW AË`"D   KMC     OKS     BÔÂ¼uÃF9BáAË`"Ø  C[YC  AöîïC¤GÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprDFW AË`"Ú   KMC     OKS     BÆÂ½°¤F9¼ÍAË`#o   C[&fC  AÍÇCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprDFW AË`#p   KMC     OKS     B£×Â¾ôF9¼ÍAË`$  C]¦fC  A¹33CpüGÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprDFW AË`$   KMC     OKS     B¸RÂÀDDF9¼ÍAË`$   C]¦fC  AÉ©C,LGÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprDFW AË`$   KMC     OKS     BºÂÁ®F9BáAË`%2   C]YC  Aî³ÅCÛìGÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprDFW AË`%2   KMC     OKS     B»¼ÂÂÐ7F9¼ÍAË`%È  C]¦fC AöîïC´GÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprDFW AË`%È   KMC     OKS     B¯ÉÂÄìF9¼ÍAË`&_   C[ÙC BUUCb+GÃO¿  GÃOGÃOÁ×
Á×
    FSL00345    T.N-rrppppprDFW AË`&^   KMC     OKS     BbýÂF9BáAË` :  C[ÙC Aÿ*GÃO?NÙ¿  GÃOGÃOÁ×
Á×
    FSL00017    R.N-rrpppp-rBWI AË` F   KLG     AKP     BÂ%F9BáAË` Ñ  CY&fC  AûCP3?P Å¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprRDU AË` Ü   KLG     AKP     B×
Â½qF9BáAË`!h  CX&fC AòÑZCO±?NÙ¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprRDU AË`!r   KLG     AKP     BÂT{F9BáAË`!ÿ  CXYC AæxCOkµ?Oß;¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprACY AË`"   KLG     AKP     BBÂèöF9BáAË`"  CVÙC AÚÜCN¯¥?P£×¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprGSO AË`"   KLG     AKP     BõÃÂ|F9BáAË`#-  CVYC AÚÜCN¹?Q©ü¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprGSO AË`#4   KLG     AKP     B	®Â
=F9BáAË`#Ã  CTÙC  Aê0CMá?Që¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprRDU AË`#Ê   KLG     AKP     Bd±Â=F9BáAË`$Z  CSYC  AÚÜCKî?På`¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprCHS AË`$`   KLG     AKP     BOÂF9¼ÍAË`$ñ  CR¦fC AÖGCJÃN?P£×¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprRDU AË`$ö   KLG     AKP     B½qÂ]F9¼ÍAË`%  CR&fC AÉ©CD*Ä?NÙ¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprRDU AË`%   KLG     AKP     B \)Â´èF9¼ÍAË`&  CSYC  AÅòCDï?N¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprRDU AË`&"   KLG     AKP     AûèÂÍF9BáAË`&¶  CU¦fC A¬ÚtCCéGÃO¿  GÃOGÃOÁ×
Á×
    FSL00017    T.N-rrppppprMYR AË`&¸   KLG     AKP     BÝÞÂØÆF06fAË`$ä  CT&fC  AÍÇGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00018    R.N-rrpppp-rABQ AË`$Ø   KPH     XKA     B	aHÂÖffF/¼{AË`%|   CT&fC  Aâ[B¢3GÃO¿  GÃOGÃOÁ×
Á×
    FSL00018    T.N-rrppppprELP AË`%n   KPH     XKA     B	Ü)ÂÔ»¼F06fAË`&  CT¦fC AÁn]B£¢GÃO¿  GÃOGÃOÁ×
Á×
    FSL00018    T.N-rrppppprABQ AË`&   KPH     XKA     B
NÂÓÆF06fAË`&©  CTYC  A¹33B¤¯çGÃO¿  GÃOGÃOÁ×
Á×
    FSL00018    T.N-rrppppprABQ AË`&   KPH     XKA     B §ÂÝ¨F£3AË`ï   CbYC Bl¤úC\ÈGÃO¿  GÃOGÃOÁ×
Á×
    FSL00460    R.N-rrpppp-rRKS AË`ì   KDE     NKS     AöÂ¨ÂF¯®AË`"2  C^ÙC AÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00020    R.N-rrppp--rATL AË`"&   KAT     LKT     AñÂ¨G®F)AË`"Ê  C_&fC AfxCNGÃO¿  GÃOGÃOÁ×
Á×
    FSL00020    R.N-rrppp--rATL AË`"¼   KAT     LKT     B'Â¯ÌÍDz AË`&"   CyC  Ad GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00489    R.N-rrpppp-rMKE AË`&   ORD     ATL     B'DDÂ¯ÌÍD×¯!AË`&@   CyC AaC4e>¿ ¿  GÃOGÃOÁ×
Á×
    FSL00489    T.N-rrppppprMKE AË`&   ORD     ATL     B'  Â¯ÌÍE¹_AË`&^   C3C  AíÌC4eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00489    T.N-rrppppprMKE AË`&   ORD     ATL     B&ª«Â¯ÌÍEIßAË`&|   C,ÍC AÞ=qC4eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00489    T.N-rrppppprMKE AË`&   ORD     ATL     B&DDÂ¯»¼EpZAË`&   CÓ3C  AÖGC!ÞGÃO¿  GÃOGÃOÁ×
Á×
    FSL00489    T.N-rrppppprMKE AË`'   ORD     ATL     B3Â¹E¤ý÷AË`#¬   CxÙC B£×GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00021    R.N-rrpppp-rORD AË`%È   MSP     ORD     B1Â·UUEÚì0AË`$`   Cj¦fC  B"Bâ¯-GÃO¿  GÃOGÃOÁ×
Á×
    FSL00021    T.N-rrppppprORD AË`%È   MSP     ORD     B0""Âµ¢"EÚî AË`%   Ck&fC  B*ËªBâ¯bGÃO¿  GÃOGÃOÁ×
Á×
    FSL00021    T.N-rrppppprORD AË`%È   MSP     ORD     B.  Â´ffEÚäßAË`%È   Cn¦fC BMÇCÝjGÃO¿  GÃOGÃOÁ×
Á×
    FSL00021    T.N-rrppppprORD AË`%È   MSP     ORD     B'Â¯»¼D}iAË`&   C3C A¤JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00022    R.N-rrpppp-rORD AË`&|   ORD     EWR     B'Â¯DÉLAË`&"   C9C AFB´e?ïS¿  GÃOGÃOÁ×
Á×
    FSL00022    T.N-rrppppprORD AË`&|   ORD     EWR     B'Â¯LÍE"ýçAË`&@   C,ÍC  A(öB´eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00022    T.N-rrppppprORD AË`&|   ORD     EWR     B'ª«Â¯EGü¥AË`&^   C3C AÑä±B£ÀGÃO¿  GÃOGÃOÁ×
Á×
    FSL00022    T.N-rrppppprORD AË`&|   ORD     EWR     B'»¼Â®ÄDEg¡AË`&|   ClÍC Aê0B§UGÃO¿  GÃOGÃOÁ×
Á×
    FSL00022    T.N-rrppppprIND AË`&ô   ORD     EWR     B'»¼Â®wwEH¹AË`&   C¬ÍC  Aî³ÅB´eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00022    T.N-rrppppprIND AË`&ô   ORD     EWR     BDDÂÂDD¿AË` ú   CyC  @(öGÃO>¥`B¿  GÃOGÃOÁ×
Á×
    FSL00589    R.N-rrpppp-rDFW AË`!r   DFW     MCI     BÂÂ""D¶E9AË`!   C9C AíÌCFkK>®¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`!r   DFW     MCI     B  ÂÂDDEyDAË`!6   C9C  AíÌCôÂ>°£×¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`!r   DFW     MCI     BÂÂffE(ShAË`!T   C¹C @æxCì>·ÎÙ¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`!r   DFW     MCI     BDDÂÂffEA4AË`!r   CùC  AíÌ    >0j¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`!ê   DFW     MCI     BÂÂUUET6AË`!   C9C AEòA®j¹>ù\3¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`!ê   DFW     MCI     B  ÂÂLÍEj$ÀAË`!®   CS3C  Ad A_ÑGÃO¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`!ê   DFW     MCI     BffÂÂLÍEÙAË`!Ì   C3C  A(ö    GÃO¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`!ê   DFW     MCI     B""ÂÂEøzAË`"Ú   Cp¦fC¡ Ad AÑGÃO¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`%   DFW     MCI     B
ª«ÂÁF
$AË`#   CgYC  A(öA¡ÐGÃO¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`%   DFW     MCI     BÂÀÌÍF
AË`$B   CfÙC A*BN8GÃO¿  GÃOGÃOÁ×
Á×
    FSL00589    T.N-rrppppprDFW AË`%   DFW     MCI     BwwÂ¿÷wF
$AË`%   CfYC  AíÌBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00589    R.N-rrpppp-rDFW AË`%   DFW     MCI     B#ÂffDIy	AË`°   CùCr  A$JC`ýt>  ¿  GÃOGÃOÁ×
Á×
    FSL00025    T.N-rrppppprLGA AË`°   YYZ     LGA     B#wwÂwwDÅAË`Î   CS3Cz  AMÇC`ü>í¿  GÃOGÃOÁ×
Á×
    FSL00025    T.N-rrppppprLGA AË` (   YYZ     LGA     B#ffÂCÚ<AË`ì   CS3Cr  A(öCsn~GÃO¿  GÃOGÃOÁ×
Á×
    FSL00025    T.N-rrppppprLGA AË` (   YYZ     LGA     B#DDÂCÒ(ÜAË` 
   C,ÍCy  A$JC`ýtGÃO¿  GÃOGÃOÁ×
Á×
    FSL00025    T.N-rrppppprLGA AË` (   YYZ     LGA     B#33Âª«CYRAË` (   ClÍCp  @æxCsm&GÃO¿  GÃOGÃOÁ×
Á×
    FSL00025    R.N-rrpppp-rLGA AË` (   YYZ     LGA     BÂîïEÎAË`!T   Ct&fC  Aî³ÅGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    R.N-rrpppp-rEWR AË`!Ì   BNA     LGA     BwwÂ»¼E¸òjAË`!r   CyYC AÖGB4eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprEWR AË`!Ì   BNA     LGA     BÌÍÂ E£zBAË`!   C~YCz  AÉ©BYÝ*GÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprEWR AË`!Ì   BNA     LGA     B ""ÂLÍEì)AË`!®   C3C  AÉ©BHÊâGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprEWR AË`!Ì   BNA     LGA     B wwÂEt­AË`!Ì   CÓ3C|  A¹33BHÊâGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`"D   BNA     LGA     B ÌÍÂæfEtýAË`!ê   C3Cx  A°ø	BHËÃGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`"D   BNA     LGA     B!  Â³3EjBAË`"   C3Cz  AµB}Â,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`"D   BNA     LGA     B!""ÂffEZAË`"&   ClÍC|  A µBôUGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`"D   BNA     LGA     B!33Â*«EK4ÓAË`"D   CìÍCw  A(öB£ÀWGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`"¼   BNA     LGA     B!DDÂîïE;¼ÝAË`"b   C3Ch  AaB£ÀGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`"¼   BNA     LGA     B!ffÂ³3E,"AË`"   C3Co  A²¢BGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`"¼   BNA     LGA     B!wwÂE&pAË`"   C3C|  AÐ7BdGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`"¼   BNA     LGA     B!ª«ÂffEìAË`"¼   ClÍC AfxBTO>»çm¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`#4   BNA     LGA     B!îïÂLÍEâXAË`"Ú   CÓ3C  AVGB{h>¹Û#¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`#4   BNA     LGA     B"""Â*«DôÁ AË`"ø   C3C  A5BT;>³¶F¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`#4   BNA     LGA     B"ffÂDÒ<]AË`#   C9Cx  AEòB´>³¶F¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`#4   BNA     LGA     B"Â÷wD®êÇAË`#4   C3Cp  AMÇB4e>ÈÌ¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`#¬   BNA     LGA     B"ÌÍÂÕUDkAË`#R   C3Cf  AEòBT;>ëÃ!¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`#¬   BNA     LGA     B"ÝÞÂ³3DìJAË`#p   C3Ci  AEòBðO>Â$x¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`#¬   BNA     LGA     B#ÂDaUAË`#   C3Cp  A=PÈBl(b?	j¼¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`#¬   BNA     LGA     B#DDÂffDoæAË`#¬   C3Cm  A5BTu>ëÑ×¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`$$   BNA     LGA     B#wwÂLÍD ªAË`#Ê   CyCi  AMÇB4(>Çèê¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`$$   BNA     LGA     B#ª«Â33Dc½AË`#è   C9Ci  AMÇB4e>È#â¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`$$   BNA     LGA     B#ÌÍÂ33D[a;AË`$   CyCj  AÐ7    >=Rk¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`$$   BNA     LGA     B#»¼ÂLÍDUø8AË`$$   C¹Cm  AVGC{ò>Ã!¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`$   BNA     LGA     B#ª«ÂffDVºAË`$B   C9Cr  AEòC{F>þÙ¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`$   BNA     LGA     B#ÂnïDBAË`$`   CÓ3Cn  A^=qCN>T|Â¿  GÃOGÃOÁ×
Á×
    FSL00266    T.N-rrppppprLGA AË`$   BNA     LGA     B#ffÂ CóaÿAË`$~   C,ÍCm  AEòC`üGÃO¿  GÃOGÃOÁ×
Á×
    FSL00266    R.N-rrpppp-rLGA AË`$   BNA     LGA     B'Â¯33Dãê	AË`°   C3C  A²¢CªÈÓGÃO¿  GÃOGÃOÁ×
Á×
    FSL00028    T.N-rrppppprORD AË`Î   DCA     ORD     B'ÌÍÂ¯;¼D±ûAË`Î   C3C  AfxCªÇ¦?NV¿  GÃOGÃOÁ×
Á×
    FSL00028    T.N-rrppppprORD AË`Î   DCA     ORD     B'ÌÍÂ¯ffD
AË`ì   CyC A²¢Cÿ'>¥ãT¿  GÃOGÃOÁ×
Á×
    FSL00028    T.N-rrppppprORD AË` F   DCA     ORD     B'ÌÍÂ¯D-¨éAË` 
   C3C AaCÿ'>\)¿  GÃOGÃOÁ×
Á×
    FSL00028    T.N-rrppppprORD AË` F   DCA     ORD     B'ÌÍÂ¯¢"CärAË` (   CùC  A^=qCÿ'>Xb¿  GÃOGÃOÁ×
Á×
    FSL00028    T.N-rrppppprORD AË` F   DCA     ORD     B'ÌÍÂ¯³3Cé_AË` F   CyC @ÅòCÿ'>Y¿  GÃOGÃOÁ×
Á×
    FSL00028    R.N-rrpppp-rORD AË` F   DCA     ORD     B&îïÂÕUFSôAË` (   C_YC  BI©CË/?@Ä¿  GÃOGÃOÁ×
Á×
    FSL00030    T.N-rrppppprIAD AË` F   EWR     ORD     B'»¼ÂLÍFQAË` Ü   C]¦fC BEòCòGÃO¿  GÃOGÃOÁ×
Á×
    FSL00030    T.N-rrppppprPIT AË`#   EWR     ORD     B(wwÂÄDFU,AË`!   C]ÙC  BSó|CèGÃO¿  GÃOGÃOÁ×
Á×
    FSL00030    T.N-rrppppprPIT AË`#   EWR     ORD     B)DDÂ33FOAË`"D   C]ÙC  B\.¦CË/GÃO¿  GÃOGÃOÁ×
Á×
    FSL00030    T.N-rrppppprPIT AË`#   EWR     ORD     B)îïÂ¢"FQAË`"ø   C]YC  Bb[CGÃO¿  GÃOGÃOÁ×
Á×
    FSL00030    T.N-rrppppprPIT AË`#   EWR     ORD     B*Â ""F	ØAË`#¬   CaYC  Bb[CDèGÃO¿  GÃOGÃOÁ×
Á×
    FSL00030    T.N-rrppppprBUF AË`%æ   EWR     ORD     B*33Â¡¢"FÊAË`$`   Cd¦fC  Bl¤úCDèGÃO¿  GÃOGÃOÁ×
Á×
    FSL00030    T.N-rrppppprBUF AË`%æ   EWR     ORD     B*ffÂ£*«F«AË`%   CdÙC  BpÂCÜÌGÃO¿  GÃOGÃOÁ×
Á×
    FSL00030    T.N-rrppppprBUF AË`%æ   EWR     ORD     B*ÝÞÂ¤¢"EãóÊAË`%È   Cl¦fC  BdiÐCQGÃO¿  GÃOGÃOÁ×
Á×
    FSL00030    R.N-rrpppp-rBUF AË`%æ   EWR     ORD     BwwÂÃF&FAË` F   CX&fC A°ø	GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    R.N-rrpppp-rDFW AË`     MSP     DFW     B  ÂÃF%£ÇAË` d   CX¦fC  A¬ÚtC+âhGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`     MSP     DFW     B
ÂÃ  Fß	AË`    C\&fC   AíÌC+áwGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`     MSP     DFW     B
  ÂÂîïFGhAË`     Ce&fC¢ AaC%ùóGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`     MSP     DFW     B	wwÂÂîïEô²AË` ¾   ClÙC¦  AFC4eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`!   MSP     DFW     B	  ÂÂîïEäfeAË` Ü   Cs¦fC§  AfxC4eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`!   MSP     DFW     BÂÃEäAË` ú   CtYC§  A^=qCN5GÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`!   MSP     DFW     B""ÂÃ""EáaAË`!   Cu&fC¨  AVGCK/NGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`!   MSP     DFW     Bª«ÂÃ33EÍ§AË`!6   CzYC  A,ÚtCCîíGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`!   MSP     DFW     BDDÂÃ;¼E¤ AË`!T   CÙC¥  Ad C=sGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`!   MSP     DFW     BÝÞÂÃ;¼Eú²AË`!r   C3C  @æxC4eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`!   MSP     DFW     BwwÂÃDDEsù-AË`!   C3C  @ÅòC=rGÃO¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`!   MSP     DFW     BÂÃLÍETâ=AË`!®   CÓ3C Ad C=s¯?_1¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"   MSP     DFW     Bª«ÂÃ;¼EPÿ_AË`!Ì   CùC  A=PÈC!³?_§¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"   MSP     DFW     BwwÂÃEJ^?AË`!ê   C¹C  A=PÈBñòg>íù¨¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"   MSP     DFW     BUUÂÂæfE'ñÞAË`"   C9C A,ÚtBß>ÓòB¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"   MSP     DFW     B""ÂÂ»¼E§±AË`"&   CyC @öîïBñò>Û"Ñ¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"   MSP     DFW     BîïÂÂDÞìPAË`"D   C9C  @¤JBññj>ËC¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"   MSP     DFW     B»¼ÂÂnïDÁ[AË`"b   C3C  @µBýÂ,>±&é¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"   MSP     DFW     BÂÂUUD±nAË`"   CyC  @ÅòB÷dò>° Å¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"   MSP     DFW     BffÂÂ33DªAË`"   C¹C @ÖGBýÃ?>ÉRé¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"ø   MSP     DFW     BDDÂÂDqFAË`"¼   C9C  @²¢B÷g>ì¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"ø   MSP     DFW     BÂÂD\$MAË`"Ú   C9C  @fxC4e>G¢¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"ø   MSP     DFW     BÝÞÂÂD3[AË`"ø   CùC ?²¢C4e> t¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`"ø   MSP     DFW     B»¼ÂÂCÂUAË`#   C3C @²¢C4e>9Ë ¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`#   MSP     DFW     BÂÂCÎYAË`#R   CS3C  @EòC4e=¹Ý¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`#   MSP     DFW     BffÂÂDcê	AË`#p   CyC @ÅòC4e>0¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`#   MSP     DFW     B""ÂÂ""D¼JKAË`#   CùC @ÖGCB\>½-ÿ¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`#   MSP     DFW     BDDÂÂ;¼D½ÐoAË`#¬   C3Cm  A²¢C×>gµ¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`$   MSP     DFW     BÂÂ;¼D½¼îAË`#Ê   C3Cb  A(ö    >¹in¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`$   MSP     DFW     BÌÍÂÂ;¼D°cAË`#è   C¹Cp  @ÖG    >¹SL¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`$   MSP     DFW     BÂÂ;¼D^AË`$   C3C  @ÖG    >¸õ^¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`$   MSP     DFW     B33ÂÂ33D/iAË`$$   CÓ3C  ?²¢AÔú>GEÜ¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`$~   MSP     DFW     B""ÂÂD-åAË`$B   C¹C      BÐ{>Â¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`$~   MSP     DFW     BîïÂÂD'.ÌAË`$`   C¹B  ?²¢C!µ>¨½¿  GÃOGÃOÁ×
Á×
    FSL00807    T.N-rrppppprDFW AË`$~   MSP     DFW     BÌÍÂÂC×WYAË`$~   C3C  @fxC4e>9ÈÙ¿  GÃOGÃOÁ×
Á×
    FSL00807    R.N-rrpppp-rDFW AË`$~   MSP     DFW     BDDÂ¹»¼F$¹AË` 
   C^YC A*AüáQGÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprTUL AË`"&   IAH     ORD     BÝÞÂ¸æfF(aAË` ¾   C^&fC AíÌB§GÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprTUL AË`"&   IAH     ORD     BÂ¸æfF*ÒAË`!r   C^YC A¹33    GÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprTUL AË`"&   IAH     ORD     BUUÂ¸ F%ñAË`"&   C]¦fC AÑä±AáGÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprTUL AË`"&   IAH     ORD     BÂ·ÌÍF')AË`"Ú   C\ÙC  AÅòAØü>GÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprSTL AË`$ö   IAH     ORD     BÝÞÂ·F(aAË`#   C]&fC  AöîïAå©ÎGÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprSTL AË`$ö   IAH     ORD     BÂ¶LÍF%ñAË`$B   C\YC BíÌAáÁGÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprSTL AË`$ö   IAH     ORD     BUUÂµnïF%ñAË`$ö   C[¦fC  Aÿ*BmGÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprSTL AË`$ö   IAH     ORD     B33ÂµF')AË`%ª   CYÙC  AûAyãGÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprSTL AË`'Æ   IAH     ORD     BÝÞÂ´ÕUF-BAË`&^   CZ&fC Bd A GÃO¿  GÃOGÃOÁ×
Á×
    FSL00032    T.N-rrppppprSTL AË`'Æ   IAH     ORD     BDDÂÂDe¾5AË`!   C9C @ÖGGÃO>·ÎÙ¿  GÃOGÃOÁ×
Á×
    FSL00522    R.N-rrpppp-rDFW AË`"   DFW     CVG     B  ÂÁ÷wDÎmAË`!®   C¹C @ÖGCpæ>Òñª¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprDFW AË`"   DFW     CVG     B  ÂÁÄDEÇðAË`!Ì   CÓ3C A(öB´e>Þ5?¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprDFW AË`"   DFW     CVG     BDDÂÁEG&AË`!ê   C3C}  @öîïBM^ë>ê~ú¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprDFW AË`"   DFW     CVG     BwwÂÁnïEhñAË`"   C¹Cx  @öîïBl*À>êáµ¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprOKC AË`"   DFW     CVG     BÌÍÂÁLÍE¶2AË`"&   CìÍCi  A,ÚtB¥@GÃO¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprOKC AË`"   DFW     CVG     B  ÂÁE$xAË`"D   C,ÍC A5B}Â,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprOKC AË`"   DFW     CVG     B33ÂÀÕUE¦ÛäAË`"b   CìÍC A=PÈBæzGÃO¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprOKC AË`"   DFW     CVG     BDDÂ¿  EöÑAË`#4   CoÙC AMÇBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprDFW AË`%n   DFW     CVG     BDDÂ½33F
!­AË`#è   ChÙC  AvîïBözGÃO¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprDFW AË`%n   DFW     CVG     BÂ»F
]AË`$   Cg¦fC  A^=qBhGÃO¿  GÃOGÃOÁ×
Á×
    FSL00522    T.N-rrppppprDFW AË`%n   DFW     CVG     B	33Â¹îïF
ÔAË`%P   CgÙC  AfxB}ÂeGÃO¿  GÃOGÃOÁ×
Á×
    FSL00522    R.N-rrpppp-rDFW AË`%n   DFW     CVG     BÂ®;¼F"AË`°   C\¦fC{  AöîïCØGÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    T.N-rrppppprBNA AË` d   ATL     DFW     BÂ¯³3F&¥AË` d   CX¦fC AÞ=qCØGÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    T.N-rrppppprBNA AË` d   ATL     DFW     B  Â±""F&¢AË`!   CY&fC AÉ©CºGÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    T.N-rrppppprMEM AË`#4   ATL     DFW     BffÂ²¢"F&¡_AË`!Ì   CZ¦fC AÍÇCW\GÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    T.N-rrppppprMEM AË`#4   ATL     DFW     B»¼Â´F&¥AË`"   CY¦fC  A¬ÚtCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    T.N-rrppppprMEM AË`#4   ATL     DFW     BÂµ¢"F&¥AË`#4   CZ¦fC  AFCÝ GÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    T.N-rrppppprMEM AË`#4   ATL     DFW     BwwÂ·""F&¦?AË`#è   CZÙC A¬ÚtCW\GÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    T.N-rrppppprIAH AË`&   ATL     DFW     BîïÂ¸¢"F&£ÏAË`$   C[ÙC AFCôÂGÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    T.N-rrppppprIAH AË`&   ATL     DFW     Bª«Âº;¼F&¥AË`%P   C[¦fC  A(öC_GÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    T.N-rrppppprIAH AË`&   ATL     DFW     BwwÂ»ÌÍF&¡_AË`&   CYYC An³ÅC+GÃO¿  GÃOGÃOÁ×
Á×
    FSL00797    R.N-rrpppp-rIAH AË`&   ATL     DFW     B%""Â«ÌÍEî  AË`Î   Cm&fC BK¸RCßÊGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË` (   PHL     ORD     B%""Â¬  Eî¯AË`ì   Cm&fC  BI©Cÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË` (   PHL     ORD     B%33Â¬33EåcýAË` 
   Cn&fC BG½CºkGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË` (   PHL     ORD     B%33Â¬ffEÛÀAË` (   Cp&fC  BAn]Cÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË` (   PHL     ORD     B%33Â¬EÒZAË` F   CqÙC  B7$iCÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË` ¾   PHL     ORD     B%33Â¬ÄDEÑ{»AË` d   Cr¦fC  B;AþCÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË` ¾   PHL     ORD     B%33Â¬÷wEÑ­AË`    Cs&fC B933Cÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË` ¾   PHL     ORD     B%33Â­""EÐùAË`     Cs&fC B?_Cÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË` ¾   PHL     ORD     B%DDÂ­UUEÆ;ûAË` ¾   CtÙC B5CºkGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`!6   PHL     ORD     B%ffÂ­ E¼v/AË` Ü   CwYC B3ÔCåÒGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`!6   PHL     ORD     B%Â­ª«E²"AË` ú   Cx¦fC  B,ÚtCæGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`!6   PHL     ORD     B%ª«Â­ÕUE£wÒAË`!   C|&fC  B&®CæGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`!6   PHL     ORD     B%ÌÍÂ®  E¾AË`!6   C~YC  AûCåÒGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`!®   PHL     ORD     B%îïÂ®*«E0.AË`!r   C,ÍC  AæxCåÒGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`!®   PHL     ORD     B&Â®UUEopAË`!   C3C  AÖGCæGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`!®   PHL     ORD     B&DDÂ®wwERÉËAË`!®   C,ÍC  AÚÜCnÙGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`!®   PHL     ORD     B&wwÂ®EOîFAË`!Ì   CìÍC  AÞ=qCnqGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`"&   PHL     ORD     B&Â®»¼EC®}AË`!ê   CS3C  AæxCHGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`"&   PHL     ORD     B&ÌÍÂ®æfE#KîAË`"   CS3C AFCzôGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`"&   PHL     ORD     B'  Â¯E	¾AË`"&   CÓ3C AaCnqGÃO¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`"&   PHL     ORD     B'""Â¯*«ECAË`"D   C3C AÐ7CGº>×P¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`"   PHL     ORD     B'ffÂ¯;¼DÊ¥AË`"b   CÓ3C A*C¦·l>Ï\)¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`"   PHL     ORD     B'ª«Â¯LÍDºAË`"   CùC A²¢C¦·>Ä¦¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`"   PHL     ORD     B'ÌÍÂ¯]ÞD~AË`"   CÓ3C AaC>hs¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`"   PHL     ORD     B'ÌÍÂ¯wwDW	RAË`"¼   C3C  AFCÿ'>=¯¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`#   PHL     ORD     B'ÌÍÂ¯DªÎAË`"Ú   CÓ3C  A µCÿ'>=¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`#   PHL     ORD     B'ÌÍÂ¯¢"CÕ-AË`"ø   CÓ3C  A*Cÿ'>û¿  GÃOGÃOÁ×
Á×
    FSL00641    T.N-rrppppprORD AË`#   PHL     ORD     B'ÌÍÂ¯»¼CK=AË`#   C¹C  @æxCÿ'>§¿  GÃOGÃOÁ×
Á×
    FSL00641    R.N-rrpppp-rORD AË`#   PHL     ORD     B»¼ÂÂUUEâMAË`%ª   CuÙC  AÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00036    R.N-rrpppp-rOKC AË`'Æ   ICT     DFW     BÂÂÄDF
\3AË`&^   CeÙC  A¨¼ßCEþGÃO¿  GÃOGÃOÁ×
Á×
    FSL00036    T.N-rrppppprOKC AË`'Æ   ICT     DFW     BwwÂÂD»Á¾AË`Î   CyCk  @ÅòC4e>¢VV¿  GÃOGÃOÁ×
Á×
    FSL00615    T.N-rrppppprDFW AË`Î   CVG     DFW     BDDÂÂD¢AË`ì   C3Cr  @öîïC4e>\)¿  GÃOGÃOÁ×
Á×
    FSL00615    T.N-rrppppprDFW AË` F   CVG     DFW     Bª«ÂÂDÈ-jAË` 
   C9Ca  A²¢    ?F´¿  GÃOGÃOÁ×
Á×
    FSL00615    T.N-rrppppprDFW AË`Î   CVG     DFW     BÂÂDaEAË` 
   C3C @ÅòC4e>ÌÍ¿  GÃOGÃOÁ×
Á×
    FSL00615    T.N-rrppppprDFW AË` F   CVG     DFW     BÝÞÂÂD|AË` (   C3CZ  ?ÅòC4e>q©ü¿  GÃOGÃOÁ×
Á×
    FSL00615    T.N-rrppppprDFW AË` F   CVG     DFW     B»¼ÂÂC»Á¾AË` F   CÓ3Cf  @ÅòC4e>cSø¿  GÃOGÃOÁ×
Á×
    FSL00615    R.N-rrpppp-rDFW AË` F   CVG     DFW     B_\)Â)F)AË`%Ó   CY&fC%  BÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00040    R.N-rrppp--rXXD AË`%æ                   BD¸Á\F)AË` ü  CX&fC§  B&®GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00041    R.N-rrppp--rXXD AË` ú                   BD=qÁ°ffF)AË`$  CZ&fC  A,ÚtGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00041    R.N-rrppp--rXXD AË`$~                   B/ÐHÂºF9ÆAË`!®   C_ÙC AöîïGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    R.N-rrpppp-rMKE AË`"b   RJAA    KORD    B.féÂ¸HÜF8òjAË`"D   C^¦fC  Aÿ*BÖÏvGÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprMKE AË`"b   RJAA    KORD    B.4nÂ·ùõF/,AË`"b   C[ÙC B&®B×~wGÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprMKE AË`"¼   RJAA    KORD    B._Â·´TF%wãAË`"   CZÙC BQä±B×àGÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprMKE AË`"¼   RJAA    KORD    B-ÔþÂ·ftFïAË`"   C\ÙC  BVGB×ÝSGÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprMKE AË`"¼   RJAA    KORD    B- Â·zFQ{AË`"¼   C^ÙC Bb[BØ¤GÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprMKE AË`"¼   RJAA    KORD    B,J¦ÂµöFºAË`#R   CbYC  B\.¦BØþ&GÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprORD AË`$B   RJAA    KORD    B*ÍÂ³µêEá"AË`#è   Co&fC BAn]Bì¦ÚGÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprORD AË`$B   RJAA    KORD    B*¨§Â³^5EÎÉAË`$   Cs¦fC BEòBË~GÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprORD AË`$B   RJAA    KORD    B*~(Â²ÖÉEºòzAË`$$   Cv&fC B.é?BÅÙQGÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprORD AË`$B   RJAA    KORD    B)ýÙÂ±½~EhAË`$   CÓ3C  B£×BÍµáGÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprMKE AË`%ª   RJAA    KORD    B)ÜÆÂ±8E\YòAË`$º   ClÍC  AûBÕè_GÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprORD AË`%È   RJAA    KORD    B(Ç_Â°sME6tAË`%P   ClÍC  AµBédÄGÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprORD AË`%È   RJAA    KORD    B(qÄÂ°1NE§óAË`%n   C3C  A¨¼ßBõó=GÃO¿  GÃOGÃOÁ×
Á×
    FSL00042    T.N-rrppppprMKE AË`%ª   RJAA    KORD    B(.IÂ¯¾DÒ±hAË`%ª   C9C A µBÔÍ|?&ªJ¿  GÃOGÃOÁ×
Á×
    FSL00042    R.N-rrpppp-rORD AË`%È   RJAA    KORD    BH
=ÁyF)AË` ~  CV&fC¬  BFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00887    R.N-rrppp--rXXD AË`                    BH(öÁ©p¤F¯®AË`$  CY&fC¢  AíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00887    R.N-rrppp--rXXD AË`$                   BXG®ÂffF¯®AË` S   CY&fC+  AÁn]GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00045    R.N-rrppp--rXXD AË` F                   BVÂÂ0ÌÍF¯®AË`#×   C]&fCC  BFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00045    R.N-rrppp--rXXD AË`#Ê                   B\G®ÁÝ\)Fì{AË`"ñ   C\&fCb  ?ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00046    R.N-rrppp--rXXD AË`"ø                   AÇãBÝF¤kAË`"¼   Cp¦fC  @ÖGGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00048    R.N-rrppp--rXXY AË`&^   RJAA    WSSS    @¬®}BØ+F ÃAË`&@   CkÙBÌ  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00048    R.N-rrppp--rXXY AË`&^   RJAA    WSSS    B04ÃF07AË`!®   CSÙC BÁlGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00050    R.N-rrppp--rXXC AË`!®   VHHH    KSFO    B{\Á8õÃF¯®AË`"À  CT&fC­  B*ËªGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00625    R.N-rrppp--rFAE AË`"¼                   BxG®Á{F)AË`&D  CW&fC¬ B	ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00625    R.N-rrppp--rVEY AË`&@                   Aòò|Â÷åF4úÒAË` (   CRÙC}  BFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    R.N-rrppp--rLAX AË`#   YSSY    KLAX    B)*Âð¥ÉF4º4AË`"Ú   CR¦fCx  Bd GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    R.N-rrpppp-rLAX AË`#   YSSY    KLAX    B|PÂðb'F+2AË`"ø   CW&fCm  B7ÀBi³CGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`#   YSSY    KLAX    BÊÛÂð!ËF!­gAË`#   C\&fCh  B+BjsâGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`#p   YSSY    KLAX    BõÂïßHFmAË`#4   C`&fCj  B	ßBjÆGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`#p   YSSY    KLAX    Bm¬ÂïFQ[AË`#R   Ce¦fCf  BüBjÆØGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`#p   YSSY    KLAX    B¼¹ÂïZºF¶:AË`#p   Ck&fCg  BüBkfÕGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`#p   YSSY    KLAX    BýÂïxEöñAË`#   CpYCg  BÐ7Bki6GÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`#è   YSSY    KLAX    BPËÂîà\EâîáAË`#¬   CuÙCf  Aî³ÅBk8|GÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`#è   YSSY    KLAX    BEÂî¤÷EÏß¤AË`#Ê   C{¦fCh  B£×BkßÙGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`#è   YSSY    KLAX    BÝçÂîkE¼¢AË`#è   ClÍCp  Aÿ*BkówGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`#è   YSSY    KLAX    B¤ÃÂíÄ&E1AË`$$   C¬ÍCk  AÑä±BlãGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`%2   YSSY    KLAX    BX®Âí?VE_~AË`$~   CìÍC^  AµB_BGÃO¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`%P   YSSY    KLAX    BöÂìðE9BáAË`$º   C3Cj  A°ø	B4uØ?Õð¿  GÃOGÃOÁ×
Á×
    FSL00271    T.N-rrppppprLAX AË`%2   YSSY    KLAX    BeãÂì·µEó£AË`$ö   CS3Cg  AaB5zÓ>ÈS¿  GÃOGÃOÁ×
Á×
    FSL00271    R.N-rrpppp-rLAX AË`%2   YSSY    KLAX    BXffÂ£×F£3AË` I   C^&fC$  AÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00052    R.N-rrppp--rXXD AË` F                   BW×
Â"¸RF£3AË`#Í   C\&fCB  AFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00052    R.N-rrppp--rXXD AË`#Ê                   A²gmBæÑìE³Æ]AË`°   ClÍC  A µC¶ôGÃO¿  GÃOGÃOÁ×
Á×
    FSL00053    T.N-rrppppprXXY AË` 
   RJAA    VHHH    A²]dBæE zAË`Î   CS3C  AÐ7CÁGÃO¿  GÃOGÃOÁ×
Á×
    FSL00053    T.N-rrppppprXXY AË` 
   RJAA    VHHH    A²R½BæbEHÊAË`ì   CÓ3C AÐ7C=b^¿  GÃOGÃOÁ×
Á×
    FSL00053    T.N-rrppppprXXY AË` ú   RJAA    VHHH    A²B'BåþEtZ·AË` (   C¹C AíÌCÏV=x Ñ¿  GÃOGÃOÁ×
Á×
    FSL00053    T.N-rrppppprXXY AË` ú   RJAA    VHHH    A²0¾Bå¥EN-AË` d   C9Cu  @æxCå®=¿  GÃOGÃOÁ×
Á×
    FSL00053    T.N-rrppppprSZX AË` Ü   RJAA    VHHH    A²_BäøüE( AË` ¾   C3C&  @fxCÐ±=k@¾¿  GÃOGÃOÁ×
Á×
    FSL00053    T.N-rrppppprSZX AË` Ü   RJAA    VHHH    A±ÉºBäh
EææAË`!6   CS3C+  A²¢C"P=¨¿  GÃOGÃOÁ×
Á×
    FSL00053    T.N-rrppppprSZX AË`!ê   RJAA    VHHH    A±¤©Bä$D¶ôÊAË`!T   CyC  A(öC<=ýß¿  GÃOGÃOÁ×
Á×
    FSL00053    R.N-rrpppp-rXXY AË`!Ì   RJAA    VHHH    B,ðoÃàF&©çAË`#   CX¦fC  BSó|GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00055    R.N-rrppp--rXXC AË`&¸   RJAA    KSFO    B)ûÃå%F02¾AË`&   C^YC Aÿ*GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00055    R.N-rrppp--rXXC AË`&¸   RJAA    KSFO    @ºàvÃ(éF&¥AË`!   CdÙC BUUGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00731    R.N-rrppp--rXXC AË`%   NZAA    KLAX    AûçÃï¾F&³¨AË`%   Ce&fC BG½GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00731    R.N-rrppp--rXXC AË`%   NZAA    KLAX    B-«ÔÃC-F02¾AË`!   CY&fC  B=PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00058    R.N-rrppp--rXXC AË`$   RCTP    KSFO    B)éyÃìF0>ïAË`$   C^&fC  B²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00058    R.N-rrppp--rXXC AË`$   RCTP    KSFO    BJ=qÂ*ÂF*ùHAË`!  CW&fCV  A¤JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00062    R.N-rrppp--rXXD AË`!                   BE×
Â@ffF*ùHAË`$  Cc&fC_  A½PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00062    R.N-rrppp--rXXD AË`$                   Bh\)ÂõÃFffAË`!ü   C\&fC7  A¨¼ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00065    R.N-rrppp--rXXD AË`!ê                   Bh{Â"ffFffAË`%   C[&fC1  A½PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00065    R.N-rrppp--rXXD AË`%n                   BTffÂ×
F£3AË` )  C^&fC'  A,ÚtGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00066    R.N-rrppp--rXXD AË` (                   BS×
Â"ëF£3AË`#­  C^&fCN  A¹33GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00066    R.N-rrppp--rXXD AË`#¬                   BDeFÃ7ÂF0=·AË`"   C`&fC BUUGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00420    R.N-rrppp--rXXC AË`%   RJAA    KJFK    BDUMÃhF0A_AË`%   Cc¦fC  AíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00420    R.N-rrppp--rXXC AË`%   RJAA    KJFK    BXQìÁÜ£×F)AË`"ñ  CY&fC8  @(öGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00067    R.N-rrppp--rXXD AË`"ø                   BXffÂ	{F¯®AË`&u   CY&fC&  An³ÅGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00067    R.N-rrppp--rXXD AË`&|                   =Æ§ðÃ"¸F&´àAË`ì   Ce&fC  AVGGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00651    R.N-rrppp--rXXC AË`#   KLAX    YSSY    À.HéÃ%F&®ÈAË`#p   Cd¦fCq  Ad GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00651    R.N-rrppp--rXXC AË`#   KLAX    YSSY    BVG®ÂP{F)AË`!j  Cf&fC5  A=PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00069    R.N-rrppp--rXXD AË`!r                   BOQìÂfáHF&° AË`$î  Cf&fC @æxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00069    R.N-rrppp--rYQX AË`$Ø                   B!ñ'Ã	FF01AË`!   C^¦fC B£×GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00070    R.N-rrppp--rXXC AË`!6   RJAA    KLAX    B\
=Á®F¯®AË`%É   CX&fC¬ AöîïGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00071    R.N-rrppp--rXXD AË`%È                   BD(ÜÂ»wéF0:AË`!T   Cf&fC  A¨¼ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00073    R.N-rrppp--rDLH AË`!T   VHHH    KORD    B4Âµ²aF0(ýAË`$Ø   CbYC BaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00073    R.N-rrpppp-rORD AË`&^   VHHH    KORD    B/^jÂ³¶FF

AË`&   Cb&fC  BrÑZCKGÃO¿  GÃOGÃOÁ×
Á×
    FSL00073    T.N-rrppppprORD AË`&^   VHHH    KORD    B.\xÂ³U¨F AË`&@   Cf¦fC BfxC,/GÃO¿  GÃOGÃOÁ×
Á×
    FSL00073    T.N-rrppppprORD AË`&^   VHHH    KORD    B-Ê	Â³pEíôAË`&^   Cj&fC BXC}aGÃO¿  GÃOGÃOÁ×
Á×
    FSL00073    T.N-rrppppprORD AË`&^   VHHH    KORD    B-7ÏÂ²ê0EÚ(AË`&|   Co&fC  BQä±Cñ¦GÃO¿  GÃOGÃOÁ×
Á×
    FSL00073    T.N-rrppppprMKE AË`'   VHHH    KORD    B,|ÓÂ²ÒaEÇ[¶AË`&   CrYC  B3ÔC%¹GÃO¿  GÃOGÃOÁ×
Á×
    FSL00073    T.N-rrppppprMKE AË`'   VHHH    KORD    BZ+Â­¤DF&¥AË`Í  CX&fC  AÍÇGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00421    R.N-rrppp--rATL AË`Î                   B¤úÂ³î¦D6áHAË`#S   C3C  @öîïGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00421    R.N-rrppp--rMEM AË`#R                   B(µÂ¯åDÖ¡AË`%2   CS3C  An³ÅGÃOGÃO¿  GÃOGÃOÁ×
Á×
@þFtFSL00642    R.N-rrpppp-rORD AË`%   ORD     SEA     B(Â¯ÿ&DÛqAË`%P   CS3C  AfxCX>95¿  GÃOGÃOÁ×
Á×
AFÜFSL00642    T.N-rrppppprORD AË`%   ORD     SEA     B(1~Â°G®Då°AË`%   C¹C  AfxCL>Ï²p¿  GÃOGÃOÁ×
Á×
A"FSL00642    T.N-rrppppprORD AË`%   ORD     SEA     B(=qÂ°d±EP}AË`%   C3C  An³ÅCÐÂGÃO¿  GÃOGÃOÁ×
Á×
A'ò|FSL00642    T.N-rrppppprORD AË`%   ORD     SEA     B(:Â°DE>áAË`%ª   C3C AFCsGÃO¿  GÃOGÃOÁ×
Á×
A
óFSL00642    T.N-rrppppprMKE AË`&   ORD     SEA     B(DDÂ°×
Ee5¨AË`%æ   CS3C  AÚÜCÄ,GÃO¿  GÃOGÃOÁ×
Á×
A.9ÁFSL00642    T.N-rrppppprMKE AË`&   ORD     SEA     B(P7Â±Ec[AË`%æ   C¬ÍC  Aâ[Cp7GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00642    T.N-rrppppprMKE AË`&   ORD     SEA     B(\)Â±<Ep'AË`&   C&fC BíÌCEGÃO¿  GÃOGÃOÁ×
Á×
ACFSL00642    T.N-rrppppprMKE AË`&   ORD     SEA     B(X¿Â±eE«vAË`&"   C{ÙC B,ÚtCÍ¬GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00642    T.N-rrppppprORD AË`&   ORD     SEA     B(ZtÂ±¸RE¾ AË`&@   CvYC  B"CJÇGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00642    T.N-rrppppprORD AË`&   ORD     SEA     B(X¿Â±û¼EÑÍAË`&^   CsÙC B=PÈC¢LGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00642    T.N-rrppppprORD AË`&   ORD     SEA     B(P7Â²`mEäAË`&   CnÙC B?_CÈ GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00642    R.N-rrpppp-rORD AË`&   ORD     SEA     BÓ Â§±~Eí²AË`°   Co¦fC Aê0CyGÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprCVG AË` d   EWR     SDF     BÄDÂ§ïÉE×½ÃAË`Î   Ct&fC A¬ÚtC{{GÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprCVG AË` d   EWR     SDF     B¶Â¨.ïEÂ5AË`ì   Cy&fC A¤JCédGÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprCVG AË` d   EWR     SDF     B§AÂ¨nïE«Ô¼AË` 
   C&fC  AFC?GÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprCVG AË` d   EWR     SDF     BÂ¨®ïE?AË` (   CÓ3C Ad CóÒGÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprCVG AË` d   EWR     SDF     B§Â¨ì`EAËAË` F   C3C  AFC8/GÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.B-rrppppprCVG AË` d   EWR     SDF     B¥Â©$±EZ-AË` d   C,ÍC  AFCüaGÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!   EWR     SDF     B¿&Â©[OE¼AË`    C3C  AíÌC¨GÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!   EWR     SDF     BÌÍÂ©EYAË`    C¬ÍC  A*CIÜGÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.N-rrppppprSDF AË`                    BØ¿Â©E¢ AË`     C,ÍC  A²¢C~éGÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!   EWR     SDF     BØ¿Â©ÆÔEdÊ^AË` ¾   C3C An³ÅCÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.B-rrppppprSDF AË`!   EWR     SDF     BÂ©îEOÑAË` Ü   C3C  A²¢Ch¸®GÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!   EWR     SDF     Bd±Âª\E:áhAË` ú   C3C  A*CeÁEGÃO¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!   EWR     SDF     B:Âª4èE£AË`!   C3C  AfxCpcl>éc¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!Ì   EWR     SDF     B%Âª`mE4þAË`!6   C3C  A5C`/>î.¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!Ì   EWR     SDF     BÂªEÓuAË`!T   C3C  A,ÚtC{7(>ð{¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!Ì   EWR     SDF     BÝÞÂª±~EA'AË`!r   C9C  A5CpïU>ëTþ¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!Ì   EWR     SDF     B±~Âª×
EqìAË`!   CS3C  A=PÈCoi=>éþ¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!Ì   EWR     SDF     BÂªþKEqìAË`!®   CS3C  A5CpT>ñ3¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`!Ì   EWR     SDF     BÂªîïEqìAË`!®   CS3C  A=PÈBa?å>Æx¿  GÃOGÃOÁ×
Á×
    FSL00422    T.N-rrppppprCVG AË`!                   BkÂ«(DùAË`!Ì   CyC A(öCx
«?$©¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`"   EWR     SDF     BåÂ«FÔDàÊ=AË`!ê   C¹C AíÌCê>ËbV¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`"   EWR     SDF     BßÂ«V0DÜ7ÏAË`"   C3C  A$JC¨fu>Õ¡¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`"   EWR     SDF     B#×Â«c×DªNAË`"&   CùC  @æxC©\>ÉyT¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`"   EWR     SDF     B:Â«µDlûdAË`"D   C¹C  @ÅòC/>©D¿  GÃOGÃOÁ×
Á×
    FSL00422    T.B-rrppppprSDF AË`"   EWR     SDF     Bª«Â«CffAË`"b   C9C @µC9m¼?D©¿  GÃOGÃOÁ×
Á×
    FSL00422    T.N-rrppppprSDF AË`"                   B§Â«)D=¼îAË`"b   CùC«  A²¢C©/3?LÌÍ¿  GÃOGÃOÁ×
Á×
    FSL00422    T.B-rrppppprSDF AË`"   EWR     SDF     BÚtÂ«CáPAË`"   CùC A$JCí>:¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`"   EWR     SDF     B¯ÉÂ«cC-¼jAË`"   C9C  @µC 6×>vÌ2¿  GÃOGÃOÁ×
Á×
    FSL00422    T.G-rrppppprSDF AË`"   EWR     SDF     B¥Â«®BáPAË`"   C9C¢  ?ÅòC!=ë¿  GÃOGÃOÁ×
Á×
    FSL00422    R.G-rrpppp-rSDF AË`"   EWR     SDF     B6Âë0¤Cs×
AË`ì   CyGÃOGÃOGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    XWN-rrpBFp-rLAX AË` 
   ONT     DFW     B0ßÂë.<C¬AË`ï   CS3C  @fxCJ>¿¸L¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-iippppprLAX AË` 
   ONT     DFW     B+"Âë+ÕC¬6FAË`ò   C9C  @(öC>ÀrE¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-iippppprLAX AË` 
   ONT     DFW     B%dÂë)mCÛt¼AË`õ   CùC @(öCJ>ÁMÀ¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-iippppprLAX AË` 
   ONT     DFW     B¦Âë'DAË`ø   C¹C~  @(öCJ>Á\á¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-iippppprLAX AË` 
   ONT     DFW     BéÂë$D£TAË`û   CyCq  @(öC>Àç;¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-iippppprLAX AË` 
   ONT     DFW     B+Âë"6D(féAË`þ   CS3Cy  @ÅòCJ>Àõ¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-iippppprLAX AË` 
   ONT     DFW     BnÂëÏD6áHAË`    C9C  @¤JC>À¡P¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-iippppprLAX AË` 
   ONT     DFW     B°ÂëgDF¸AË`    CùC  @fxCJ>Á¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-iippppprLAX AË` 
   ONT     DFW     BòÂëÿDROßAË`    C¹C  @²¢CJ>Àö¦¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-iippppprLAX AË` 
   ONT     DFW     Bý5ÂëD^AË` 
   CyC  ?²¢C>Àím¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-iippppprLAX AË` 
   ONT     DFW     B÷wÂë0D¤AË`    CÓ3C^  ?²¢CJ>Ý¤T¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprLAX AË`    ONT     DFW     BÎÂëDÙ+AË`   CyCl  A,ÚtC%øÝ>Æfü¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprLAX AË`    ONT     DFW     B¥Âë	cE
~VAË` 0   C3Ct  AVGCt÷>Ì¢ã¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-irppppprLAX AË`    ONT     DFW     B~KÂêúE&²AË` A  CÓ3C}  A¨¼ßC÷>ß ¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprLAX AË`    ONT     DFW     BUUÂêê«EBáªAË` S   C¹Cn  AµC$t>çXe¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprLAX AË`    ONT     DFW     B*«ÂêÙEThRAË` d  ClÍCo  AÁn]CYmGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-irppppprLAX AË`    ONT     DFW     BÂê¼Ef¾AË` v   CS3Co  A°ø	Bðñ²GÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.B-irppppprLAX AË` Ü   ONT     DFW     BjÂêÆEv BAË`   C3Cl  A¬ÚtB»Á?K¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprLAX AË` Ü   ONT     DFW     BµÂêd±E5/AË`    C3Cr  AÞ=qB¶"·GÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprLAX AË` Ü   ONT     DFW     BüÂê5ÃEZAË` ª  CÓ3Cr  Aâ[BºBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprLAX AË` Ü   ONT     DFW     B÷wÂêDE£Ô{AË` ¼   ClÍCm  Aâ[B¹ï¿GÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprLAX AË` Ü   ONT     DFW     BòYÂéÑìE±¤)AË` Í  C,ÍC|  AÞ=qB¹ÕGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprLAX AË` Ü   ONT     DFW     BëÂéÞE¾ AË` ß   C¬ÍCy  Aî³ÅB»ÈGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprSAN AË`!6   ONT     DFW     BîïÂéEÀg®AË` á  CS3Cy  AöîïBcGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.N-rrppppprLAX AË` ú                   Bä±ÂéiÐEÊúNAË` ð  C}¦fCu  AòÑZBÀ?EGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprSAN AË`!6   ONT     DFW     BßÂé4EÖ'AË`!   Cz&fCv  AÞ=qB¹v©GÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprSAN AË`!6   ONT     DFW     BØ¿ÂèýqEã\AË`!  Cv¦fCp  Aÿ*B»+GÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprSAN AË`!6   ONT     DFW     BÎÂèÆÔEí¾wAË`!%   CsYCp  B£×B¾¹SGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.G-irppppprSAN AË`!6   ONT     DFW     B33ÂæwwF
ÍAË`!ê   Ch¦fCz  AûBÂùëGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.N-rrppppprPHX AË`!ê                   BDDÂã]ÞF
ÍAË`"ø   Ch¦fC|  AÁn]BÅ"éGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.N-rrppppprPHX AË`"ø                   BUUÂà]ÞF
ÍAË`#è   ChÙC A µBÅ²¾GÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.N-rrppppprTUS AË`#è                   BUUÂÝffF
ÍAË`$ö   Ci&fC  A²¢BÇ%ãGÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.N-rrppppprPHX AË`$ö                   BDDÂÚ F
ÍAË`%æ   ChÙC  AMÇBÈÛ3GÃO¿  GÃOGÃOÁ×
Á×
    FSL00424    T.N-rrppppprPHX AË`%æ                   AâªÂ¤.F&YpAË` 7   C[&fC   AíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00352    R.N-rrppp--rMIA AË` (                   Aû{Â©Í:F&)ãAË`#½   CY&fC  A°ø	GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00352    R.N-rrppp--rATL AË`#¬                   B!ëÂ¬2YE¾ AË`°   CwYC  BíÌA×ùºGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00074    T.N-rrppppprIND AË`!   IND     ORD     B$èÂ¬záE«s3AË` Ü   C|&fC  B"C® ÐGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00074    T.N-rrppppprIND AË`!   IND     ORD     B%NÂ¬­:E?cAË`!   C,ÍC  B7ÀC½8GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00074    T.N-rrppppprIND AË`!   IND     ORD     B%1~Â­~KE<WAË`!   C3C AæxCGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00074    T.N-rrppppprORD AË`#   IND     ORD     B%,`Â­ÌÍEd_AË`!Ì   CÓ3C AÖGC5GÃO¿  GÃOGÃOÁ×
Á×
A#=FSL00074    T.N-rrppppprORD AË`#   IND     ORD     B&§AÂ®èE>EzAË`"Ú   ClÍC Aî³ÅCâ GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00074    T.N-rrppppprORD AË`#   IND     ORD     B'ÔÂ¯)ÐERåAË`#   ClÍC  Ad CGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00074    T.N-rrppppprORD AË`#   IND     ORD     B'¨öÂ¯(öDärAË`#p   CÓ3C Aa?4>ùõ¿  GÃOGÃOÁ×
Á×
A¬FSL00074    T.N-rrppppprORD AË`$$   IND     ORD     B'ÑìÂ¯KòDñ[AË`#   C9C  AFC+¨>á½!¿  GÃOGÃOÁ×
Á×
AÛFSL00074    T.N-rrppppprORD AË`$$   IND     ORD     B'×
Â¯§D_AË`#è   C9C  A µC>rÝ}¿  GÃOGÃOÁ×
Á×
Aù	FSL00074    R.N-rrpppp-rORD AË`$$   IND     ORD     B&wwÂF)AË`     C[ÙC Bn³ÅB34GÃO¿  GÃOGÃOÁ×
Á×
    FSL00353    T.N-rrppppprALB AË`                     B)""ÂÝÞF)AË`!   C[&fC  Bb[B ²GÃO¿  GÃOGÃOÁ×
Á×
    FSL00353    T.N-rrppppprBOS AË`!                   B+ª«Â*«F)AË`"   C[ÙC B,ÚtB¾TGÃO¿  GÃOGÃOÁ×
Á×
    FSL00353    T.N-rrppppprBOS AË`"                   B.Â F)AË`#   CZYC BüBíGÃO¿  GÃOGÃOÁ×
Á×
    FSL00353    T.N-rrppppprFVE AË`#                   B0wwÂ³3F)AË`$   C\&fC B µBáGÃO¿  GÃOGÃOÁ×
Á×
    FSL00353    T.N-rrppppprFVE AË`$                   B2ÝÞÂF)AË`%   C`&fC BüBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00353    T.N-rrppppprFVE AË`%                   B5""ÂyÝÞF)AË`&   C^ÙC AµBÞGÃO¿  GÃOGÃOÁ×
Á×
    FSL00353    T.N-rrppppprYQY AË`&                   Aí:Â¾""D È1AË`¸   C9CS  AÐ7C­»Þ>ÙY^¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprDFW AË` 
   EFD     SDF     Aí\Â¾,`D½¼îAË`É  CùC]  AvîïC§->Ï©÷¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprDFW AË` 
   EFD     SDF     AíÝÞÂ¾8RDÑîVAË`Û   C3C]  A^=qC¤V>ÃØL¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprDFW AË` 
   EFD     SDF     Aî(öÂ¾EE9AË`ì  C3C`  AEòC¢ÚÄ>¿=¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprDFW AË` 
   EFD     SDF     Aî~KÂ¾S E3¤9AË`þ   CyCf  @µC¢äª>Ý¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprIAH AË` d   EFD     SDF     AîÐ7Â¾býE^âAË`   C3C A,ÚtC¡¢>Úð¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprIAH AË` d   EFD     SDF     Aï33Â¾tE¦¨AË` !   C3C  A^=qC¢³ÖGÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprIAH AË` d   EFD     SDF     AïÂ¾ùEÓ¶AË` 2  C,ÍC AíÌC¢ñcGÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.B-irppppprIAH AË` d   EFD     SDF     AðÂ¾§EÓÇAË` D   CìÍC  AvîïC¬*GÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.B-irppppprIAH AË` d   EFD     SDF     AðÂ¾jEÇðAË` U  C¬ÍC  Ad AaÛGÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprIAH AË` d   EFD     SDF     AðîïÂ¾wwE»¤{AË` f   CYC An³ÅAÈ0ªGÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.N-rrppppprIAH AË` d                   AðòYÂ¾uÃE½¼îAË` g   C~ÙC AvîïB}^GÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprDFW AË` ¾   EFD     SDF     AñbýÂ¾hEÃZAË` x  C|YC  AfxAÎûÉGÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprDFW AË` ¾   EFD     SDF     Añä±Â¾X¿EÍÕÓAË`    Cy¦fC  An³ÅAÊÈ/GÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprDFW AË` ¾   EFD     SDF     Aòm:Â¾HEÙAË`   CwYC  AEòAË?GÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprDFW AË` ¾   EFD     SDF     AòõÃÂ¾6Eê8BAË` ­   CsÙC}  An³ÅAÝ¡GÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.G-irppppprDFW AË` ¾   EFD     SDF     AøîïÂ½wwF¯®AË`!r   C_¦fC  AVGAÔ¶GÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.N-rrppppprIAH AË`!r                   B   Â»ÕUF/¼{AË`"b   C^ÙC  A¬ÚtB+GÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.N-rrppppprHOU AË`"                   B»¼Â¹¢"F/¼{AË`#p   C]&fC  A(öBhâGÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.N-rrppppprIAH AË`#p                   BffÂ·wwF/¼{AË`$~   C[ÙC AíÌBi|GÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.N-rrppppprDFW AË`$~                   BÂµ;¼F/¼{AË`%n   CZ¦fC  A¬ÚtBlªXGÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.N-rrppppprMOB AË`%n                   B
»¼Â²æfF/¼{AË`&|   CW&fC A¨¼ßBq	ÙGÃO¿  GÃOGÃOÁ×
Á×
    FSL00075    T.N-rrppppprMEM AË`&|                   Aä±Ã`ÁÃoAË`$$   C9GÃOGÃOGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    XWN-rrpBFp-rHLK AË`$`   KOA     ONT     AàøÃíAÃoAË`$'   C9BÎ  @²¢C}J"=ÖÕ´¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-iippppprHLK AË`$`   KOA     ONT     AÝ?ÃzBó×
AË`$*   CyBü  @fxC}J"=æuÓ¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-iippppprHLK AË`$`   KOA     ONT     AÙÃC^AË`$-   C3C  @EòC}P=èN¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-iippppprHLK AË`$`   KOA     ONT     AÕÌÃCìAË`$0   C¹C  @$JC}J"=âE¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-iippppprHLK AË`$`   KOA     ONT     AÒÃ"C¶áHAË`$3   CyC  ?²¢C}J"=Ù¸¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-iippppprHLK AË`$`   KOA     ONT     AÎZÃ¯CÇ¤ÝAË`$6   CS3C  ?²¢C}J"=Ô·¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-iippppprHLK AË`$`   KOA     ONT     AÊ¡Ã<CÜúáAË`$9   C3C)  ?²¢C}J"=Ö³¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-iippppprHLK AË`$`   KOA     ONT     AÆèÃÉCüûçAË`$<   CùC6  ?²¢C}J"=ÛÛ)¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-iippppprHLK AË`$`   KOA     ONT     AÃ.ÃWDr-AË`$?   C¹CT  ?²¢C}P=Ùÿ¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-iippppprHLK AË`$`   KOA     ONT     A¿uÃäDÇ®AË`$B   C¹Bd  ?²¢C}J"=Ó°ø¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-iippppprHLK AË`$`   KOA     ONT     A»¼ÃqD~AË`$E   CS3C  @EòC}J">t>º¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprOGG AË`$º   KOA     ONT     AòYÃ&fDÄAË`$V  CyC @$JC§>+¤¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprOGG AË`$º   KOA     ONT     A33Ã-§EZAË`$h   CùA¸  @$JC<>}z¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-irppppprOGG AË`$º   KOA     ONT     AòÃ07E*\AË`$y  CùBÎ  @öîïC­~ý>Ñá¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprOGG AË`$º   KOA     ONT     AëÃ1~EFO}AË`$   CyB¨  A5C°òÇ>âØ¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-irppppprOGG AË`$º   KOA     ONT     A:Ã)ÐEUîAË`$  C9Bº  A(öB1x>ºãA¿  GÃOGÃOÁ×
Á×
    FSL00652    T.B-irppppprOGG AË`$º   KOA     ONT     A_ÃOEaîÙAË`$®   CyB¢  @æxB%ñ>8`¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%2   KOA     ONT     AµÃE}5AË`$¿  C9B  A(öBë>.Þ¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%2   KOA     ONT     A§AÃútEà AË`$Ñ   ClÍB¢  A$JBrGÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%2   KOA     ONT     AÐ7ÃéÐEZ^AË`$â  C¬ÍB  @öîïBÎFGÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%2   KOA     ONT     AõÃÃØ¿E«BoAË`$ô   CS3Aø  @µBBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%2   KOA     ONT     A ""ÃÆÔE·AË`%  CÓ3C®  @ÅòBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%2   KOA     ONT     A DDÃ·wEÁ[AË`%   C¬ÍC¬ A(öBø2GÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.N-rrppppprHNL AË`%2                   A KÃ´EÂ!AË`%   CÓ3C­  A(öBègGÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%n   KOA     ONT     A wwÃ¡µEÍ¥AË`%(  C3C©  A$JB_GÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%n   KOA     ONT     A £×Ã§EØÕAË`%:   C~¦fC©  A*BåGÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%n   KOA     ONT     A Ð7ÃzEãîéAË`%K  C{&fCª AÐ7BrGÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.G-irppppprKOA AË`%n   KOA     ONT     A¢îïÃDF)AË`&"   Cc&fC   AÉ©B%×GÃO¿  GÃOGÃOÁ×
Á×
    FSL00652    T.N-rrppppprHLK AË`&"                   B ?&Â /ÉF|0AË`    C`YC  B;AþC GÃO¿  GÃOGÃOÁ×
Á×
@¯ËFSL00695    T.N-rrppppprCLE AË`$   PHL     DEN     B :Â¢ÆÔFwOAË`!®   C_YC B7$iCâÔGÃO¿  GÃOGÃOÁ×
Á×
@CFSL00695    T.N-rrppppprCLE AË`$   PHL     DEN     B Â¥`mF|0AË`"Ú   C_ÙC BAn]CGÃO¿  GÃOGÃOÁ×
Á×
@CFSL00695    T.N-rrppppprCLE AË`$   PHL     DEN     B!0Â§ïÉF±AË`$   C_YC BG½C£KGÃO¿  GÃOGÃOÁ×
Á×
@²ï5FSL00695    R.N-rrpppp-rCLE AË`$   PHL     DEN     BÂ¨ð¤E¾ AË`°   Cx¦fC  AÖGCn4GÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`ì   MHT     SDF     BP7Â©.E¾bAË`Î   CxÙC  AÖGCmüRGÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`    MHT     SDF     BjÂ©iÐE±*=AË`ì   C|&fC  AÁn]CmAGÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.B-rrppppprSDF AË`    MHT     SDF     B0Â©òE£3AË` 
   CìÍC  A¹33CSþ~GÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`    MHT     SDF     BffÂ©EÍAË` 
   C¬ÍC AFCQ¼äGÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.N-rrppppprSDF AË` (                   B'AÂ©¨EAË` (   ClÍC AFCL©zGÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`    MHT     SDF     B¸RÂ©ÄDEM`AË` F   C¬ÍC Ad CNçÅGÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`    MHT     SDF     BIcÂ©àmEvãTAË` d   CìÍC Ad CNçÅGÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`    MHT     SDF     BÝÞÂ©û¼EPúAË`    C3C  AaCNëGÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`!6   MHT     SDF     BtÂª0EOtZAË`     C,ÍC  A(öCNÂGÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`!6   MHT     SDF     B""Âª@ EL7LAË` ¾   ClÍC AaCa\GÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.B-rrppppprSDF AË`!6   MHT     SDF     BÂªtèEBAË` Ü   C3C  A²¢C2KGÃO¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`!6   MHT     SDF     BòYÂª¢ýE0È´AË` ú   CS3C A²¢C:r>þøØ¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`!6   MHT     SDF     B  ÂªÏ\EZAË`!   C3C AvîïC^¸>ïçÈ¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`!6   MHT     SDF     B  ÂªÌÍE
AË`!   C3C  A*B´e=Zc"¿  GÃOGÃOÁ×
Á×
    FSL00427    T.N-rrppppprSDF AË`!                   BòÂªû¼DòPåAË`!6   C3C  Ad C>ûq6¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`!ê   MHT     SDF     B0Â«(Dß¥¢AË`!T   CÓ3C  A$JCIì>ìü¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`!ê   MHT     SDF     B0Â«P7DÍ[çAË`!r   CÓ3C  AVGCÿ'>Ôµ¿  GÃOGÃOÁ×
Á×
    FSL00427    T.G-rrppppprSDF AË`!ê   MHT     SDF     B\Â«tèDÓuAË`!   C3C  @öîïCV©>ÃÓà¿  GÃOGÃOÁ×
Á×
    FSL00427    T.B-rrppppprSDF AË`!ê   MHT     SDF     Bä±Â«ÍD5?AË`!®   C¹C¥  A=PÈCd;>¬[¿  GÃOGÃOÁ×
Á×
    FSL00427    T.B-rrppppprSDF AË`!ê   MHT     SDF     B´èÂ«=CMÓAË`!Ì   CùC A²¢C-ä%>^o¿  GÃOGÃOÁ×
Á×
    FSL00427    R.G-rrpppp-rSDF AË`!ê   MHT     SDF     B=´èÂôÞD:AË` Ü   ClÍCX  @fxGÃOGÃO¿  GÃOGÃOÁ×
Á×
AFÜFSL00855    R.N-rrpppp-rSEA AË`!   SEA     DEN     B=\ÂôÞDÛqAË` ú   CS3C  @$JC4eGÃO¿  GÃOGÃOÁ×
Á×
A
óFSL00855    T.N-rrppppprSEA AË`!   SEA     DEN     B=ZtÂô¸Dåª³AË` ú   C,ÍCX      C5Ô§GÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00855    T.N-rrppppprSEA AË`!   SEA     DEN     B=4èÂôEÛqAË`!   C,ÍCf  @²¢C6GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00855    T.N-rrppppprSEA AË`!   SEA     DEN     B=òÂô mE?AË`!6   C}¦fC~  @(öC6^,GÃO¿  GÃOGÃOÁ×
Á×
ACFSL00855    T.N-rrppppprSEA AË`!r   SEA     DEN     B<ÅùÂô{Eeª³AË`!6   Cy¦fCx  @öîïC!)GÃO¿  GÃOGÃOÁ×
Á×
A,§ðFSL00855    T.N-rrppppprSEA AË`!r   SEA     DEN     B<åÂô~KEc[AË`!T   CtÙCu  A²¢C¶GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00855    T.N-rrppppprSEA AË`!r   SEA     DEN     B<býÂôeE¾/AË`!r   Ct&fCg  A=PÈCëpGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00855    T.N-rrppppprSEA AË`!r   SEA     DEN     B<'AÂôNE«·zAË`!r   CqYCd  AaC]NGÃO¿  GÃOGÃOÁ×
Á×
A'ò|FSL00855    T.N-rrppppprSEA AË`!®   SEA     DEN     B<µÂô(E¾ AË`!   Cn¦fCe  B+Bè GÃO¿  GÃOGÃOÁ×
Á×
A+FSL00855    T.N-rrppppprSEA AË`!®   SEA     DEN     B;ð¤Âóí:EÑÇRAË`!®   CjÙCT  BUUBÄÖGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00855    T.N-rrppppprSEA AË`!®   SEA     DEN     B;Ó Âó®Eä­AË`!®   CgYCU  B7$iBÍä~GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00855    T.N-rrppppprSEA AË`!®   SEA     DEN     B;ª«ÂóbýE÷àìAË`!Ì   Cd¦fCT  BXBÒfGÃO¿  GÃOGÃOÁ×
Á×
A4FSL00855    T.N-rrppppprSEA AË`"D   SEA     DEN     B;Âó%Fc[AË`!ê   CaÙCS  BüBÓ	GÃO¿  GÃOGÃOÁ×
Á×
A)MFSL00855    T.N-rrppppprSEA AË`"D   SEA     DEN     B;=qÂòÊ=Fà AË`"&   C\¦fCR  BæfBà¸êGÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00855    T.N-rrppppprSEA AË`"D   SEA     DEN     B:ÕUÂò\)Fp'AË`"D   CY¦fCY  B£×Bæ¢GÃO¿  GÃOGÃOÁ×
Á×
AüPFSL00855    T.N-rrppppprSEA AË`"D   SEA     DEN     B8býÂïQìF7AË`#p   CU¦fC[  Bxý¹BßÛyGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00855    T.N-rrppppprGEG AË`%P   SEA     DEN     B6*«Âì*«FAË`$   CU&fC_  BVGBÚÍGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00855    T.N-rrppppprGEG AË`%P   SEA     DEN     B5wwÂë:áF"ÐAË`$ö   CR¦fCd  BMÇBÜþGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00855    T.N-rrppppprGEG AË`%P   SEA     DEN     B4ßÂêoÉF+s3AË`%P   CZÙCu  BEòBÝ¦GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00855    R.N-rrpppp-rGEG AË`%P   SEA     DEN     B  ÂÞîïF06fAË`     CV¦fC  B*ËªB¡wyGÃO¿  GÃOGÃOÁ×
Á×
    FSL00429    T.N-rrppppprSLC AË`                     B""ÂÛ¢"F06fAË`!   CUÙC B7$iB LGÃO¿  GÃOGÃOÁ×
Á×
    FSL00429    T.N-rrppppprSLC AË`!                   B ""ÂØUUFCC3AË`"   C^YC  B"B¢ÇÝGÃO¿  GÃOGÃOÁ×
Á×
    FSL00429    T.N-rrppppprDEN AË`"                   B»¼ÂÕFCC3AË`#   C_¦fC B(öB»OGÃO¿  GÃOGÃOÁ×
Á×
    FSL00429    T.N-rrppppprDEN AË`#                   BîïÂÑÕUFCC3AË`$   CVÙC B+BÂsGÃO¿  GÃOGÃOÁ×
Á×
    FSL00429    T.N-rrppppprDEN AË`$                   BDDÂÎª«FCC3AË`%   CZYC BíÌBÀDGÃO¿  GÃOGÃOÁ×
Á×
    FSL00429    T.N-rrppppprDEN AË`%                   BÂËwwFCC3AË`&   CY¦fC B µB¿è[GÃO¿  GÃOGÃOÁ×
Á×
    FSL00429    T.N-rrppppprDEN AË`&                   BÓ ÂôrYBCoAË`%P   C3GÃOGÃOGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    XWN-rrpBFp-rSFO AË`%   OAK     DFW     BÖÂôvBÛt¼AË`%S   C3C  A*C>ê±ä¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-iippppprSFO AË`%   OAK     DFW     BØÂôzâCpÊÁAË`%V   C¹C A*C>íu¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-iippppprSFO AË`%   OAK     DFW     BÛÂô&C¼ùÛAË`%Y   C9C AFC>í%¿  GÃOGÃOÁ×
Á×
    FSL00355    T.B-iippppprSFO AË`%   OAK     DFW     BÝÂôjCõ]/AË`%\   CùC AÐ7Cl>ìßP¿  GÃOGÃOÁ×
Á×
    FSL00355    T.B-iippppprSFO AË`%   OAK     DFW     Bà
Âô®Dr-AË`%_   CùC AvîïC>ëÇÌ¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-iippppprSFO AË`%   OAK     DFW     BâÂôóD¯AË`%b   CùC An³ÅC>ëÙ¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-iippppprSFO AË`%   OAK     DFW     BåÂô7D*°!AË`%e   CùC A=PÈC>ëÀ¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-iippppprSFO AË`%   OAK     DFW     Bç|Âô{D;s¶AË`%h   CÓ3C  A5Cl>ëéü¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-iippppprSFO AË`%   OAK     DFW     BéøÂô¿DI+AË`%k   C3C  A=PÈC>ëöú¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-iippppprSFO AË`%   OAK     DFW     BìsÂôDXhsAË`%n   C3C A$JC>ì=¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-iippppprSFO AË`%   OAK     DFW     BîïÂô¡HDZ^AË`%q   CÓ3C^  AMÇC? mÚ¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`%æ   OAK     DFW     B  Âô¸RDÔ7AË`%  CÓ3C`  A^=qC(Ä>â%¿  GÃOGÃOÁ×
Á×
    FSL00355    T.B-irppppprSFO AË`%æ   OAK     DFW     B(öÂôÃjEÅAË`%   CS3C`  AÅòC¥ÆþGÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.B-irppppprSFO AË`%æ   OAK     DFW     BZtÂôÃjE06fAË`%¥  ClÍCg  AÚÜ    GÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`%æ   OAK     DFW     BòÂô¹,ENAË`%·   CìÍC_  AöîïA³ãGÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.B-irppppprSFO AË`%æ   OAK     DFW     B¨öÂôOEq]AË`%È  C3CT  AæxB0GÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.B-irppppprSFO AË`%æ   OAK     DFW     B´èÂôy,EÉAË`%Ú   C,ÍCH  B	ßB )×GÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&^   OAK     DFW     B¿&ÂôT{EAË`%ë  C3CK  B	ßB¤øGÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&^   OAK     DFW     BÅùÂô*«Ef%AË`%ý   C¬ÍCJ  BÐ7Bª®GÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&^   OAK     DFW     BÐ7Âô  Ef%AË`&  CÓ3CJ  B	ßB¦S GÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&^   OAK     DFW     BÜ)ÂóÕUEAË`&    ClÍCF  B	ßB¤<GÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&^   OAK     DFW     BæfÂó©ÐE¥B=AË`&1  C}ÙCI  B£×B¦=GÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&^   OAK     DFW     BîïÂó E³ìAË`&B   CzÙCT  BUUB¨[eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.N-rrppppprSFO AË`&^                   Bð¤Âó|E³¼AË`&C   CzÙCU  BUUBðOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&   OAK     DFW     BüÂóNEÂAË`&T  Cw&fCY  BrêB¥=¼GÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&   OAK     DFW     BÔÂóÞEÏ¥AË`&f   Cs¦fC[  B$JB§ýòGÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&   OAK     DFW     BÆÂòì`EÜ7ÏAË`&w  Cp&fC]  B0ø	B¦@GÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&   OAK     DFW     BÂò¸REæPAË`&   Co¦fC_  BVGB¨ÇGÃO¿  GÃOGÃOÁ×
Á×
    FSL00355    T.G-irppppprSFO AË`&   OAK     DFW     B;¼Âë5ÃCs×
AË` (   CS3GÃOGÃOGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    XWN-rrpBFp-rLGB AË` d   ONT     HNL     B7³Âë5`CAAË` +   CS3C{  @¤JC))s>`¼#¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-iippppprLGB AË` d   ONT     HNL     B3«Âë4üC¸gmAË` .   C3C|  @¤JC)j>c¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-iippppprLGB AË` d   ONT     HNL     B/¢Âë4CáPAË` 1   C¹C  @(öC))s>d¨¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-iippppprLGB AË` d   ONT     HNL     B+Âë46D=AË` 4   CyCy  @µC))s>bìm¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-iippppprLGB AË` d   ONT     HNL     B'Âë3ÒD
AË` 7   CS3Co  @¤JC)>cÖ¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-iippppprLGB AË` d   ONT     HNL     B#Âë3oD"NVAË` :   C3Co  @ÖGC)&Ò>b0n¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-iippppprLGB AË` d   ONT     HNL     BÂë3D.}AË` =   CùC}  @ÖGC)>b,ó¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-iippppprLGB AË` d   ONT     HNL     BvÂë2¨D9*AË` @   CÓ3C  @ÅòC))s>aëÄ¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-iippppprLGB AË` d   ONT     HNL     BmÂë2EDCÕAË` C   C¹C @¤JC))s>b ¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-iippppprLGB AË` d   ONT     HNL     BeÂë1áDOCAË` F   C3C  @²¢C)j>bE¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-iippppprLGB AË` d   ONT     HNL     B\Âë1~DffAË` I   C3C.  ?²¢C))s>¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË` ¾   ONT     HNL     B0ÂëIcDÉ+AË` Z  CÓ3Ce  A²¢C>¿~¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-irppppprLAX AË` ¾   ONT     HNL     B'AÂëaHE¢ÑAË` l   CÓ3Cj  An³ÅCÒà>Îø¦¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË` ¾   ONT     HNL     B;¼Âëy,EÔAË` }  CùCn  A µC'>ÕH¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË` ¾   ONT     HNL     BNÂë7EáAË`    C3Cp  A µCV>Ë¥5¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-irppppprLAX AË` ¾   ONT     HNL     BiÐÂë¤±E)AË`    C3Ck  A¨¼ßCØ>ÐÓ
¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË` ¾   ONT     HNL     BÔÂëºE?Õ`AË` ²   C3Cg  AÉ©CðGÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË`!6   ONT     HNL     B£×ÂëÐ7EQ½AË` Ã  CS3Cg  AÖGC GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË`!6   ONT     HNL     BÇ®Âëê«E`yAË` Õ   ClÍCe  AÚÜC#GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË`!6   ONT     HNL     BîïÂìÔEz bAË` æ  CìÍCi  AÞ=qCoGÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË`!6   ONT     HNL     B	0Âì$±EøRAË` ø   C3Cw  Aÿ*C§¨GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË`!6   ONT     HNL     B	?&ÂìCjErÁAË`!	  ClÍCw  BÁlC×GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË`!6   ONT     HNL     B	hÂìaHE¬g
AË`!   CìÍCu  B²¢C7\GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË`!   ONT     HNL     B	ffÂì]ÞE¬g
AË`!   CìÍCu  B²¢BÐ{GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.N-rrppppprLAX AË`!6                   B	ÆÂì E¹¼ÍAË`!,  C3Cr  B	ßC²GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-irppppprLAX AË`!   ONT     HNL     B	±~Âì¢ýEÅÕAË`!>   C~YCn  BaCÙGÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-irppppprLAX AË`!   ONT     HNL     B	³3ÂìÉcEÑîVAË`!O  CzÙCn  BFC¢'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.B-irppppprLAX AË`!   ONT     HNL     B	¥ÂìîïEÝtÍAË`!a   CwÙCu  B"CØ"GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË`!   ONT     HNL     B	0Âí{EèûDAË`!r  CtYCs  B$JC7jGÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.G-irppppprLAX AË`!   ONT     HNL     B	  Âî F£3AË`"&   CbÙCn  B933C)GÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.N-rrppppprLAX AË`"&                   BÂðF£3AË`#4   Cb¦fCn  BAn]C»YGÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.N-rrppppprLAX AË`#4                   B""Âò³3F£3AË`$$   Cb¦fCl  BC}(C»VGÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.N-rrppppprLAX AË`$$                   B33ÂôÄDF£3AË`%2   CaÙCm  B=PÈC¢PGÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    T.N-rrppppprLAX AË`%2                   BDDÂöÕUF£3AË`&"   Cb¦fCo  BQä±C¢PGÃO¿  GÃOGÃOÁ×
Á×
    FSL00431    R.N-rrpppp-rSBP AË`&"                   Bª«Â UUF&° AË`Î   CXÙC B5CGÃO¿  GÃOGÃOÁ×
Á×
    FSL00356    T.N-rrppppprRDU AË`Î                   BÌÍÂ¢F&° AË` ¾   CXÙC  B,ÚtCµGÃO¿  GÃOGÃOÁ×
Á×
    FSL00356    T.N-rrppppprRDU AË` Ü                   Bª«Â¤ÌÍF&° AË`!Ì   CY&fC  B.é?C\GÃO¿  GÃOGÃOÁ×
Á×
    FSL00356    T.N-rrppppprGSO AË`!Ì                   BUUÂ§  F&° AË`"Ú   CX&fC B µC}"2GÃO¿  GÃOGÃOÁ×
Á×
    FSL00356    T.N-rrppppprSDF AË`"Ú                   B  Â©;¼F&° AË`#Ê   CXÙC  B+C}_GÃO¿  GÃOGÃOÁ×
Á×
    FSL00356    T.N-rrppppprSDF AË`#è                   Bª«Â«nïF&° AË`$Ø   CXÙC BüC}">GÃO¿  GÃOGÃOÁ×
Á×
    FSL00356    T.N-rrppppprSDF AË`$Ø                   B33Â­¢"F&° AË`%È   CXÙC BÁlC{GÃO¿  GÃOGÃOÁ×
Á×
    FSL00356    T.N-rrppppprSDF AË`%æ                   B
ª«Â¶  F
ÍAË` d   ChYC  AíÌBHGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.N-rrppppprDFW AË` d                   BwwÂ³]ÞF
ÍAË`!T   ChYC Ad BEÏGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.N-rrppppprBNA AË`!r                   BUUÂ°³3F
ÍAË`"b   ChYC A¹33Bn,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.N-rrppppprMEM AË`"b                   B""Â®  EûuÃAË`#p   Cn&fC~  AíÌB#ûGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.N-rrppppprSDF AË`#p                   Bm:Â­EíÖÙAË`#   CqÙC  AÐ7B¯GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.B-rrppppprSDF AË`$`   DFW     SDF     BÅùÂ­J=EãtþAË`#¬   Ct&fC  A*BOÏGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`$`   DFW     SDF     B6Â­$±EÛ+AË`#Ê   Cu¦fC  AfxBÅ,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`$`   DFW     SDF     B¥Â¬ÿ&EÛ3AË`#è   Cu¦fC AaBaGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`$`   DFW     SDF     B0Â¬ÚtEÛ3AË`$   Cu¦fC A¬ÚtBZ¯GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`$`   DFW     SDF     BUUÂ¬nïEÛ3AË`$$   CuÙC AµBåëGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.N-rrppppprSDF AË`$~                   BÔÂ¬´èEÛ3AË`$$   Cu¦fC  A¬ÚtCVBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`$`   DFW     SDF     B÷wÂ¬\EÛ3AË`$B   Cu¦fC  AµBÆGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprBNA AË`%   DFW     SDF     BhÂ¬iÐEÛ3AË`$`   CuÙC A°ø	BÅ,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprBNA AË`%   DFW     SDF     BÐ7Â¬FÔEÎúAË`$~   CxYC A°ø	B GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprBNA AË`%   DFW     SDF     B4èÂ¬$±EºßAË`$   C}&fC  AFBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprBNA AË`%   DFW     SDF     BåÂ¬jE§¼9AË`$º   CS3C  AFB«eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprBNA AË`%   DFW     SDF     B÷wÂ«âýEöAË`$Ø   CÓ3C  A*B¦9GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprBNA AË`%   DFW     SDF     BUUÂ«ÃjEßAË`$ö   ClÍC AVGB»HGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`%È   DFW     SDF     B¯ÉÂ«¤±EaPAË`%   ClÍC  AEòBÃGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`%È   DFW     SDF     BÂ«]ÞEO¥AË`%2   C3C  AfxB«GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.N-rrppppprBNA AË`%n                   BÂ«ÔEO¥AË`%2   C¬ÍC  A^=qCTæGÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`%È   DFW     SDF     BaHÂ«iÐEOÕãAË`%P   C3C AEòB»GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`%È   DFW     SDF     B½qÂ«WåEH7+AË`%n   C,ÍC AMÇA©ÿ®GÃO¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`%È   DFW     SDF     B0Â«NE=í²AË`%   CùC  AEòA?>ü¢h¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`%È   DFW     SDF     BffÂ«FÔE$6AË`%ª   CyC A=PÈA-t±>âª¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`&|   DFW     SDF     B´èÂ«>KE
MAË`%È   C3C A=PÈADIé>ÝÍ¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`&|   DFW     SDF     BµÂ«6DãuAË`%æ   CÓ3C @öîïA4ö>×k¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`&|   DFW     SDF     BNÂ«6Dá+ÇAË`&   C3C A(ö    >Ó^¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`&|   DFW     SDF     BOÂ«>KDà+AË`&"   CÓ3C  @öîïC®XK>×5\¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`&|   DFW     SDF     BÜ)Â«EùDÔú AË`&@   CyC A=PÈC­V[>·FÇ¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`&|   DFW     SDF     BOÂ«RÆD¡DAË`&^   CùC @ÖGC¨÷>ºÀÄ¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`&Ö   DFW     SDF     B""Â«UUDffAË`&k  C3C  @¤JC¡ú=Îl¿  GÃOGÃOÁ×
Á×
    FSL00432    T.N-rrppppprCVG AË`&|                   B6Â«rYDSÖAË`&|   C9C« @öîïC·?§¿  GÃOGÃOÁ×
Á×
    FSL00432    T.G-rrppppprSDF AË`&Ö   DFW     SDF     B\Â«D0È´AË`&   CS3A  A(öCmj\>Ã¿  GÃOGÃOÁ×
Á×
    FSL00432    T.B-rrppppprSDF AË`&Ö   DFW     SDF     B2pÂºO¥E§AË`!   C3C  A¬ÚtGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00874    R.N-rrppp--rMSP AË`!r                   BDDÂ·»¼F06fAË`     CV¦fC  B0ø	BÁÖ7GÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.N-rrppppprSTL AË`                     BUUÂ´ffF06fAË`!   CZ&fC  B,ÚtBÃô!GÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.N-rrppppprORD AË`!                   BÂ±*«F)AË`"   C[YC  B7ÀBÂfGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.N-rrppppprSDF AË`"                   BLÍÂ¯5ÃEí²AË`#4   Cn¦fC Aî³ÅBºÖGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`#è   DEN     SDF     B?&Â®×
E×AË`#R   CsÙC AÁn]B¼AôGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`#è   DEN     SDF     B1~Â®{¼E½* AË`#p   Cy¦fC A¬ÚtB¼MGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`#è   DEN     SDF     B%Â®%E¢ÈBAË`#   C3C  AíÌB»ñ3GÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`#è   DEN     SDF     B""Â®  E~¸AË`#   C3C  AÐ7B¹8HGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.N-rrppppprSDF AË`#¬                   BÂ­ÒÆEjAË`#¬   C¬ÍC  AvîïB¾ÊCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`#è   DEN     SDF     B§Â­DEd8AË`#Ê   ClÍC AMÇB¼·ÁGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`#è   DEN     SDF     BµÂ­9,EOÕãAË`#è   C3C  AVGB½ãGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`$   DEN     SDF     B  Â¬îENâAË`$   C3C AEòBµPÕGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`$   DEN     SDF     BåÂ¬¬`EGtAË`$$   CìÍC AMÇBfcGÃO¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`$   DEN     SDF     B,`Â¬tE:OAË`$B   C¹C A,ÚtBfÖ?N¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`$   DEN     SDF     B@ÚÂ¬?&E mAË`$`   CS3C A,ÚtB`?ø¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`$   DEN     SDF     BIcÂ¬	cEAË`$~   CùC Ad Bªïw?¶¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`$   DEN     SDF     BQìÂ«×
DÆ¸AË`$   CS3C A(öBªR?Ý¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`%   DEN     SDF     BUUÂ«ÌÍD¶áHAË`$   C9Cy  AMÇB¡õ>\­¿  GÃOGÃOÁ×
Á×
    FSL00080    T.N-rrppppprIND AË`$                   B@ÚÂ«¨Du]/AË`$º   CÓ3C  @µBÓ3D>Êb]¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`%   DEN     SDF     BOÂ«D(féAË`$Ø   C¹C¯ @ÖGBñ}>ÄA¿  GÃOGÃOÁ×
Á×
    FSL00080    T.B-rrppppprSDF AË`%   DEN     SDF     Bí:Â«ÚCr°AË`$ö   CS3C  @æxC!Ö>Ë}¿  GÃOGÃOÁ×
Á×
    FSL00080    T.G-rrppppprSDF AË`%   DEN     SDF     BËÂ«záBÛt¼AË`%   C3C®  @²¢C ¹:>DèÂ¿  GÃOGÃOÁ×
Á×
    FSL00080    R.G-rrpppp-rSDF AË`%   DEN     SDF     B¶æÂúFBâAË`    Ca&fC B*ËªGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00639    R.N-rrppp--rDCA AË`                    Bë<Â£§F&c1AË`$
   CX&fC  B3ÔGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00639    R.N-rrppp--rRDU AË`$                   BÌÍÂ®33F&° AË`    CX¦fC  BÐ7C}">GÃO¿  GÃOGÃOÁ×
Á×
    FSL00357    T.N-rrppppprSDF AË`                    B33Â°]ÞF&° AË`!r   CX¦fC  AòÑZCy¹åGÃO¿  GÃOGÃOÁ×
Á×
    FSL00357    T.N-rrppppprSDF AË`!r                   BwwÂ²F+íAË`"   CX&fC Aê0Cx0»GÃO¿  GÃOGÃOÁ×
Á×
    FSL00357    T.N-rrppppprBNA AË`"                   B»¼Â´³3F9¼ÍAË`#   C[&fC AÞ=qCx0¾GÃO¿  GÃOGÃOÁ×
Á×
    FSL00357    T.N-rrppppprBNA AË`#                   BîïÂ¶æfF9¼ÍAË`$~   C[¦fC AÍÇCw¾GÃO¿  GÃOGÃOÁ×
Á×
    FSL00357    T.N-rrppppprMEM AË`$~                   B
DDÂ¹*«F9¼ÍAË`%   C[&fC  A½PÈCyÎ^GÃO¿  GÃOGÃOÁ×
Á×
    FSL00357    T.N-rrppppprDFW AË`%                   B	Â»wwF9¼ÍAË`&|   C[&fC  AµC_GÃO¿  GÃOGÃOÁ×
Á×
    FSL00357    T.N-rrppppprMEM AË`&|                   B%§AÂ°/ÉE«s3AË`°   C{YC B"C9ÝGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00277    T.N-rrppppprIND AË`ì   ORD     STL     B%UUÂ°1~E¾×ÈAË`°   CwÙC B*ËªC6_GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00277    T.N-rrppppprIND AË`ì   ORD     STL     B$ôÂ°6EÑÑAË`Î   Cs¦fC  B,ÚtC9ÿdGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00277    T.N-rrppppprIND AË`ì   ORD     STL     B$§Â°;¼Eä­AË`ì   CoYC  B0ø	C9²çGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00277    T.N-rrppppprIND AË`ì   ORD     STL     B$ mÂ°@ E÷°'AË` 
   Cj&fC  B*ËªC8tGÃO¿  GÃOGÃOÁ×
Á×
A"FSL00277    T.N-rrppppprSTL AË`!r   ORD     STL     B#ZtÂ°IcF^zAË` F   Ce&fC  B*ËªC9gzGÃO¿  GÃOGÃOÁ×
Á×
A±ÄFSL00277    T.N-rrppppprSTL AË`!r   ORD     STL     BUUÂ±òEøiyAË`!r   Cj¦fC Aÿ*CH³@GÃO¿  GÃOGÃOÁ×
Á×
ACFSL00277    T.N-rrppppprSTL AË`!r   ORD     STL     BNÂ±§E÷¦fAË`!r   Ck&fC  AûCNGÃO¿  GÃOGÃOÁ×
Á×
ACFSL00277    T.N-rrppppprSTL AË`!r   ORD     STL     B¢"Â±5ÃEäÙAË`!   CqYC BíÌCLñÑGÃO¿  GÃOGÃOÁ×
Á×
A)MFSL00277    T.N-rrppppprSTL AË`"b   ORD     STL     BþKÂ±­:EÑoAË`"&   CuYC Aÿ*CQ¢GÃO¿  GÃOGÃOÁ×
Á×
A)MFSL00277    T.N-rrppppprSTL AË`"b   ORD     STL     BÓ Â±ïÉE¾EzAË`"D   Cy&fC AÉ©C|8JGÃO¿  GÃOGÃOÁ×
Á×
ACFSL00277    T.N-rrppppprSTL AË`"b   ORD     STL     B¯ÉÂ²#×E«BoAË`"b   C~ÙC AÉ©CzþðGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00277    T.N-rrppppprSTL AË`"b   ORD     STL     BòÂ²WåE?cAË`"   C,ÍC AÅòCzþðGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00277    T.N-rrppppprSTL AË`"ø   ORD     STL     BiÐÂ²EÑAË`"   ClÍC  AÐ7C{GÃO¿  GÃOGÃOÁ×
Á×
ACFSL00277    T.N-rrppppprSTL AË`"ø   ORD     STL     BIcÂ²½qEdAË`"   ClÍC A^=qC|$|GÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00277    T.N-rrppppprSTL AË`"ø   ORD     STL     BÕUÂ³G®E>l~AË`"ø   ClÍC AMÇCw8GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00277    T.N-rrppppprSTL AË`"ø   ORD     STL     BòÂ³m:EffAË`#4   CyC A,ÚtCa¦>ã¿  GÃOGÃOÁ×
Á×
A
óFSL00277    T.N-rrppppprSTL AË`$$   ORD     STL     B»¼Â³ßDäAË`#   CS3C  A$JCc¬³?çL¿  GÃOGÃOÁ×
Á×
Ag8FSL00277    T.N-rrppppprSTL AË`$$   ORD     STL     B mÂ´'ADÊXAË`#Ê   C¹C§ @¤JC>Ã\{¿  GÃOGÃOÁ×
Á×
AÛFSL00277    T.N-rrppppprSTL AË`$$   ORD     STL     BÄDÂ´®DÊXAË`$$   C9Að  AfxCBÐ>¯gl¿  GÃOGÃOÁ×
Á×
A®}FSL00277    R.N-rrpppp-rSTL AË`$$   ORD     STL     BkîïÃF£3AË` 
   C\YC @öîïCÙªGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.N-rrppppprYAK AË` 
                   BnÃ]ÞF£3AË` ú   C[YC @(öCìÏGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.N-rrppppprYAK AË` ú                   Bq""Ã7wF£3AË`"   C[¦fC @æxC­JGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.N-rrppppprMDO AË`"                   BrKÃèEìûdAË`"   CZ&fC+  @$JCC´GÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`#4   ONT     ANC     BrÃNEÍtJAË`"   C_&fCU  @(öCq¢GÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`#4   ONT     ANC     BrÓ ÃùE®°BAË`"¼   CeÙC  AíÌCXÖGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`#4   ONT     ANC     Bs0Ã¼E»uAË`"Ú   CnYC  AíÌCxdGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`#4   ONT     ANC     BsW
ÃñEoÖéAË`"ø   Ct&fBø  @æxCÄGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`#4   ONT     ANC     BswwÃÍEWC×AË`"ø   CvÙC  @¤JC$ÑGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.N-rrppppprANC AË`"ø                   BsÆÃ"ýEPöAË`#   CwÙC  @ÅòCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`#4   ONT     ANC     BsÅùÃNïEGCTAË`#4   CxÙC  @¤JCèGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.B-rrppppprANC AË`#è   ONT     ANC     BsÚtÃ{¼E0g+AË`#R   C{YC  @¤JCAÀGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`#è   ONT     ANC     BsáHÃ¨ENAË`#p   C}&fB<  @$JC.GÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.B-rrppppprANC AË`#è   ONT     ANC     BtjÃÏ\DòPåAË`#   C|Ù@À  Ad C!GÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`#è   ONT     ANC     Bt*«Ãö0D©íAË`#¬   Cz&fC² A²¢CÖGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`#è   ONT     ANC     BtUUÃ¿D/BAË`#Ê   C|¦fC©  @fxCÈGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.B-rrppppprANC AË`#è   ONT     ANC     BtòÃ,`D¬AË`#è   C|¦fAp  @fxChGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.G-rrppppprANC AË`$$   ONT     ANC     BtºÃ+òC¼ùÛAË`$   C}YC² A(ö@¶æGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.B-rrppppprANC AË`$$   ONT     ANC     Bt»¼Ã*«C¶áHAË`$   C}YC² Ad BÓGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.N-rrppppprANC AË`$                   BtÎÃOCF¸AË`$$   C}&fA   @öîïBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    T.B-rrppppprANC AË`$$   ONT     ANC     BtÑìÃòBMÓAË`$$   C|YB  ?ÅòB­§4GÃO¿  GÃOGÃOÁ×
Á×
    FSL00081    R.G-rrpppp-rANC AË`$$   ONT     ANC     BpaÂÿß&F&~AË`!  C\&fCj  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00463    R.N-rrppp--rYQH AË`!                   Br£Ãº+F&~AË`%%   C\&fC   @²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00463    R.N-rrppp--rYMA AË`%                   B!ffÂÎÊ=FT¹AË`ì   CgÙC B(öC5¨GÃO¿  GÃOGÃO            FSL00279    T.N-rrppppprDEN AË` (   LNK     DEN     B!S ÂÎúE÷¦fAË`ì   Cl¦fC Aâ[Cp¤GÃO¿  GÃOGÃO    Á×
    FSL00279    T.N-rrppppprDEN AË` (   LNK     DEN     B!?&ÂÏ)ÐEäAË` 
   Cr&fC  AÖGCóGÃO¿  GÃOGÃO    Á×
    FSL00279    T.N-rrppppprDEN AË` (   LNK     DEN     B!,`ÂÏZtEÑ>ÅAË` (   Cw¦fC AÖGCðGÃO¿  GÃOGÃO    Á×
    FSL00279    T.N-rrppppprDEN AË` (   LNK     DEN     B!  ÂÏÚE¾(8AË` F   C|ÙC  Aê0CoøGÃO¿  GÃOGÃO    Á×
    FSL00279    T.N-rrppppprDEN AË` Ü   LNK     DEN     B µÂÏæfE«_±AË`    C3C AÉ©Cn¤GÃO¿  GÃOGÃO            FSL00279    T.N-rrppppprDEN AË` Ü   LNK     DEN     B µÂÐE?cAË`     C3C A¤JCÿ'GÃO¿  GÃOGÃO    Á×
    FSL00279    T.N-rrppppprDEN AË` Ü   LNK     DEN     B jÂÐuÃEOÙAË` Ü   C3C A²¢CCGÃO¿  GÃOGÃO    Á×
    FSL00279    T.N-rrppppprDEN AË` Ü   LNK     DEN     B ÔÂÐ²YEdAË` ú   ClÍCz  A^=qCÍ¿GÃO¿  GÃOGÃOÁ×
Á×
    FSL00279    T.N-rrppppprDEN AË`!ê   LNK     DEN     B \ÂÐýqE>l~AË`!6   C3Ce  A(öCTGÃO¿  GÃOGÃOÁ×
Á×
    FSL00279    T.N-rrppppprDEN AË`!ê   LNK     DEN     B 6ÂÑP7EffAË`!   C3C'  @µCqËè>¬.d¿  GÃOGÃOÁ×
Á×
    FSL00279    T.N-rrppppprDEN AË`!ê   LNK     DEN     B¸RÂÑX¿DäAË`!ê   C¹CJ  @ÅòC;®x>k½¤¿  GÃOGÃOÁ×
Á×
    FSL00279    R.N-rrpppp-rDEN AË`!ê   LNK     DEN     B+33ÂffF)AË` d   C\¦fC  BEòBg&£GÃO¿  GÃOGÃOÁ×
Á×
    FSL00509    T.N-rrppppprBOS AË` d                   B.""Â  F)AË`!T   C\¦fC B=PÈBjLxGÃO¿  GÃOGÃOÁ×
Á×
    FSL00509    T.N-rrppppprMHT AË`!T                   B1  ÂF)AË`"b   CYÙC BíÌBo`GÃO¿  GÃOGÃOÁ×
Á×
    FSL00509    T.N-rrppppprBOS AË`"b                   B3UUÂ¢"F)AË`#R   C\YC B(öB.6GÃO¿  GÃOGÃOÁ×
Á×
    FSL00509    T.N-rrppppprFVE AË`#p                   B5ffÂF)AË`$`   Ca&fC BÐ7B#üGÃO¿  GÃOGÃOÁ×
Á×
    FSL00509    T.N-rrppppprFVE AË`$`                   B7UUÂzÝÞF)AË`%n   C`YC  A¨¼ßB_uGÃO¿  GÃOGÃOÁ×
Á×
    FSL00509    T.N-rrppppprFVE AË`%n                   B9""ÂtÌÍF)AË`&^   Ca¦fC  A µBõGÃO¿  GÃOGÃOÁ×
Á×
    FSL00509    T.N-rrppppprYHZ AË`&^                   BôÂÐZtE«_±AË`Î   CìÍC  AÅòC
$GÃO¿  GÃOGÃO            FSL00359    T.N-rrppppprDEN AË` 
   ICT     DEN     B,`ÂÐtE" AË`ì   C¬ÍC AµCÜ^GÃO¿  GÃOGÃO    Á×
    FSL00359    T.N-rrppppprDEN AË` 
   ICT     DEN     BZtÂÐùEFAË` 
   CS3C A²¢C¡çGÃO¿  GÃOGÃOÁ×
Á×
    FSL00359    T.N-rrppppprDEN AË` 
   ICT     DEN     B""ÂÐ¤±EcýAË` d   CÓ3C|  AEòC«rÅGÃO¿  GÃOGÃOÁ×
Á×
    FSL00359    T.N-rrppppprDEN AË` ú   ICT     DEN     BNÂÐ¶E=ãñAË`    CÓ3CH  @¤JC «>G¿  GÃOGÃOÁ×
Á×
    FSL00359    T.N-rrppppprDEN AË` ú   ICT     DEN     By,ÂÐâýEffAË`     C3CR  @ÅòCÕ¾?IÕ¿  GÃOGÃOÁ×
Á×
    FSL00359    T.N-rrppppprDEN AË` ú   ICT     DEN     By,ÂÑ%DäAË` ú   C3CT  A$JCÿ'>s¿  GÃOGÃOÁ×
Á×
    FSL00359    R.N-rrpppp-rDEN AË` ú   ICT     DEN     BffÂ×UUF)AË`     C^YC AöîïB«3GÃO¿  GÃOGÃOÁ×
Á×
    FSL00851    T.N-rrppppprWMS AË`                     BÌÍÂÔUUF)AË`!   C^&fC AÞ=qB¬bfGÃO¿  GÃOGÃOÁ×
Á×
    FSL00851    T.N-rrppppprALS AË`!®                   B""ÂÑ]ÞF)AË`"   C^YC  A½PÈB­÷GÃO¿  GÃOGÃOÁ×
Á×
    FSL00851    T.N-rrppppprAMA AË`"                   BffÂÎnïF)AË`#   C^&fC  A¨¼ßB®ÎÎGÃO¿  GÃOGÃOÁ×
Á×
    FSL00851    T.N-rrppppprMAF AË`#¬                   BÂË F)AË`$   C]ÙC Ad B°GÃO¿  GÃOGÃOÁ×
Á×
    FSL00851    T.N-rrppppprMAF AË`$                   B»¼ÂÈF)AË`%   C]¦fC  AFB±p>GÃO¿  GÃOGÃOÁ×
Á×
    FSL00851    T.N-rrppppprDFW AË`%ª                   BÌÍÂÅF)AË`&   C\ÙC  A µB²¹ÇGÃO¿  GÃOGÃOÁ×
Á×
    FSL00851    T.N-rrppppprDFW AË`&                   B~KÂE¾(8AË`°   C{YC AÍÇBÑA­GÃO¿  GÃOGÃOÁ×
Á×
Áù FSL00548    T.N-rrppppprRIC AË` 
   ORD     RIC     BaHÂÚtE«BoAË`Î   CÙC  A¬ÚtBÓ(GÃO¿  GÃOGÃOÁ×
Á×
Áù FSL00548    T.N-rrppppprRIC AË` 
   ORD     RIC     B,`ÂÚE5¢AË`ì   CìÍC|  AFBÔéGÃO¿  GÃOGÃOÁ×
Á×
Áû"ÑFSL00548    T.N-rrppppprRIC AË` 
   ORD     RIC     BÆÂRÆEøAË` 
   ClÍCt  A²¢BÓGÃO¿  GÃOGÃOÁ×
Á×
Áù FSL00548    T.N-rrppppprRIC AË` 
   ORD     RIC     BôÂ mEcýAË` (   C3C~  An³ÅBÕò5GÃO¿  GÃOGÃOÁ×
Á×
Áù FSL00548    T.N-rrppppprIAD AË` Ü   ORD     RIC     B¿&ÂÆÔE>l~AË` F   CS3C  A=PÈBÔéÙGÃO¿  GÃOGÃOÁ×
Á×
Áù FSL00548    T.N-rrppppprIAD AË` Ü   ORD     RIC     BG®Âÿ&E?cAË` ¾   C¹C  A$JBÕS.?
ó¿  GÃOGÃOÁ×
Á×
Áù FSL00548    T.N-rrppppprIAD AË` Ü   ORD     RIC     BEùÂÁµDäAË` Ü   CS3C An³ÅBµê?!ìè¿  GÃOGÃOÁ×
Á×
ÁøÈFSL00548    T.N-rrppppprIAD AË` Ü   ORD     RIC     BhÂy,DffAË`!   C9C AFB>Ã¯.¿  GÃOGÃOÁ×
Á×
Á÷ÿ/FSL00548    T.N-rrppppprRIC AË`!ê   ORD     RIC     BIcÂÄDD.IAË`!®   CyCp  An³ÅB½¸E>¾{¿  GÃOGÃOÁ×
Á×
ÁøÈFSL00548    T.N-rrppppprRIC AË`!ê   ORD     RIC     BòÂ¬`@¿AË`!ê   C3CP  @$JC#§>T4¿  GÃOGÃOÁ×
Á×
Áù FSL00548    R.N-rrpppp-rRIC AË`!ê   ORD     RIC     B)§Â F3[AË` F   C\YC  Bxý¹B®rGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00743    T.N-rrppppprALB AË`"b   ORD     HPN     B)Ð7Âl`F)AË`!r   C\¦fC BrÑZB°ujGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00743    T.N-rrppppprALB AË`"b   ORD     HPN     B)èÂAµFaAË`"D   C\&fC  BVGB²S"GÃO¿  GÃOGÃOÁ×
Á×
ACFSL00743    T.N-rrppppprALB AË`"b   ORD     HPN     B)ßÂÝÞFà AË`"b   C_YC BK¸RB¸èGÃO¿  GÃOGÃOÁ×
Á×
A eFSL00743    T.N-rrppppprALB AË`"b   ORD     HPN     B)OÂ>KFYAË`"   Cc¦fC  B3ÔBÌ)GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00743    T.N-rrppppprALB AË`#4   ORD     HPN     B)åÂ§E÷¦fAË`"ø   ChÙC  B&®BÌ[©?K¿  GÃOGÃOÁ×
Á×
A
óFSL00743    T.N-rrppppprALB AË`#4   ORD     HPN     B(÷wÂÄDEäAË`#   CmÙC BaBÌïGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00743    T.N-rrppppprALB AË`#4   ORD     HPN     B(ÑìÂzEÑoAË`#4   Cs&fC  B	ßBÐe?GÃO¿  GÃOGÃOÁ×
Á×
A,§ðFSL00743    T.N-rrppppprALB AË`#4   ORD     HPN     B(iÐÂ!HE¾XüAË`#R   CwÙC B£×BðÎOGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00743    T.N-rrppppprALB AË`$   ORD     HPN     B'ù,ÂÁµE«L0AË`#   C|&fC  Aî³ÅBñ	½GÃO¿  GÃOGÃOÁ×
Á×
AjFSL00743    T.N-rrppppprALB AË`$   ORD     HPN     B'nïÂKEffAË`#Ê   ClÍC AÚÜBðzGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00743    T.N-rrppppprALB AË`$   ORD     HPN     B&÷wÂä±EAË`$   CÓ3C  AÑä±BðÃGÃO¿  GÃOGÃOÁ×
Á×
ACFSL00743    T.N-rrppppprALB AË`$   ORD     HPN     B&ºÂ³3Ed$AË`$$   CÓ3C{  AæxBó¬GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00743    T.N-rrppppprALB AË`$   ORD     HPN     B&d±ÂiÐE>XüAË`$B   CS3C  AÚÜBð]ÁGÃO¿  GÃOGÃOÁ×
Á×
AjFSL00743    T.N-rrppppprALB AË`$   ORD     HPN     B&§Â(ERåAË`$~   CS3C A¬ÚtB÷©GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00743    T.N-rrppppprALB AË`$   ORD     HPN     B%¢"Â  Dä$AË`$   CS3Ct  AMÇCI?:Yj¿  GÃOGÃOÁ×
Á×
AÒ FSL00743    T.N-rrppppprALB AË`$   ORD     HPN     B%ÂÄDDffAË`$ö   CS3Cq  A=PÈC"t>³r¿  GÃOGÃOÁ×
Á×
A@OFSL00743    T.N-rrppppprHPN AË`%ª   ORD     HPN     B$m:ÂcDÊXAË`%P   C¹C_  AfxC×(>µg¿  GÃOGÃOÁ×
Á×
AÛFSL00743    R.N-rrpppp-rHPN AË`%ª   ORD     HPN     AõÂ¢ffF06fAË`Î   CTÙC A²¢@dìÕGÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.N-rrppppprCHS AË`Î                   AýÂ¢DDF06fAË` ¾   CTÙC  A(ö@dæ
GÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.N-rrppppprCHS AË` ¾                   BÂ¢33F~AË`!Ì   CkÙC A¬Út?åFGÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.N-rrppppprATL AË`!Ì                   BDDÂ¢<E½)AË`"   C~ÙC A¤JC²@WGÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCLT AË`"b   MIA     CAE     By,Â¢NE±íPAË`"   CìÍC  A²¢C¢ð¤GÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCLT AË`"b   MIA     CAE     B¯ÉÂ¢]E¤6AË`"&   CS3C  A=PÈC¦ðGÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCLT AË`"b   MIA     CAE     BëÂ¢_EáAË`"&   ClÍC A,ÚtC±EGÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCLT AË`"b   MIA     CAE     B'AÂ¢YEZAË`"D   CìÍC{  A5A4òþGÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCLT AË`"b   MIA     CAE     BbýÂ¢S Ev²AË`"D   ClÍC{  A5A5GÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCLT AË`"b   MIA     CAE     BÂ¢QE_¥¢AË`"b   CÓ3C Ad @¡JGÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCLT AË`"¼   MIA     CAE     BÕUÂ¢UUENAË`"b   C3C A,ÚtC¯±cGÃO¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCLT AË`"¼   MIA     CAE     BòÂ¢WåEE*áAË`"   CS3C Ad C±Q½>¬¤¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCLT AË`"¼   MIA     CAE     B;¼Â¢[OEB°åAË`"   C9C  A,ÚtC¯ï?¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCLT AË`"¼   MIA     CAE     BiÐÂ¢]ÞE?s×AË`"   C3C  A=PÈC°ÔÎ>ÐÑ¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCLT AË`"¼   MIA     CAE     BÆÂ¢_E1¼AË`"   C¹C A=PÈC±>äÂ/¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCLT AË`"¼   MIA     CAE     BÌÍÂ¢ffEà AË`"´   CyC  @öîïC­a²>às¢¿  GÃOGÃOÁ×
Á×
    FSL00941    T.N-rrppppprCLT AË`"¼                   BºÂ¢b"E£TAË`"¼   CÓ3C  @öîïC©>Ðø¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCAE AË`#   MIA     CAE     BßÂ¢d±E ÷ðAË`"¼   C3C @öîïC°>Ò÷¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCAE AË`#   MIA     CAE     BjÂ¢gADÓt{AË`"Ú   CS3C  @µC¯îC>GÙQ¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCAE AË`#   MIA     CAE     B*«Â¢höD'AË`"Ú   C¹C|  @¤JC±³>Ùt¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCAE AË`#   MIA     CAE     BP7Â¢j«DoDAË`"ø   C3Cs  @fxC±fí>N¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCAE AË`#   MIA     CAE     BtÂ¢höDROßAË`"ø   C¹C^  @fx@®$$>Ãï¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCAE AË`#   MIA     CAE     B0Â¢b"D&àÅAË`#   CyCP  @µA®uÖ>C¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCAE AË`#p   MIA     CAE     B¬`Â¢T{CÙîAË`#   CS3CD  Ad BK >¬t ¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCAE AË`#p   MIA     CAE     BÑìÂ¢>KBffAË`#4   CÓ3BÊ  @$JBG¦>ZM¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCAE AË`#p   MIA     CAE     B¿&Â¢HBùïAË`#4   CÓ3BÖ  @¤JCc|Y>º¤¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCAE AË`#p   MIA     CAE     BáHÂ¢33CÇ®AË`#R   C3C,  A=PÈBMd)>+¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCAE AË`#p   MIA     CAE     BÕUÂ¢(öCffAË`#R   C3C<  A=PÈBð´>aÉ,¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCAE AË`#p   MIA     CAE     BÀÚÂ¢1~CÇ¤ÝAË`#p   CyC?  A5C[É¨>?2¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCAE AË`#¬   MIA     CAE     B®Â¢:CÀ%AË`#p   CS3C>  A5C^Dü>]q¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCAE AË`#¬   MIA     CAE     B£×Â¢EùCøÕAË`#   C3C.  A$JCvËQ=ñ­?¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCAE AË`#¬   MIA     CAE     B´èÂ¢P7CyïAË`#   CyC#  A²¢CåÐ>'=¿  GÃOGÃOÁ×
Á×
    FSL00941    T.B-rrppppprCAE AË`#¬   MIA     CAE     BÅùÂ¢HB5?AË`#¬   C3Bî  @EòB'õU=éêÀ¿  GÃOGÃOÁ×
Á×
    FSL00941    T.G-rrppppprCAE AË`#¬   MIA     CAE     BÎÂ¢CjÁMÓAË`#¬   C¹Bº  ?²¢BHÊâ>Ìt¿  GÃOGÃOÁ×
Á×
    FSL00941    R.G-rrpppp-rCAE AË`#¬   MIA     CAE     B=¬`ÂôÞDuAË`!   CS3Ca  @¤JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00621    R.N-rrpppp-rSEA AË`!ê   SEA     LAX     B=µÂôÞDffAË`!®   CS3C=  @²¢C4eGÃO¿  GÃOGÃO<£×
Á×
    FSL00621    T.N-rrppppprSEA AË`!ê   SEA     LAX     B=IcÂôDå¥AË`!Ì   CS3C  ?ÅòC7t¹GÃO¿  GÃOGÃO<#×
Á×
    FSL00621    T.N-rrppppprSEA AË`!ê   SEA     LAX     B=ÂôEP}AË`!ê   ClÍC @²¢C4eGÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`!ê   SEA     LAX     B<í:ÂôÞE>õAË`!ê   C}ÙC  @fxC/í%GÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`"D   SEA     LAX     B<§Âô¿EdAË`"   Cy¦fCt  AíÌC-ådGÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`"D   SEA     LAX     B<LÍÂô E§¡AË`"&   CtÙCy  Ad C+	°GÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`"D   SEA     LAX     B<ÂôEp'AË`"D   CtYCi  AEòC*JVGÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`"D   SEA     LAX     B;ÉcÂôcE«µAË`"D   Cr&fCg  A¹33C+á}GÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`"   SEA     LAX     B;OÂôÔE¾áAË`"b   CnYCe  B+C-­GÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`"   SEA     LAX     B;y,ÂôùEÒZAË`"b   CkÙCR  B(¼ßC1$ÎGÃO¿  GÃOGÃO<#×
Á×
    FSL00621    T.N-rrppppprSEA AË`"   SEA     LAX     B;EùÂôjEäû#AË`"   Cf¦fCT  B;AþC.N'GÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`"   SEA     LAX     B:÷wÂô~KEø8´AË`"   CeYCR  BheC,pGÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`"ø   SEA     LAX     B:ÚtÂô{¼F^AË`"   Cb¦fCO  BíÌC*gGÃO¿  GÃOGÃO=#×
Á×
    FSL00621    T.N-rrppppprSEA AË`"ø   SEA     LAX     B:åÂôxRFà AË`"¼   C]YCP  BÈÑC.'ËGÃO¿  GÃOGÃO    Á×
    FSL00621    T.N-rrppppprSEA AË`"ø   SEA     LAX     B9ÅùÂôhFffAË`"ø   C[&fCY  B\.¦C+;GÃO¿  GÃOGÃO>#×
        FSL00621    T.N-rrppppprSEA AË`"ø   SEA     LAX     B8'AÂôP7FWAË`#p   CZÙCZ  B=PÈC-pÅGÃO¿  GÃOGÃO>.{        FSL00621    T.N-rrppppprSEA AË`$Ø   SEA     LAX     B6 Âô7wFAË`#è   CY¦fCZ  BQä±C-WGÃO¿  GÃOGÃO            FSL00621    T.N-rrppppprSEA AË`$Ø   SEA     LAX     B4ä±ÂôFAË`$`   CZYC\  BQä±C-bñGÃO¿  GÃOGÃO            FSL00621    T.N-rrppppprSEA AË`$Ø   SEA     LAX     B3IcÂôÔFýµAË`$Ø   CZÙC]  BK¸RC-&¿GÃO¿  GÃOGÃO            FSL00621    T.N-rrppppprSEA AË`$Ø   SEA     LAX     B1¨öÂóîïF7AË`%P   C[ÙC^  BG½C-wUGÃO¿  GÃOGÃO            FSL00621    T.N-rrppppprEUG AË`&¸   SEA     LAX     B0  ÂóÕUF7AË`%È   C\YC]  BSó|C-$rGÃO¿  GÃOGÃO            FSL00621    T.N-rrppppprEUG AË`&¸   SEA     LAX     B.]ÞÂó¿&FvAË`&@   C[¦fCY  BOÕçC-ô¹GÃO¿  GÃOGÃO            FSL00621    T.N-rrppppprEUG AË`&¸   SEA     LAX     B'ù,Â¯èDP}AË`!®   C3C  AfxGÃOGÃO¿  GÃOGÃOÁ×
Á×
AüPFSL00747    R.N-rrpppp-rORD AË`!ê   ORD     PDX     B'ôÂ°	cDffAË`!Ì   CùC£  AíÌCÌh>º=?¿  GÃOGÃOÁ×
Á×
AüPFSL00747    T.N-rrppppprORD AË`!ê   ORD     PDX     B'í:Â°1~Då5¨AË`!ê   CùC A²¢CL>á*P¿  GÃOGÃOÁ×
Á×
AÛFSL00747    T.N-rrppppprORD AË`!ê   ORD     PDX     B'èÂ°UUEÇðAË`!ê   C¬ÍC  AaCô
GÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00747    T.N-rrppppprORD AË`!ê   ORD     PDX     B'âýÂ°y,E?AË`"   C,ÍC A°ø	CôpGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00747    T.N-rrppppprORD AË`"b   ORD     PDX     B'õÃÂ°´èEdÔAË`"&   CS3C  Aâ[CvVGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00747    T.N-rrppppprORD AË`"b   ORD     PDX     B(òÂ°òYEAË`"D   CìÍC  Aâ[CGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00747    T.N-rrppppprORD AË`"b   ORD     PDX     B(OÂ±0Eª­AË`"b   CYC BFC¹GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00747    T.N-rrppppprORD AË`"b   ORD     PDX     B(*«Â±<E«ÊûAË`"   C{¦fC  B0ø	C¦îGÃO¿  GÃOGÃOÁ×
Á×
AjFSL00747    T.N-rrppppprORD AË`"Ú   ORD     PDX     B(IcÂ±µE¾ÁAË`"   CvÙC B,ÚtCBÞGÃO¿  GÃOGÃOÁ×
Á×
A'ò|FSL00747    T.N-rrppppprORD AË`"Ú   ORD     PDX     B(\)Â±ºEÑ½AË`"¼   Cr¦fC  B;AþCºkGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00747    T.N-rrppppprORD AË`"Ú   ORD     PDX     B(W
Â²Eä­AË`"Ú   CnÙC B=PÈCüVGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00747    T.N-rrppppprORD AË`"Ú   ORD     PDX     B(QìÂ²QE÷¦fAË`"ø   CiÙC  BEòC5GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00747    T.N-rrppppprMKE AË`#   ORD     PDX     B(KÂ²åFh;AË`#   CdYC  BOÕçCäGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00747    T.N-rrppppprMKE AË`#   ORD     PDX     B(aHÂ²í:Fà AË`#4   Ca&fC BC}(C³[GÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00747    T.N-rrppppprMKE AË`#   ORD     PDX     B(ª«Â³zFuAË`#   C[ÙC BI©CLñGÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00747    T.N-rrppppprMKE AË`#   ORD     PDX     B)Ó Âµ×åFÏAË`$º   CY&fC BXCáÖGÃO¿  GÃOGÃOÁ×
Á×
A'ò|FSL00747    T.N-rrppppprMSP AË`'0   ORD     PDX     B)í:Â¶F!öAË`$Ø   CW¦fC  B`L;CGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00747    T.N-rrppppprMSP AË`'0   ORD     PDX     B+§Â¸c×F&ÝAË`&   CXÙC  BdiÐC¿ GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00747    T.N-rrppppprMSP AË`'0   ORD     PDX     B''AÂ¯úDæ¾AË`°   CÓ3C AFC6¶m>ú[¿  GÃOGÃOÁ×
Á×
AÕgFSL00627    T.N-rrppppprMKE AË`Î   ORD     BNA     B&÷wÂ¯öE)yAË`Î   C3C A(öC+áGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00627    T.N-rrppppprMKE AË`Î   ORD     BNA     B&ºÂ¯ñ~E?/AË`Î   CìÍC Aâ[C*BGÃO¿  GÃOGÃOÁ×
Á×
ACFSL00627    T.N-rrppppprMKE AË` (   ORD     BNA     B&UUÂ¯çAEeª³AË`ì   C¬ÍC AÑä±C(GÃO¿  GÃOGÃOÁ×
Á×
A#=FSL00627    T.N-rrppppprMKE AË` (   ORD     BNA     B&¸Â¯ä±EAË` 
   CS3C  AæxC.¦ßGÃO¿  GÃOGÃOÁ×
Á×
A"FSL00627    T.N-rrppppprMKE AË` (   ORD     BNA     B%ÚtÂ¯â"Eª­AË` (   CYC B+C/¹àGÃO¿  GÃOGÃOÁ×
Á×
AjFSL00627    T.N-rrppppprMKE AË` (   ORD     BNA     B%jÂ¯àmE«·zAË` (   C{&fC BrêC1ÄPGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00627    T.N-rrppppprSTL AË`    ORD     BNA     B%,`Â¯ÝE¾°ÅAË` F   Cw¦fC B*ËªC/þGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00627    T.N-rrppppprSTL AË`    ORD     BNA     B$ÎÂ¯ÙEÑAË` d   Cs&fC B,ÚtC/ÛqGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00627    T.N-rrppppprSTL AË`    ORD     BNA     B$]ÞÂ¯ÕUEäÊ^AË`    Co¦fC  B5C/­GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00627    T.N-rrppppprSTL AË`    ORD     BNA     B#úáÂ¯ÑìE÷°'AË`     CjYC  B(¼ßC0NGÃO¿  GÃOGÃOÁ×
Á×
A µFSL00627    T.N-rrppppprSDF AË`#4   ORD     BNA     B#1~Â¯ËFc[AË` ¾   Ce&fC B(¼ßC0"5GÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00627    T.N-rrppppprSDF AË`#4   ORD     BNA     BNÂ¯iÐF
AË`"   CcYC BüC+, GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00627    T.N-rrppppprSDF AË`#4   ORD     BNA     B¢"Â®ÆÔF
ÍAË`#4   Ce&fC  AûC$ÅeGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00627    T.N-rrppppprSDF AË`#4   ORD     BNA     BÂ®FÔE÷åAË`#¬   ClÙC AÖGCFGÃO¿  GÃOGÃOÁ×
Á×
ACFSL00627    T.N-rrppppprSDF AË`$$   ORD     BNA     BòÂ®RÆEähÕAË`#Ê   Cr&fC  AÅòC>×­GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00627    T.N-rrppppprSDF AË`$$   ORD     BNA     B|Â®_EÑyKAË`$   CuÙC A¤JC>áGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00627    T.N-rrppppprSDF AË`$$   ORD     BNA     BËÂ®oÉE¾v?AË`$$   CzÙC  A*C>VâGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00627    T.N-rrppppprSDF AË`$$   ORD     BNA     BÂ®d±E«_±AË`$`   C¦fC  AÐ7C-6GÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00627    T.N-rrppppprSDF AË`$º   ORD     BNA     BuÃÂ®P7E\¥AË`$~   CìÍC AVGC%=dGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00627    T.N-rrppppprSDF AË`$º   ORD     BNA     BòÂ®CjE(ÕAË`$   C¬ÍC  AMÇC&iKGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00627    T.N-rrppppprSDF AË`$º   ORD     BNA     B¥Â®6EdrAË`$º   CÓ3C  A$JC%ùGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00627    T.N-rrppppprSDF AË`$º   ORD     BNA     BDDÂ®ÞE>l~AË`$Ø   C¹C A,ÚtCà?p¯¿  GÃOGÃOÁ×
Á×
A&`ªFSL00627    T.N-rrppppprBNA AË`%ª   ORD     BNA     B/ÉÂ­®ïEÝAË`%P   C¹Cw  A5CC±>îPÀ¿  GÃOGÃOÁ×
Á×
Aù	FSL00627    T.N-rrppppprBNA AË`%ª   ORD     BNA     BæfÂ­µDäAË`%n   C¹C A$JC?É¼¿  GÃOGÃOÁ×
Á×
Aù	FSL00627    T.N-rrppppprBNA AË`%ª   ORD     BNA     By,Â­CjD_AË`%ª   CyC A(öC@R>Ø*½¿  GÃOGÃOÁ×
Á×
A@OFSL00627    T.N-rrppppprBNA AË`%ª   ORD     BNA     BâýÂ­CjDÊXAË`&   C¹C @ÖGC4e>u¿  GÃOGÃOÁ×
Á×
A
óFSL00627    R.N-rrpppp-rBNA AË`&@   ORD     BNA     BÆÂ¨ì`Dr°AË`"Ú   CyCF  @²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00622    R.N-rrpppp-rATL AË`#4   ATL     DEN     B¸Â©D´nAË`"ø   C3Cu  @$JC¬>§5¿  GÃOGÃOÁ×
Á×
A'ò|FSL00622    T.N-rrppppprATL AË`#4   ATL     DEN     B¥Â©+DäÀAË`#   CyC  @EòC¬>r¹¿¿  GÃOGÃOÁ×
Á×
A®}FSL00622    T.N-rrppppprATL AË`#4   ATL     DEN     B¬`Â©DDEuAË`#   CÓ3C @ÅòCì¯>å5x¿  GÃOGÃOÁ×
Á×
AÛFSL00622    T.N-rrppppprATL AË`#4   ATL     DEN     B¸RÂ©_E?/AË`#4   CS3C  A²¢C*d>¿  GÃOGÃOÁ×
Á×
A±ÄFSL00622    T.N-rrppppprATL AË`#   ATL     DEN     BÌÍÂ©§Ed­AË`#R   CS3C  AEòCCGÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00622    T.N-rrppppprATL AË`#   ATL     DEN     BâýÂ©»¼E§¡AË`#p   C¬ÍC  AaCÃÜGÃO¿  GÃOGÃOÁ×
Á×
A&`ªFSL00622    T.N-rrppppprATL AË`#   ATL     DEN     BôÂ©âýEp'AË`#   C3C A²¢C!GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00622    T.N-rrppppprATL AË`#   ATL     DEN     BÆÂª"ýE«|ôAË`#¬   C3C  A²¢C¾ªGÃO¿  GÃOGÃOÁ×
Á×
A$ÎÙFSL00622    T.N-rrppppprATL AË`$   ATL     DEN     B'AÂªLÍE¾áAË`#¬   C~¦fC AµCà¼GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00622    T.N-rrppppprATL AË`$   ATL     DEN     BIcÂªcEÑAË`#Ê   CxÙC|  A°ø	CÜþGÃO¿  GÃOGÃOÁ×
Á×
A#=FSL00622    T.N-rrppppprATL AË`$   ATL     DEN     BrYÂªÚtEäÔAË`$   CuYC A(öC GÃO¿  GÃOGÃOÁ×
Á×
AFSL00622    T.N-rrppppprATL AË`$   ATL     DEN     B0Â«'AE÷¹èAË`$$   CoÙC  AFChGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00622    T.N-rrppppprATL AË`%   ATL     DEN     B½qÂ«vFh;AË`$B   CkYC  A¹33CñeGÃO¿  GÃOGÃOÁ×
Á×
A#=FSL00622    T.N-rrppppprATL AË`%   ATL     DEN     BÜ)Â«®ïFäàAË`$`   CeÙC|  AÍÇCÝGÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00622    T.N-rrppppprATL AË`%   ATL     DEN     B	hÂ®&fF¨AË`%   Cb¦fCz  AòÑZC³f?K¿  GÃOGÃOÁ×
Á×
A®}FSL00622    R.N-rrpppp-rATL AË`%   ATL     DEN     B0_ÂönDÖ¡AË`!6   CÓ3C¤  @$JGÃOGÃO¿  GÃOGÃOÁ×
Á×
AüPFSL00657    R.N-rrpppp-rEUG AË`!r   EUG     SFO     B033Âöq~DuAË`!6   C3C° @ÖGC<¼-GÃO¿  GÃOGÃOÁ×
Á×
A"FSL00657    T.N-rrppppprEUG AË`!r   EUG     SFO     B/úáÂötèDåÑ·AË`!T   CÓ3    @ÅòC:æ`GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00657    T.N-rrppppprEUG AË`!r   EUG     SFO     B/ÑìÂöxREîóAË`!r   CìÍC A²¢C=sÒGÃO¿  GÃOGÃOÁ×
Á×
AjFSL00657    T.N-rrppppprEUG AË`!r   EUG     SFO     B/£×Âö{¼E>ºAË`!r   C¦fC¢ @²¢C<jnGÃO¿  GÃOGÃOÁ×
Á×
AjFSL00657    T.N-rrppppprEUG AË`!Ì   EUG     SFO     B/X¿ÂöµEdAË`!   C|YCn  @µC=GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00657    T.N-rrppppprEUG AË`!Ì   EUG     SFO     B/ÂöEàAË`!®   CyYCq  AMÇC>Æ-GÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00657    T.N-rrppppprEUG AË`!Ì   EUG     SFO     B.ÉcÂö§E©AË`!Ì   CvÙC]  AFC<òGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00657    T.N-rrppppprEUG AË`!Ì   EUG     SFO     B.\ÂöE«·zAË`!Ì   CtYCO  AÉ©C:²ËGÃO¿  GÃOGÃOÁ×
Á×
A±ÄFSL00657    T.N-rrppppprEUG AË`"&   EUG     SFO     B.NÂö0E¾ AË`!ê   CqYCM  Aÿ*C<öPGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00657    T.N-rrppppprEUG AË`"&   EUG     SFO     B-üÂö)EÑÚÔAË`"   Cl&fCL  B£×C<HGÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00657    T.N-rrppppprEUG AË`"&   EUG     SFO     B-ºÂö mEäû#AË`"&   Ci¦fCN  BG½C;JGÃO¿  GÃOGÃOÁ×
Á×
A1]cFSL00657    T.N-rrppppprEUG AË`"&   EUG     SFO     B-Âö mEøiyAË`"&   ChYCK  BrC4eGÃO¿  GÃOGÃOÁ×
Á×
AØ­FSL00657    T.N-rrppppprEUG AË`"Ú   EUG     SFO     B-rYÂöF^zAË`"&   Cb¦fCH  BºC1º#GÃO¿  GÃOGÃOÁ×
Á×
ACFSL00657    T.N-rrppppprEUG AË`"Ú   EUG     SFO     B,BÂö\FýCAË`"   C]&fCF  B*C-ëfGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00657    T.N-rrppppprEUG AË`"Ú   EUG     SFO     B+hÂöF~ÉAË`"Ú   C\¦fCR  B=PÈC.¨nGÃO¿  GÃOGÃOÁ×
Á×
A/ËFSL00657    T.N-rrppppprEUG AË`"Ú   EUG     SFO     B'býÂöX¿FåSAË`$   C^¦fCX  BAn]C/GÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00657    T.N-rrppppprSFO AË`'   EUG     SFO     B#]ÞÂö/ÉFÖ²AË`%2   C^ÙC[  BOÕçC/v)GÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00657    T.N-rrppppprSFO AË`'   EUG     SFO     BuÃÂöùFê4AË`&^   C\YC_  Bõ1C/;±GÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00657    T.N-rrppppprSFO AË`'   EUG     SFO     B»¼ÂôhD$©AË`!®   CS3Cl  An³ÅGÃOGÃO¿  GÃOGÃOÁ×
Á×
A96zFSL00659    R.N-rrpppp-rSFO AË`!ê   OAK     LAX     B±~ÂôwwDffAË`!®   C9C]  AFC{}>®>¿  GÃOGÃOÁ×
Á×
@þFtFSL00659    T.N-rrppppprSFO AË`!ê   OAK     LAX     B±~Âô7DåÑ·AË`!Ì   CS3CT  A¨¼ßCÿ'GÃO¿  GÃOGÃOÁ×
Á×
A=ëîFSL00659    T.N-rrppppprSFO AË`!ê   OAK     LAX     B§ÂôE´nAË`!ê   CÓ3Cp  A°ø	CWBGÃO¿  GÃOGÃOÁ×
Á×
A/ËFSL00659    T.N-rrppppprSFO AË`!ê   OAK     LAX     BiÐÂô¸E?/AË`!ê   C3CQ  A°ø	C9m¼GÃO¿  GÃOGÃOÁ×
Á×
A±ÄFSL00659    T.N-rrppppprSJC AË`"D   OAK     LAX     BNÂôEe2AË`"   C,ÍCT  B	ßC7GÃO¿  GÃOGÃOÁ×
Á×
AjFSL00659    T.N-rrppppprSJC AË`"D   OAK     LAX     B%ÂôEmAË`"&   CS3CK  BíÌC4eGÃO¿  GÃOGÃOÁ×
Á×
A)MFSL00659    T.N-rrppppprSJC AË`"D   OAK     LAX     BôÂôÞE+AË`"&   ClÍCG  Aÿ*C0GÃO¿  GÃOGÃOÁ×
Á×
A.9ÁFSL00659    T.N-rrppppprSJC AË`"D   OAK     LAX         ÂôtèE«ÊûAË`"b   C}YCY  BrêC3äGÃO¿  GÃOGÃOÁ×
Á×
A96zFSL00659    XBN-rrpppFprSFO AË`"   OAK     LAX     BÂôWåE¾°ÅAË`"b   Cz&fC_  Brê=±ãGÃO¿  GÃOGÃOÁ×
Á×
@èMFSL00659    T.N-rrppppprSFO AË`"   OAK     LAX     B?&Âô)ÐEÑäAË`"   CuÙC^  BOÕçBþn~GÃO¿  GÃOGÃOÁ×
Á×
A±ÄFSL00659    T.N-rrppppprSFO AË`"   OAK     LAX     Bù,Âóû¼EäÔAË`"   CsÙCb  Btà$Bþp{GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00659    T.N-rrppppprSFO AË`"   OAK     LAX     By,Âó¦fE÷×+AË`"Ú   CnÙCf  B{BýÁ¾GÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00659    T.N-rrppppprSFO AË`#p   OAK     LAX     Bä±ÂóFÔFc[AË`#   Cj¦fCe  B}OBÿ³*GÃO¿  GÃOGÃOÁ×
Á×
AÒ FSL00659    XBN-rrpppFprSFO AË`#p   OAK     LAX         Âòä±Fî¡AË`#4   CfYCo  BrC3·ZGÃO¿  GÃOGÃOÁ×
Á×
A!«6FSL00659    XBN-rrpppFprSFO AË`#p   OAK     LAX         Âò
FkGAË`#R   CaÙCm  BvîïB´eGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00659    XBN-rrpppFprSFO AË`#p   OAK     LAX     B/ÉÂðO\F_?AË`$   C^ÙCn  BG½?é	GÃO¿  GÃOGÃOÁ×
Á×
AcñFSL00659    XBN-rrpppFprLAX AË`%È   OAK     LAX     BÂîs3Fà AË`%È   CgYCj  B0ø	C	îKGÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00659    T.N-rrppppprLAX AË`%È   OAK     LAX     B""ÂîzáFbAË`%È   CeÙCi  B3ÔCUGÃO¿  GÃOGÃOÁ×
Á×
A"FSL00659    T.N-rrppppprLAX AË`%È   OAK     LAX     B
Ü)Âî]FT¹AË`%È   Cl&fCl  B,ÚtCGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00659    T.N-rrppppprLAX AË`%È   OAK     LAX     B
 mÂîDDE÷X_AË`%æ   Cq¦fCl  B"C^GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00659    T.N-rrppppprLAX AË`&"   OAK     LAX     B
_Âî(öEäAË`%æ   Cx&fCh  B"Cé"GÃO¿  GÃOGÃOÁ×
Á×
A±ÄFSL00659    T.N-rrppppprLAX AË`&"   OAK     LAX     B
ÂîEÑoAË`&   C}¦fCb  B+CGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00659    T.N-rrppppprLAX AË`&"   OAK     LAX     B	Ü)ÂíôE¾;ºAË`&"   C,ÍCo  BüCÍyGÃO¿  GÃOGÃOÁ×
Á×
ACFSL00659    T.N-rrppppprLAX AË`&"   OAK     LAX     B	¯ÉÂíâ"E«%,AË`&"   C3Cp  B£×CºGÃO¿  GÃOGÃOÁ×
Á×
A2ï5FSL00659    T.N-rrppppprLAX AË`&¸   OAK     LAX     B	]ÞÂíÄDE\¥AË`&@   ClÍCk  AòÑZCéBGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00659    T.N-rrppppprLAX AË`&¸   OAK     LAX     BáHÂí¯ÉE(ÕAË`&^   CìÍCg  AÍÇC!ÏñGÃO¿  GÃOGÃOÁ×
Á×
AAFSL00659    T.N-rrppppprLAX AË`&¸   OAK     LAX     B½qÂìÞ¸DÀ¸AË`%   C3C  ?ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
A±ÄFSL00684    R.N-rrpppp-rLAX AË`%È   LAX     LAS     B®Âì÷wDuAË`%ª   C3Cb  @fxC|¿.>sú±¿  GÃOGÃOÁ×
Á×
A¬FSL00684    T.N-rrppppprLAX AË`%È   LAX     LAS     BåÂíDå¥AË`%È   C¹Cl  A=PÈCp>[>¿  GÃOGÃOÁ×
Á×
A±ÄFSL00684    T.N-rrppppprLAX AË`%È   LAX     LAS     BµÂíE÷AË`%È   C3Ce  AvîïCh>¸w³¿  GÃOGÃOÁ×
Á×
A2ï5FSL00684    T.N-rrppppprLAX AË`%È   LAX     LAS     BUUÂítE>ÎAË`%æ   C3Cp  A½PÈC6/RGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00684    T.N-rrppppprLAX AË`&|   LAX     LAS     B'AÂíÆEdû#AË`%æ   ClÍCg  A½PÈC!³GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00684    T.N-rrppppprLAX AË`&|   LAX     LAS     B®ÂìµEAË`&^   CìÍCn  AÑä±BáZYGÃO¿  GÃOGÃOÁ×
Á×
A,§ðFSL00684    T.N-rrppppprLAX AË`&|   LAX     LAS     Bä±ÂìQìEyèAË`&|   C,ÍCw  AòÑZBq	GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00684    T.N-rrppppprLAX AË`&|   LAX     LAS     BÂì!HE«·zAË`&   ClÍC  B²¢Bo¾zGÃO¿  GÃOGÃOÁ×
Á×
A1]cFSL00684    T.N-rrppppprLAX AË`&Ö   LAX     LAS     B|ÂìÔE¾ÄFAË`&   CS3Co  Aÿ*AçÁIGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00684    T.N-rrppppprLAX AË`&Ö   LAX     LAS     B	åÂï¿DAË`$~   C3C @ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
A.9ÁFSL00467    R.N-rrpppp-rLAX AË`$   SBA     SFO     B	 Âï¢ýDP}AË`$~   CS3Cf  @fxC\}>¦>þ¿  GÃOGÃOÁ×
Á×
A
óFSL00467    T.N-rrppppprLAX AË`$   SBA     SFO     B	y,ÂïºáDäÀAË`$   CùCV  @æxCî>f¦¿  GÃOGÃOÁ×
Á×
A
óFSL00467    T.N-rrppppprLAX AË`$   SBA     SFO     B	wwÂïËòEÙ	AË`$   ClÍCd  AFC§GÃO¿  GÃOGÃOÁ×
Á×
A"FSL00467    T.N-rrppppprLAX AË`$   SBA     SFO     B	wwÂïÝÞE?AË`$º   CìÍCh  A½PÈCÿ'GÃO¿  GÃOGÃOÁ×
Á×
A
óFSL00467    T.N-rrppppprLAX AË`$ö   SBA     SFO     B	|ÂïúEdç¡AË`$º   CìÍCa  AÖGC:GÃO¿  GÃOGÃOÁ×
Á×
A,§ðFSL00467    T.N-rrppppprLAX AË`$ö   SBA     SFO     B	¥ÂðìEàAË`$Ø   C3Ce  AÞ=qCLgGÃO¿  GÃOGÃOÁ×
Á×
A=ëîFSL00467    T.N-rrppppprLAX AË`$ö   SBA     SFO     B	ÌÍÂð"ýEÛqAË`$ö   CÓ3Cg  AæxC~GÃO¿  GÃOGÃOÁ×
Á×
ACFSL00467    T.N-rrppppprLAX AË`$ö   SBA     SFO     B	õÃÂð4èE«µAË`$ö   ClÍCr  BUUChOGÃO¿  GÃOGÃOÁ×
Á×
ACFSL00467    T.N-rrppppprLAX AË`%P   SBA     SFO     B
6ÂðQìE¾ÎAË`%   C&fCr  B(¼ßC<GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00467    T.N-rrppppprLAX AË`%P   SBA     SFO     B
tÂðnEÑîVAË`%2   Cz¦fCp  B933C¾ÕGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00467    T.N-rrppppprLAX AË`%P   SBA     SFO     B
ÕUÂðtEäÝàAË`%P   CtÙCk  B5CÏGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00467    T.N-rrppppprLAX AË`%P   SBA     SFO     BOÂðºE÷ÍjAË`%n   Co¦fCk  BAn]C÷pGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00467    T.N-rrppppprSFO AË`'   SBA     SFO     BÂðôFYAË`%   Cj¦fCn  BMÇC{GÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00467    T.N-rrppppprSFO AË`'   SBA     SFO     B4èÂæ]DjAË`&|   C9Ce  AVGGÃOGÃO¿  GÃOGÃOÁ×
Á×
A µFSL00568    R.N-rrpppp-rLAS AË`&Ö   LAS     SFO     B0Âæl`Då¥AË`&   CùCW  AVGC`ÿR>f Õ¿  GÃOGÃOÁ×
Á×
AFÜFSL00568    T.N-rrppppprLAS AË`&Ö   LAS     SFO     B@ÚÂæ[ODP}AË`#Ê   CyCb  An³ÅGÃOGÃO¿  GÃOGÃOÁ×
Á×
@þFtFSL00090    R.N-rrpppp-rLAS AË`$$   LAS     LAX     BOÂæhDäç¡AË`#è   CS3Ci  AvîïCVE³>tFµ¿  GÃOGÃOÁ×
Á×
A®}FSL00090    T.N-rrppppprLAS AË`$$   LAS     LAX     B
=Âæ}qEîóAË`$   CyCf  AaCx0D>lR¿  GÃOGÃOÁ×
Á×
Aù	FSL00090    T.N-rrppppprLAS AË`$$   LAS     LAX     BßÂæÔE>AË`$$   ClÍCr  A¹33CK½IGÃO¿  GÃOGÃOÁ×
Á×
A'ò|FSL00090    T.N-rrppppprLAS AË`$$   LAS     LAX     B³3ÂæEe¥AË`$$   C3Ci  AµC/GÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00090    T.N-rrppppprLAS AË`$`   LAS     LAX     BkÂæjEmAË`$B   ClÍCq  AÍÇC1IAGÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00090    T.N-rrppppprLAS AË`$`   LAS     LAX     BÆÂæ&EîóAË`$`   CÓ3Cv  AæxC.GÃO¿  GÃOGÃOÁ×
Á×
A+FSL00090    T.N-rrppppprLAS AË`$`   LAS     LAX     BØ¿Âæ{¼E«|ôAË`$`   C,ÍCs  AöîïC-M5GÃO¿  GÃOGÃOÁ×
Á×
ACFSL00090    T.N-rrppppprLAS AË`$`   LAS     LAX     By,ÂætèE¾ÄFAË`$~   CS3Cs  B	ßC+áGÃO¿  GÃOGÃOÁ×
Á×
A,§ðFSL00090    T.N-rrppppprLAS AË`$Ø   LAS     LAX     B*«Âæp¤EÑAË`$   C{YCs  BUUC-ÏGÃO¿  GÃOGÃOÁ×
Á×
A,§ðFSL00090    T.N-rrppppprLAS AË`$Ø   LAS     LAX     BÝÞÂæjEä¶ÜAË`$º   Cu¦fCr  BrêCN
ÄGÃO¿  GÃOGÃOÁ×
Á×
A7¤¨FSL00090    T.N-rrppppprLAS AË`$Ø   LAS     LAX     BåÂæºE÷ÍjAË`$Ø   CoÙCq  B(öCmX¸GÃO¿  GÃOGÃOÁ×
Á×
A.9ÁFSL00090    T.N-rrppppprLAS AË`$Ø   LAS     LAX     BÂç'AFYAË`%   CjÙCx  BFCl·TGÃO¿  GÃOGÃOÁ×
Á×
A.9ÁFSL00090    T.N-rrppppprLAS AË`&¸   LAS     LAX     B
EùÂé7Fc[AË`&@   Ck&fCo  Bd Cj$`GÃO¿  GÃOGÃOÁ×
Á×
A.9ÁFSL00090    T.N-rrppppprLAS AË`&¸   LAS     LAX     B	»¼ÂéÔE÷$AË`&|   Cp&fCw  B	ßCoÂºGÃO¿  GÃOGÃOÁ×
Á×
A,§ðFSL00090    T.N-rrppppprLAS AË`&¸   LAS     LAX     B,õÃÂé]E¾§AË`°   CxYCf  BÁlCikGÃO¿  GÃOGÃOÁ×
Á×
A!«6FSL00732    T.N-rrppppprBOI AË` (   BOI     SFO     B,¬`Âé§EÑ³ÐAË`Î   CsÙCi  BÐ7ChõéGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00732    T.N-rrppppprBOI AË` (   BOI     SFO     B,ZtÂéÌÍEäÔAË`ì   Cn¦fCg  B(öCmGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00732    T.N-rrppppprBOI AË` (   BOI     SFO     B+×
Âê%E÷¦fAË` (   ChÙCh  AûCiyGÃO¿  GÃOGÃOÁ×
Á×
A'ò|FSL00732    T.N-rrppppprBOI AË` (   BOI     SFO     B+BÂê§FYAË` d   CcYCe  B	ßCjDGÃO¿  GÃOGÃOÁ×
Á×
A2ï5FSL00732    T.N-rrppppprBOI AË`!ê   BOI     SFO     B*ÅùÂêáHFî¡AË`     C^&fCk  Bd CiO(GÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00732    T.N-rrppppprBOI AË`!ê   BOI     SFO     B*S Âë(öFkGAË` ¾   C[&fCq  B=PÈCgiäGÃO¿  GÃOGÃOÁ×
Á×
A&`ªFSL00732    T.N-rrppppprBOI AË`!ê   BOI     SFO     B'åÂìâ"F^íAË`!ê   C_&fCn  Bj0Cg*GÃO¿  GÃOGÃOÁ×
Á×
A#=FSL00732    T.N-rrppppprBOI AË`!ê   BOI     SFO     B$þKÂî{¼Fh®AË`#   C`ÙCl  BÈÑCfåGÃO¿  GÃOGÃOÁ×
Á×
AFSL00732    T.N-rrppppprSFO AË`&¸   BOI     SFO     B"~KÂïýqFmAË`$B   Ca&fCh  B(öCfOPGÃO¿  GÃOGÃOÁ×
Á×
AFSL00732    T.N-rrppppprSFO AË`&¸   BOI     SFO     B µÂñoÉFmAË`%n   Ca&fCe  BMðCePGÃO¿  GÃOGÃOÁ×
Á×
AÒ FSL00732    T.N-rrppppprSFO AË`&¸   BOI     SFO     BÔÂòÚtFwOAË`&   Cb&fCe  B(öCdËGÃO¿  GÃOGÃOÁ×
Á×
AÒ FSL00732    R.N-rrpppp-rSFO AË`&¸   BOI     SFO     B¬`Âó7wDªÎAË` ú   C¹CR  AvîïGÃOGÃO¿  GÃOGÃOÁ×
Á×
AFÜFSL00282    R.N-rrpppp-rSFO AË`!6   SMF     LAX     BÂó%DuAË`!   C3C]  AvîïBþ£>w;/¿  GÃOGÃOÁ×
Á×
A±ÄFSL00282    T.N-rrppppprSFO AË`!6   SMF     LAX     Bm:Âó	cDåÑ·AË`!   CS3Cg  AÐ7Bôõ½?6½¿  GÃOGÃOÁ×
Á×
A1]cFSL00282    T.N-rrppppprSFO AË`!6   SMF     LAX     BX¿Âò÷wE´nAË`!6   ClÍCY  A¬ÚtBïÝGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00282    T.N-rrppppprSFO AË`!6   SMF     LAX     BDDÂòåE?AË`!6   CÓ3CR  AÖGBïGÃO¿  GÃOGÃOÁ×
Á×
AjFSL00282    T.N-rrppppprSFO AË`!®   SMF     LAX     BOÂòÆÔEdAË`!T   CÓ3CS  Aÿ*B÷eGÃO¿  GÃOGÃOÁ×
Á×
A+FSL00282    T.N-rrppppprSFO AË`!®   SMF     LAX     Bí:Âò¹,E±bAË`!r   CìÍCH  AöîïC[(GÃO¿  GÃOGÃOÁ×
Á×
AjFSL00282    T.N-rrppppprSFO AË`!®   SMF     LAX     BUUÂò§E©AË`!®   C,ÍC\  Aî³ÅC2GÃO¿  GÃOGÃOÁ×
Á×
A'ò|FSL00282    T.N-rrppppprSFO AË`!®   SMF     LAX     BOÂòhE«·zAË`!Ì   C|ÙCb  B*ËªBÿgfGÃO¿  GÃOGÃOÁ×
Á×
A
óFSL00282    T.N-rrppppprSFO AË`"D   SMF     LAX     BÔÂòµE¾ÎAË`"   CwÙCe  B$JBûç^GÃO¿  GÃOGÃOÁ×
Á×
A/ËFSL00282    T.N-rrppppprSFO AË`"D   SMF     LAX     B_ÂñÂEÑªAË`"&   Cv&fCa  B?_BÖGÃO¿  GÃOGÃOÁ×
Á×
A"FSL00282    T.N-rrppppprSFO AË`"D   SMF     LAX     B{ÂñiÐEäÀAË`"D   CsYCe  Bj0BááGÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00282    T.N-rrppppprSFO AË`"D   SMF     LAX     B¸RÂñ2YE÷ÍjAË`"b   CmÙCg  B`L;Cº³GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00282    T.N-rrppppprLAX AË`$Ø   SMF     LAX     BbýÂðþKFc[AË`"   CiÙCe  BrÑZCYQGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00282    T.N-rrppppprLAX AË`$Ø   SMF     LAX     BáHÂï6F
!­AË`#¬   Cg¦fCl  Bl¤úCÙGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00282    T.N-rrppppprLAX AË`$Ø   SMF     LAX     BÀÚÂï<F
+nAË`$Ø   CgÙCk  BXC4¢vGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00282    T.N-rrppppprLAX AË`$Ø   SMF     LAX     B´èÂî«F
ìAË`&   CgYCp  B7$iC$]ÊGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00282    T.N-rrppppprLAX AË`&   SMF     LAX     B
ËÂîEùE÷¥AË`&^   Co&fCl  B7$iC6GÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00282    T.N-rrppppprLAX AË`&   SMF     LAX     B
~KÂî%EäKAË`&|   Cu&fCl  B.é?CÕGÃO¿  GÃOGÃOÁ×
Á×
@þFtFSL00282    T.N-rrppppprLAX AË`&   SMF     LAX     B
33ÂîùEÑyKAË`&|   CzÙCg  B$JCòêGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00282    T.N-rrppppprLAX AË`&   SMF     LAX     B	èÂíåE¾v?AË`&   C¦fCq  BUUC24GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00282    T.N-rrppppprLAX AË`'   SMF     LAX     B:ÂæZtDAË`"¼   CS3Cc  A^=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
A µFSL00822    R.N-rrpppp-rLAS AË`"ø   LAS     DEN     B mÂæaHDæÊAË`"Ú   C9Co  An³ÅCP>JË¿  GÃOGÃOÁ×
Á×
Ag8FSL00822    T.N-rrppppprLAS AË`"ø   LAS     DEN     BåÂærYEP}AË`"Ú   C¹Cm  AíÌCõ>´_½¿  GÃOGÃOÁ×
Á×
A+FSL00822    T.N-rrppppprLAS AË`"ø   LAS     DEN     B@ÚÂæ{¼E>õAË`"ø   C3Ce  A¬ÚtC§¯¼GÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00822    T.N-rrppppprLAS AË`"ø   LAS     DEN     BåÂæd±Ed­AË`#   CS3Ci  AæxAß6kGÃO¿  GÃOGÃOÁ×
Á×
A:ÈLFSL00822    T.N-rrppppprLAS AË`#p   LAS     DEN     B¬`ÂæHEAË`#   CS3Cr  AÞ=qB	ÒGÃO¿  GÃOGÃOÁ×
Á×
AjFSL00822    T.N-rrppppprLAS AË`#p   LAS     DEN     BÄDÂåÝÞEffAË`#R   C¬ÍCz  Aâ[B§;¡GÃO¿  GÃOGÃOÁ×
Á×
A
óFSL00822    T.N-rrppppprLAS AË`#p   LAS     DEN     BÎÂå®E«vAË`#p   C3Cy  AòÑZB§ÇGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00822    T.N-rrppppprLAS AË`#p   LAS     DEN     BßÂåDE¾ AË`#p   C¦fCz  AûBñ5GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00822    T.N-rrppppprLAS AË`#Ê   LAS     DEN     BÂåNEÑªAË`#   Cz¦fC|  B+B}GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00822    T.N-rrppppprLAS AË`#Ê   LAS     DEN     B*«ÂåEäñbAË`#¬   CuYCx  BUUBùQGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00822    T.N-rrppppprLAS AË`#Ê   LAS     DEN     BP7ÂäÅùE÷×+AË`#Ê   Cp&fC{  B7ÀBrGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00822    T.N-rrppppprLAS AË`#Ê   LAS     DEN     BtÂäòFqüAË`#Ê   CjÙC~  B(öB¯úGÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00822    T.N-rrppppprLAS AË`$$   LAS     DEN     B¬`Âä.FóAË`$   CeYC~  B,ÚtBÀGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00822    T.N-rrppppprLAS AË`$$   LAS     DEN     BßÂã×åFKAË`$$   C`YC  B3ÔBìGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00822    T.N-rrppppprLAS AË`$$   LAS     DEN     BjÂã)F"/AË`$$   C\&fC  B(¼ßBíGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00822    R.N-rrpppp-rLAS AË`$$   LAS     DEN     BUÂ¹  F`wAË` d   C^&fC  A=PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00512    R.N-rrppp--rLIT AË` d                   B'Â³ïAöGEAË`#é  C3GÃOGÃOGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00512    XWN-rrpBF--rMEM AË`#è                   A»¼ÂÆ""EIîAË`Î   C3C±  @²¢CO>à¿  GÃOGÃOÁ×
Á×
    FSL00362    T.N-rrppppprMEX AË`Î                   A33ÂÆ33Eà AË` ¾   CùC:      C9þÿ>^E}¿  GÃOGÃOÁ×
Á×
    FSL00362    R.N-rrpppp-rMEX AË` ¾                   BÂ©&¯F'ñÞAË`"   CZ&fC  A¨¼ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00528    R.N-rrppp--rCLT AË`"                   B
ìÂ±\EÏCAË`%  C{&fC A µGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00528    R.N-rrppp--rBNA AË`%                   Að""Â³*«E´ùAË`$~   C3Cu  AMÇGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00284    R.N-rrpppp-rMOB AË`$~                   Aó33Â±  E> AË`%n   C3C  @öîïB	É?\9¿  GÃOGÃOÁ×
Á×
    FSL00284    T.N-rrppppprMOB AË`%n                   AöDDÂ°;¼E:°¤AË`&|   CÓ3Cw  A$JB4@>+a¿  GÃOGÃOÁ×
Á×
    FSL00284    T.N-rrppppprMOB AË`&|                   B»¼ÂËæfF06fAË`Î   CVÙC B µB·áGÃO¿  GÃOGÃOÁ×
Á×
    FSL00701    T.N-rrppppprDEN AË`Î                   BffÂÈ³3F06fAË` ¾   CW&fC Bd B¹úGÃO¿  GÃOGÃOÁ×
Á×
    FSL00701    T.N-rrppppprDEN AË` ¾                   B  ÂÅ F06fAË`!®   CVÙC Bd B»*GÃO¿  GÃOGÃOÁ×
Á×
    FSL00701    T.N-rrppppprMCI AË`!Ì                   BwwÂÂUUF06fAË`"¼   CU¦fC B µB½¤%GÃO¿  GÃOGÃOÁ×
Á×
    FSL00701    T.N-rrppppprTUL AË`"¼                   BîïÂ¿33F06fAË`#¬   CUYC BUUB½¾,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00701    T.N-rrppppprMCI AË`#¬                   BDDÂ¼F06fAË`$º   CVYC  B7ÀBÀ(¿GÃO¿  GÃOGÃOÁ×
Á×
    FSL00701    T.N-rrppppprSTL AË`$º                   B""Â¹  F06fAË`%ª   CWYC  BrêBÈô GÃO¿  GÃOGÃOÁ×
Á×
    FSL00701    T.N-rrppppprMCI AË`%ª                   BCUUÂõDDESt{AË`ì   C{YCy  @¤JC©?GÃO¿  GÃOGÃOÁ×
Á×
    FSL00287    T.N-rrppppprSEA AË` 
                   BDÂöîïD=AË` Ü   C,ÍBÌ  @(öCåGÃO¿  GÃOGÃOÁ×
Á×
    FSL00287    R.N-rrpppp-rPAE AË` ú                   B!»¼ÂÉ*«F06fAË`    CVÙC B"B¬}GÃO¿  GÃOGÃOÁ×
Á×
    FSL00434    T.N-rrppppprDEN AË`                    B"îïÂÆF06fAË`!r   CU&fC B*ËªBTÂGÃO¿  GÃOGÃOÁ×
Á×
    FSL00434    T.N-rrppppprMCI AË`!                   B#îïÂÂÝÞF06fAË`"   CW&fC  B"B¢GÃO¿  GÃOGÃOÁ×
Á×
    FSL00434    T.N-rrppppprOMA AË`"                   B$îïÂ¿³3F¯\AË`#p   C^YC BüB¢GÃO¿  GÃOGÃOÁ×
Á×
    FSL00434    T.N-rrppppprMCI AË`#                   B%»¼Â¼îïE> AË`$~   C3C AÞ=qB£ûGÃO¿  GÃOGÃOÁ×
Á×
    FSL00434    T.N-rrppppprMCI AË`$~                   B&  Â»DDD'£×AË`%n   CÓ3C¤ @ÖGBªÝk?Ft¿  GÃOGÃOÁ×
Á×
    FSL00434    R.N-rrpppp-rDSM AË`%n                   BDDÂÀ]ÞF06fAË` d   CYYC  BüA½x1GÃO¿  GÃOGÃOÁ×
Á×
    FSL00365    T.N-rrppppprDFW AË` d                   B""Â¿F06fAË`!T   CW&fC  B7ÀA³ÙiGÃO¿  GÃOGÃOÁ×
Á×
    FSL00365    T.N-rrppppprTUL AË`!T                   BÂ¾ª«F06fAË`"D   CXÙC BüAÄ»õGÃO¿  GÃOGÃOÁ×
Á×
    FSL00365    T.N-rrppppprDFW AË`"b                   BîïÂ½»¼F06fAË`#R   CWÙC BUUAÎ+GÃO¿  GÃOGÃOÁ×
Á×
    FSL00365    T.N-rrppppprMCI AË`#R                   BÝÞÂ¼ÄDFMqAË`$B   Cf&fC  AûAÑkRGÃO¿  GÃOGÃOÁ×
Á×
    FSL00365    T.N-rrppppprMCI AË`$B                   B#ffÂ»æfEqìAË`%P   C3C AÍÇAÑrGÃO¿  GÃOGÃOÁ×
Á×
    FSL00365    T.N-rrppppprMCI AË`%P                   B%ÌÍÂ»*«DM½qAË`&@   CÓ3C¨  Ad Aûs>øÂ¿  GÃOGÃOÁ×
Á×
    FSL00365    R.N-rrpppp-rDSM AË`&@                   BffÂ©DDF)AË`Î   C^YC AÉ©C§I$GÃO¿  GÃOGÃOÁ×
Á×
    FSL00366    T.N-rrppppprATL AË`ì                   BîïÂ©F£3AË` Ü   Cb¦fC  Aî³ÅC°¥GÃO¿  GÃOGÃOÁ×
Á×
    FSL00366    T.N-rrppppprATL AË` Ü                   BDDÂ©ÌÍEqAË`!Ì   C3C A¤JC°}ÖGÃO¿  GÃOGÃOÁ×
Á×
    FSL00366    T.N-rrppppprSDF AË`!Ì                   Bª«ÂªEÇ®AË`"Ú   C3C AEòC£¶>>Ý¤¿  GÃOGÃOÁ×
Á×
    FSL00366    T.N-rrppppprSDF AË`"Ú                   B33Â«nïDU\)AË`#Ê   C3C§ @öîïC¿>Ç:¿  GÃOGÃOÁ×
Á×
    FSL00366    R.N-rrpppp-rSDF AË`#Ê                   BvÂF'£×AË`&«  CY&fC  BG½GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00437    R.N-rrppp--rRDU AË`&                   Bª«Â© E AË`    C3C  A¬ÚtCbu~GÃO¿  GÃOGÃOÁ×
Á×
    FSL00096    T.N-rrppppprSDF AË`                    B""ÂªLÍE'£×AË`!   C9C A²¢CTCÉ>êhì¿  GÃOGÃOÁ×
Á×
    FSL00096    T.N-rrppppprSDF AË`!                   BwwÂ«ffDYAË`"   CyC  @¤JCN>¼î¿  GÃOGÃOÁ×
Á×
    FSL00096    R.N-rrpppp-rSDF AË`"                   B""Âñ¢"E©AË` ¾   C~YCe  B$JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00369    R.N-rrpppp-rSFO AË` ¾                   BÂðª«E5ÃAË`!®   CYC\  B*ËªAÑjGÃO¿  GÃOGÃOÁ×
Á×
    FSL00369    T.N-rrppppprSFO AË`!®                   BÂïE¯\AË`"¼   C3CM  A½PÈB_´ÿ>×(I¿  GÃOGÃOÁ×
Á×
    FSL00369    R.N-rrpppp-rRNO AË`"¼                   B®]ÂQFâëAË`"q  C^&fC  BFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00730    R.N-rrppp--rIAD AË`"b                   B _ÂPE0È´AË`%÷   C3Co  AvîïGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00730    R.N-rrppp--rLGA AË`%æ                   B+Â©ìÍF'£×AË`!Á   CX&fC  BaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00720    R.N-rrppp--rSDF AË`!®                   B·Â±®EÔ$AË`%G   Cx&fC AÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00720    R.N-rrppp--rBNA AË`%2                   B. ÂèaµEäAË`"D   C3C2  A µGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00501    R.N-rrppp--rBOI AË`"D                   B=ª«ÂòE¡ìÍAË`ì   CsYCj  A^=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00370    R.N-rrpppp-rSEA AË`ì                   B=ª«ÂïffEÛ3AË` Ü   Ck¦fCU  B3ÔB´eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00370    T.N-rrppppprSEA AË` Ü                   B>ÝÞÂê÷wD AË`"Ú   ClÍC4  @(öB¤âGÃO¿  GÃOGÃOÁ×
Á×
    FSL00370    R.N-rrpppp-rGEG AË`"Ú                   AÔ""Â£  DÉîAË` d   CyCp  @²¢C®>=_¿  GÃOGÃOÁ×
Á×
    FSL00703    T.N-rrppppprMIA AË` d                   AÔ  Â£¢"Cs×
AË`!T   C9B|      C}~=³È¿  GÃOGÃOÁ×
Á×
    FSL00703    R.N-rrpppp-rMIA AË`!T                   BÌÍÂéîïEHAË`°   CìÍCj  AÑä±GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00687    R.N-rrpppp-rLAX AË`°                   BwwÂçUUFAË` ¾   CgYCu  Aÿ*B¥f£GÃO¿  GÃOGÃOÁ×
Á×
    FSL00687    T.N-rrppppprLAX AË` ¾                   B	ª«Âä»¼F)AË`!®   C^YCx  Aî³ÅB%3GÃO¿  GÃOGÃOÁ×
Á×
    FSL00687    T.N-rrppppprPHX AË`!®                   BÌÍÂâF)AË`"¼   C^¦fC  Aÿ*BèlGÃO¿  GÃOGÃOÁ×
Á×
    FSL00687    T.N-rrppppprPHX AË`"¼                   BÌÍÂßnïF)AË`#¬   C^¦fC AöîïBåñGÃO¿  GÃOGÃOÁ×
Á×
    FSL00687    T.N-rrppppprPHX AË`#¬                   BÌÍÂÜ»¼F)AË`$   C]ÙC BÐ7B],GÃO¿  GÃOGÃOÁ×
Á×
    FSL00687    T.N-rrppppprPHX AË`$                   BUUÂÙæfF+s3AË`%ª   CWYC  AÞ=qBºµGÃO¿  GÃOGÃOÁ×
Á×
    FSL00687    T.N-rrppppprABQ AË`%ª                   BUUÂ×F06fAË`&   CT¦fC AÑä±BþìGÃO¿  GÃOGÃOÁ×
Á×
    FSL00687    T.N-rrppppprABQ AË`&                   B4ÂÔF#Ô{AË` F   CVYC B(¼ßBÖxGÃO¿  GÃOGÃOÁ×
Á×
    FSL00373    T.N-rrppppprCPR AË` F                   B1îïÂÐîïF06fAË`!6   CU¦fC B.é?BÙ¤GÃO¿  GÃOGÃOÁ×
Á×
    FSL00373    T.N-rrppppprBIL AË`!6                   B/ÌÍÂÍÝÞF06fAË`"&   CUYC  B(¼ßBÚ_ÄGÃO¿  GÃOGÃOÁ×
Á×
    FSL00373    T.N-rrppppprCPR AË`"&                   B-»¼ÂÊÄDF06fAË`#4   CXÙC B3ÔBØâÃGÃO¿  GÃOGÃOÁ×
Á×
    FSL00373    T.N-rrppppprFSD AË`#4                   B+ÂÇª«F06fAË`$$   CWYC B.é?BÙýËGÃO¿  GÃOGÃOÁ×
Á×
    FSL00373    T.N-rrppppprLNK AË`$$                   B)ÂÄF06fAË`%2   CWÙC B5B×ÅòGÃO¿  GÃOGÃOÁ×
Á×
    FSL00373    T.N-rrppppprMCK AË`%2                   B(UUÂÁDDF06fAË`&"   CYYC  B$JBÉ¾?GÃO¿  GÃOGÃOÁ×
Á×
    FSL00373    T.N-rrppppprMCI AË`&@                   B  ÂæF06fAË`ì   CWYC B.é?B´eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00473    T.N-rrppppprLAS AË`ì                   BUUÂâ»¼F06fAË` ú   CX&fC B(öB®IÁGÃO¿  GÃOGÃOÁ×
Á×
    FSL00473    T.N-rrppppprLAS AË` ú                   BÂß³3F06fAË`!ê   CX¦fC~  Bd B°<GÃO¿  GÃOGÃOÁ×
Á×
    FSL00473    T.N-rrppppprSLC AË`!ê                   Bª«ÂÜF06fAË`"ø   CW¦fC  BíÌB±ÜGÃO¿  GÃOGÃOÁ×
Á×
    FSL00473    T.N-rrppppprSLC AË`"ø                   B»¼ÂØÝÞF9¼ÍAË`$   CS&fC Aî³ÅB²ø³GÃO¿  GÃOGÃOÁ×
Á×
    FSL00473    T.N-rrppppprWMS AË`$$                   B»¼ÂÖ33F9¼ÍAË`$ö   C]¦fC  B	ßB´eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00473    T.N-rrppppprDEN AË`$ö                   BffÂÓF9¼ÍAË`%æ   CXÙC  Aÿ*Bº
GÃO¿  GÃOGÃOÁ×
Á×
    FSL00473    T.N-rrppppprDEN AË`%æ                   B4èÂë4èCs×
AË`!   C9GÃOGÃOGÃOGÃO/[æÿGÃOGÃOÁ×
Á×
    FSL00098    XWN7rrpBFp-rLAX AË`!6   ONT     FAT     B2"Âë5:CàBAË`!   C9C @²¢C@þ~>h´/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7iippppprLAX AË`!6   ONT     FAT     B/\Âë5CÃoAË`!   C3C @fxC@þ~>!n/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7iippppprLAX AË`!6   ONT     FAT     B,Âë5ÞCðÊÁAË`!!   C¹C  @fxC@þ~>!³k/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!6   ONT     FAT     B)ÐÂë60DAË`!$   CS3Cm  @fxC@þ~>"`°/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!6   ONT     FAT     B'
Âë6D&àÅAË`!'   C9Co  @²¢C@þ~>!üC/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!6   ONT     FAT     B$DÂë6ÔD2NÙAË`!*   CùCk  @¤JC@þ~>¾/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!6   ONT     FAT     B!~Âë7&D9*AË`!-   CÓ3Cn  @¤JC@þ~>/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!6   ONT     FAT     B¸Âë7xD@%AË`!0   C¹C{  @(öC@þ~>/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!6   ONT     FAT     BóÂë7ÉDFáËAË`!3   C3C  @(öC@Ûº>é7/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!T   ONT     FAT     B-Âë8DL7LAË`!6   CyC @(öC@þ~>ÿC/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!T   ONT     FAT     BgÂë8mDSòAË`!9   CyC @(öC@þ~>>/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!T   ONT     FAT     B¡Âë8¿D_DAË`!<   C9C  @²¢C@þ~>¸/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!T   ONT     FAT     BÛÂë9Dp®AË`!?   C3C  @fxC@þ~>Ï/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!T   ONT     FAT     BÂë9cDMPAË`!B   CS3C @$JC@þ~>!:`/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!T   ONT     FAT     BOÂë9µD»dAË`!E   C3C¥  ?ÅòC@þ~>"¤K/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7iippppprLAX AË`!T   ONT     FAT     BÂë:D²°bAË`!H   C3C<  @EòC@þ~>Yn/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!T   ONT     FAT     BÂëHDÎAË`!Y  CÓ3Cg  A,ÚtCÿ'>döõ/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!   ONT     FAT     B
=ÂëV0DÞAË`!k   CùCc  A,ÚtCÈ
>W{´/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!   ONT     FAT     B§Âëd±Dà+AË`!|  C¹Cl  A(öCZk>f{/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.B7irppppprLAX AË`!   ONT     FAT     BÂëq~Dà+AË`!   CÓ3Ce  A5Czz>x</¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!   ONT     FAT     B,`Âë}qDàÊ=AË`!   CÓ3Ce  A=PÈC]?LÌÍ/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!   ONT     FAT     B=qÂëDùïAË`!£  C3Cg  AEòCÈ>oj</¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!®   ONT     FAT     BNÂëUEøAË`!®  C3Cr  AFCØ>ÐÁO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!®   ONT     FAT     B]ÞÂë mE+s3AË`!¯   C3Ct  A¨¼ßCX0?LÌÍ/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!®   ONT     FAT     Bm:Âë¬`ECsøAË`!¯  C3Cj  AÅòC\¸GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!®   ONT     FAT     B~KÂë¸RETÉAË`!°   ClÍCj  AÍÇCÄ¦GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!®   ONT     FAT     B\ÂëÅE`7ðAË`!Á  C¬ÍCg  AÉ©C×GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!ê   ONT     FAT     B¢"ÂëÒÆEp®AË`!Ó   C¬ÍCm  AÉ©C@ÖGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!ê   ONT     FAT     B´èÂëàmE}ï¾AË`!ä  C3Cn  AÍÇC@ÖGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!ê   ONT     FAT     BÉcÂëîïEß®AË`!ë  CS3Cq  Aâ[C$GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!ê   ONT     FAT     BÝÞÂëÿ&EMAË`!ì   C3Ct  Aê0C"GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`!ê   ONT     FAT     BôÂìEAË`!ý  CS3C|  AöîïCêûGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`"&   ONT     FAT     B	UUÂì]ÞE«s3AË`"   CìÍCx  B£×C¿îGÃO¿  GÃOGÃOÁ×
Á×
    FSL00098    T.N-rrppppprLAX AË`"&                   B	ÂìEöAË`"   CÓ3Cx  Aÿ*BóQGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`"&   ONT     FAT     B	 mÂì1~EøÅAË`"   ClÍCv  AöîïC×ÌGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`"&   ONT     FAT     B	8RÂìBE¢AË`"'  C3C|  Aÿ*C~¯GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`"&   ONT     FAT     B	P7ÂìS E©s#AË`"(   CS3Cx  AûC~¯GÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`"&   ONT     FAT     B	iÐÂìeE°6fAË`"9  ClÍCx  B£×CÃ¡GÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`"D   ONT     FAT     B	µÂìwwE¶°AË`"D  ClÍCv  B²¢CØZGÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`"D   ONT     FAT     B	OÂì=E¼°´AË`"E   C3Cu  BÁlC$bGÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7irppppprLAX AË`"D   ONT     FAT     B
¶ÂíåEð8sAË`"   Cs&fCz  B"CzGÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprLAX AË`#è   ONT     FAT     BKÂíd±FYAË`"Ú   CkÙCu  B*ËªC©BGÃO0	p_GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprLAX AË`#è   ONT     FAT     B""ÂíFYAË`#   Ck¦fCt  B7$iC¨É,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00098    T.N-rrppppprLAX AË`#                   Bä±Âí´F¢¨AË`#4   CkYCr  B;AþCª¶GÃO0	p_GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprLAX AË`#è   ONT     FAT     BtÂîjEíÖÙAË`#   CqÙCt  BG½C©){GÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprLAX AË`#è   ONT     FAT     B mÂîHE½ÕPAË`#è   C~YCr  B"C§¤jGÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$$   ONT     FAT     BÚtÂîV0E¯ZòAË`#è   C¦fCr  Bd C§fBGÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$$   ONT     FAT     B{Âîd±E¡XAË`$   C3Cl  B²¢C¦¸GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$$   ONT     FAT     BNÂîrYEÇ¾AË`$   C3C`  AæxC§esGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$$   ONT     FAT     BUUÂîwwEÇ®AË`$   CÓ3Ca  AÚÜCØrGÃO¿  GÃOGÃOÁ×
Á×
    FSL00098    T.N-rrppppprTPH AË`$                   BÔÂî EeÃAË`$$   CS3Cb  AÞ=qCª|0GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$$   ONT     FAT     B¿&Âî§Eq¾AË`$$   CìÍCb  AÞ=qC§GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$~   ONT     FAT     BõÃÂîtE[ÖFAË`$B   CÓ3Ck  A¹33C§päGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$~   ONT     FAT     B*«Âî§AEQîVAË`$B   C,ÍCl  A¨¼ßC§yGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$~   ONT     FAT     BX¿Âî´EQÍAË`$`   C¬ÍCi  AµC¥xlGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$~   ONT     FAT     BµÂîÀÚEOÕãAË`$`   C¬ÍCj  AÁn]C£ÿäGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprBFL AË`$~   ONT     FAT     B¨öÂîÌÍEG¤ÝAË`$~   C3Ch  A°ø	C¤TüGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`$º   ONT     FAT     BÉcÂîÛOE>wAË`$~   CS3Ca  A¨¼ßC<GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`$º   ONT     FAT     Bä±Âîí:E+ªAË`$   C3C]  AÐ7C¦GÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`$º   ONT     FAT     BþKÂîþKE~¸AË`$   CìÍC[  An³ÅCoGÃO/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`$º   ONT     FAT     BåÂï7E»AË`$º   CùCI  A=PÈCÄ(>jÀµ/¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`$º   ONT     FAT     B/ÉÂï"ýDØhsAË`$º   C9CH  A5C;`>ëY /¤í@GÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`%   ONT     FAT     BG®Âï4èD©AË`$Ø   C3C7  A5CØZ>c±¶/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`%   ONT     FAT     B\)ÂïHDu]/AË`$Ø   CS3C2  A^=qCÅæ>ço*/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`%   ONT     FAT     BiÐÂï]D@%AË`$ö   C3C*  A*C76>^¤Ô/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`%   ONT     FAT     Bm:Âïp¤D)íAË`$ö   CyC'  AíÌC{c>ÉRc/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7rrppppprFAT AË`%   ONT     FAT     BNÂï®CÞAË`%   C¹C#  AíÌClM>¡#/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7rrppppprFAT AË`%P   ONT     FAT     BaHÂï&Dr-AË`%   C3C'  AFB)'>À/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7rrppppprFAT AË`%P   ONT     FAT     BffÂï D	(öAË`%  C3C'  AFCªË>|h¿  GÃOGÃOÁ×
Á×
    FSL00098    T.N-rrppppprFAT AË`%                   B;¼ÂïcC¸gmAË`%2   CÓ3C  AfxCK½I>'×/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.B7rrppppprFAT AË`%P   ONT     FAT     B.ÂïùC¢ÑAË`%2   C3C  A=PÈCr>>%E©/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`%P   ONT     FAT     BOÂïvBÕ\)AË`%P   CÓ3Bè  @öîïBòÞh>: á/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`%n   ONT     FAT     BåÂïtèBÏCAË`%P   CùC
  @ÅòC=KuL/[æÿGÃOGÃOÁ×
Á×
    FSL00098    T.G7rrppppprFAT AË`%n   ONT     FAT     B#×Âï&C6áHAË`%P   C3Bþ  Ad C8>uªÁ/[æÿGÃOGÃOÁ×
Á×
    FSL00098    R.G7rrpppp-rFAT AË`%P   ONT     FAT     BLÂæ.]F0È´AË`"Ó   CU&fCy  B+GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00733    R.N-rrppp--rPHX AË`"¼                   B¿&ÂÛÛOF0È´AË`&X  CU&fC  B	ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00733    R.N-rrppp--rGNR AË`&@                   BüÂèËF06fAË`ì   CW&fCr  B933B¦a#GÃO7ISGÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprLAS AË`!T   MHR     SDF     B;¼Âç¢ýF06fAË` F   CUYC}  BG½B§ÖÛGÃO78¤GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprLAS AË`!T   MHR     SDF     BffÂæÕUF06fAË`    CV¦fCv  BAn]B¨+&GÃO¿  GÃOGÃOÁ×
Á×
    FSL00441    T.N-rrppppprTPH AË`                    BwwÂætF0BAË`     CU&fC~  BI©B©üvGÃO78¤GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprLAS AË`!T   MHR     SDF     B±~Âå?&F0AË` ú   CUYC}  B^=qB©GôGÃO78¤GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprLAS AË`!T   MHR     SDF     BéÐÂä
=F0BAË`!T   CU&fC BMÇB©\GÃO78¤GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprSLC AË`#   MHR     SDF     BÂã]ÞF06fAË`!   CUYC  BEòB§5GÃO¿  GÃOGÃOÁ×
Á×
    FSL00441    T.N-rrppppprTPH AË`!                   B¸ÂâÖ0F06fAË`!®   CVYC{  B=PÈB®@£GÃO7jáGÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprSLC AË`#   MHR     SDF     BNÂá¢ýF06fAË`"   CUYC  BI©B«GÃO78¤GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprSLC AË`#   MHR     SDF     B|Âàp¤F0BAË`"b   CV&fC B?_B«i0GÃO7ZGÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprSLC AË`#   MHR     SDF     BÂàF06fAË`"   CV&fC  B?_B¬ÛÎGÃO¿  GÃOGÃOÁ×
Á×
    FSL00441    T.N-rrppppprSLC AË`"                   B§AÂß@ F06fAË`"¼   CW¦fC  B.é?B«îGÃO7{¨GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprSLC AË`#   MHR     SDF     BÐ7ÂÞ F0*5AË`#   CW¦fC B"B¬6)GÃO7jáGÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprRKS AË`$Ø   MHR     SDF     BòYÂÜòYF06fAË`#p   CW¦fC  B*ËªB­BGÃO78¤GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprRKS AË`$Ø   MHR     SDF     BîïÂÜÌÍF/¼{AË`#p   CWÙC B.é?B¹8HGÃO¿  GÃOGÃOÁ×
Á×
    FSL00441    T.N-rrppppprSLC AË`#p                   B0ÂÛÉcFÈAË`#Ê   C]YC BI©B«[øGÃO7ÀðGÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprRKS AË`$Ø   MHR     SDF     B1~ÂÚ§F)AË`$$   C^&fC BMÇB¯¡GÃO7ÚGÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprRKS AË`$Ø   MHR     SDF     BKÂÙP7FhAË`$~   C^&fC  BMÇB¯düGÃO7â~GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprRKS AË`$Ø   MHR     SDF     BDDÂÙffF)AË`$~   C^YC  BOÕçC,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00441    T.N-rrppppprRKS AË`$~                   BbýÂØÆF)AË`$Ø   C^¦fC BMÇB®ÕZGÃO7â~GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprDEN AË`&   MHR     SDF     By,ÂÖÖ0F)AË`%2   C_&fC  BQä±B¯ÿôGÃO7óEGÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprDEN AË`&   MHR     SDF     BÂÕîïF)AË`%n   C_&fC BOÕçB°5<GÃO¿  GÃOGÃOÁ×
Á×
    FSL00441    T.N-rrppppprDEN AË`%n                   BòÂÕOF)AË`%   C_YC BK¸RB±­LGÃO7óEGÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprDEN AË`&   MHR     SDF     BåÂÔb"FhAË`%æ   C_¦fC B?_B±ÓÆGÃO7û¨GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprDEN AË`&   MHR     SDF     B¢"ÂÓ+F5ËAË`&@   C_&fC B"B²ïGÃO7û¨GÃOGÃOÁ×
Á×
    FSL00441    T.G2rrppppprDEN AË`&   MHR     SDF     Bª«ÂÒF)AË`&^   C_¦fC BFB±eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00441    T.N-rrppppprDEN AË`&^                   B»¼Â¸;¼F)AË` 
   CZÙC  B	ßBèéGÃO¿  GÃOGÃOÁ×
Á×
    FSL00376    T.N-rrppppprSTL AË` 
                   B""Âµ*«F)AË`!   C[¦fC AæxBÑAÝGÃO¿  GÃOGÃOÁ×
Á×
    FSL00376    T.N-rrppppprIND AË`!                   BÂ²F)AË`"   C\ÙC  B	ßBÐrGÃO¿  GÃOGÃOÁ×
Á×
    FSL00376    T.N-rrppppprSTL AË`"                   Bª«Â¯Eø=AË`"ø   ClYC Aê0B×^7GÃO¿  GÃOGÃOÁ×
Á×
    FSL00376    T.N-rrppppprSTL AË`"ø                   B=Â®åEí²AË`#   Co¦fC Aê0BÜâÅGÃO9¼¾aGÃOGÃOÁ×
Á×
    FSL00376    T.B0rrppppprSDF AË`#¬   OMA     SDF     BaHÂ®cEÙ½ÓAË`#4   CsÙC  AÉ©BÍ(GÃO:±GÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`#¬   OMA     SDF     BEùÂ®/ÉE¿CAË`#R   Cy¦fC  A µBÅWþGÃO:\3qGÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`#¬   OMA     SDF     B*«Â­ÛOE¥ÔAË`#p   C,ÍC  AÐ7BÆ`GÃO:d¶GÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`#¬   OMA     SDF     BÆÂ­cEìAË`#   C¬ÍC A*BÄöGÃO:y	lGÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`#¬   OMA     SDF     Bù,Â­<EfHAË`#¬   ClÍC AVGBÆð´GÃO;-àGÃOGÃOÁ×
Á×
    FSL00376    T.B0rrppppprSDF AË`$B   OMA     SDF     BjÂ¬ó3EHfAË`#Ê   CS3C  A=PÈB¬vGÃO;	 'GÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`$B   OMA     SDF     BåÂ¬´E=[dAË`#è   C9C  A5B¡½?+ÿ,;0ò|GÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`$B   OMA     SDF     B""Â¬ E1*=AË`#ï  C3C  Ad B¨Ç?LÌÍ¿  GÃOGÃOÁ×
Á×
    FSL00376    T.N-rrppppprSDF AË`$                   B'AÂ¬|E-¼jAË`$   C¹C  Ad BT;=wôe:ù	lGÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`$B   OMA     SDF     B4èÂ¬G®EYüAË`$$   C3C A$JB¥Nð?Zß;	 'GÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`$B   OMA     SDF     B:Â¬ÆDîãAË`$B   C¹C  @æxB®x ?±U;*dÃGÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`$Ø   OMA     SDF     B;¼Â«Þ¸DÔ9AË`$`   CS3C @µB²"g?
%*;Îp;GÃOGÃOÁ×
Á×
    FSL00376    T.G0rrppppprSDF AË`$Ø   OMA     SDF     B=qÂ«®ïDAË`$~   C¹C  @EòB±÷p>üàÔ;ØDÐGÃOGÃOÁ×
Á×
    FSL00376    T.B0rrppppprSDF AË`$Ø   OMA     SDF     BÂ«ìD~úAË`$   CS3C° @µBþÕ«>ÃÉ;ò{³GÃOGÃOÁ×
Á×
    FSL00376    T.B0rrppppprSDF AË`$Ø   OMA     SDF     BÚtÂ«cC¬6FAË`$º   C¹C @öîïC"§>ì;ï4×GÃOGÃOÁ×
Á×
    FSL00376    R.G0rrpppp-rSDF AË`$Ø   OMA     SDF     BA#ÂÞ~KFa§AË` ý   C\&fC  B5GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00716    R.N-rrppp--rSLC AË` ú                   BVyÂÓF9kAË`$   C_&fC BíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00716    R.N-rrppp--rDEN AË`$~                   BçÒÂëúáDû'»AË`"Ð   C3Ci  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00729    R.N-rrppp--rLAX AË`"¼                   AªBðÄDF9¼ÍAË`    C\¦fCw  AÞ=qCZòGÃO¿  GÃOGÃOÁ×
Á×
    FSL00495    T.N-rrppppprXMN AË`                    A¤  BïF9¼ÍAË`!   C\&fCt  A¹33CXK7GÃO¿  GÃOGÃOÁ×
Á×
    FSL00495    T.N-rrppppprSFS1AË`$                   ABî]ÞF9¼ÍAË`"   C\YCc  A¬ÚtCXÛüGÃO¿  GÃOGÃOÁ×
Á×
    FSL00495    T.N-rrppppprSFS1AË`$                   A  BëæfF9¼ÍAË`$~   C\YC[  A*CWó2GÃO¿  GÃOGÃOÁ×
Á×
    FSL00495    R.N-rrpppp-rSFS1AË`$                   A¸DDBó33F£3AË` 
   Cq&fC  BÐ7CEGGÃO¿  GÃOGÃOÁ×
Á×
    FSL00291    T.N-rrppppprKHH1AË` F                   A±BòF£3AË`!   Cq&fC  Aê0CTð,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00291    T.N-rrppppprKHH1AË`!                   AªDDBð³3F£3AË`"   Cp¦fC A½PÈCZêGÃO¿  GÃOGÃOÁ×
Á×
    FSL00291    T.N-rrppppprKHH1AË`"ø                   A£33BïffF£3AË`#   CpYC  A²¢CXVBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00291    T.N-rrppppprKHH1AË`#4                   A  BîF£3AË`$   Cp&fC  A$JCWÓ©GÃO¿  GÃOGÃOÁ×
Á×
    FSL00291    T.N-rrppppprSFS1AË`%æ                   AÌÍBìÄDF£3AË`%   CoÙC  @öîïCX½GÃO¿  GÃOGÃOÁ×
Á×
    FSL00291    T.N-rrppppprSFS1AË`%æ                   ABëwwF£3AË`&   CpYC @µCWÓ½GÃO¿  GÃOGÃOÁ×
Á×
    FSL00291    R.N-rrpppp-rSFS1AË`&"                   A°DDÂÌÍF06fAË` 
   C`YC_  AÞ=qBüÚGÃO¿  GÃOGÃOÁ×
Á×
    FSL00690    T.N-rrppppprGDT AË` 
                   Aª""ÂF06fAË` ú   C^ÙCi  BíÌCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00690    T.N-rrppppprGDT AË` ú                   A¢ÌÍÂ³3F06fAË`!ê   C^&fCm  B&®CNÂGÃO¿  GÃOGÃOÁ×
Á×
    FSL00690    T.N-rrppppprGDT AË`!ê                   AÂF06fAË`"ø   C]YCn  BC}(C¡ÈGÃO¿  GÃOGÃOÁ×
Á×
    FSL00690    T.N-rrppppprGDT AË`"ø                   A33Â F06fAË`#è   C^&fCp  B0ø	CxGÃO¿  GÃOGÃOÁ×
Á×
    FSL00690    T.N-rrppppprGDT AË`#è                   ADDÂ÷wF06fAË`$ö   C^&fCr  B933C
|GÃO¿  GÃOGÃOÁ×
Á×
    FSL00690    T.N-rrppppprGDT AË`$ö                   ADDÂ  F06fAË`%æ   C^ÙCm  B.é?C7+GÃO¿  GÃOGÃOÁ×
Á×
    FSL00690    T.N-rrppppprSDQ1AË`&@                   Ac33Â¦ª«F06fAË`     C]ÙC}  BÁlCHâéGÃO¿  GÃOGÃOÁ×
Á×
    FSL00761    T.N-rrppppprSJO AË`                     AS33Â§F06fAË`!   C^YC{  Aÿ*C@6zGÃO¿  GÃOGÃOÁ×
Á×
    FSL00761    T.N-rrppppprSJO AË`!®                   ACwwÂ§wwF06fAË`"   C_¦fC  Aê0C>TGÃO¿  GÃOGÃOÁ×
Á×
    FSL00761    T.N-rrppppprTGU AË`"                   A3wwÂ§æfFø AË`#   CvYC AÍÇC@6zGÃO¿  GÃOGÃOÁ×
Á×
    FSL00761    T.N-rrppppprSJO AË`#                   A1p¤Â§ôèEí²AË`#¬   C~YC AÁn]C@éGÃO0	p_GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`$`   MIA     SJO     A/ mÂ¨EÜmAË`#Ê   CÓ3C A½PÈCA:.GÃO0	p_GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`$`   MIA     SJO     A-ä±Â¨\EË°AË`#è   CìÍC   AvîïC@û©GÃO0	p_GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`$`   MIA     SJO     A,/ÉÂ¨)E¹¼ÍAË`$   ClÍC¦ AíÌCA-ÚGÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`$`   MIA     SJO     A*µÂ¨)ÐE§¼9AË`$$   ClÍC  @ÅòCB<¥?K/ÛæÿGÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`$`   MIA     SJO     A(èÂ¨9,EàBAË`$B   C¬ÍCQ  @²¢CD¯¿GÃO/ÛæÿGÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`$Ø   MIA     SJO     A'òÂ¨P7EMqAË`$`   C3C:  @EòCOã?R¾/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.B7rrppppprSJO AË`$Ø   MIA     SJO     A&ª«Â¨vEsøAË`$~   C¹CD  AíÌCi¼o>Úb;/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`$Ø   MIA     SJO     A%Ð7Â¨Eb±ìAË`$   CÓ3CJ  @æxCj>Öl/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`$Ø   MIA     SJO     A&ffÂ¨EdAË`$¨  C3CI  @öîïB=ô?)Y¿  GÃOGÃOÁ×
Á×
    FSL00761    T.N-rrppppprSJO AË`$                   A$õÃÂ¨ÃjE[CøAË`$º   CyCG  A²¢Cgñy?LÌÍ/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.B7rrppppprSJO AË`$Ø   MIA     SJO     A#ÉcÂ¨Ö0EC¤¼AË`$Ø   CùCj  @fxCNÂ>Û³0/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`%n   MIA     SJO     A"µÂ¨ßE)¼JAË`$ö   CùC  @²¢C@å+>ãÑè/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`%n   MIA     SJO     A!\)Â¨èöEøÕAË`%   CÓ3C  @$JCBVS>Ì/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`%n   MIA     SJO     A QìÂ¨ñ~E5`AË`%2   C3C)  @$JCB]­>¸ñÌ/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`%n   MIA     SJO     A\)Â¨öDü8ÕAË`%P   C3BÒ  @ÖGC=s¡>©%/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.B7rrppppprSJO AË`%n   MIA     SJO     AîïÂ¨ÄDDèhöAË`%l  C3B¸  @¤JBÒ\Y>áúÎ¿  GÃOGÃOÁ×
Á×
    FSL00761    T.N-rrppppprSJO AË`%                   AèÂ¨Þ¸DèhöAË`%n   CùBò  @²¢C?LÌÍ/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.B7rrppppprSJO AË`&"   MIA     SJO     AîïÂ¨ÃjDæâÑAË`%   CùB°  @¤JB°n¡>Z²/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.B7rrppppprSJO AË`&"   MIA     SJO     A@ÚÂ¨©ÐD¾ AË`%ª   CyB°  A,ÚtBi>hWQ/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`&"   MIA     SJO     A~KÂ¨ D¡)ºAË`%È   C3B°  A=PÈBÒð>Bw¥/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`&"   MIA     SJO     A®Â¨DD¢ÑAË`%æ   C¹BÊ  AEòB>>	Ä£/¤í@GÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`&"   MIA     SJO     AÝÞÂ¨tèDe\¬AË`&   C9BÚ  AVGBo>	Sß/[æÿGÃOGÃOÁ×
Á×
    FSL00761    T.G7rrppppprSJO AË`&"   MIA     SJO     AëÂ¨oÉDW¥`AË`&"   CS3BÈ  @fxB&=13/[æÿGÃOGÃOÁ×
Á×
    FSL00761    R.G7rrpppp-rSJO AË`&"   MIA     SJO     B33Âß*«F£3AË`    CcYC B.é?Cq.4GÃO¿  GÃOGÃOÁ×
Á×
    FSL00293    T.N-rrppppprLAS AË`                    BÂá""F£3AË`!   Cb¦fC B*ËªCqZGÃO¿  GÃOGÃOÁ×
Á×
    FSL00293    T.N-rrppppprPHX AË`!                   BÌÍÂãF£3AË`"   CcYC  B&®Cp	§GÃO¿  GÃOGÃOÁ×
Á×
    FSL00293    T.N-rrppppprPHX AË`"                   BÂå  FYAË`#   CjYCz  BFCoì+GÃO¿  GÃOGÃOÁ×
Á×
    FSL00293    T.N-rrppppprPHX AË`#                   B	ÂæÝÞFYAË`$~   Cj¦fCw  BUUCqÐGÃO¿  GÃOGÃOÁ×
Á×
    FSL00293    T.N-rrppppprLAX AË`$~                   Bª«Âè³3E5ÃAË`%n   C3Cp  Aê0Cr0GÃO¿  GÃOGÃOÁ×
Á×
    FSL00293    T.N-rrppppprLAX AË`%                   BîïÂêwwE4ùAË`&|   CS3Cq  A°ø	C'£>éP&¿  GÃOGÃOÁ×
Á×
    FSL00293    R.N-rrpppp-rLAX AË`&|                   BÚtÂôvBg¥ãAË` d   CÓ3GÃOGÃOGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    XWN-rrpBFp-rSFO AË`     OAK     DSM     BØöÂôw¼BÛt¼AË` g   C¹Cu  A²¢ClXÐ>4¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-iippppprSFO AË`     OAK     DSM     B×wÂôxÚCL7LAË` j   C3Cs  A=PÈCl/v>|a¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-iippppprSFO AË`     OAK     DSM     BÕùÂôyùC AË` m   CS3Cu  AVGClXÐ>û¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË`     OAK     DSM     BÔ{Âô{CÞAË` p   CÓ3Cs  AfxClXÐ>®Á¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË`     OAK     DSM     BÒüÂô|7DeãAË` s   CS3Cs  An³ÅClG>y¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË`     OAK     DSM     BÑ~Âô}VDìAË` v   C3Cu  A²¢ClXÐ>¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË`     OAK     DSM     BÐ Âô~tD'£×AË` y   CÓ3Cy  AvîïClA.>ú¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË`     OAK     DSM     BÎÂôD6áHAË` |   C3Cz  A*ClXÐ>5'¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË`     OAK     DSM     BÍÂô²DCoAË`    CyC  An³ÅClG>1/¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË`     OAK     DSM     BËÂôÐDQÍAË`    CS3C  A*ClA.>®´¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË`     OAK     DSM     BÊÂôïDdAË`    C3C}  AMÇClXÐ>Lï¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË` Ü   OAK     DSM     BÈÂôDyïAË`    CÓ3C{  A^=qClG>Ö_¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË` Ü   OAK     DSM     BÇ
Âô-DAHAË`    C3Ct  AVGClXÐ>h ¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË` Ü   OAK     DSM     BÅÂôLD¯\AË`    CS3Co  A^=qClXÐ>êï¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË` Ü   OAK     DSM     BÄÂôjD¯AË`    C3Cl  A^=qCl/v>¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-iippppprSFO AË` Ü   OAK     DSM     BÂÂôD¨`AË`    C¹Cf  AfxClXÐ>ÕY¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË` Ü   OAK     DSM     BÓ ÂôÆDÉ+AË` ¥  CÓ3Cc  AVGCç1>xû
¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-irppppprSFO AË` Ü   OAK     DSM     BîïÂô{Dí¾wAË` ·   C,ÍC]  A²¢C°oÍGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-irppppprSFO AË` Ü   OAK     DSM     BÂô\EÇmAË` È  ClÍCi  A¨¼ßA®rÀGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË` Ü   OAK     DSM     B""ÂôÔEÔAË` Ú   CÓ3Cf  AÁn]BÁ
GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-irppppprSFO AË`!   OAK     DSM     B33ÂôwwE(féAË` ë  CS3Cf  AÖGBsÒbGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     B@ÚÂôeE8ÈöAË` ý   C3C^  AÍÇBLºGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     BNÂôS ELhAË`!  CÓ3CU  AÅòBLGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     B\)Âô@ÚEU½²AË`!   ClÍCL  AÚÜBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     BkÂô.E\
AË`!  CS3CK  Aî³ÅBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     B~KÂôtEb±ìAË`!,   CS3CK  AöîïBâ²GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSJC AË`!T   OAK     DSM     BÂôEpÊÁAË`!=  C¬ÍCK  B²¢B
GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-irppppprSJC AË`!T   OAK     DSM     B mÂóîïE4ÝAË`!O   CìÍCN  BÐ7BÓGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.B-irppppprSJC AË`!T   OAK     DSM     Bª«Âó×åEÓAË`!U  C3CK  B£×BòGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSJC AË`!T   OAK     DSM     B´èÂóÀÚE5`AË`!V   CS3CS  BüBöGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSJC AË`!T   OAK     DSM     B»¼Âó¨öE¯mAË`!V  CÓ3CT  BFB£¿"GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSJC AË`!T   OAK     DSM     BÅùÂó7EàRAË`!h   ClÍCT  BUUB¢×GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     BîïÂó""E´ùAË`!r   CzYC^  B$JBîéGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.N-rrppppprSFO AË`!r                   BÎÂówwE øöAË`!y  C&fCU  BrêCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     B×
Âó]ÞE¨6%AË`!   C}YCX  Bd B¡GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     BßÂóDDE­í/AË`!  C{ÙCZ  Bd B¡GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     BèÂó*«E²°bAË`!   C{&fC_  B(¼ßB¡GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     Bð¤Âó7E¸ÈöAË`!  Cy¦fCa  B&®B¡®èGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-irppppprSFO AË`!   OAK     DSM     BEùÂò7EãîéAË`!Ì   Cs¦fCc  BpÂB¡gGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprSMF AË`#è   OAK     DSM     Bª«ÂðïÉF~=AË`"&   ChYCf  B}OB 5_GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprSMF AË`#è   OAK     DSM     BîïÂðFÈ AË`"   C]¦fCk  BæfB¢÷GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.N-rrppppprSFO AË`"                   BÂïÀÚF ZwAË`"   C[&fCn  BaBýEGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprSMF AË`#è   OAK     DSM     BuÃÂî7F0NÉAË`"Ú   CX&fCc  B`L;B¡>,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprSMF AË`#è   OAK     DSM     BÑìÂítèF0*5AË`#4   CVÙCi  BXB¡GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprSMF AË`#è   OAK     DSM     B  ÂìÌÍF06fAË`#p   CYÙCb  BXB¤gGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.N-rrppppprSFO AË`#p                   B,`ÂìQF06fAË`#   CU¦fCw  B\.¦B®GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprSMF AË`#è   OAK     DSM     BÂë)ÐF06fAË`#è   CUYCv  B?_B¢EVGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprTPH AË`&"   OAK     DSM     BáHÂêòF06fAË`$B   CWYCb  B(¼ßB¢]GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprTPH AË`&"   OAK     DSM     B  Âéª«F06fAË`$`   CW&fCf  B,ÚtB¢"GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.N-rrppppprLAS AË`$~                   B4èÂèøRF06fAË`$   CV¦fCm  B(¼ßB£#pGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprTPH AË`&"   OAK     DSM     BÂçàmF06fAË`$ö   CU&fCx  B933B£³åGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprTPH AË`&"   OAK     DSM     BÓ ÂæÀ F0*5AË`%P   CT¦fCv  BAn]B¤6GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprTPH AË`&"   OAK     DSM     BÝÞÂæF06fAË`%n   CTYCx  BAn]B§àGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.N-rrppppprTPH AË`%n                   B""ÂåtF06fAË`%ª   CTYC{  B;AþB¤@GÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.G-rrppppprTPH AË`&"   OAK     DSM     Bª«ÂãUUF06fAË`&^   CT&fC BC}(B¦ªGÃO¿  GÃOGÃOÁ×
Á×
    FSL00444    T.N-rrppppprTPH AË`&^                   B»¼Â§höF06fAË` F   CT¦fC  AÅòC)ápGÃO¿  GÃOGÃOÁ×
Á×
    FSL00297    T.N-rrppppprATL AË`!6   DTW     RSW     BµÂ§
F06fAË`!6   CU¦fC A µC*DGÃO¿  GÃOGÃOÁ×
Á×
    FSL00297    T.N-rrppppprATL AË`!6   DTW     RSW     AüÂ¦ÃjF06fAË`"   CT¦fC A(öC*GÃO¿  GÃOGÃOÁ×
Á×
    FSL00297    T.N-rrppppprMIA AË`&|   DTW     RSW     AõÂ¦eF06fAË`"ø   CT¦fC  AEòC(æ<GÃO¿  GÃOGÃOÁ×
Á×
    FSL00297    T.N-rrppppprMIA AË`&|   DTW     RSW     Aíp¤Â¦F06fAË`#Ê   CV&fC AMÇC(ÖgGÃO¿  GÃOGÃOÁ×
Á×
    FSL00297    T.N-rrppppprMIA AË`&|   DTW     RSW     Aå§AÂ¥§AF.N¸AË`$º   CW¦fC A,ÚtC)XkGÃO¿  GÃOGÃOÁ×
Á×
    FSL00297    T.N-rrppppprMIA AË`&|   DTW     RSW     AÞÂ¥FÔF fAË`%   Co&fBÂ  ?²¢C(Ç´GÃO¿  GÃOGÃOÁ×
Á×
    FSL00297    T.N-rrppppprMIA AË`&|   DTW     RSW     A×tÂ¤¨öEM½qAË`&|   C3A   @$JC?³¿  GÃOGÃOÁ×
Á×
    FSL00297    R.N-rrpppp-rMIA AË`&|   DTW     RSW     B,×
Âµ"ýF06fAË` F   C]&fC  B0ø	BûÅSGÃO¿  GÃOGÃOÁ×
Á×
    FSL00299    T.N-rrppppprSDF AË`"Ú   MSP     MCO     B)_Â³záF06fAË`!   CY&fC B.é?CN,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00299    T.N-rrppppprSDF AË`"Ú   MSP     MCO     B%õÃÂ±çAF06fAË`"   CW&fC  BAn]C	GGÃO¿  GÃOGÃOÁ×
Á×
    FSL00299    T.N-rrppppprSDF AË`"Ú   MSP     MCO     B"tÂ°ZtF06fAË`"Ú   CX&fC  B933C
RGÃO¿  GÃOGÃOÁ×
Á×
    FSL00299    R.N-rrpppp-rSDF AË`"Ú   MSP     MCO     B"Â]ÞDffAË`%n   C¹Cx  AfxCaà?LÌÍ¿  GÃOGÃO>>      FSL00108    T.N-rrppppprLGA AË`%È   EWR     SFO     B" ÂzDÛqAË`%   C3Cp  AVGCf>gå¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    T.N-rrppppprLGA AË`%È   EWR     SFO     B" ÂDäç¡AË`%ª   C3Co  A5Cÿ'>¯ã¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    T.N-rrppppprLGA AË`%È   EWR     SFO     B"y,Â¹,EÇðAË`%È   C¹C AfxCé>±F¡¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    T.N-rrppppprLGA AË`%È   EWR     SFO     B"wwÂØ¿E?AË`%È   C¬ÍCv  A¨¼ßC8ðGÃO¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    T.N-rrppppprLGA AË`&"   EWR     SFO     B"|ÂúEep.AË`%æ   C3C~  A¹33C2TGÃO¿  GÃOGÃO>>    FSL00108    T.N-rrppppprLGA AË`&"   EWR     SFO     B"jÂÞEAË`&   CÓ3C A¹33C··GÃO¿  GÃOGÃO=LÌÍ>    FSL00108    T.N-rrppppprLGA AË`&"   EWR     SFO     B"Â9,E#AË`&   GÃOGÃOGÃOC¬lGÃO¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    XBN-iiHBFpprLGA AË`&"   EWR     SFO     B"§ÂT{Ep'AË`&"   C3C  AÑä±C¬ØGÃO¿  GÃOGÃO@#33@#33    FSL00108    T.N-rrppppprLGA AË`&"   EWR     SFO     B"0Ây,E¡öAË`&"   GÃOGÃOGÃOCPfGÃO¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    XBN-iiHBFpprEWR AË`&   EWR     SFO     B"¸ÂÞE«|ôAË`&@   C}YCx  AÍÇCOíGÃO¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    T.N-rrppppprEWR AË`&   EWR     SFO     B"zÂ½pEµAË`&@   GÃOGÃOGÃOCc²GÃO¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    XBN-iiHBFpprEWR AË`&   EWR     SFO     B"=ÂÝE¾CAË`&^   Cy¦fC  Aê0CdHGÃO¿  GÃOGÃO@#33@#33    FSL00108    T.N-rrppppprEWR AË`&   EWR     SFO     B"tèÂýEÈ-jAË`&^   GÃOGÃOGÃOC{àGÃO¿  GÃOGÃO=LÌÍ>    FSL00108    XBN-iiHBFpprEWR AË`&   EWR     SFO     B"_ÂEÑ½AË`&|   Ct¦fC AòÑZC{àGÃO¿  GÃOGÃO@#33@#33    FSL00108    T.N-rrppppprEWR AË`&   EWR     SFO     B"FÔÂ=pEÛCøAË`&|   GÃOGÃOGÃOCy GÃO¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    XBN-iiHBFpprEWR AË`&   EWR     SFO     B".Â]ÞEäÊ^AË`&   CoÙC BÐ7CyñGÃO¿  GÃOGÃO@#33@#33    FSL00108    T.N-rrppppprEWR AË`&   EWR     SFO     B"0Â}pEî=CAË`&   GÃOGÃOGÃOCyDAGÃO¿  GÃOGÃO=LÌÍ=LÌÍ    FSL00108    XBN-iiHBFpprEWR AË`'   EWR     SFO     AçèÂ¥ÉcFc[AË`Î   ClYC @µCdGÃO¿  GÃOGÃOÁ×
Á×
A$ÎÙFSL00603    T.N-rrppppprRSW AË` 
   MCO     DEN     AèîïÂ¥ôFøbAË`ì   Cg&fC AíÌC£æGÃO¿  GÃOGÃOÁ×
Á×
AcñFSL00603    T.N-rrppppprRSW AË` 
   MCO     DEN     AéòYÂ¦FffAË` 
   CaÙC  A(öC£ÜªGÃO¿  GÃOGÃOÁ×
Á×
AÒ FSL00603    T.N-rrppppprRSW AË` 
   MCO     DEN     Aêù,Â¦höF" OAË` F   C]YC @æxCn%GÃO¿  GÃOGÃOÁ×
Á×
AcñFSL00603    T.N-rrppppprATL AË`"ø   MCO     DEN     Að§AÂ¨»¼F&´àAË`!r   CYÙC  AfxC¶ÝGÃO¿  GÃOGÃOÁ×
Á×
AcñFSL00603    T.N-rrppppprATL AË`"ø   MCO     DEN     AõÅùÂ«OF&° AË`"   CYÙC AFC*ÒGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00603    T.N-rrppppprATL AË`"ø   MCO     DEN     A÷åÂ«»¼F+ÕAË`"Ú   CW¦fC  AÑä±Câ&GÃO¿  GÃOGÃOÁ×
Á×
A&`ªFSL00603    T.N-rrppppprATL AË`"ø   MCO     DEN     AøX¿Â¬T{F4ùAË`#4   CW&fC  A¬ÚtCÙ¾GÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00603    T.N-rrppppprMEM AË`&¸   MCO     DEN     Aý""Â®¥F9Á­AË`$`   CX¦fC AÅòC¨-GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00603    T.N-rrppppprMEM AË`&¸   MCO     DEN     B í:Â±jF9¼ÍAË`%   C[¦fC AÍÇC?uGÃO¿  GÃOGÃOÁ×
Á×
AcñFSL00603    T.N-rrppppprMEM AË`&¸   MCO     DEN     BS Â¦aHF> AË` d   CX¦fC A¨¼ßC"æþGÃO¿  GÃOGÃOÁ×
Á×
    FSL00828    T.N-rrppppprMYR AË`"&   DTW     MCO     B mÂ¥¿&F> AË`!6   CX¦fC  A¤JC!GÃO¿  GÃOGÃOÁ×
Á×
    FSL00828    T.N-rrppppprMYR AË`"&   DTW     MCO     AýÐ7Â¥ mF9BáAË`"&   CV&fC  AµC!GÃO¿  GÃOGÃOÁ×
Á×
    FSL00828    T.N-rrppppprMYR AË`"&   DTW     MCO     Aö@ÚÂ¤²YFøRAË`"ø   Cg&fC AMÇC'1÷GÃO¿  GÃOGÃOÁ×
Á×
    FSL00828    T.N-rrppppprMCO AË`&   DTW     MCO     Aîp¤Â¤CjEî  AË`#è   Cr¦fCs  @ÖGC'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00828    T.N-rrppppprMCO AË`&   DTW     MCO     Aç§AÂ£tE^âAË`$º   CS3C  @fxCÁr?13,¿  GÃOGÃOÁ×
Á×
    FSL00828    T.N-rrppppprMCO AË`&   DTW     MCO     AãÐ7Â¢Ü)E¤AË`%ª   C3C @EòCB{>¶èÈ¿  GÃOGÃOÁ×
Á×
    FSL00828    T.N-rrppppprMCO AË`&   DTW     MCO     Aã_Â¢ mAó×
AË`&|   C3C¬  @EòBæ]=<Z¿  GÃOGÃOÁ×
Á×
    FSL00828    R.N-rrpppp-rMCO AË`&   DTW     MCO     B.33ÂµéÐF06fAË` d   C^&fC  B(¼ßBðrGÃO¿  GÃOGÃOÁ×
Á×
    FSL00111    T.N-rrppppprORD AË`#   MSP     RSW     B*kÂ´¼F06fAË`!T   CZ¦fC B,ÚtCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00111    T.N-rrppppprORD AË`#   MSP     RSW     B&ºÂ³À F06fAË`"&   CX&fC  B"CåTGÃO¿  GÃOGÃOÁ×
Á×
    FSL00111    T.N-rrppppprORD AË`#   MSP     RSW     B#
=Â² mF7ÕAË`#   C[¦fC  B(¼ßC§GÃO¿  GÃOGÃOÁ×
Á×
    FSL00111    R.N-rrpppp-rORD AË`#   MSP     RSW     B&býÂÇñ~FCC3AË`ì   C\¦fC B5CkxòGÃO¿  GÃOGÃOÁ×
Á×
    FSL00112    T.N-rrppppprDEN AË`#p   MSP     PHX     B$OÂÉÆFCC3AË` ¾   CZ&fC  BUUCkÀGÃO¿  GÃOGÃOÁ×
Á×
    FSL00112    T.N-rrppppprDEN AË`#p   MSP     PHX     B!½qÂË7wFCC3AË`!®   C[¦fC  BrêCj;ÐGÃO¿  GÃOGÃOÁ×
Á×
    FSL00112    T.N-rrppppprDEN AË`#p   MSP     PHX     BiÐÂÌËFCC3AË`"   CZ¦fC B$JCi¸GÃO¿  GÃOGÃOÁ×
Á×
    FSL00112    T.N-rrppppprDEN AË`#p   MSP     PHX     BÂÎV0FCC3AË`#p   CY&fC  B&®Ch¿GÃO¿  GÃOGÃOÁ×
Á×
    FSL00112    R.N-rrpppp-rDEN AË`#p   MSP     PHX     B*'AÂ ¢ýF9³AË` d   C`¦fC  B?_CÒvGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`"Ú   LGA     ORD     B*Â£µF9·ìAË`!   CaYC B;AþCbGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`"Ú   LGA     ORD     B*í:Â¦b"F5däAË`"¼   CbYC B0ø	CÉ|GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00763    T.N-rrppppprORD AË`"Ú   LGA     ORD     B*úáÂ¦ÝF+irAË`"ø   Ca¦fC B*ËªC,GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00763    T.N-rrppppprORD AË`$º   LGA     ORD     B+\Â§)F!ãAË`#R   C\YC  B«<C²GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`$º   LGA     ORD     B+Â¨jFWÅAË`#   C^&fC BdiÐCjGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00763    T.N-rrppppprORD AË`$º   LGA     ORD     B*ÆÂªåF£3AË`$º   C_ÙC  BC}(C GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`$º   LGA     ORD     B* mÂ«OFÑ_AË`%2   C`¦fC  BSó|CÈGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00763    T.N-rrppppprORD AË`%n   LGA     ORD     B*jÂ«ÝÞF2AË`%P   CbÙC  B`L;CÙKGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00763    T.N-rrppppprORD AË`%n   LGA     ORD     B)í:Â¬E÷káAË`%P   Ch¦fC BfxC|GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`%n   LGA     ORD     B)ÚtÂ¬?&Eä_AË`%n   Cm&fC  Bb[CðGÃO¿  GÃOGÃOÁ×
Á×
A#=FSL00763    T.N-rrppppprORD AË`%n   LGA     ORD     B)ÉcÂ¬ùEÑHAË`%   CpYC BOÕçC¶GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00763    T.N-rrppppprORD AË`&   LGA     ORD     B)¸RÂ¬ÆÔE¾XüAË`%ª   Ct¦fC  B?_C?¨GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`&   LGA     ORD     B)§AÂ­E«irAË`%È   Cx¦fC  B"CKôGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`&   LGA     ORD     B)0Â­LÍE+áAË`%æ   C}&fC BíÌCoGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`&   LGA     ORD     B)Â­E(ÕAË`&   ClÍC BÐ7CKôGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`&   LGA     ORD     B)tÂ­ÎEdrAË`&"   C3C B	ßC2ûGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00763    T.N-rrppppprORD AË`&   LGA     ORD     B)býÂ®ìE>l~AË`&^   C¬ÍC  AÁn]Cc®GÃO¿  GÃOGÃOÁ×
Á×
A'ò|FSL00763    T.N-rrppppprORD AË`&   LGA     ORD     B).Â®¨E?cAË`&   C3C  A¨¼ßC NGÃO¿  GÃOGÃOÁ×
Á×
A$ÎÙFSL00763    T.N-rrppppprORD AË`&   LGA     ORD     B QìÂ§ôèF&° AË` F   CY¦fC  A(öC®çGÃO¿  GÃOGÃOÁ×
Á×
    FSL00113    T.N-rrppppprATL AË`!6   TPA     DTW     B¸Â¨F&° AË`!6   CZ&fC A¬ÚtC²±«GÃO¿  GÃOGÃOÁ×
Á×
    FSL00113    T.N-rrppppprATL AË`!6   TPA     DTW     Bí:Â¨!HF&° AË`"   CY¦fC  A¹33C²²AGÃO¿  GÃOGÃOÁ×
Á×
    FSL00113    T.N-rrppppprCLE AË`&|   TPA     DTW     B¸RÂ¨:F7[3AË`"ø   CY&fC  AÍÇC²GÃO¿  GÃOGÃOÁ×
Á×
    FSL00113    T.N-rrppppprCLE AË`&|   TPA     DTW     BaHÂ¨RÆF9¼ÍAË`#Ê   CY&fC AÖGC²|ñGÃO¿  GÃOGÃOÁ×
Á×
    FSL00113    T.N-rrppppprCLE AË`&|   TPA     DTW     BÆÂ¨m:F9¼ÍAË`$º   CY¦fC  Aî³ÅC²fGÃO¿  GÃOGÃOÁ×
Á×
    FSL00113    T.N-rrppppprCLE AË`&|   TPA     DTW     BËÂ¨®F9¼ÍAË`%   CZ¦fC AûC²hõGÃO¿  GÃOGÃOÁ×
Á×
    FSL00113    T.N-rrppppprCLE AË`&|   TPA     DTW     BuÃÂ¨ mF9¼ÍAË`&|   C]&fC  B"C²}¥GÃO¿  GÃOGÃOÁ×
Á×
    FSL00113    R.N-rrpppp-rCLE AË`&|   TPA     DTW     B,`ÂÕ*«Dåª³AË`$   C3C @fxGÃOGÃO¿  GÃOGÃOÁ×
Á×
AüPFSL00753    R.N-rrpppp-rABQ AË`$~   ABQ     DEN     B1~ÂÕ§EÛqAË`$$   CÓ3C A(öB©ï>µ9¿  GÃOGÃOÁ×
Á×
A7¤¨FSL00753    T.N-rrppppprABQ AË`$~   ABQ     DEN     Bd±ÂÕ	cE>ºAË`$B   CùC  A²¢A_Ñ>3W¿  GÃOGÃOÁ×
Á×
AjFSL00753    T.N-rrppppprABQ AË`$~   ABQ     DEN     BjÂÔâ"Ee"'AË`$`   C3C  AÐ7AÒQGÃO¿  GÃOGÃOÁ×
Á×
AB¡bFSL00753    T.N-rrppppprABQ AË`$~   ABQ     DEN     B6ÂÔÎEvÜAË`$~   CìÍC  AaBèvGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00753    T.N-rrppppprALS AË`$Ø   ABQ     DEN     BÂÔ§AEjAË`$   CÓ3C  A¬ÚtB®ðGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00753    T.N-rrppppprALS AË`$Ø   ABQ     DEN     BòYÂÔOE«Ô¼AË`$º   CìÍC  AFAqÇGÃO¿  GÃOGÃOÁ×
Á×
A!«6FSL00753    T.N-rrppppprALS AË`$Ø   ABQ     DEN     BNÂÔ\E¾°ÅAË`$Ø   ClÍC  AµAhGÃO¿  GÃOGÃOÁ×
Á×
A eFSL00753    T.N-rrppppprALS AË`$Ø   ABQ     DEN     B¨öÂÔjEÑªAË`$Ø   C{¦fC AÍÇAl½GÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00753    T.N-rrppppprALS AË`%2   ABQ     DEN     B'AÂÔrYEä£[AË`$ö   CuÙC  AÅòAqÿÄGÃO¿  GÃOGÃOÁ×
Á×
A+FSL00753    T.N-rrppppprALS AË`%2   ABQ     DEN     B~KÂÔgAE÷×+AË`%   CpYC  AÉ©AdÔ÷GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00753    T.N-rrppppprALS AË`%2   ABQ     DEN     B mÂÔRÆFmAË`%2   CjÙC  AÉ©AbßGÃO¿  GÃOGÃOÁ×
Á×
AÒ FSL00753    T.N-rrppppprALS AË`%2   ABQ     DEN     B¢"ÂÔAµFà AË`%P   CeÙC AûAké
GÃO¿  GÃOGÃOÁ×
Á×
A&`ªFSL00753    T.N-rrppppprDEN AË`&Ö   ABQ     DEN     B¸ÂÔ1~FuAË`%n   C`YC B²¢AivGGÃO¿  GÃOGÃOÁ×
Á×
A)MFSL00753    T.N-rrppppprDEN AË`&Ö   ABQ     DEN     B\ÂÓ¯ÉF¥¬AË`&   Ca¦fC  BFA9}(GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00753    T.N-rrppppprDEN AË`&Ö   ABQ     DEN     BDDÂËåF9¼ÍAË`ì   CX¦fC BrêC|ÄþGÃO¿  GÃOGÃOÁ×
Á×
    FSL00116    T.N-rrppppprDEN AË`$B   DTW     LAS     B
=ÂÌýqF9¼ÍAË` ¾   CS¦fC BaC|xGÃO¿  GÃOGÃOÁ×
Á×
    FSL00116    T.N-rrppppprDEN AË`$B   DTW     LAS     BÉcÂÎáHF9¼ÍAË`!®   CQ¦fC  B(öC{¥îGÃO¿  GÃOGÃOÁ×
Á×
    FSL00116    T.N-rrppppprDEN AË`$B   DTW     LAS     B|ÂÐÂF9¼ÍAË`"   CW¦fC  BÐ7CzëVGÃO¿  GÃOGÃOÁ×
Á×
    FSL00116    T.N-rrppppprDEN AË`$B   DTW     LAS     B(öÂÒ¢ýF9¼ÍAË`#p   CW¦fC  AûCzGÃO¿  GÃOGÃOÁ×
Á×
    FSL00116    T.N-rrppppprDEN AË`$B   DTW     LAS     Bù,ÂÔF9¼ÍAË`$B   CY&fC BÐ7C|éGÃO¿  GÃOGÃOÁ×
Á×
    FSL00116    R.N-rrpppp-rDEN AË`$B   DTW     LAS     B,»¼ÂxRFì{AË` F   C[&fC  BQä±CæGÃO¿  GÃOGÃOÁ×
Á×
    FSL00301    T.N-rrppppprALB AË`#Ê   BOS     SEA     B/DDÂòYF6áHAË`!6   Cc&fC  AÖGCO5GÃO¿  GÃOGÃOÁ×
Á×
    FSL00301    T.N-rrppppprALB AË`#Ê   BOS     SEA     B2jÂåF9¼ÍAË`"   Cb¦fC A µCéGÃO¿  GÃOGÃOÁ×
Á×
    FSL00301    T.N-rrppppprALB AË`#Ê   BOS     SEA     B3âýÂ´èF9¼ÍAË`"ø   Cc&fC  A°ø	CóGÃO¿  GÃOGÃOÁ×
Á×
    FSL00301    T.N-rrppppprALB AË`#Ê   BOS     SEA     B5ZtÂµF9¼ÍAË`#Ê   Cc&fC  A½PÈCÖèGÃO¿  GÃOGÃOÁ×
Á×
    FSL00301    R.N-rrpppp-rALB AË`#Ê   BOS     SEA     B&üÂ§Eª\AË`"b   C{¦fC B µGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00122    R.N-rrpppp-rSDF AË`&Ö   DTW     TPA     B#Ó Â§¸FÓ×AË`#4   C`&fC BC}(CGIñGÃO¿  GÃOGÃOÁ×
Á×
    FSL00122    T.N-rrppppprSDF AË`&Ö   DTW     TPA     B Â¨"ýF)AË`$$   C[&fC  BZÜCEW«GÃO¿  GÃOGÃOÁ×
Á×
    FSL00122    T.N-rrppppprSDF AË`&Ö   DTW     TPA     BúáÂ¨nF06fAË`$ö   CV¦fC  B*ËªC=eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00122    T.N-rrppppprSDF AË`&Ö   DTW     TPA     BnïÂ¨vF06fAË`%æ   CS¦fC  B;AþC5]GÃO¿  GÃOGÃOÁ×
Á×
    FSL00122    T.N-rrppppprSDF AË`&Ö   DTW     TPA     B¯ÉÂ¯®F&« AË`°   CX¦fC  B$JCÛGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00646    T.N-rrppppprSTL AË`"   EWR     SFO     B%Â²d±F&« AË` Ü   CXYC BFC'¸GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00646    T.N-rrppppprSTL AË`"   EWR     SFO     BÂµ'AF&¦?AË`"   CYYC BÐ7CÿSGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00646    T.N-rrppppprSTL AË`"   EWR     SFO     BÂ·öF&° AË`#4   CWÙC AÞ=qC7÷GÃO¿  GÃOGÃOÁ×
Á×
A±ÄFSL00646    T.N-rrppppprDFW AË`$º   EWR     SFO     BòYÂ¸NF+s3AË`#R   CUYC AöîïCòÔGÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00646    T.N-rrppppprDFW AË`$º   EWR     SFO     BÎÂ¸òYF4þzAË`#   C^YC  B(öCà!GÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00646    T.N-rrppppprDFW AË`$º   EWR     SFO     B""Â»»¼F9³AË`$º   C^ÙC Aâ[CSGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00646    R.N-rrpppp-rDFW AË`$º   EWR     SFO     AüÐ7Â£¾KF06fAË` F   CT¦fC  Ad     GÃO¿  GÃOGÃOÁ×
Á×
    FSL00126    T.N-rrppppprGSO AË`$º   MIA     DTW     BOÂ£¿&F06fAË`!6   CT&fC  AÐ7C³òÀGÃO¿  GÃOGÃOÁ×
Á×
    FSL00126    T.N-rrppppprGSO AË`$º   MIA     DTW     BÐ7Â£½qF9¼ÍAË`"   CW¦fC  AÁn]>S,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00126    T.N-rrppppprGSO AË`$º   MIA     DTW     B	Â£¾KF9¼ÍAË`"ø   CY&fC  AÉ©C³òÖGÃO¿  GÃOGÃOÁ×
Á×
    FSL00126    T.N-rrppppprGSO AË`$º   MIA     DTW     B=qÂ£ÌÍF9¼ÍAË`#Ê   CX&fC  AÑä±C³ GÃO¿  GÃOGÃOÁ×
Á×
    FSL00126    T.N-rrppppprGSO AË`$º   MIA     DTW     BéÐÂ£â"F9¼ÍAË`$º   CZ&fC  AÞ=qC²³yGÃO¿  GÃOGÃOÁ×
Á×
    FSL00126    R.N-rrpppp-rGSO AË`$º   MIA     DTW     B,OÂµF9¼ÍAË` (   Cd¦fC  BFC[eGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00492    T.N-rrppppprROC AË`#¬   BOS     SFO     B, mÂùF9·ìAË`!T   Cb&fC  B=PÈCtGÃO¿  GÃOGÃOÁ×
Á×
AcñFSL00492    T.N-rrppppprROC AË`#¬   BOS     SFO     B,µÂîïF9·ìAË`"   Cb&fC B,ÚtCgôGÃO¿  GÃOGÃOÁ×
Á×
AÒ FSL00492    T.N-rrppppprROC AË`#¬   BOS     SFO     B,IcÂ âýF9Á­AË`#¬   Cc¦fC  B*ËªCîGÃO¿  GÃOGÃOÁ×
Á×
AcñFSL00492    R.N-rrpppp-rROC AË`#¬   BOS     SFO     Bù,Â«»¼F9¼ÍAË` 
   CY¦fC  Aâ[C£áGÃO¿  GÃOGÃOÁ×
Á×
    FSL00129    T.N-rrppppprATL AË` Ü   MIA     MSP     B\Â¬ËF9¼ÍAË` Ü   CY¦fC AÅòC¢ÁõGÃO¿  GÃOGÃOÁ×
Á×
    FSL00129    T.N-rrppppprATL AË` Ü   MIA     MSP     B mÂ­Ü)F9¼ÍAË`!Ì   C[¦fC  AòÑZC¢$GÃO¿  GÃOGÃOÁ×
Á×
    FSL00129    T.N-rrppppprSTL AË`&"   MIA     MSP     B,`Â®ïÉF9¼ÍAË`"   C[&fC BÁlC¢_pGÃO¿  GÃOGÃOÁ×
Á×
    FSL00129    T.N-rrppppprSTL AË`&"   MIA     MSP     B6Â°F9¼ÍAË`#   C\¦fC  BaC¡øMGÃO¿  GÃOGÃOÁ×
Á×
    FSL00129    T.N-rrppppprSTL AË`&"   MIA     MSP     B""Â±F9¼ÍAË`$`   CZ¦fC  BüC¡ÀÃGÃO¿  GÃOGÃOÁ×
Á×
    FSL00129    T.N-rrppppprSTL AË`&"   MIA     MSP     B.Â²(öF9¼ÍAË`%P   C^&fC  B(öC¢ánGÃO¿  GÃOGÃOÁ×
Á×
    FSL00129    T.N-rrppppprSTL AË`&"   MIA     MSP     BW
Â³DF9¼ÍAË`&"   C^&fC  B(¼ßC¥ÄGÃO¿  GÃOGÃOÁ×
Á×
    FSL00129    R.N-rrpppp-rSTL AË`&"   MIA     MSP     B	p¤Â®ÁµF9¼ÍAË` (   CY¦fC AÖGC¦à­GÃO¿  GÃOGÃOÁ×
Á×
    FSL00130    T.N-rrppppprSTL AË`"¼   TPA     MSP     BºÂ¯ìF9¼ÍAË` ú   CZ¦fC  AòÑZC¦Õ7GÃO¿  GÃOGÃOÁ×
Á×
    FSL00130    T.N-rrppppprSTL AË`"¼   TPA     MSP     Bð¤Â°p¤F9¼ÍAË`!ê   C\&fC Aî³ÅC¥È	GÃO¿  GÃOGÃOÁ×
Á×
    FSL00130    T.N-rrppppprSTL AË`"¼   TPA     MSP     B""Â±]ÞF9¼ÍAË`"¼   C[&fC BÁlC¤ïGÃO¿  GÃOGÃOÁ×
Á×
    FSL00130    R.N-rrpppp-rSTL AË`"¼   TPA     MSP     BOÂëD´nAË` d   C¹C_  A*GÃOGÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00131    R.N-rrpppp-rIAD AË` Ü   IAD     LAX     Bm:ÂDÅAË`    CùC A(öCd é>»qO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprIAD AË` Ü   IAD     LAX     BW
ÂÞDäç¡AË`    CS3C AíÌCuØB?o¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprIAD AË` Ü   IAD     LAX     B]ÞÂ{¼EjAË` Ü   CÓ3C~  A$JC	Ì>©½ä¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprIAD AË` Ü   IAD     LAX     B]ÞÂOE>õAË` Ü   CS3C AfxCÿ'GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprBWI AË`!6   IAD     LAX     BbýÂÎEdç¡AË` ú   C3Cw  A(öCm¦GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprBWI AË`!6   IAD     LAX     Bd±ÂE±bAË`!   C,ÍCq  A¤JCf<GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprBWI AË`!6   IAD     LAX     BhÂ1~Ep'AË`!6   C¬ÍCx  AÁn]CDþGÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprBWI AË`!6   IAD     LAX     BiÐÂM§E«£øAË`!6   C~ÙC  AÍÇCÝbGÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprIAD AË`!®   IAD     LAX     Bm:ÂtE¾§AË`!r   C|&fC  AÑä±C¢%GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprIAD AË`!®   IAD     LAX     BnïÂòYEÑÍAË`!   CvYC AÉ©CF_GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprIAD AË`!®   IAD     LAX     Bm:ÂQEäÀAË`!®   CqYC AÍÇC½GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprIAD AË`!®   IAD     LAX     Bm:Â§E÷¹èAË`!Ì   ClÙC  BüCÿ'GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprRDU AË`"¼   IAD     LAX     BUUÂ mFh;AË`"   ChÙC  B+C%¨GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprRDU AË`"¼   IAD     LAX     BQìÂbýFýCAË`"   Cd&fC  B(öCCPGÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprRDU AË`"¼   IAD     LAX     BNÂ®ïF~ÉAË`"¼   C_ÙC B(¼ßCZSGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00131    T.N-rrppppprRDU AË`"¼   IAD     LAX     BKÂ ùF" OAË`"Ú   C[YC B.é?CoTGÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprCLE AË`&^   IAD     LAX     BÌÍÂ¢¤±F&° AË`$   CXÙC  B=PÈCNªGÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprCLE AË`&^   IAD     LAX     B""Â¥DDF&« AË`%2   CY&fC B7$iC`+GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    T.N-rrppppprCLE AË`&^   IAD     LAX     BhÂ§â"F&° AË`&^   CXÙC  B.é?C6GÃO¿  GÃOGÃOÁ×
Á×
Â="FSL00131    R.N-rrpppp-rCLE AË`&^   IAD     LAX     Aù0Â¨0F9¼ÍAË` 
   CTÙC AÉ©C£÷ßGÃO¿  GÃOGÃOÁ×
Á×
    FSL00610    T.N-rrppppprATL AË`#   RSW     MSP     AÿÓ Â©UF9¼ÍAË` Ü   CV¦fC  AÑä±C£¶<GÃO¿  GÃOGÃOÁ×
Á×
    FSL00610    T.N-rrppppprATL AË`#   RSW     MSP     BjÂªåF9¼ÍAË`!Ì   CYYC AÞ=qC£tLGÃO¿  GÃOGÃOÁ×
Á×
    FSL00610    T.N-rrppppprATL AË`#   RSW     MSP     B¸Â«ÞF9¼ÍAË`"   CXÙC AÅòC£OtGÃO¿  GÃOGÃOÁ×
Á×
    FSL00610    T.N-rrppppprATL AË`#   RSW     MSP     B	?&Â¬+F9¼ÍAË`#   CY&fC  AµC£iGÃO¿  GÃOGÃOÁ×
Á×
    FSL00610    R.N-rrpppp-rATL AË`#   RSW     MSP     B&%Â¬jFCC3AË` F   Ca¦fC  BüBç¥ªGÃO¿  GÃOGÃOÁ×
Á×
    FSL00303    T.N-rrppppprIAD AË`#Ê   MSP     DCA     B#ÎÂªtFCC3AË`!6   C^¦fC  B&®BçÎ×GÃO¿  GÃOGÃOÁ×
Á×
    FSL00303    T.N-rrppppprIAD AË`#Ê   MSP     DCA     B"Â§býFCC3AË`"   C]ÙC Bd BÖ¢ÕGÃO¿  GÃOGÃOÁ×
Á×
    FSL00303    T.N-rrppppprIAD AË`#Ê   MSP     DCA     B hÂ¤·wFCC3AË`"ø   C`¦fC  BÐ7B×v§GÃO¿  GÃOGÃOÁ×
Á×
    FSL00303    T.N-rrppppprIAD AË`#Ê   MSP     DCA     BÜ)Â¢ÆF1¤)AË`#Ê   CXÙC  BFBÔ¡_GÃO¿  GÃOGÃOÁ×
Á×
    FSL00303    T.N-rrppppprIAD AË`#Ê   MSP     DCA     B¥Â[OF~fAË`$º   Cd&fC  B"BÍ1?K¿  GÃOGÃOÁ×
Á×
    FSL00303    T.N-rrppppprDCA AË`'l   MSP     DCA     BÉcÂä±ENAË`%   C¬ÍC  A°ø	BÇÑvGÃO¿  GÃOGÃOÁ×
Á×
    FSL00303    T.N-rrppppprDCA AË`'l   MSP     DCA     BòYÂìE
AË`&|   CÓ3Cp  A5BÍõf?"Þº¿  GÃOGÃOÁ×
Á×
    FSL00303    T.N-rrppppprDCA AË`'l   MSP     DCA     B³3ÂÞ{¼EÉîAË`"¼   C~YC  A¬ÚtGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00132    R.N-rrpppp-rLVS AË`&@   PHX     DTW     BZtÂÜ0¤F-ZáAË`#¬   CV&fC  AÖGB]GÃO¿  GÃOGÃOÁ×
Á×
    FSL00132    T.N-rrppppprLVS AË`&@   PHX     DTW     B	éÐÂÙÛOF06fAË`$~   CT¦fC AÑä±B
ÜGÃO¿  GÃOGÃOÁ×
Á×
    FSL00132    T.N-rrppppprLVS AË`&@   PHX     DTW     B4èÂ×tF06fAË`%n   CTYC  AæxBä#GÃO¿  GÃOGÃOÁ×
Á×
    FSL00132    T.N-rrppppprLVS AË`&@   PHX     DTW     BÔÂÕF06fAË`&@   CT¦fC AÖGBDGÃO¿  GÃOGÃOÁ×
Á×
    FSL00132    R.N-rrpppp-rLVS AË`&@   PHX     DTW     BnïÂ£àmF9¼ÍAË` (   CUYC AµC5}6GÃO¿  GÃOGÃOÁ×
Á×
    FSL00134    T.N-rrppppprMIA AË`#¬   DTW     MIA     AûDDÂ£ F9¼ÍAË` ú   CU¦fC  A(öC+
6GÃO¿  GÃOGÃOÁ×
Á×
    FSL00134    T.N-rrppppprMIA AË`#¬   DTW     MIA     Aó®Â£F9¼ÍAË`!ê   CVÙC  A¨¼ßC$®@GÃO¿  GÃOGÃOÁ×
Á×
    FSL00134    T.N-rrppppprMIA AË`#¬   DTW     MIA     AìOÂ¢]F9¼ÍAË`"¼   C\&fC  AÐ7CæGÃO¿  GÃOGÃOÁ×
Á×
    FSL00134    T.N-rrppppprMIA AË`#¬   DTW     MIA     Aä´èÂ¡0F9¼ÍAË`#¬   C[ÙC  AvîïC<ýGÃO¿  GÃOGÃOÁ×
Á×
    FSL00134    T.N-rrppppprMIA AË`#¬   DTW     MIA     AÝNÂ ÑF06fAË`$~   CY&fC A,ÚtCiáGÃO¿  GÃOGÃOÁ×
Á×
    FSL00134    T.N-rrppppprMIA AË`'N   DTW     MIA     AÖbýÂ E§£×AË`%n   CìÍB  ?ÅòCã!GÃO¿  GÃOGÃOÁ×
Á×
    FSL00134    T.N-rrppppprMIA AË`'N   DTW     MIA     AÐÅùÂ 
EM½qAË`&@   CÓ3Cu  @EòC3?T»¿  GÃOGÃOÁ×
Á×
    FSL00134    T.N-rrppppprMIA AË`'N   DTW     MIA     B)ÆÂ¥×åE!ìÍAË`Î   C,ÍC Aî³ÅCµGÃO¿  GÃOGÃOÁ×
Á×
    FSL00136    T.N-rrppppprDTW AË`Î   LGA     DTW     B(îïÂ¦¯ÉCffAË` ¾   CÓ3C|  A,ÚtCy6i> r¿  GÃOGÃOÁ×
Á×
    FSL00136    R.N-rrpppp-rDTW AË` ¾   LGA     DTW     B/þKÂÁµFCC3AË`    CcYC  BíÌCFGÃO¿  GÃOGÃOÁ×
Á×
    FSL00137    T.N-rrppppprBUF AË`"D   BOS     MSP     B0tÂ !HFCC3AË`!T   C`¦fC AæxCÃºGÃO¿  GÃOGÃOÁ×
Á×
    FSL00137    T.N-rrppppprBUF AË`"D   BOS     MSP     B0ÝÞÂ¢FCC3AË`"D   CbÙC  B²¢Ck¼GÃO¿  GÃOGÃOÁ×
Á×
    FSL00137    T.N-rrppppprBUF AË`"D   BOS     MSP     B14èÂ¤í:FCC3AË`#   CbÙC  B²¢CGÃO¿  GÃOGÃOÁ×
Á×
    FSL00137    T.N-rrppppprDTW AË`&¸   BOS     MSP     B1jÂ§P7FCC3AË`$   CbÙC  Aÿ*CÕ·?NV¿  GÃOGÃOÁ×
Á×
    FSL00137    T.N-rrppppprDTW AË`&¸   BOS     MSP     B1ÂÂ©ÃjFCC3AË`$Ø   Cb&fC  Aê0CpGÃO¿  GÃOGÃOÁ×
Á×
    FSL00137    T.N-rrppppprDTW AË`&¸   BOS     MSP     B1ù,Â¬>KFCC3AË`%È   CdÙC  BFC:jGÃO¿  GÃOGÃOÁ×
Á×
    FSL00137    T.N-rrppppprDTW AË`&¸   BOS     MSP     B*DDÂâýE©AË`!ê   CzÙC AÚÜGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00138    R.N-rrpppp-rBUF AË`%n   BOS     DTW     B*Ø¿Â×åF#Ô{AË`"¼   CZ¦fC B;AþC6WGÃO¿  GÃOGÃOÁ×
Á×
    FSL00138    T.N-rrppppprBUF AË`%n   BOS     DTW     B+]ÞÂ®F9¼ÍAË`#¬   Cc¦fC AæxCc*GÃO¿  GÃOGÃOÁ×
Á×
    FSL00138    T.N-rrppppprBUF AË`%n   BOS     DTW     B+ÝÞÂZtF9¼ÍAË`$~   CbYC B£×CVGÃO¿  GÃOGÃOÁ×
Á×
    FSL00138    T.N-rrppppprBUF AË`%n   BOS     DTW     B,QìÂ¢ýF9¼ÍAË`%n   Cc&fC BÐ7CÔÞGÃO¿  GÃOGÃOÁ×
Á×
    FSL00138    R.N-rrpppp-rBUF AË`%n   BOS     DTW     BºÂìßDÖ¡AË`"¼   C9C  @²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00574    R.N-rrpppp-rLAX AË`"ø   LAX     SEA     B¸RÂìó3DjAË`"Ú   CyC   @(öCÀ_>6`¿  GÃOGÃOÁ×
Á×
A±ÄFSL00574    T.N-rrppppprLAX AË`"ø   LAX     SEA     B´èÂí	cDå5¨AË`"Ú   CùCn  A^=qCËú>ÍÕò¿  GÃOGÃOÁ×
Á×
AjFSL00574    T.N-rrppppprLAX AË`"ø   LAX     SEA     B´èÂí¿EyèAË`"ø   C3Cp  A²¢Cÿ'>$¿  GÃOGÃOÁ×
Á×
AØ­FSL00574    T.N-rrppppprLAX AË`"ø   LAX     SEA     B³3Âí*«E?VAË`"ø   CÓ3Ck  A¹33C¢>¨Ô¿  GÃOGÃOÁ×
Á×
AÕgFSL00574    T.N-rrppppprLAX AË`#R   LAX     SEA     B±~ÂíEùEe"'AË`#   ClÍCg  A°ø	CöGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00574    T.N-rrppppprLAX AË`#R   LAX     SEA     B¯ÉÂíjEâ'AË`#4   C,ÍCp  AÚÜCFGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00574    T.N-rrppppprLAX AË`#R   LAX     SEA     B³3Âí¥EjAË`#R   C3Cx  AæxCm¦GÃO¿  GÃOGÃOÁ×
Á×
A,§ðFSL00574    T.N-rrppppprLAX AË`#R   LAX     SEA     BßÂí¾KE«µAË`#R   C3Cy  AòÑZCð"GÃO¿  GÃOGÃOÁ×
Á×
A1]cFSL00574    T.N-rrppppprLAX AË`#¬   LAX     SEA     B;¼Âí´èE¾ºAË`#p   C3Cu  B	ßA8<NGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00574    T.N-rrppppprLAX AË`#¬   LAX     SEA     BOÂí¸EÑ OAË`#   CzÙCm  B+AÇAþGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00574    T.N-rrppppprLAX AË`#¬   LAX     SEA     B	0Âí\EäAË`#¬   CuYCm  Bd A`ïGÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00574    T.N-rrppppprLAX AË`#¬   LAX     SEA     B	ÔÂíE÷×+AË`#Ê   CpYCs  B"C«ø GÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00574    T.N-rrppppprLAX AË`$$   LAX     SEA     B	úáÂí¶FYAË`#è   CkYCr  B$JC©+àGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00574    T.N-rrppppprLAX AË`$$   LAX     SEA     B
 mÂíØ¿FóAË`$   CeÙCp  B(¼ßC¨Ë_GÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00574    T.N-rrppppprLAX AË`$$   LAX     SEA     B""ÂíòYFffAË`$$   Ca&fCr  B5C©:hGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00574    T.N-rrppppprLAX AË`$$   LAX     SEA     B=ÂîùF"/AË`$B   C[ÙCu  B933C©«¤GÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00574    T.N-rrppppprLAX AË`%   LAX     SEA     BþKÂîF+vAË`$`   CWYC{  B7$iC©+öGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00574    T.N-rrppppprLAX AË`%   LAX     SEA     BòÂî9,F5ÜAË`$~   CTÙC|  BüC©(nGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00574    T.N-rrppppprLAX AË`%   LAX     SEA     BÂîÕUF+UðAË`%   CW&fCz  Bb[C¬ôGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00574    R.N-rrpppp-rLAX AË`%   LAX     SEA     B%?&ÂüEÛ	rAË` 
   CoÙC  AöîïBGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00676    T.N-rrppppprLGA AË`    IAD     BOS     B%»¼Â[OEÑyKAË` F   Cr&fC Aî³ÅBÅµGÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00676    T.N-rrppppprLGA AË`    IAD     BOS     B%ù,ÂÍE¾v?AË` d   Cv¦fC Aâ[BEaGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00676    T.N-rrppppprLGA AË`    IAD     BOS     B& mÂÝE«L0AË`    C{&fC AÚÜBZ¦GÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00676    T.N-rrppppprLGA AË`    IAD     BOS     B&KÂ¤±E\¥AË`     CÙC A½PÈB`GÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00676    T.N-rrppppprBOS AË`!r   IAD     BOS     B&rYÂtEÑAË`     C3C A¤JBÈGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00676    T.N-rrppppprBOS AË`!r   IAD     BOS     B&Â?&Ecê	AË` ¾   CÓ3C  A(öBMGÃO¿  GÃOGÃOÁ×
Á×
A®}FSL00676    T.N-rrppppprBOS AË`!r   IAD     BOS     B'ä±ÂÄDE>1ùAË`!r   C3C  AaBÎSGÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00676    T.N-rrppppprBOS AË`!r   IAD     BOS     B(IcÂ§E+áAË`!®   C,ÍC  AEòB=R¦GÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00676    T.N-rrppppprBOS AË`"   IAD     BOS     B(¬`Â\)DärAË`!Ì   C9C  A,ÚtB4=?CA¿  GÃOGÃOÁ×
Á×
Aù	FSL00676    T.N-rrppppprBOS AË`"   IAD     BOS     B)d±ÂZtDffAË`"&   C9Cp  A5?Ö>«a%¿  GÃOGÃOÁ×
Á×
Aù	FSL00676    T.N-rrppppprBOS AË`"   IAD     BOS     B)ÔÂ´èDffAË`"   C9Cl  A,ÚtCW>«	¿  GÃOGÃOÁ×
Á×
AÕgFSL00676    T.N-rrppppprBOS AË`"   IAD     BOS     B)p¤Âù,    AË`"Ú   C3CY  @ÅòCa>¡S¿  GÃOGÃOÁ×
Á×
Ag8FSL00676    R.N-rrpppp-rBOS AË`"ø   IAD     BOS     BòÂ¸q~F&« AË`ì   CZ¦fC  A(öC.CGÃO¿  GÃOGÃOÁ×
Á×
¾Hè§FSL00147    T.N-rrppppprLIT AË`!   MCO     SFO     BUUÂ»2YF&« AË`!   CZ&fC A²¢C¬GÃO¿  GÃOGÃOÁ×
Á×
¾Hè§FSL00147    T.N-rrppppprLIT AË`!   MCO     SFO     B mÂ½ì`F&´àAË`"D   CY¦fC  AFC«GÃO¿  GÃOGÃOÁ×
Á×
¾Hè§FSL00147    T.N-rrppppprAMA AË`%È   MCO     SFO     Bä±ÂÀ F&« AË`#p   CYYC AFCµ¬GÃO¿  GÃOGÃOÁ×
Á×
¾Hè§FSL00147    T.N-rrppppprAMA AË`%È   MCO     SFO     BåÂÃBF&° AË`$   CYYC AÚÜCLGÃO¿  GÃOGÃOÁ×
Á×
¾®}FSL00147    T.N-rrppppprAMA AË`%È   MCO     SFO     BßÂÅâ"F&° AË`%È   CY&fC  AÖGC9GÃO¿  GÃOGÃOÁ×
Á×
¾®}FSL00147    R.N-rrpppp-rAMA AË`%È   MCO     SFO     A.µÂ«Ó F,pËAË`     C^&fCd  Ad C{GÃO¿  GÃOGÃOÁ×
Á×
A@OFSL00859    T.N-rrppppprSJO AË` ú   GUA     SJO     A+býÂ«ZtF!çìAË` ú   CcÙCm  A¬ÚtCGÃO¿  GÃOGÃOÁ×
Á×
A&`ªFSL00859    T.N-rrppppprSJO AË` ú   GUA     SJO     A)Ð7Â«ÞF?cAË`!   CiÙCw  A¨¼ßC¼kGÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00859    T.N-rrppppprSJO AË`!6   GUA     SJO     A)Â«Fª[AË`!   CoYC  A¤JC1 GÃO¿  GÃOGÃOÁ×
Á×
A)MFSL00859    T.N-rrppppprSJO AË`!6   GUA     SJO     A(X¿ÂªçAFSAË`!6   CuYC  Ad C*íGÃO¿  GÃOGÃOÁ×
Á×
A/ËFSL00859    T.N-rrppppprSJO AË`!6   GUA     SJO     A'UUÂªÀÚE÷káAË`!6   C{ÙC Ad C/ëGÃO¿  GÃOGÃOÁ×
Á×
A4FSL00859    T.N-rrppppprSJO AË`!6   GUA     SJO     A&KÂªåEä|WAË`!T   C3C  AíÌCðGÃO¿  GÃOGÃOÁ×
Á×
A!«6FSL00859    T.N-rrppppprSJO AË`!®   GUA     SJO     A%@ÚÂªp¤EÑ\AË`!r   CÓ3C  AíÌCM3GÃO¿  GÃOGÃOÁ×
Á×
A!«6FSL00859    T.N-rrppppprSJO AË`!®   GUA     SJO     A$DDÂªJ=E¾;ºAË`!   CS3C­ AÐ7CoGÃO¿  GÃOGÃOÁ×
Á×
A eFSL00859    T.N-rrppppprSJO AË`!®   GUA     SJO     A#
=ÂªOE«_±AË`!®   C3A0  A²¢CërGÃO¿  GÃOGÃOÁ×
Á×
A!«6FSL00859    T.N-rrppppprSJO AË`!®   GUA     SJO     A _Â©µÃE+áAË`!ê   C¬ÍBØ  @²¢CGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00859    T.N-rrppppprSJO AË`"D   GUA     SJO     A§AÂ©tEYAË`!ê   C¹C
  @fxC)ä?)ëî¿  GÃOGÃOÁ×
Á×
A<ZFSL00859    T.N-rrppppprSJO AË`"D   GUA     SJO     AQìÂ©ffEdrAË`"&   C¹C+  @öîïCYf>G¿  GÃOGÃOÁ×
Á×
A±ÄFSL00859    T.N-rrppppprSJO AË`"D   GUA     SJO     AiÐÂ©9,E>EzAË`"D   C3C  @EòBõaB>ó+.¿  GÃOGÃOÁ×
Á×
A2ï5FSL00859    T.N-rrppppprSJO AË`"D   GUA     SJO     AòYÂ©Eñ[AË`"b   CS3C  @(öBçÍ>»,¿  GÃOGÃOÁ×
Á×
A'ò|FSL00859    T.N-rrppppprSJO AË`#4   GUA     SJO     A¸RÂ¨ËDäAË`"¼   CyBÂ  @(öB>O*U¿  GÃOGÃOÁ×
Á×
A,§ðFSL00859    T.N-rrppppprSJO AË`#4   GUA     SJO     AwwÂ¨ìD?cAË`"ø   C3B²  AEòB¥ñ>+,¿  GÃOGÃOÁ×
Á×
A+FSL00859    R.N-rrpppp-rSJO AË`#4   GUA     SJO     B'üÂ¯çAD´nAË`!6   CùC  AvîïGÃOGÃO¿  GÃOGÃOÁ×
Á×
A
óFSL00768    R.N-rrpppp-rMKE AË`!r   ORD     TPA     B'Ü)Â¯üDffAË`!6   C3C A²¢ChÁ>?¼À¿  GÃOGÃOÁ×
Á×
A µFSL00768    T.N-rrppppprMKE AË`!r   ORD     TPA     B'¸Â°òDå°AË`!T   CùC AíÌCNc>ÀRÓ¿  GÃOGÃOÁ×
Á×
AFÜFSL00768    T.N-rrppppprMKE AË`!r   ORD     TPA     B'iÐÂ°ùEffAË`!r   C¬ÍC  AaC'JGÃO¿  GÃOGÃOÁ×
Á×
A¬FSL00768    T.N-rrppppprMKE AË`!r   ORD     TPA     B'/ÉÂ¯í:E> AË`!   C3C AÞ=qCGÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00768    T.N-rrppppprORD AË`!ê   ORD     TPA     B&þKÂ¯ÕUEe5¨AË`!   CÓ3C  AÚÜCTGÃO¿  GÃOGÃOÁ×
Á×
AØ­FSL00768    T.N-rrppppprORD AË`!ê   ORD     TPA     B&ª«Â¯¼EmAË`!®   C3C Aê0CdzGÃO¿  GÃOGÃOÁ×
Á×
ACFSL00768    T.N-rrppppprORD AË`!ê   ORD     TPA     B&¸Â¯¾KE©AË`!ê   C&fC B	ßC5baGÃO¿  GÃOGÃOÁ×
Á×
A
óFSL00768    T.N-rrppppprORD AË`!ê   ORD     TPA     B%ÄDÂ¯¼E«s3AË`!ê   C{&fC B$JC1ÙÿGÃO¿  GÃOGÃOÁ×
Á×
@÷ÿ/FSL00768    T.N-rrppppprORD AË`"b   ORD     TPA     B%NÂ¯µÃE¾°ÅAË`"   Cw¦fC B.é?C-fËGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00768    T.N-rrppppprORD AË`"b   ORD     TPA     B$Ü)Â¯ mEÑÍAË`"&   Cs&fC  B.é?CGÃO¿  GÃOGÃOÁ×
Á×
A
óFSL00768    T.N-rrppppprORD AË`"b   ORD     TPA     B$QìÂ¯Eä­AË`"D   Co¦fC  B0ø	C ñÇGÃO¿  GÃOGÃOÁ×
Á×
AüPFSL00768    T.N-rrppppprORD AË`"b   ORD     TPA     B#¯ÉÂ¯kE÷Ã©AË`"   CjYC B*ËªC QGÃO¿  GÃOGÃOÁ×
Á×
AjFSL00768    T.N-rrppppprSDF AË`#¬   ORD     TPA     B#EùÂ¯X¿F~AË`"   Ce¦fC B0ø	C yGÃO¿  GÃOGÃOÁ×
Á×
A±ÄFSL00768    T.N-rrppppprSDF AË`#¬   ORD     TPA     B"NÂ¯-:FäàAË`"Ú   C`&fC B µC ¢GGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00768    T.N-rrppppprSDF AË`#¬   ORD     TPA     B½qÂ®7F~ÉAË`#¬   C[ÙC B"C!\GÃO¿  GÃOGÃOÁ×
Á×
AÕgFSL00768    T.N-rrppppprSDF AË`#¬   ORD     TPA     B±~Â®b"F!ñ­AË`#è   CX¦fC B3ÔC!ØGÃO¿  GÃOGÃOÁ×
Á×
@þFtFSL00768    T.N-rrppppprATL AË`'0   ORD     TPA     BºÂ­¹,F+s3AË`$Ø   CVYC  B"C!GÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00768    T.N-rrppppprATL AË`'0   ORD     TPA     BÄDÂ¬ê«F06fAË`&   CTYC BaC!þ}GÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00768    T.N-rrppppprATL AË`'0   ORD     TPA     B@33Â÷ÕUF6).AË` '  CbÍCr  Aâ[Cu²GÃO¿  GÃOGÃOÁ×
Á×
    FSL00647    T.G-rrppppprYVR AË`"&                   BBDDÂù F9ðAË` Ü   CcÍCx  AÅòCå³GÃO¿  GÃOGÃOÁ×
Á×
    FSL00647    T.G-rrppppprYVR AË`"&                   BD»¼ÂûF9h­AË`!   CdÍCx  AFCg_GÃO¿  GÃOGÃOÁ×
Á×
    FSL00647    T.G-rrppppprYVR AË`"&                   BFÂü-§F9(AË`"   GÃOC}  A µCæ¥GÃO¿  GÃOGÃOÁ×
Á×
    FSL00647    XTN-rrHpppprSEA AË`!ê                   BIÂþ;¼F9®+AË`"ø  Cd@ Cu  AFCwgGÃO¿  GÃOGÃOÁ×
Á×
    FSL00647    T.G-rrppppprYZT AË`$ö                   BLÂÿîïF9wNAË`#¬  CeÍCt  A µCúoGÃO¿  GÃOGÃOÁ×
Á×
    FSL00647    T.G-rrppppprYZT AË`$ö                   BNffÃ ÑF9´DAË`$`  Ccs3Cx  AvîïC:vGÃO¿  GÃOGÃOÁ×
Á×
    FSL00647    T.G-rrppppprYZT AË`$ö                   BPÌÍÃ³3F9ÄAË`%   Ce@ Cq  AFCRGÃO¿  GÃOGÃOÁ×
Á×
    FSL00647    T.G-rrppppprYZT AË`$ö                   BR ÃWÒF9Ý¸AË`%   GÃOC{  AaCº~GÃO¿  GÃOGÃOÁ×
Á×
    FSL00647    XTN-rrHppp-rKTN AË`%                   A°õÃÂüF&° AË` ¾   Cc¦fCm  B7ÀGÃOGÃO¿  GÃOGÃOÂÇúáÂÇúáA®}FSL00152    R.N-rrpppp-rMIA AË`$B   CCS     MIA     A·µÂ
F&« AË`!ê   CcYCp  AÚÜCñGÃO¿  GÃOGÃOÂÇúáÂÇúáA®}FSL00152    T.N-rrppppprMIA AË`$B   CCS     MIA     A½0ÂP7F&° AË`#   Cb&fC  AaC-cGÃO¿  GÃOGÃOÂÇúáÂÇúáA¬FSL00152    T.N-rrppppprMIA AË`$B   CCS     MIA     AÃÂ7F&° AË`$B   C^&fC~  AEòCßQGÃO¿  GÃOGÃOÂÇúáÂÇúáA¬FSL00152    T.N-rrppppprMIA AË`$B   CCS     MIA     AÅzáÂFÔF!ìÍAË`$   C]YCl  @öîïC°GÃO¿  GÃOGÃOÂÇúáÂÇúáA®}FSL00152    T.N-rrppppprMIA AË`%   CCS     MIA     AÆ±~Â¾KFI$AË`$Ø   Cc&fCm  @öîïCGÃO¿  GÃOGÃOÂÇúáÂÇúáAg8FSL00152    T.N-rrppppprMIA AË`%   CCS     MIA     AÇ6Âð¤F¸üAË`$ö   Ci&fCb  @æxCºÿGÃO¿  GÃOGÃOÂÇúáÂÇúáACFSL00152    T.N-rrppppprMIA AË`%   CCS     MIA     AÇ¸RÂ"ýFA7AË`%   CnYC  ?²¢CcËGÃO¿  GÃOGÃOÂÇúáÂÇúáAù	FSL00152    T.N-rrppppprMIA AË`%   CCS     MIA     AÈ{ÂEùE÷;AË`%2   Cs&fA°  @²¢C®GÃO¿  GÃOGÃOÂÇúáÂÇúáAÛFSL00152    T.N-rrppppprMIA AË`%n   CCS     MIA     AÈ\)ÂaHEäUSAË`%2   CwÙA¸  @¤JC¢fGÃO¿  GÃOGÃOÂÇúáÂÇúáA®}FSL00152    T.N-rrppppprMIA AË`%n   CCS     MIA     AÈ§AÂ~KEÑ+DAË`%P   C|¦fAà  @öîïCsGÃO¿  GÃOGÃOÂÇúáÂÇúáA¬FSL00152    T.N-rrppppprMIA AË`%n   CCS     MIA     AÈîïÂ¿E¾EzAË`%n   CÓ3B  A$JC#GÃO¿  GÃOGÃOÂÇúáÂÇúáA®}FSL00152    T.N-rrppppprMIA AË`%n   CCS     MIA     AÉDDÂ¯ÉE«s3AË`%n   C,ÍB  @µCe·GÃO¿  GÃOGÃOÂÇúáÂÇúáAØ­FSL00152    T.N-rrppppprPBI AË`%ª   CCS     MIA     AÉ0ÂÂE\¥AË`%   CìÍCG  @µC¾SGÃO¿  GÃOGÃOÂÇúáÂÇúáA<ZFSL00152    T.N-rrppppprPBI AË`%ª   CCS     MIA     AÊjÂÕUEAË`%   ClÍCV  @ÖGC¢¾GÃO¿  GÃOGÃOÂÇúáÂÇúáAg8FSL00152    T.N-rrppppprPBI AË`%ª   CCS     MIA     AÊzáÂéÐEd$AË`%ª   ClÍC @¤JC¢Ç[GÃO¿  GÃOGÃOÂÇúáÂÇúáAg8FSL00152    T.N-rrppppprPBI AË`%ª   CCS     MIA     AËjÂµE>XüAË`%È   CùB  @²¢C¢>ÃKÈ¿  GÃOGÃOÂÇúáÂÇúáAÕgFSL00152    T.N-rrppppprMIA AË`'0   CCS     MIA     B*Â£×F9¼ÍAË`!  C]ÙC B.é?GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00683    R.N-rrpppp-rALB AË`!r   KJF     KKP     B*í:ÂúF9¼ÍAË`"  C_¦fC B0ø	CuP?MV¿  GÃOGÃOÁ×
Á×
    FSL00683    T.N-rrppppprALB AË`"   KJF     KKP     B+ÅùÂRÆF9BáAË`"®   C_&fC  BrêC¹GÃO¿  GÃOGÃOÁ×
Á×
    FSL00683    T.N-rrppppprROC AË`"   KJF     KKP     B-IcÂuÃF9¼ÍAË`#D  Ca¦fC B"CÒÒGÃO¿  GÃOGÃOÁ×
Á×
    FSL00683    T.N-rrppppprROC AË`#4   KJF     KKP     B.Ð7ÂtF9¼ÍAË`#Û   Cb&fC  BrêCÜÄGÃO¿  GÃOGÃOÁ×
Á×
    FSL00683    T.N-rrppppprCLE AË`#Ê   KJF     KKP     B0\)Â ËF9¼ÍAË`$q  Cf&fC  B£×CÇGÃO¿  GÃOGÃOÁ×
Á×
    FSL00683    T.N-rrppppprROC AË`$`   KJF     KKP     B1éÐÂ¢ùF9¼ÍAË`%   CeYC BíÌC!þGÃO¿  GÃOGÃOÁ×
Á×
    FSL00683    T.N-rrppppprROC AË`$ö   KJF     KKP     B3kÂ£@ F9¼ÍAË`%   Cd&fC B+CÆGÃO¿  GÃOGÃOÁ×
Á×
    FSL00683    T.N-rrppppprROC AË`%   KJF     KKP     B4éÐÂ¤}qF9¼ÍAË`&4  Ce&fC  B£×C`GÃO¿  GÃOGÃOÁ×
Á×
    FSL00683    T.N-rrppppprCLE AË`&"   KJF     KKP     AµjBì4VF2"AË`%î   GÃOC Aâ[GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00484    XTN-rrHppp-rFOC AË`&¸                   Bu#Ã¯¥F9¬óAË`È   GÃOC`  A5C*ÙGÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    XTN-rrHpppprYMA AË`°                   BuÃDDF9¨AË`Ì  C^s3C_  A5B¸ñpGÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    T.G-rrppppprYMA AË`!®                   BuDDÃâ"F9³AË`   C\YCX  AMÇCáêGÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    T.G-rrppppprYMA AË`!®                   BuUUÃ	F9Á­AË`!5   C^ÍCX  A$JCI<GÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    T.G-rrppppprYMA AË`!®                   Bu;sÃ×
F9à(AË`#N   GÃOCu  A$JCÜ8GÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    XTN-rrHpppprMDO AË`#4                   BtÃ*«F1NÑAË`%n   C\À C @¤JC×"GÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    T.G-rrppppprMDO AË`%ª                   BtDDÃ{¼EÖ!¬AË`&	  C^ÍC@  @²¢CGÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    T.G-rrppppprMDO AË`&"                   Bt33Ã³3E¿vGAË`&'  CbÀ C6  AíÌCÌGÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    T.G-rrppppprMDO AË`&"                   BtÃUUExöæAË`&  CsÙC  @æxC}~GÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    T.G-rrppppprANC AË`&                   Bt""ÃEW2AË`&  Cw¦fC7  @²¢C`æGÃO¿  GÃOGÃOÁ×
Á×
    FSL00155    T.G-rrppppprANC AË`&                   B	ª«Â·KFCC3AË`"Ç   C\¦fC Ad GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00156    R.N-rrpppp-rDFW AË`"¼   KDF     WKL     B
¿&ÂµÆÔFCC3AË`#]  C\YC A¬ÚtBÐuGÃO¿  GÃOGÃOÁ×
Á×
    FSL00156    T.N-rrppppprDFW AË`#R   KDF     WKL     BÓ Â´?&FCC3AË`#ô   C\YC  AµB!xGÃO¿  GÃOGÃOÁ×
Á×
    FSL00156    T.N-rrppppprDFW AË`#è   KDF     WKL     BëÂ²´FCC3AË`$  C]¦fC A¬ÚtB 4GÃO¿  GÃOGÃOÁ×
Á×
    FSL00156    T.N-rrppppprBNA AË`$~   KDF     WKL     BüÂ±(FCC3AË`%   C]&fC  A¹33Bõ¢GÃO¿  GÃOGÃOÁ×
Á×
    FSL00156    T.N-rrppppprBNA AË`%   KDF     WKL     BòÂ¯åFBÉHAË`%·   C\YC  AÍÇBGÃO¿  GÃOGÃOÁ×
Á×
    FSL00156    T.N-rrppppprATL AË`%ª   KDF     WKL     B0Â®jFBÉHAË`&M  C]&fC AÍÇB~GÃO¿  GÃOGÃOÁ×
Á×
    FSL00156    T.N-rrppppprSDF AË`&@   KDF     WKL     Bp¤Â¢F&° AË`   CX&fC AÑä±GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00158    R.N-rrppp--rRDU AË`     KAT     LKD     B  ÂEF)AË`!ï   C]¦fC  AûBx-GÃO¿  GÃOGÃOÁ×
Á×
    FSL00158    R.N-rrppp--rGSO AË`!ê   KAT     LKD     BjÂ¶EùF06fAË`"¦   CZYC  A°ø	GÃO?MÒò¿  GÃOGÃOÁ×
Á×
    FSL00309    R.N-rrpppp-rMEM AË`"   KDF     WKM     B=qÂ´F06fAË`#<  CZ&fC  A¹33B½{ÔGÃO¿  GÃOGÃOÁ×
Á×
    FSL00309    T.N-rrppppprDFW AË`#4   KDF     WKM     BîïÂ²ù,F06fAË`#Ó   CYYC AÍÇB¾¡ï?KC¿  GÃOGÃOÁ×
Á×
    FSL00309    T.N-rrppppprMOB AË`#Ê   KDF     WKM     BjÂ±RÆF06fAË`$i  CX&fC  AÑä±BÂMGÃO¿  GÃOGÃOÁ×
Á×
    FSL00309    T.N-rrppppprMOB AË`$`   KDF     WKM     B £×Â¯½qF06fAË`$ÿ  CWYC  Aâ[BÒÚ3GÃO¿  GÃOGÃOÁ×
Á×
    FSL00309    T.N-rrppppprATL AË`$ö   KDF     WKM     Aÿ~KÂ®&fF/¼{AË`%   CW&fC AÑä±BÓeÙGÃO¿  GÃOGÃOÁ×
Á×
    FSL00309    T.N-rrppppprATL AË`%   KDF     WKM     Aý®Â¬{F06fAË`&,  CUYC AÁn]BÔ:N?NV¿  GÃOGÃOÁ×
Á×
    FSL00309    T.N-rrppppprATL AË`&"   KDF     WKM     B""Âêª«EâAË`Å   C9Ct  AûCo"GÃO¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË`Î                   B""ÂêæfEüéAË`ã   C,ÍCl  AÅòCÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË`Î                   BÂë*«E9AË`    CùCl  AÉ©Co"GÃO¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË` F                   B  Âë]ÞEÝAË`    C9Cm  AÉ©CCâGÃO¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË` F                   B  ÂëEB{@AË` =   CÓ3Cl  AÉ©Cÿ'?NV¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË` F                   BîïÂëÄDE;·ýAË` [   C,ÍCl  AÁn]CWO>ÊüÆ¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË` F                   BîïÂëîïE$¯ðAË` y   CùCu  A¨¼ßCÿ'>ÄûÒ¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË` ¾                   BÝÞÂìECñAË`    CfCq  AaCôÂ>¤2¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË` ¾                   BÝÞÂì*«DÜAË` µ   Cà Cg  A=PÈCÿ'>kÆw¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË` ¾                   BÝÞÂìLÍD8
AË` Ó   C¬ÍC-  @fxCÿ'>ï¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË` ¾                   BÌÍÂìnïDku?AË` ñ   CyC  ?ÅòCôÂ>¢Ð¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË`!6                   BÌÍÂìDFYAË`!   C  Ct  @$JCÿ'>ip¯¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË`!6                   BÌÍÂì¢"C$©AË`!-   C      ?²¢Cÿ'>hI¿  GÃOGÃOÁ×
Á×
    FSL00735    T.G-rrppppprLAX AË`!6                   B»¼Âì»¼BZ<AË`!K   Cf    @$JC{ò>y¹¿  GÃOGÃOÁ×
Á×
    FSL00735    R.G-rrpppp-rLAX AË`!6                   Aë§AÂµnF&6AË`´   C[¦fC  B	ßC.?N{¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprMOB AË`°   KTP     AKL     Aí§Â¶¡HF&° AË` J  C[ÙC  B+C ?Mh¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprIAH AË` F   KTP     AKL     Aîp¤Â·ÒÆF&6AË` á   C[ÙC  BaCÂ?Mh¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprIAH AË` Ü   KTP     AKL     AïÉcÂ¹®F&° AË`!w  C[¦fC BFCÊ¾?MÒò¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprHOU AË`!r   KTP     AKL     Að,`ÂºBF&6AË`"   C[YC  B7ÀC>f?MÒò¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprIAH AË`"   KTP     AKL     Að
=Â»jF&6AË`"¤  C\&fC B+C<?MOß¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprDFW AË`"   KTP     AKL     AïÚtÂ¼Ç®F&6AË`#:  C\YC BüCñ?MÒò¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprHOU AË`#4   KTP     AKL     Aïª«Â¾ÍF&° AË`#Ñ   C\ÙC B£×CñÇ?NV¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprDFW AË`#Ê   KTP     AKL     AðG®Â¿W
F&6AË`$g  C]ÙC  AµCbÞGÃO¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprDFW AË`$`   KTP     AKL     AñÂÂÀF&6AË`$þ   C^YC  A¨¼ßC	æ?LD¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprDFW AË`$ö   KTP     AKL     Aó6ÂÁîF&6AË`%  C]&fC  A¨¼ßCÄV?NV¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprDFW AË`%   KTP     AKL     Aô§AÂÃ:F&6AË`&+   C\&fC  AaCÁf?Mh¿  GÃOGÃOÁ×
Á×
    FSL00160    T.N-rrppppprIAH AË`&"   KTP     AKL     B	ð¤Â¯òF&° AË`%©  CXÙC A(öGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00388    R.N-rrpppp-rATL AË`%ª   KAT     LKD     B
 mÂ°ïÉF&° AË`&@   CXÙC  A µCï4GÃO¿  GÃOGÃOÁ×
Á×
    FSL00388    T.N-rrppppprATL AË`&@   KAT     LKD     BÌÍÂônïBDJAË`%  CS3C  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    R.G-rrpppp-rSFO AË`%ª                   BÌÍÂônïB¿jAË`%  C9C  AVG    <½Ò¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂôwwC]HéAË`%  CÆfC AíÌCj?LÌÍ¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂôwwC»èÁAË`%  CS3C AÐ7    =Ã¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂôwwD
é AË`%  C,ÍC An³Å    =·°¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂôwwD,IÇAË`%  CùC AEò    =v¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂô DD7
AË`%  Cà C  Ad Cÿ'?LÌÍ¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂô D\KQAË`%¡  CÓ3Cw  A$J    =E¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂô DvãTAË`%¤  C3Cp  A,Út    =YÈ4¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂôD¨AË`%§  CFfCg  A=PÈCÿ'?LÌÍ¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂôDhAË`%ª  C  Cd  A=PÈ    <ßÃ¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BÝÞÂôDÝ¹AË`%­  C3C]  AEò    <HÀ¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`%ª                   BîïÂôDRåAË`%°  CfCV  AEòCj?LÌÍ¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSJC AË`%È                   BîïÂôDåSAË`%³  CìÍCR  AEò    <Ç¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSJC AË`%È                   BîïÂôD¢êeAË`%¶  C¬ÍCU  AMÇ    <ÅÕ<¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSJC AË`%È                   BîïÂôD©ÆAË`%¹  CyCT  AMÇCÿ'?LÌÍ¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSJC AË`%È                   B  Âôª«Dß~AË`%È   Cà CR  A^=qCGº>Õ;æ¿  GÃOGÃOÁ×
Á×
    FSL00485    T.B-rrppppprSFO AË`&                   BÂô»¼E3:AË`%×   C,ÍCV  A(öCGºGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.B-rrppppprSFO AË`&                   BDDÂô»¼E?`UAË`%æ   CfCh  Aî³Å    GÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&                   BwwÂôÄDEAQÄAË`%õ   CùCa  AÑä±CªÈ¨GÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.B-rrppppprSFO AË`&                   BÂô»¼EAxÈAË`&   CìÍCc  AöîïAÔyGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.B-rrppppprSFO AË`&@                   B»¼Âô¢"EA©AË`&   CùCh  B£×BaC¯GÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&@                   BÌÍÂô ENÄÊAË`&"   CS3C`  AûBðOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.B-rrppppprSFO AË`&@                   BÌÍÂôffEnU¥AË`&1   C3CW  AæxB´eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&@                   BÝÞÂôDDEt\AË`&@   CfCJ  BÐ7BðOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&|                   BÝÞÂô""EÅAË`&O   C` CJ  BÐ7B´eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&|                   BîïÂó÷wEAË`&^   ClÍCL  BüBdGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&|                   BîïÂóÌÍE£ÒAË`&m   C~ÙCI  BüB´eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&|                   B  Âó¢"E²1AË`&|   C{¦fCS  Bd BdGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&¸                   B  ÂówwE½HAË`&   Cy@ C[  B µB´e?K¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&¸                   BÂóDDEË-AË`&  Cu¦fC\  B.é?B¡TGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&¸                   BÂóEÙ£ AË`&©  Cqó3C_  B7$iB´eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00485    T.G-rrppppprSFO AË`&¸                   B@DDÃ{¼FU~AË`²   CaÀ C  A µCSöGÃO¿  GÃOGÃOÁ×
Á×
    FSL00389    T.G-rrppppprYZT AË`!                   BBª«Ã@ FsùAË` f   CbÀ C AFCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00389    T.G-rrppppprYZT AË`!                   BE  ÃFi AË`!   Cc@ C  A(öCUÆGÃO¿  GÃOGÃOÁ×
Á×
    FSL00389    R.G-rrpppp-rYZT AË`!                   B{ÂµÆF9BáAË`$  C[YC A¹33GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00873    R.N-rrpppp-rMEM AË`$~   KAT     LKS     BËÂ¶ù,F9BáAË`%0   C]YC AÅòC,ãGÃO¿  GÃOGÃOÁ×
Á×
    FSL00873    T.N-rrppppprMOB AË`%   KAT     LKS     B'AÂ¹Ç®F9BáAË`&]   C\&fC  AÁn]CÔãGÃO¿  GÃOGÃOÁ×
Á×
    FSL00873    R.N-rrpppp-rMEM AË`&@   KAT     LKS     A¡wwÂ¡HF&° AË`!   CbÙCy  B933GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00594    R.N-rrpppp-rSJU AË`!   TIS     TKA     A£býÂ´F&° AË`"/  CcYCv  BAn]C|GÃO¿  GÃOGÃOÁ×
Á×
    FSL00594    T.N-rrppppprGDT AË`"&   TIS     TKA     A¥  ÂÍ§F&6AË`"Å  Cc&fCy  BC}(CÌGÃO¿  GÃOGÃOÁ×
Á×
    FSL00594    T.N-rrppppprSTT AË`"¼   TIS     TKA     A¦ÂçAF&° AË`#\   CbÙCy  BC}(Cý,GÃO¿  GÃOGÃOÁ×
Á×
    FSL00594    T.N-rrppppprGDT AË`#R   TIS     TKA     A¨(öÂÿ&F&° AË`#ò  Cc&fCy  BG½CÐ.GÃO¿  GÃOGÃOÁ×
Á×
    FSL00594    T.N-rrppppprGDT AË`#è   TIS     TKA     A©®Â¿F&° AË`$   CcYCx  BK¸RC¸GÃO¿  GÃOGÃOÁ×
Á×
    FSL00594    T.N-rrppppprGDT AË`$~   TIS     TKA     A«:Â33F&6AË`%  CbÙCv  BOÕçC§pGÃO¿  GÃOGÃOÁ×
Á×
    FSL00594    T.N-rrppppprGDT AË`%   TIS     TKA     A­ffÂ?&F&° AË`%µ  CbÙCv  BMÇC¶òGÃO¿  GÃOGÃOÁ×
Á×
    FSL00594    T.N-rrppppprGDT AË`%ª   TIS     TKA     A¯±~ÂNF&6AË`&L   Cc&fCu  B5C3ªGÃO¿  GÃOGÃOÁ×
Á×
    FSL00594    T.N-rrppppprGDT AË`&@   TIS     TKA     BÉcÂ£Ï\F£3AË`%Ú   Cc&fC AÍÇGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00165    R.N-rrppp--rRDU AË`%È   KRD     UKA     Bd  ÂôffF&° AË`    C_ÍCh  AEòCøNGÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYDQ AË`"&                   BeffÂ÷  F& 'AË` Æ   C^ó3Cc  A²¢CGÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYDQ AË`"&                   Bf»¼Âù³3F&×AË`!z   C^ÙCe  @öîïCîGÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYDQ AË`"&                   Bg0[Âú©õF&¿ÙAË`!·   GÃOCv  @öîïC¥GÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    XTN-rrHpppprYDQ AË`!®                   Bh  ÂüffF,dAË`".  C]¦fCo  @æxCLGÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYDQ AË`"&                   Bi33Âÿ33F9¿=AË`"â  C`YCV  A(öCd?På`¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYQH AË`$ö                   BjffÃDF9ÐOAË`#  C_ÍCd  AíÌCùµGÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYQH AË`$ö                   BkwwÃs3F9¹$AË`$K   C`s3Cu  A5CDDGÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYQH AË`$ö                   BlÃæfF9ÄAË`$ÿ   C`¦fCi  @µC5GÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYQH AË`$ö                   Blà¶ÃqF9Ø×AË`%<  GÃOCo  @¤JC^GÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    XTN-rrHpppprYQH AË`%2                   BmwwÃffF9¾AË`%³   C_YCc  @æxC_^GÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYMA AË`'Æ                   BnffÃæfF9ë!AË`&g  C^ÙCf  @ÅòCjýGÃO¿  GÃOGÃOÁ×
Á×
    FSL00766    T.G-rrppppprYMA AË`'Æ                   B mÂßF&° AË`$Ø   CX¦fC BrêGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00578    R.N-rrpppp-rGSO AË`$Ø   KIA     DKA     B{Â éÐF&° AË`%n  CXYC B+Ci[BGÃO¿  GÃOGÃOÁ×
Á×
    FSL00578    T.N-rrppppprGSO AË`%n   KIA     DKA     B÷wÂ¡û¼F&° AË`&   CXYC BÁlChúoGÃO¿  GÃOGÃOÁ×
Á×
    FSL00578    T.N-rrppppprRDU AË`&   KIA     DKA     BZtÂ£
=F&° AË`&  CX¦fC  B	ßCh¡ÐGÃO¿  GÃOGÃOÁ×
Á×
    FSL00578    T.N-rrppppprRDU AË`&   KIA     DKA     B+÷wÂ©$±F9¼ÍAË`æ  Cd¦fC  B,ÚtCgÎGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprCLE AË`ì   KJF     KKS     B+éÐÂª¤±F9¼ÍAË` }   CcÙC B(¼ßC|ÉGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprORD AË`    KJF     KKS     B+£×Â¬$±F9¼ÍAË`!   Ce&fC B933CdÀGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprCLE AË`!   KJF     KKS     B+W
Â­F9¼ÍAË`!©  CbÙC B*ËªCQGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprMKE AË`!®   KJF     KKS     B+Â¯7F9¼ÍAË`"@   Cb&fC  B5CúwGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprDTW AË`"D   KJF     KKS     B*¶Â°DF9¼ÍAË`"Ö  Ca&fC  B0ø	CÚéGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprORD AË`"Ú   KJF     KKS     B*]ÞÂ±ôèF9¼ÍAË`#m   C^YC  B933CGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprORD AË`#p   KJF     KKS     B*jÂ³b"F9¼ÍAË`$  C]¦fC B?_CwGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprORD AË`$   KJF     KKS     B)§AÂ´ÈF9BáAË`$  C\ÙC  B?_CUNGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprORD AË`$   KJF     KKS     B)Â¶8RF9¼ÍAË`%0   C_YC B(¼ßCw
GÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprORD AË`%2   KJF     KKS     B)ËÂ·ª«F9¼ÍAË`%Æ  C^&fC B*ËªCèGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprORD AË`%È   KJF     KKS     B)úáÂ¹¸F9¼ÍAË`&]   C_¦fC  B3ÔCÕGÃO¿  GÃOGÃOÁ×
Á×
    FSL00311    T.N-rrppppprORD AË`&^   KJF     KKS     B
]ÞÂ¸ó3F&° AË`É  CYYC  AÅòCzäGÃO¿  GÃOGÃOÁ×
Á×
    FSL00534    T.N-rrppppprDFW AË`°   KBO     SKD     B	¥ÂºKF&6AË` _  CZ&fC  A°ø	C~ýxGÃO¿  GÃOGÃOÁ×
Á×
    FSL00534    T.N-rrppppprDFW AË` F   KBO     SKD     BëÂ»¤±F&6AË` ö   CZ&fC A¹33C~í¿GÃO¿  GÃOGÃOÁ×
Á×
    FSL00534    R.N-rrpppp-rDFW AË` ú   KBO     SKD     B	Â¦aHFBÉHAË` ú   CY¦fC A°ø	C©Õ©GÃO¿  GÃOGÃOÁ×
Á×
    FSL00171    T.N-rrppppprATL AË` ú   KMC     OKC     BÓ Â¦çAFCC3AË`!   CZÙC  A¬ÚtC§õGÃO¿  GÃOGÃOÁ×
Á×
    FSL00171    R.N-rrpppp-rGSO AË`!   KMC     OKC     B.òYÂl`F9BáAË` >  CbYC Aÿ*Cæ?MV¿  GÃOGÃOÁ×
Á×
    FSL00172    R.N-rrppp--rALB AË` (   BOS     PDX     B1ÂÞFBÉHAË`"   Cb&fC BÐ7GÃO?Mh¿  GÃOGÃOÁ×
Á×
    FSL00172    R.N-rrpppp-rROC AË`"   BOS     PDX     B1Ó Â 0FBÉHAË`#.  Ca¦fC AòÑZC«.?Gï¿  GÃOGÃOÁ×
Á×
    FSL00172    T.N-rrppppprROC AË`#4   BOS     PDX     B2 mÂ¡ÆFBÉHAË`#Å   Cc&fC AöîïCä?K¿  GÃOGÃOÁ×
Á×
    FSL00172    T.N-rrppppprROC AË`#Ê   BOS     PDX     B3Ð7Â¢õÃFBÉHAË`$[  CdÙC B(öC?N{¿  GÃOGÃOÁ×
Á×
    FSL00172    T.N-rrppppprROC AË`$`   BOS     PDX     B5:Â¤@ FBÉHAË`$ñ  CdYC AòÑZCZÖ?Gl¿  GÃOGÃOÁ×
Á×
    FSL00172    T.N-rrppppprCLE AË`$ö   BOS     PDX     B6¬`Â¥ìFBÉHAË`%   Cf&fC Aî³ÅC[Ï?MOß¿  GÃOGÃOÁ×
Á×
    FSL00172    T.N-rrppppprBUF AË`%   BOS     PDX     B÷wÂ!HF/¼{AË`å   CT&fC BÐ7AázGÃO¿  GÃOGÃOÁ×
Á×
    FSL00393    T.N-rrppppprGSO AË`Î   KPB     IKB     BNÂzF/¼{AË` {   CT&fC BüAésOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00393    T.N-rrppppprGSO AË` d   KPB     IKB     B¨öÂÚtF/¼{AË`!  CU&fC B7ÀAßP¦GÃO¿  GÃOGÃOÁ×
Á×
    FSL00393    T.N-rrppppprGSO AË`!   KPB     IKB     B{ÂQìF/¼{AË`!¨   CT¦fC B(¼ßA¾M¼GÃO¿  GÃOGÃOÁ×
Á×
    FSL00393    T.N-rrppppprRDU AË`!   KPB     IKB     B ÂÑF/¼{AË`">  CU&fC B0ø	A´´¼GÃO¿  GÃOGÃOÁ×
Á×
    FSL00393    T.N-rrppppprRDU AË`"&   KPB     IKB     BÌÍÂÆF/¼{AË`"Õ   CU&fC B933B&GÃO¿  GÃOGÃOÁ×
Á×
    FSL00393    T.N-rrppppprIAD AË`"¼   KPB     IKB     BµÂ%F06fAË`#k   CWÙC B"B ÛGÃO¿  GÃOGÃOÁ×
Á×
    FSL00393    T.N-rrppppprDCA AË`#p   KPB     IKB     B%ÕUÂÌÍEÙ+AË`%   CpYC Aÿ*BOrÓ?0bN¿  GÃOGÃOÁ×
Á×
    FSL00393    R.N-rrpppp-rLGA AË`%n   KPB     IKB     BÂÂº+F)HAË`    CcYC A$JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00176    R.N-rrpppp-rDFW AË`     KJA     NKD     B=Â»¨F)HAË`!5   CcYC  AEòCátGÃO¿  GÃOGÃOÁ×
Á×
    FSL00176    T.N-rrppppprSHV AË`!6   KJA     NKD     BNÂ½ÞF£3AË`!Ë  Cc¦fC A^=qC¶XGÃO¿  GÃOGÃOÁ×
Á×
    FSL00176    T.N-rrppppprDFW AË`!Ì   KJA     NKD     B(öÂ¾{F)HAË`"b   CdYC A²¢CçGÃO¿  GÃOGÃOÁ×
Á×
    FSL00176    R.N-rrpppp-rDFW AË`"b   KJA     NKD     BºÂðF)AË`$ó  C\¦fCp  BG½GÃO?NV¿  GÃOGÃOÁ×
Á×
    FSL00316    R.N-rrppp--rLAX AË`$ö   KSF     OKL     BåÂ¨¯ÉF9BáAË`ü   C]¦fC  BrêCª°GÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprCLE AË`ì   KIA     DKS     B½qÂª§F9BáAË`   C]&fC B&®CTGÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprCLE AË`    KIA     DKS     BáHÂ«iÐF9BáAË`!(  C\YC B&®CxRGÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprSTL AË`!   KIA     DKS     BµÂ¬ÅùF9BáAË`!¿   C\¦fC B"CTsGÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprCVG AË`!®   KIA     DKS     B¸Â®#×F9¼ÍAË`"U  CZÙC BFC/GÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprSDF AË`"D   KIA     DKS     B4èÂ¯F9¼ÍAË`"ì   C\¦fC B7ÀCådGÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprSTL AË`"Ú   KIA     DKS     BKÂ°èöF9¼ÍAË`#  C]¦fC  Bd Cã¼GÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprSTL AË`#p   KIA     DKS     BZtÂ²KòF9BáAË`$  C^&fC BrêCÌGÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprSTL AË`$   KIA     DKS     BffÂ³®ïF9BáAË`$¯   C^¦fC BUUCzGÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprSTL AË`$   KIA     DKS     BáHÂ¶ÚtF06fAË`&   CW&fC  B,ÚtC)dGÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    T.N-rrppppprSTL AË`&   KIA     DKS     B*«Â¸0¤F06fAË`&­  CVÙC B&®C£GÃO¿  GÃOGÃOÁ×
Á×
    FSL00317    R.N-rrpppp-rSTL AË`&   KIA     DKS     B¨öÂ¯F£3AË`%Â   C`¦fC  BÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00182    R.N-rrppp--rSTL AË`%È   KCV     GKS     B Â¢*«F/¼{AË` 6   CS¦fC  A¹33?ë5TGÃO¿  GÃOGÃOÁ×
Á×
    FSL00319    T.N-rrppppprCLT AË` (   KMC     OKL     BèÂ¡ÃjF/¼{AË` Ì   CSYC AÁn]A@ÁGÃO¿  GÃOGÃOÁ×
Á×
    FSL00319    T.N-rrppppprGSO AË` ¾   KMC     OKL     B~KÂJ=F9¼ÍAË`"T   CW¦fC  AÍÇB&ËGÃO¿  GÃOGÃOÁ×
Á×
    FSL00319    T.N-rrppppprGSO AË`"D   KMC     OKL     B½qÂtF9BáAË`"ê  CWÙC  AÖGB¹GÃO¿  GÃOGÃOÁ×
Á×
    FSL00319    T.N-rrppppprRDU AË`"Ú   KMC     OKL     BüÂF9BáAË`#   CX¦fC Aâ[B*cGÃO¿  GÃOGÃOÁ×
Á×
    FSL00319    T.N-rrppppprGSO AË`#p   KMC     OKL     B?&ÂÂF9BáAË`$  CY&fC Aî³ÅB>ÈGÃO¿  GÃOGÃOÁ×
Á×
    FSL00319    T.N-rrppppprIAD AË`$   KMC     OKL     BÂâ"F9BáAË`$­  CYYC  BÐ7BÂGÃO¿  GÃOGÃOÁ×
Á×
    FSL00319    T.N-rrppppprIAD AË`$   KMC     OKL     B½qÂéÐF9¼ÍAË`%D   C[¦fC AöîïB$GÃO¿  GÃOGÃOÁ×
Á×
    FSL00319    T.N-rrppppprIAD AË`%2   KMC     OKL     BèÂéÐF9¼ÍAË`%Ú  CZYC  BaB*ÙÆGÃO¿  GÃOGÃOÁ×
Á×
    FSL00319    R.N-rrpppp-rPIT AË`%È   KMC     OKL     Aö{Âc×F9BáAË` 8  CXYC  AFCDúGÃO¿  GÃOGÃOÁ×
Á×
    FSL00186    T.N-rrppppprCHS AË` (   KLG     AKF     AñDDÂ¿&F9BáAË` Ï   CVYC A*CDGÃO¿  GÃOGÃOÁ×
Á×
    FSL00186    T.N-rrppppprPBI AË` ¾   KLG     AKF     Aìp¤Â
=F9BáAË`!e  CYYC AµCA¦GÃO¿  GÃOGÃOÁ×
Á×
    FSL00186    T.N-rrppppprMIA AË`!T   KLG     AKF     Aç0ÂNF9BáAË`!ü   C\&fC  AFC@b1GÃO¿  GÃOGÃOÁ×
Á×
    FSL00186    T.N-rrppppprMIA AË`!ê   KLG     AKF     Aâª«Â{F9BáAË`"  C[¦fC  A µC@0GÃO¿  GÃOGÃOÁ×
Á×
    FSL00186    R.N-rrpppp-rMIA AË`"   KLG     AKF     B¯ÉÂ)F9BáAË`"1  C[YC B²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00397    R.N-rrpppp-rGSO AË`"&   KLG     AMY     Bp¤Â.ïF9BáAË`"È   C[&fC Aî³ÅCO(GÃO¿  GÃOGÃOÁ×
Á×
    FSL00397    T.N-rrppppprGSO AË`"¼   KLG     AMY     B;¼ÂÈF9BáAË`#^   C[YC  Aâ[CPÙGÃO¿  GÃOGÃOÁ×
Á×
    FSL00397    T.N-rrppppprGSO AË`#R   KLG     AMY     BµÂffF9BáAË`#ô  CYÙC  AÖGCPøUGÃO¿  GÃOGÃOÁ×
Á×
    FSL00397    T.N-rrppppprRDU AË`#è   KLG     AMY     BÀÚÂµF9BáAË`$   CYYC AÍÇCPJ0GÃO¿  GÃOGÃOÁ×
Á×
    FSL00397    T.N-rrppppprGSO AË`$~   KLG     AMY     BjÂtF9BáAË`%!  CXÙC  AÅòCPåGÃO¿  GÃOGÃOÁ×
Á×
    FSL00397    T.N-rrppppprGSO AË`%   KLG     AMY     B	.Â
=F9BáAË`%¸   CYÙC  AÉ©CHGÃO¿  GÃOGÃOÁ×
Á×
    FSL00397    T.N-rrppppprGSO AË`%ª   KLG     AMY     BÇ®ÂQF9BáAË`&N  CT¦fC AÑä±C@øGÃO¿  GÃOGÃOÁ×
Á×
    FSL00397    T.N-rrppppprRDU AË`&@   KLG     AMY     BM^Âä|rFäAË`#w   C^&fCv  BÁlGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00475    R.N-rrppp--rPHX AË`#p                   BÙâÂ«FäAË`!%  Cg&fC  B(öGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00398    R.N-rrppp--rCVG AË`!                   BnïÂ±âFAË`$«   Ci&fC AÁn]GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00398    R.N-rrppp--rSDF AË`$                   B wwÂÆ°FøÕAË`    C\&fC  B(öGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00190    R.N-rrppp--rMCK AË`                    B {¼Â»
FøÕAË`$!  CZ&fC  B$JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00190    R.N-rrppp--rMCI AË`$                   BáHÂåF9BáAË`"  CYÙC BaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00191    R.N-rrpppp-rGSO AË`"   KEW     RKA     BnïÂ¹,F9BáAË`#+   CZ¦fC BíÌCm]ÛGÃO¿  GÃOGÃOÁ×
Á×
    FSL00191    T.N-rrppppprGSO AË`#   KEW     RKA     B
=ÂàmF9BáAË`#Á  CY¦fC  B µCnÛiGÃO¿  GÃOGÃOÁ×
Á×
    FSL00191    T.N-rrppppprGSO AË`#¬   KEW     RKA     B§ÂôèF9BáAË`$X   CZÙC  B	ßCksÐGÃO¿  GÃOGÃOÁ×
Á×
    FSL00191    T.N-rrppppprGSO AË`$B   KEW     RKA     BüÂ ùF9BáAË`$î   CY¦fC  BÁlCi²´GÃO¿  GÃOGÃOÁ×
Á×
    FSL00191    T.N-rrppppprGSO AË`$Ø   KEW     RKA     BhÂ¡ÆF9BáAË`%  CY&fC  AûCiGÃO¿  GÃOGÃOÁ×
Á×
    FSL00191    T.N-rrppppprGSO AË`%n   KEW     RKA     BÑìÂ¢ mF9BáAË`&   CX¦fC AòÑZCiGGÃO¿  GÃOGÃOÁ×
Á×
    FSL00191    T.N-rrppppprRDU AË`&   KEW     RKA     BOëÎÃ}§F%Ø4AË`!l   Ce&fC}  AvîïGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00852    R.N-rrppp--rYZT AË`!T                   B[	ÃìMF%ñÎAË`$ò   Cb&fC     GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00852    R.N-rrppp--rKTN AË`$Ø                   B\N¿ÁHoiF03öAË`"   CP&f@@  B=PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00800    R.N-rrppp--rXXD AË`&@   EDDM    KIAD    Bq²êÃvF& 'AË` Ä   C[&fCT  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00853    R.N-rrppp--rYQH AË` ¾                   BsaÃBYF&fAË`$J   C[&fCH  @fxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00853    R.N-rrppp--rMDO AË`$B                   B(ÞÂ¯öD³©AË`"   C9C  AíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00399    R.N-rrpppp-rORD AË`#p   KORD    KDEN    B(1AÂ°jE 	ÿAË`"ø   C3C Ad C¦JGÃO¿  GÃOGÃOÁ×
Á×
    FSL00399    T.N-rrppppprORD AË`#p   KORD    KDEN    B(?Â°ÅzEá-AË`#4   ClÍC AòÑZCòGÃO¿  GÃOGÃOÁ×
Á×
    FSL00399    T.N-rrppppprORD AË`#p   KORD    KDEN    B'½<Â±¯BEßïAË`#   CpYC BEòC@GÃO¿  GÃOGÃOÁ×
Á×
    FSL00399    T.N-rrppppprMCI AË`'¨   KORD    KDEN    B'xRÂ²U2F¦yAË`#è   CbÙC BOÕçC!GÃO¿  GÃOGÃOÁ×
Á×
    FSL00399    T.N-rrppppprMCI AË`'¨   KORD    KDEN    B']Â²ÞOF¾AË`$$   C]¦fC  BEòC6GÃO¿  GÃOGÃOÁ×
Á×
    FSL00399    R.N-rrpppp-rMCI AË`'¨   KORD    KDEN    BÂÇØvFN-AË`&}   C^&fGÃOGÃOGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00723    XWN-rrpBF--rOKC AË`&|                   BÂ¦Ï\F9¼ÍAË`Ç  CW¦fC Aê0C¬Þ\?O ¿  GÃOGÃOÁ×
Á×
    FSL00194    R.N-rrpppp-rGSO AË`°   KFL     LKC     B\ÂÛjF&¹ÁAË`"Þ   CZ&fC AµGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00499    R.N-rrppp--rPHX AË`"Ú                   BÇeÂã³éF&¹ÁAË`&c  CY&fCx  AÚÜGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00499    R.N-rrppp--rTUS AË`&^                   AÿKÂ½ìF9¼ÍAË`   C[ÙC Ad C8GÃO¿  GÃOGÃOÁ×
Á×
    FSL00195    R.N-rrpppp-rDFW AË` 
   KFL     LKD     B zPÂTVFðAË`#   C`&fC B3ÔGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00401    R.N-rrppp--rIAD AË`"ø                   BõÃÂ¤HFðAË`&   C`&fC  B933GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00401    R.N-rrppp--rPIT AË`&                   B"uÝÂ¯C®XyAË`"b   C¹Ck  A,ÚtGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00450    R.N-rrpppp-rALB AË`#   KJFK    EGLL    B"VÂt¯DêZeAË`"   C3C{  A=PÈC¦>Gn¿  GÃOGÃOÁ×
Á×
    FSL00450    T.N-rrppppprALB AË`#   KJFK    EGLL    B")âÂ÷ÜETAË`"Ú   CÓ3C Ad B¯,µGÃO¿  GÃOGÃOÁ×
Á×
    FSL00450    T.N-rrppppprALB AË`#   KJFK    EGLL    B"õÝÂ¸REÉÕ²AË`#p   Cv&fC Aê0BMGÃO¿  GÃOGÃOÁ×
Á×
    FSL00450    T.N-rrppppprALB AË`$`   KJFK    EGLL    B#-Â}IEùsAË`#Ê   Cj&fC  B7ÀBº§GÃO¿  GÃOGÃOÁ×
Á×
    FSL00450    T.N-rrppppprALB AË`$`   KJFK    EGLL    B#µÃÂ8yF¦äAË`$$   C^YC  B.é?B«~GÃO¿  GÃOGÃOÁ×
Á×
    FSL00450    R.N-rrpppp-rALB AË`$`   KJFK    EGLL    BåÂ°^FðAË`"Ò   Cb&fC  AæxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00710    R.N-rrppp--rSTL AË`"¼                   B6æÂ³DjAË`&X   C3C @²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00710    R.N-rrppp--rMEM AË`&@                   B)^íÂ,C§ÈAË`!Ì   CùCV  A=PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00403    R.N-rrpppp-rBDL AË`"   KBOS    KSFO    B)UÐÂ±'DéøÜAË`"   CùCz  A,ÚtBº8è>ëz¿  GÃOGÃOÁ×
Á×
    FSL00403    T.N-rrppppprBDL AË`"   KBOS    KSFO    B)ÐÂ8ETF/AË`"&   C¬ÍC  AµA±GÃO¿  GÃOGÃOÁ×
Á×
    FSL00403    T.N-rrppppprBDL AË`"   KBOS    KSFO    B*ÐÂ¦EÉÚAË`"Ú   Cs¦fC Aî³ÅCªGÃO¿  GÃOGÃOÁ×
Á×
    FSL00403    T.N-rrppppprALB AË`#¬   KBOS    KSFO    B*ûÂc°Eù¦wAË`#   Cg¦fC  Bd CGHGÃO¿  GÃOGÃOÁ×
Á×
    FSL00403    T.N-rrppppprALB AË`#¬   KBOS    KSFO    B+"NÂBFÆAË`#p   C\ÙC BG½C7GÃO¿  GÃOGÃOÁ×
Á×
    FSL00403    R.N-rrpppp-rALB AË`#¬   KBOS    KSFO    B½ÂªhFpAË`!  Cc&fC  AòÑZGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00477    R.N-rrppp--rSDF AË` ú                   BåCÂ²'E¿jAË`$   C}&fC  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00477    R.N-rrppp--rBNA AË`$~                   B¾Âì×CBAË`!r   C9CM  @EòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00328    R.N-rrpppp-rLAX AË`"   KLAX    KDEN    B?Âí4Dç¡AË`!®   CyCf  AVGCsún>0Ä¿  GÃOGÃOÁ×
Á×
    FSL00328    T.N-rrppppprLAX AË`"   KLAX    KDEN    BÂíESMwAË`!Ì   C¬ÍCk  A½PÈC(	GÃO¿  GÃOGÃOÁ×
Á×
    FSL00328    T.N-rrppppprLAX AË`"   KLAX    KDEN    B±'ÂëþúEÉ AË`"b   C}ÙCn  AûBãGÃO¿  GÃOGÃOÁ×
Á×
    FSL00328    T.N-rrppppprLAX AË`#4   KLAX    KDEN    Bã½ÂëiEøÙ£AË`"¼   CoÙCv  BüB0ÛGÃO¿  GÃOGÃOÁ×
Á×
    FSL00328    T.N-rrppppprLAX AË`#4   KLAX    KDEN    B	üPÂêÅFFAË`"ø   Cc&fCw  BrêBT¼GÃO¿  GÃOGÃOÁ×
Á×
    FSL00328    R.N-rrpppp-rLAX AË`#4   KLAX    KDEN    B'ö+Â¯ãnD<¹AË`!r   CyC A(öGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00200    R.N-rrpppp-rORD AË`"&   KORD    KMCO    B'|Â°ÿE\AË`!®   C,ÍC  AaCN«GÃO¿  GÃOGÃOÁ×
Á×
    FSL00200    T.N-rrppppprORD AË`"&   KORD    KMCO    B&¹ÁÂ¯ðEd7AË`!ê   CÓ3C AÍÇC)BGÃO¿  GÃOGÃOÁ×
Á×
    FSL00200    R.N-rrpppp-rORD AË`"&   KORD    KMCO    B0aHÂëF/¼{AË`%¸  C[YCm  BrÑZGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00405    R.N-rrpppp-rBOI AË`%ª   PDX     SLC     B/=qÂéºáF/¼{AË`&O   CXÙCj  B*BÖêbGÃO¿  GÃOGÃOÁ×
Á×
    FSL00405    T.N-rrppppprBOI AË`&@   PDX     SLC     A33Â¢®F9¼ÍAË`    C^YC  A½PÈC®ñðGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprMBJ AË` 
   MPT     OKA     AbýÂ¢BF9¼ÍAË` £  C^ÙC~  A¹33C®øuGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprAHJ AË`     MPT     OKA     AÂ¢}qF9¼ÍAË`!:   C`¦fC AµC®þÓGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprAHJ AË`!6   MPT     OKA     AÂ¢¶F9BáAË`!Ð  Ca&fC A¨¼ßC®ïMGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprAHJ AË`!Ì   MPT     OKA     A¿&Â¢ÒÆF9BáAË`"g  CaYC  A¬ÚtC±|GÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprAHJ AË`"b   MPT     OKA     A£Ð7Â¢éÐF9BáAË`"þ   CaYC  A¬ÚtC±÷ÄGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprEYW AË`"ø   MPT     OKA     A¨õÃÂ¢ÿ&F9BáAË`#   C`ÙC  A¬ÚtC²%GÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprMBJ AË`#   MPT     OKA     A®,`Â£ùF9BáAË`$*  Ca&fC  Ad C³jGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprMIA AË`$$   MPT     OKA     A³ffÂ¢áHF9¼ÍAË`$Á   C`ÙC  Ad @ÈRãGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprMIA AË`$º   MPT     OKA     A¸®Â¢»¼F9¼ÍAË`%W  Ca&fC  A¤J@ÊæùGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprMIA AË`%P   MPT     OKA     A½ä±Â¢F9¼ÍAË`%î   C`¦fC  A µA¾ÊGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    T.N-rrppppprMIA AË`%æ   MPT     OKA     AÃ33Â¢eF9¼ÍAË`&   Ca&fC A µ@©ùóGÃO¿  GÃOGÃOÁ×
Á×
    FSL00202    R.N-rrpppp-rMIA AË`&|   MPT     OKA     BÓ Â¶WåF/¼{AË`   CVÙC  A¨¼ßBQF]GÃO¿  GÃOGÃOÁ×
Á×
    FSL00514    T.N-rrppppprDFW AË`ì   KDF     WKC     B§AÂµ!HF/¼{AË`    CU&fC  AÉ©BT"GÃO¿  GÃOGÃOÁ×
Á×
    FSL00514    T.N-rrppppprBNA AË`    KDF     WKC     BwwÂ³çAF/¼{AË`!2  CX&fC  A°ø	BV#¿GÃO¿  GÃOGÃOÁ×
Á×
    FSL00514    T.N-rrppppprSTL AË`!   KDF     WKC     BLÍÂ²¦fF/¼{AË`!É   CW&fC AÁn]BWJ¤GÃO¿  GÃOGÃOÁ×
Á×
    FSL00514    T.N-rrppppprSTL AË`!®   KDF     WKC     BÎÂ®ëF
ÍAË`#  Ce¦fC  AöîïBVHaGÃO¿  GÃOGÃOÁ×
Á×
    FSL00514    R.N-rrpppp-rSTL AË`#p   KDF     WKC     B.EùÂ¬J=F9BáAË`Ä  CdÙC  B3ÔC}Ü?O²¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprORD AË`°   KBO     SKS     B.QìÂ­¿&F9¼ÍAË` Z  CcÙC B,ÚtCt¦?J~ú¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprORD AË` F   KBO     SKS     B.S Â¯6F9¼ÍAË` ñ   CaYC B;AþCÉ?NV¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprMKE AË` Ü   KBO     SKS     B.QìÂ°®F9BáAË`!  C`ÙC  B933CîGÃO¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprMKE AË`!r   KBO     SKS     B.KÂ²""F9¼ÍAË`"   C_ÙC  B0ø	C»Ú?KÆ¨¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprMKE AË`"   KBO     SKS     B.BÂ³F9BáAË`"´  C_&fC  B,ÚtC«ËGÃO¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprORD AË`"   KBO     SKS     B.1~ÂµUF9¼ÍAË`#K   C^YC B$JCZ]GÃO¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprORD AË`#4   KBO     SKS     B.¸Â¶åF9BáAË`#á  C^¦fC B(¼ßCMGÃO¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprORD AË`#Ê   KBO     SKS     B.Â¸)F9BáAË`$x   C_¦fC  B$JCxGÃO¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprORD AË`$`   KBO     SKS     B-æfÂ¹)F9¼ÍAË`%  C_YC B=PÈCÙèGÃO¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprPIA AË`$ö   KBO     SKS     B-ÀÚÂ»\F9BáAË`%¥   C_&fC  B=PÈCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprDSM AË`%   KBO     SKS     B-Â¼®F9BáAË`&;  C`&fC  BEòCGÃO¿  GÃOGÃOÁ×
Á×
    FSL00329    T.N-rrppppprFSD AË`&"   KBO     SKS     B
\Â°G®F&° AË`#G  CY&fC  AíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00330    R.N-rrpppp-rATL AË`#4   KAT     LKS     B
ÑìÂ±¨F&6AË`#Þ   CY&fC  A µC¶GÃO¿  GÃOGÃOÁ×
Á×
    FSL00330    T.N-rrppppprMEM AË`#Ê   KAT     LKS     BòÂ²ÿ&F&° AË`$u   CYYC A¹33CGÃO¿  GÃOGÃOÁ×
Á×
    FSL00330    T.N-rrppppprMEM AË`$`   KAT     LKS     BDDÂ´P7F&° AË`%   CYYC AÍÇC¤«GÃO¿  GÃOGÃOÁ×
Á×
    FSL00330    T.N-rrppppprBNA AË`$ö   KAT     LKS     BP7Âµ{F&6AË`%¡  CY&fC AæxC8ÉGÃO¿  GÃOGÃOÁ×
Á×
    FSL00330    T.N-rrppppprBNA AË`%   KAT     LKS     BÂ¶ÃjF&° AË`&8   CY&fC Aâ[Có2GÃO¿  GÃOGÃOÁ×
Á×
    FSL00330    T.N-rrppppprMEM AË`&"   KAT     LKS     B#@Â>CÌÓ[AË`!  C3Cl  A^=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00455    R.N-rrppp--rLGA AË`!r                   AªëÂÊ@ F&° AË`%·   C_YC AæxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00331    R.N-rrpppp-rGDL AË`%ª   MMX     LAX     A®\)ÂË""F&° AË`&N   C_¦fC AÖGCmGÃO¿  GÃOGÃOÁ×
Á×
    FSL00331    T.N-rrppppprGDL AË`&@   MMX     LAX     B
Â¯³éF
¯AË`#   Ch&fCy  AaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00505    R.N-rrppp--rATL AË`"ø                   B$ÖÁÂ2}F&©çAË`$!  CX&fC  B`L;GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00480    R.N-rrppp--rALB AË`$                   BEÂç
F¥åAË`#´   C_&fCz  B0ø	GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00802    R.N-rrppp--rLAS AË`#¬                   B4Â®PÈF&ÃAË`Ì  CY&fC AòÑZGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00481    R.N-rrppp--rBNA AË`°                   B%Ì;Â·,`F{IAË`"   CX&fC B3ÔGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00205    R.N-rrppp--rMKE AË`"                   Bª«ÂµÏ8F{IAË`%  C[&fC  Aÿ*GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00205    R.N-rrppp--rSTL AË`%                   B!\ÂÖ+F06fAË`$   CU¦fC B µGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00606    R.N-rrpppp-rDEN AË`$   SLC     ATL     B æfÂÔJ=F06fAË`$¥   CUÙC BÁlBÇí)GÃO¿  GÃOGÃOÁ×
Á×
    FSL00606    T.N-rrppppprDEN AË`$   SLC     ATL     B :ÂÒwwF06fAË`%<   CV&fC  AÑä±BÈïþGÃO¿  GÃOGÃOÁ×
Á×
    FSL00606    T.N-rrppppprDEN AË`%2   SLC     ATL     BòÂÐµÃF06fAË`%Ò  CV&fC  Aâ[BÉìeGÃO¿  GÃOGÃOÁ×
Á×
    FSL00606    T.N-rrppppprDEN AË`%È   SLC     ATL     BÓ ÂÎçAF06fAË`&i  CV&fC  B²¢BÊ®GÃO¿  GÃOGÃOÁ×
Á×
    FSL00606    T.N-rrppppprDEN AË`&^   SLC     ATL     BÂ°®F&° AË`%  CY¦fC AÉ©GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00207    R.N-rrpppp-rATL AË`%   KAT     LKP     BëÂ²ìF&6AË`%¦   CYÙC AÅòC÷mGÃO¿  GÃOGÃOÁ×
Á×
    FSL00207    T.N-rrppppprMOB AË`%   KAT     LKP     BâýÂ³vF&6AË`&=   CZ¦fC AÁn]C§rGÃO¿  GÃOGÃOÁ×
Á×
    FSL00207    T.N-rrppppprMEM AË`&@   KAT     LKP     B  ÂP7F)AË`#ú   C]¦fC  B µGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00209    R.N-rrppp--rRDU AË`#è   BWI     ATL     B(öÂ¢F9¼ÍAË`%Ï   CYYC  B²¢CiC*GÃO¿  GÃOGÃOÁ×
Á×
    FSL00209    R.N-rrppp--rGSO AË`%È   BWI     ATL     Aû(öÂ°òF9BáAË`#v  C[¦fC AÑä±GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00597    R.N-rrpppp-rMOB AË`#p   ATL     MMX     A÷@ÚÂ±]ÞF9¼ÍAË`$  C[YC AÑä±C\¡GÃO¿  GÃOGÃOÁ×
Á×
    FSL00597    T.N-rrppppprMOB AË`$   ATL     MMX     AóG®Â²1~F9¼ÍAË`$¥   CX¦fC  Aâ[C[ÁÙGÃO¿  GÃOGÃOÁ×
Á×
    FSL00597    T.N-rrppppprMOB AË`$   ATL     MMX     AïG®Â³jF9¼ÍAË`%<   CZ¦fC AÖGC[WvGÃO¿  GÃOGÃOÁ×
Á×
    FSL00597    T.N-rrppppprMOB AË`%2   ATL     MMX     AëQìÂ³ÑìF9¼ÍAË`%Ò  CZ&fC AÖGC[)ÛGÃO¿  GÃOGÃOÁ×
Á×
    FSL00597    T.N-rrppppprPNS AË`%È   ATL     MMX     AçÂ´¤±F9¼ÍAË`&j   CXÙC  AÍÇC\çSGÃO¿  GÃOGÃOÁ×
Á×
    FSL00597    T.N-rrppppprMOB AË`&^   ATL     MMX     B£×Â©F9BáAË`%I  C]&fC B0ø	GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00210    R.N-rrpppp-rCLE AË`%2   DTW     ATL     BËÂªnïF9BáAË`%à   C\ÙC  B µC`­GÃO¿  GÃOGÃOÁ×
Á×
    FSL00210    T.N-rrppppprSDF AË`%È   DTW     ATL     Bð¤Â«WåF9¼ÍAË`&w   C\YC B(öC`xaGÃO¿  GÃOGÃOÁ×
Á×
    FSL00210    R.N-rrpppp-rSDF AË`&^   DTW     ATL     B¿&Â¸ýqFBÉHAË` ©  C\YC A^=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00332    R.N-rrpppp-rDFW AË`     DFW     JAX     BjÂ·_FBÉHAË`!A   C\&fC  AfxB¼DéGÃO¿  GÃOGÃOÁ×
Á×
    FSL00332    T.N-rrppppprDFW AË`!6   DFW     JAX     BBÂµÀÚFBÉHAË`!Ø   C[¦fC  A²¢B¼ô¯GÃO¿  GÃOGÃOÁ×
Á×
    FSL00332    T.N-rrppppprDFW AË`!Ì   DFW     JAX     BüÂ´FBÉHAË`"o   C[ÙC AíÌB½GÃO¿  GÃOGÃOÁ×
Á×
    FSL00332    T.N-rrppppprDFW AË`"b   DFW     JAX     B®Â²vFBÉHAË`#   C[ÙC A*B¾¡õGÃO¿  GÃOGÃOÁ×
Á×
    FSL00332    T.N-rrppppprMEM AË`"ø   DFW     JAX     B ôÂ°ä±FBÉHAË`#   C\&fC  A(öBÎ.GÃO¿  GÃOGÃOÁ×
Á×
    FSL00332    T.N-rrppppprATL AË`#   DFW     JAX     Aÿ\Â¯gAFBÉHAË`$3  C\YC  A*BÞÿåGÃO¿  GÃOGÃOÁ×
Á×
    FSL00332    T.N-rrppppprMEM AË`$$   DFW     JAX     Aý,`Â­îïFBÉHAË`$Ê  C\YC A¤JBà3LGÃO¿  GÃOGÃOÁ×
Á×
    FSL00332    T.N-rrppppprATL AË`$º   DFW     JAX     Aú»¼Â¬tFBÉHAË`%a  C^YC  A¨¼ßBàÐàGÃO¿  GÃOGÃOÁ×
Á×
    FSL00332    R.N-rrpppp-rATL AË`%P   DFW     JAX     B:Â¡®F/¼{AË`&]   CTYC AæxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00945    R.N-rrpppp-rRDU AË`&^   ATL     BDL     B(öÂ©QìCU\)AË`$6   C3C ?ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00804    R.N-rrpppp-rCVG AË`$B                   B(öÂ©kD/BAË`$Q   C3C°  A²¢Cÿ'> à¿  GÃOGÃOÁ×
Á×
    FSL00804    T.N-rrppppprDAY AË`$`                   B(öÂ©záDM½qAË`$Z   C3C¥ A$JCÿ'?	çp¿  GÃOGÃOÁ×
Á×
    FSL00804    T.N-rrppppprSDF AË`$`                   B(öÂ© Dl8RAË`$`   C3C   A=PÈCÿ'>µ¿¿  GÃOGÃOÁ×
Á×
    FSL00804    T.N-rrppppprSDF AË`$~                   B(öÂ©D(öAË`$c   C3C  A5Cÿ'?2¿  GÃOGÃOÁ×
Á×
    FSL00804    R.N-rrpppp-rSDF AË`$Ø                   B8G®Ã
F&6AË`!l   CU&fC  B}OGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00598    R.N-rrppp--rXXC AË`!r                   B8
=ÃhöF&6AË`$ð   CV&fC B*GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00598    R.N-rrppp--rXXC AË`$ö                   B%=qÃkF06fAË` d   CZ&fC B=PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00213    R.N-rrppp--rXXC AË` d                   B!®Ã=F06fAË`#è   C_&fC  B£×GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00213    R.N-rrppp--rXXC AË`#è                   BS\)ÂJF)HAË` X   Cd&fC5  AÁn]GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00214    R.N-rrppp--rXXD AË` d                   BL×
Âb  F&° AË`#Ü   Ch&fCs  A,ÚtGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00214    R.N-rrppp--rYIF AË`#è                   B\Â¨ÑìCU\)AË`"   C3B  @$JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00216    R.N-rrpppp-rATL AË`"&                   B\Â¨ÑìCU\)AË`"   C3GÃOGÃO        ¿  GÃOGÃOÁ×
Á×
    FSL00216    XWN-rrpBFpprATL AË`"&                   B\Â¨ëD6áHAË`"5   C3Cs  @²¢Cÿ'>Ä²¿  GÃOGÃOÁ×
Á×
    FSL00216    T.N-rrppppprATL AË`"D                   BÂ¨ð¤DM½qAË`";   C3C  @µC>´¦¿  GÃOGÃOÁ×
Á×
    FSL00216    T.N-rrppppprATL AË`"D                   BÂ¨õÃDs×
AË`"D   C3C  @fxCÿ'>Òn¿  GÃOGÃOÁ×
Á×
    FSL00216    T.N-rrppppprATL AË`"                   BÂ¨úáD(öAË`"G   C3Cn  @fxCÿ'>åÐ6¿  GÃOGÃOÁ×
Á×
    FSL00216    R.N-rrpppp-rCHA AË`"¼                   B
Â°áHF&° AË`%Ñ   CZ&fC A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00216    R.N-rrppp--rHSV AË`%È                   B*áHÃ-Ü)F06fAË`#   Cf&fC  BSó|GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00410    R.N-rrppp--rXXC AË`#                   B/33Ã'¡HF9BáAË`&£   Cb&fC BüGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00410    R.N-rrppp--rXXC AË`&                   B.=qÃ(kF06fAË`!<   Cd&fC~  BFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00219    R.N-rrppp--rXXC AË`!T                   B5Ã"uÃF/¼{AË`$À   CZ&fCt  BEòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00219    R.N-rrppp--rXXC AË`$Ø                   B)=qÃ1¸F&° AË` â   Cf&fC  BZÜGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00220    R.N-rrppp--rXXC AË` Ü                   B+®Ã+.F&6AË`$f   Cg&fC  BUUGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00220    R.N-rrppp--rXXC AË`$`                   B+(öÃ-(öF9BáAË` g   Cd&fC BfxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00221    R.N-rrppp--rXXC AË` d                   B0
=Ã'
=F9¼ÍAË`#è   C`&fC BUUGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00221    R.N-rrppp--rXXC AË`#è                   BòÂ¦éÐF06fAË` F   CY&fC AòÑZGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00222    R.N-rrppp--tIND AË` F                   BÂ¦ùF)AË`!6   C]ÙC B7ÀGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00222    R.N-rrppp--tCVG AË`!6                   B
=Â¤ÍEûuÃAË`"   CkÙC  BüGÃO?K¿  GÃOGÃOÁ×
Á×
    FSL00222    R.N-rrppp--tCVG AË`"                   BèÂ£ÞEÈfAË`"ø   Cw&fC  AÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00222    R.N-rrppp--tPIT AË`"ø                   B í:Â¢ ÚE> AË`#Ê   ClÍC  A¨¼ßGÃO?K¿  GÃOGÃOÁ×
Á×
    FSL00222    R.N-rrppp--tPIT AË`#Ê                   B"ÝÞÂ xRD> AË`$º   C9C AÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00222    R.N-rrppp--tPIT AË`$º                   B§ÂÔF)AË`ì   C^YC BFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00515    R.N-rrppp--tRDU AË`ì                   BòÂùF)AË` ¾   C^&fC B"GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00515    R.N-rrppp--tIAD AË` ¾                   BÄDÂO\E÷¦fAË`"   Cl&fC  AÖGGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00515    R.N-rrppp--tDCA AË`"                   B µÂ×
EÍAË`#p   ClÍCw  A¹33GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00515    R.N-rrppp--tLGA AË`#p                   B"]ÞÂEEM½qAË`$B   ClÍCu  AÁn]GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00515    R.N-rrppp--tLGA AË`$B                   B$\ÂDDffAË`%2   C¹Cb  AíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00515    R.N-rrppp--tEWR AË`%2                   B$ÂÛODffAË`&   CyCc  AfxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00515    R.N-rrppp--tLGA AË`&                   Bð¤Â¢F&° AË`ì   CX¦fC  BÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00872    R.N-rrppp--tGSO AË`ì                   BNÂ£µÃF)AË` Ü   C^&fC  AöîïGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00872    R.N-rrppp--tRDU AË` Ü                   B6Â¤èF£3AË`!®   Cc¦fC AÍÇGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00872    R.N-rrppp--tATL AË`!®                   BÂ¦)ÐFYAË`"   Ck&fC AÍÇGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00872    R.N-rrppp--tATL AË`"                   B	Ø¿Â¨  E)AË`#p   C3C  AfxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00872    R.N-rrppp--tATL AË`#p                   BÂ¨¥EAHAË`$`   C3C @fxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00872    R.N-rrppp--tATL AË`$`                   BtÂ«höEÅ*áAË`#R   CwÙC  AÚÜGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00516    R.N-rrppp--tSDF AË`#R                   B \Â®Þ¸F£3AË`%   C^YC B"GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00516    R.N-rrppp--tSDF AË`%                   B ÔÂ±ÍF£3AË`&   C^ÙC  B£×GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00516    R.N-rrppp--tSDF AË`&                   B¨öÂª7E¬g
AË`"&   C3C  A*GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00923    R.N-rrppp--tATL AË`"&                   BwwÂ¬¥FAAË`#   Ce¦fC AÁn]GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00923    R.N-rrppp--tATL AË`#                   BæfÂ®DF£3AË`#è   CcÙC A¨¼ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00923    R.N-rrppp--tATL AË`#è                   BÔÂ°"ýF£3AË`$Ø   Cc¦fC A½PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00923    R.N-rrppp--tATL AË`$Ø                   B¬`Â²µF£3AË`%ª   Cc¦fC|  AÑä±GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00923    R.N-rrppp--tMOB AË`%ª                   B ÄDÂ´µF£3AË`&   CcYCx  AÁn]GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00923    R.N-rrppp--tMOB AË`&                   B1~Â®Ó F06fAË` F   CZ&fC BOÕçGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00741    R.N-rrppp--tSTL AË` F                   B¶Â®ùF06fAË`!   CV&fC BEòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00741    R.N-rrppp--tSDF AË`!                   BÂ®jF06fAË`"   CT¦fC BEòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00741    R.N-rrppp--tCVG AË`"                   BÂ­NF06fAË`"Ú   CV¦fC B,ÚtGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00741    R.N-rrppp--tSDF AË`"Ú                   BÂ¬ÂF06fAË`#Ê   CWÙC  B+GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00741    R.N-rrppp--tATL AË`#Ê                   B\Â¬µFZ=AË`$   C_&fC|  B²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00741    R.N-rrppp--tATL AË`$                   B	LÍÂª~KE¦° AË`%   CÓ3C|  B7ÀGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00741    R.N-rrppp--tATL AË`%                   BòÂ©X¿E-ZáAË`&^   C3        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00741    XWN-rrppD--tATL AË`&^                           CU\)AË`°   Cù        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00224    XWN-rrppD--tATL AË`°                           CU\)AË`!r   Cù        GÃO?K¿  GÃOGÃOÁ×
Á×
    FSL00224    XWN-rrppD--tATL AË`!r                           CU\)AË`#4   Cù        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00224    XWN-rrppD--tATL AË`#4                           CU\)AË`$ö   Cù        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00224    XWN-rrppD--tATL AË`$ö                   B0ÂÂ>KF0XAË` F   C[¦fC  A¨¼ßA«EGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00333    T.N-rrppppprDFW AË`!r   AUS     ORD     B¸RÂÁ,`F01AË`!r   CWÙC AFAÆWGÃO¿  GÃOGÃOÁ×
Á×
Aù	FSL00333    T.N-rrppppprDFW AË`!r   AUS     ORD     BõÃÂ¿«F0;GAË`"   C^ÙC Aî³ÅB`+GÃO¿  GÃOGÃOÁ×
Á×
A±ÄFSL00333    T.N-rrppppprTUL AË`$   AUS     ORD     B{Â¿0¤F!ãAË`"ø   CZYC AaB"j¨GÃO¿  GÃOGÃOÁ×
Á×
Ag8FSL00333    T.N-rrppppprTUL AË`$   AUS     ORD     B*«Â½d±F)AË`$$   C[&fC  A¬ÚtB%O£GÃO¿  GÃOGÃOÁ×
Á×
AÛFSL00333    T.N-rrppppprTUL AË`$   AUS     ORD     BÂ¼·wFNAË`$   C]¦fC  A¬ÚtB0!GÃO¿  GÃOGÃOÁ×
Á×
ACFSL00333    R.N-rrpppp-rTUL AË`$   AUS     ORD     BµÂ¨EÄ7
AË`$`   C~¦fC  A¨¼ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00539    R.N-rrppp--tATL AË`$`                   B	ËÂ¦UF&° AË`%2   CYYCw  AÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00539    R.N-rrppp--tATL AË`%2                   BòÂ¤¬`F06fAË`&"   CU&fC AÁn]GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00539    R.N-rrppp--tGSO AË`&"                   B?&Â¼?&EqAË`!ê   C¬ÍC  A*GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00540    R.N-rrppp--tDSM AË`!ê                   BëÂ¸ÔF)AË`#¬   CZ¦fC  A½PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00540    R.N-rrppp--tSTL AË`#¬                   Bí:Â³¸F)AË`%n   C\YC|  BaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00540    R.N-rrppp--tSTL AË`%n                   B	p¤ÂªÔE³ìAË` (   ClÍC  An³ÅGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00334    R.N-rrppp--tATL AË` (                   B§AÂª ÚFZ=AË`!   C_YC AaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00334    R.N-rrppp--tATL AË`!                   BþKÂ¬F&° AË`!ê   CX&fC  AµGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00334    R.N-rrppp--tATL AË`!ê                   BÂ­úF&° AË`"Ú   CYYC  AFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00334    R.N-rrppp--tSDF AË`"Ú                   BþKÂ¯¡HF&° AË`#¬   CX¦fC Aâ[GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00334    R.N-rrppp--tCVG AË`#¬                   B  Â±J=F&° AË`$   CXYC Aÿ*GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00334    R.N-rrppp--tSTL AË`$                   BòÂ³#×F¯®AË`%n   C]¦fC BíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00334    R.N-rrppp--tSTL AË`%n                   B  Â´®E¼RAË`&^   C{ÙC A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00334    R.N-rrppp--tSTL AË`&^                   B	iÐÂ©ëE©AË`$$   C¬ÍC Ad GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00541    R.N-rrppp--tATL AË`$$                   B=qÂª ÚFAAË`%   Cd¦fC  AÞ=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00541    R.N-rrppp--tATL AË`%                   BÔÂªÔFáAË`%æ   Cb&fC  AÖGGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00541    R.N-rrppp--tATL AË`%æ                   B$µÂkC¶áHAË` d   CS3        GÃO?K¿  GÃOGÃOÁ×
Á×
    FSL00230    XWN-rrppD--tEWR AË` d                   B  Â¨ÞE´ùAË`%   C,ÍC  @ÖGGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00558    R.N-rrppp--tATL AË`%                   B
ÆÂ¦)FNAË`&|   C`&fCt  Ad GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00558    R.N-rrppp--tATL AË`&|                   Aç33Â¢gAE5ÃAË`$º   C3B  AíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00518    R.N-rrppp--tMIA AË`$º                   AíÆÂ£wwF
ÍAË`%   Ci¦fB  AfxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00518    R.N-rrppp--tMIA AË`%                   Aô@ÚÂ¤ÔF)AË`&|   C_&fC¨ ?²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00518    R.N-rrppp--tCHS AË`&|                   B	BÂªDE±*=AË`!®   C3C @µGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00336    R.N-rrppp--tATL AË`!®                   BÅùÂªµFHAË`"   C]¦fC  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00336    R.N-rrppp--tATL AË`"                   BµÂª
FAË`#p   Cc&fC  AaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00336    R.N-rrppp--tATL AË`#p                   BÂ«åEçuAË`$B   CsYC A¤JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00336    R.N-rrppp--tSDF AË`$B                   B
=Â¬jEHfAË`%2   C3C A^=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00336    R.N-rrppp--tSDF AË`%2                   B]ÞÂ¬  D5ÃAË`&   C3        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00336    XWN-rrppD--tSDF AË`&                   BôÂ­8RF&° AË` d   CYYC  Bd GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00542    R.N-rrppp--tSDF AË` d                   B  Â®F øöAË`!T   CZ&fC B*ËªGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00542    R.N-rrppp--tSDF AË`!T                   B$\Â®ÄDEî  AË`"&   Cm¦fC  B(öGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00542    R.N-rrppp--tORD AË`"&                   B(µÂ¯LÍEäAË`#   Cp&fC B*ËªGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00542    R.N-rrppp--tORD AË`#                   B+ mÂ¯0¤EfHAË`#è   C,ÍC AòÑZGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00542    R.N-rrppp--tORD AË`#è                   B,Â°ÔD> AË`$Ø   Cy        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00542    XWN-rrppD--tORD AË`$Ø                   BÌÍÂïEåSAË` Ü   C3CS  B	ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    R.N-rrpppp-rSMF AË`!6   RNO     SFO     B®ÂïECLôAË` ú   C` CU  BUUC4eGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSMF AË`!6   RNO     SFO     B\Âï\EiAË` ú   C,ÍCV  BrêCU­
GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSMF AË`!6   RNO     SFO     BÂÂïÂEÝVAË`!6   Cà C]  B&®CGºGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSMF AË`!6   RNO     SFO     BáHÂïð¤EUmAË`!6   C¹Cc  B$JC7GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`!   RNO     SFO     B×
Âð
=E®u¼AË`!T   CfCd  B933CVòGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`!   RNO     SFO     BÂÂð=qEÁÆÏAË`!r   CFfCb  B0ø	CWmGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`!   RNO     SFO     B®ÂðkEÕ5%AË`!   CfCf  B?_C»?GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`!   RNO     SFO     B£×Âð{Eè|xAË`!®   CÆfCe  BMÇCoAGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`"   RNO     SFO     B\ÂðÇ®Eû°HAË`!®   CfCf  BfxCWRGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`"   RNO     SFO     B=qÂñFhKAË`!ê   CyCh  BûCsmnGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    XTN-rrHpppprSFO AË`"   RNO     SFO     B{ÂñBFî²AË`"   C9Cg  BaCslGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    XTN-rrHpppprSFO AË`"   RNO     SFO     B×
ÂñuÃFAË`"&   CùCj  BÆCo>GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    XTN-rrHpppprSFO AË`#¬   RNO     SFO     BÂñÌÍF)AË`"b   Cà Ck  B(öCtÊlGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    XTN-rrHpppprSFO AË`#¬   RNO     SFO     B33Âó
=FAË`#4   CùCm  BíÌCq÷lGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    XTN-rrHpppprSFO AË`#¬   RNO     SFO     BzáÂó¸RFç»AË`#¬   CùCj  Bõ1Cr#GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    XTN-rrHpppprSFO AË`#¬   RNO     SFO     B33ÂóõÃF?2AË`#è   C9Cg  B+Co¼BGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    XTN-rrHpppprSFO AË`$`   RNO     SFO     B
=Âô#×F¸ËAË`$   CyCb  BûCvGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    XTN-rrHpppprSFO AË`$`   RNO     SFO     B×
ÂôLÍEþ4AË`$$   C¹C^  BßCmüGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`$`   RNO     SFO     BÂô¸Eë¶AË`$B   C¬ÍC^  BG½Csm&GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`$`   RNO     SFO     BQìÂôÌÍE×ý)AË`$`   CùCZ  BEòCpð&GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`$Ø   RNO     SFO     B¸ÂôõÃEÄ¬AË`$~   C9CX  B;AþCmüGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`$Ø   RNO     SFO     BõÃÂõ#×E±xEAË`$   ClÍCO  B3ÔCv GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`$Ø   RNO     SFO     BÂÂõLÍE'2AË`$º   C  CG  BrêCmüGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`$Ø   RNO     SFO     B\ÂõkEé AË`$Ø   CÆfCL  B+Cf/$GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`&"   RNO     SFO     B(öÂõaHEo£AË`%   CùCY  Aî³ÅC(´EGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`&"   RNO     SFO     Bp¤Âô½qEIRAË`%æ   C,ÍCZ  AÞ=qCZÀGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`&"   RNO     SFO     B
=ÂôE#8lAË`&"   CS3Co  AÍÇC÷GÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`&"   RNO     SFO     B×
Âôp¤Dú¡AË`&^   CyCo  AvîïBôÛGÃO¿  GÃOGÃOÁ×
Á×
    FSL00827    T.N-rrppppprSFO AË`'¨   RNO     SFO     B"záÂ
=D2AË`%   C5CU  AfxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    R.N-rrpppp-TJFK AË`%2   ORD     ORD     B"p¤Â#×DÒ(ÜAË`%   C1Ce  A²¢CW²?ÏT¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTJFK AË`%2   ORD     ORD     B"p¤Â=qEAË`%2   C7Cv  AvîïCÿ'>°¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTJFK AË`%2   ORD     ORD     B"p¤ÂW
E6ºDAË`%2   C/Cx  A¨¼ßCÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTJFK AË`%2   ORD     ORD     B"ffÂuÃE]\jAË`%P   C/Cw  AÅòCC¼GÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTIAD AË`%ª   ORD     ORD     B"\)Â£×EâAË`%n   C/C  AÑä±CÓ¦GÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTIAD AË`%ª   ORD     ORD     B"\)ÂÌÍE)XAË`%n   C3Cw  AÑä±Cÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTIAD AË`%ª   ORD     ORD     B"QìÂ#×E¨6%AË`%ª   C&jC  Aî³ÅCPHGÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTIAD AË`%ª   ORD     ORD     B"QìÂLÍE»8AË`%ª   C{&nC  AòÑZCÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTDCA AË`&   ORD     ORD     B"G®Â\EÎëÍAË`%È   Cv&nC AæxCËòGÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTDCA AË`&   ORD     ORD     B"=qÂÂEâF¡AË`%æ   Cq&nC  BÐ7C$AGÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTDCA AË`&   ORD     ORD     B"33ÂúáEõ]/AË`&   Cl&nC  Bd CfGÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTDCA AË`&   ORD     ORD     B"33ÂG®F>¿AË`&"   Cg&nC B(¼ßCÿ'GÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTIAD AË`&^   ORD     ORD     B"=qÂzáFúÊAË`&@   Cb&nC B3ÔCÚTGÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTIAD AË`&^   ORD     ORD     B"G®Â®FAË`&^   C^&^C  BMÇCÚGÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    T.N-rrpppppTIAD AË`&^   ORD     ORD     B"QìÂÜ)F!8\AË`&^   C[&fC  B`L;C*äGÃO¿  GÃOGÃOÁ×
Á×
    FSL00786    R.N-rrpppp-TIAD AË`&^   ORD     ORD     BnïÂ©Þ¸E«s3AË`!r   C¦fC A¹33GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00235    R.N-rrppp--tSDF AË`!r                   B'AÂ©FeÃAË`"b   Ci¦fC  BUUGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00235    R.N-rrppp--tSDF AË`"b                   BéÐÂ¨FÔF
ÍAË`#4   Cg¦fC  BUUGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00235    R.N-rrppp--tATL AË`#4                   B
=Â¨  F)AË`$$   C]¦fC}  BüGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00235    R.N-rrppp--tGSO AË`$$                   B  Â§öF)AË`$ö   C]ÙC|  BÁlGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00235    R.N-rrppp--tATL AË`$ö                   B  Â§6F)AË`%æ   C]¦fC Aÿ*GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00235    R.N-rrppp--tATL AË`%æ                   B\Â¿&F)AË`°   C^&fC  Aê0GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00236    R.N-rrppp--tGSO AË`°                   BÂëF)AË`    C]ÙC  AÚÜGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00236    R.N-rrppp--tRDU AË`                    BÉcÂ®F
ÍAË`!r   CeÙC  Aÿ*GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00236    R.N-rrppp--tBWI AË`!r                   BbýÂùE÷¦fAË`"D   Cl&fC AÉ©GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00236    R.N-rrppp--tIAD AË`"D                   B iÐÂy,EMqAË`#4   CìÍCp  A¹33GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00236    R.N-rrppp--tBWI AË`#4                   B"£×ÂÖ0E+s3AË`$   C¹        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00236    XWN-rrppD--tLGA AË`$                   B$jÂµDffAË`$ö   C3        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00236    XWN-rrppD--tLGA AË`$ö                   B$ÂËDffAË`%È   Cy        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00236    XWN-rrppD--tLGA AË`%È                   BZtÂ§jFß®AË` (   Cl&fC  AÑä±GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00237    R.N-rrppp--tATL AË` (                   B òÂ¦ÔFYAË` ú   Ck¦fC  A½PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00237    R.N-rrppp--tATL AË` ú                   AøjÂ¤ÍEÿEAË`!ê   CnYC A¨¼ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00237    R.N-rrppp--tATL AË`!ê                   AñÉcÂ£¶EffAË`"¼   CÓ3Cw  AaGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00237    R.N-rrppp--tMCO AË`"¼                   AíÂ¢ E[3AË`#¬   CùC A(öGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00237    R.N-rrppp--tMCO AË`#¬                   AëDDÂ¢  Có×
AË`$~   Cù        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00237    XWN-rrppD--tMCO AË`$~                   B  Â©V0CffAË`ì   C3        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00337    XWN-rrppD--tATL AË`ì                   BÂ½!HF
ÍAË` (   CjYC A5GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00238    R.N-rrppp--tDFW AË` (                   Bð¤ÂºýqF
ÍAË`!   CjYC  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00238    R.N-rrppp--tDFW AË`!                   BµÂ¸ÙF
ÍAË`!ê   Cj&fC A$JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00238    R.N-rrppp--tLIT AË`!ê                   B¸Â·-:F
ÍAË`"Ú   CjYC AVGGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00238    R.N-rrppp--tDFW AË`"Ú                   BjÂµ~KF
ÍAË`#¬   Ci¦fCs  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00238    R.N-rrppp--tMEM AË`#¬                   BµÂ³ºF
ÍAË`$   Ci&fC A½PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00238    R.N-rrppp--tBNA AË`$                   BffÂ±æfF
ÍAË`%n   Ch&fCr  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00238    R.N-rrppp--tSDF AË`%n                   BIcÂ¯ð¤F
ÍAË`&^   Cg¦fC  BüGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00238    R.N-rrppp--tSTL AË`&^                   BÂ¨ùE·ÕAË`%È   CÓ3C}  AVGGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00240    R.N-rrppp--tATL AË`%È                   Aîm:Â£ffF)AË`"   C_¦fC  ?ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00338    R.N-rrppp--tATL AË`"                   Aîm:Â£ffF)AË`"Ú   C_¦fC  ?ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00338    R.N-rrppp--tATL AË`"Ú                   Aîm:Â£ffF)AË`#Ê   C_¦fC  ?ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00338    R.N-rrppp--tRDU AË`#Ê                   Aîm:Â£ffF)AË`$   C_¦fC  ?ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00338    R.N-rrppp--tGSO AË`$                   Aîm:Â£ffF)AË`%   C_¦fC  ?ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00338    R.N-rrppp--tGSO AË`%                   Aîm:Â£ffF)AË`&^   C_¦fC  ?ÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00338    R.N-rrppp--tGSO AË`&^                   BÂ¦jF)AË` 
   C]ÙC  AÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00242    R.N-rrppp--tGSO AË` 
                   BÂ¦ ÚF
ÍAË` Ü   CfYC  AÑä±GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00242    R.N-rrppp--tCLE AË` Ü                   B ÔÂ¤Ø¿FRAË`!Ì   Ci¦fC  Aî³ÅGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00242    R.N-rrppp--tCLE AË`!Ì                   B$
=Â¤ùEà AË`"   CS3C AµGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00242    R.N-rrppp--tDTW AË`"                   B%®Â¤µEYAË`#   C¹        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00242    XWN-rrppD--tCLE AË`#                   B'4èÂ¤®DffAË`$`   C9        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00242    XWN-rrppD--tCLE AË`$`                   B
jÂ¨ ÚEw¦fAË`    C,ÍC  AFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00543    R.N-rrppp--tATL AË`                    BòÂ¨<E AË`!T   Cy        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00543    XWN-rrppD--tATL AË`!T                   B\Â© mD'£×AË`"D   Cù        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00543    XWN-rrppD--tATL AË`"D                   B 
=Â±Ô{F
ÍAË` 
   CdYC}  AÞ=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00599    R.N-rrppp--tSTL AË` 
                   B Â¬EqAË`!Ì   C,ÍCw  Aâ[GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00599    R.N-rrppp--tCVG AË`!Ì                   Bí:ÂªDD> AË`#   CÓ3        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00599    XWN-rrppD--tCVG AË`#                   B\Â©{D/BAË` F   CS3        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00243    XWN-rrppD--tATL AË` F                   B  Â¨´D§£×AË`Î   C9        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00244    XWN-rrppD--tATL AË`Î                   BÂ¡y,F£3AË`Î   CcYC  B7$iGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00587    R.N-rrppp--tGSO AË`Î                   B]ÞÂ¢¨F£3AË` ¾   CcÙC Bd GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00587    R.N-rrppp--tGSO AË` ¾                   B
=Â¤jF£3AË`!   CcÙC  B²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00587    R.N-rrppp--tRDU AË`!                   B/ÉÂ¦ ÚEäAË`"   Cs¦fC  Bd GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00587    R.N-rrppp--tATL AË`"                   BÉcÂ§xRE¤ÈRAË`#R   C3C AÑä±GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00587    R.N-rrppp--tATL AË`#R                   BÔÂ¨DDEBO\AË`$B   CyC AµGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00587    R.N-rrppp--tATL AË`$B                   BjÂ©]C(öAË`%   Cù        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00587    XWN-rrppD--tATL AË`%                   B{Â©Þ¸E(öAË` Ü   CÓ3C AFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00607    R.N-rrppp--tCVG AË` Ü                   BòÂ©{F	(öAË`!®   CgYC  B²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00607    R.N-rrppp--tCVG AË`!®                   B§AÂ¨cF)AË`"   C^&fC  B$JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00607    R.N-rrppp--tATL AË`"                   B\Â¨Ð7F)AË`#p   C]ÙC BíÌGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00607    R.N-rrppp--tATL AË`#p                   B§Â©tF)AË`$`   C]ÙC  BFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00607    R.N-rrppp--tATL AË`$`                   B
=Â©^¸F)AË`%2   C]¦fC  AûGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00607    R.N-rrppp--tATL AË`%2                   BòÂ©hF)AË`&"   C^¦fC AûGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00607    R.N-rrppp--tATL AË`&"                   BËÂ®  F
ÍAË` 
   Ch¦fC  A¹33GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00544    R.N-rrppp--tSDF AË` 
                   BÂ®µF
ÍAË` ú   ChYC A¬ÚtGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00544    R.N-rrppp--tSDF AË` ú                   B  Â®0¤F	¢áAË`!Ì   ChYC AÚÜGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00544    R.N-rrppp--tSDF AË`!Ì                   B  Â®FeÃAË`"¼   ChÙC AæxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00544    R.N-rrppp--tSTL AË`"¼                   BP7Â¬
E¦° AË`#   CS3C  AÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00544    R.N-rrppp--tSDF AË`#                   B
=Â«nEQÍAË`$~   C3C AMÇGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00544    R.N-rrppp--tSDF AË`$~                   BòÂª®E+s3AË`%P   Cù        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00544    XWN-rrppD--tSDF AË`%P                   BQìÂªµDs×
AË`&@   Cù        GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00544    XWN-rrppD--tCVG AË`&@                   BÂÁ/ÉE±*=AË`#è   ClÍC A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00415    R.N-rrppp--tDFW AË`#è                   BÎÂ·ÔE·ÕAË`&   C¬ÍCx  A=PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00340    R.N-rrppp--tDFW AË`&                   Bð¤Â  F£3AË`    CaYC BrêGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00416    R.N-rrppp--tRDU AË`                    B»¼ÂùF£3AË`!T   Ca¦fC  B0ø	GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00416    R.N-rrppp--tRDU AË`!T                   BâýÂÔ{F£3AË`"D   Cb&fC  B,ÚtGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00416    R.N-rrppp--tIAD AË`"D                   B*«Â¡jF£3AË`#   CbÙC  B&®GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00416    R.N-rrppp--tGSO AË`#                   B¸RÂ©ã×E·ÕAË`"¼   C{ÙC Ad GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00789    R.N-rrppp--tSDF AË`"¼                   BkÂ©"ýF
ÍAË`#¬   CeÙC  AÁn]GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00789    R.N-rrppp--tSDF AË`#¬                   B
=Â¨ ÚF)AË`$~   C\ÙC  AæxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00789    R.N-rrppp--tSDF AË`$~                   BµÂ¨µF)AË`%n   C]&fCu  A¹33GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00789    R.N-rrppp--tGSO AË`%n                   BµÂ§[OF)AË`&@   C]YC  A½PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00789    R.N-rrppp--tGSO AË`&@                   BS Â«:E> AË`"b   C3C AvîïGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00250    R.N-rrppp--tCVG AË`"b                   BÅùÂ¬®E AË`#4   CÓ3C AµGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00250    R.N-rrppp--tIND AË`#4                   B ÔÂ¬½qD AË`$$   C¹C­ Ad GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00250    R.N-rrppp--tIND AË`$$                   B ~KÂ¯FÔF)AË` (   C]&fC B3ÔGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00251    R.N-rrppp--tSDF AË` (                   B ~KÂ¯FÔF)AË`!   C]&fC B3ÔGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00251    R.N-rrppp--tATL AË`!                   B ~KÂ¯FÔFYAË`!ê   CjÙC B3ÔGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00251    R.N-rrppp--tBNA AË`!ê                   B ~KÂ¯FÔEº°¤AË`"Ú   C~YC B3ÔGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00251    R.N-rrppp--tATL AË`"Ú                   B ~KÂ¯FÔEdAË`#¬   CÓ3C B3ÔGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00251    R.N-rrppp--tATL AË`#¬                   BÇ®Â¬DF&° AË`    CX¦fC  AÉ©GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00252    R.N-rrppp--tATL AË`                    BµÂ®DF&° AË`!T   CX¦fC  Aâ[GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00252    R.N-rrppp--tSDF AË`!T                   B§Â¯=qF&° AË`"D   CXÙC AûGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00252    R.N-rrppp--tSDF AË`"D                   BÕUÂ°+F&° AË`#   CXÙC B+GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00252    R.N-rrppp--tSTL AË`#                   BLÍÂ² ÚF&° AË`$   CXÙC BFGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00252    R.N-rrppp--tSTL AË`$                   B  Â³?&F&° AË`$Ø   CX&fC  B+GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00252    R.N-rrppp--tSTL AË`$Ø                   BÌÍÂ´jF&° AË`%È   CV&fC  Bd GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00252    R.N-rrppp--tSTL AË`%È                   B Ç®ÂµF&° AË`&   CUÙC  B$JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00252    R.N-rrppp--tSTL AË`&                   B\)ÂùE§£×AË`%ª   C,ÍCd  AÞ=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00253    R.N-rrppp--tGSO AË`%ª                   B  Â¨/ÉE±*=AË` ¾   C3C  A^=qGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00254    R.N-rrppp--tATL AË` ¾                   B	Ü)Â¦P7F
ÍAË`!®   ChYC  A¤JGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00254    R.N-rrppp--tATL AË`!®                   B\Â¤hF
ÍAË`"   Ch¦fC  A½PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00254    R.N-rrppp--tGSO AË`"                   B,`Â¢G®F
ÍAË`#R   ChÙC AÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00254    R.N-rrppp--tGSO AË`#R                   B/ÉÂ F
ÍAË`$B   ChYC  Aâ[GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00254    R.N-rrppp--tCLT AË`$B                   BÂµE·ÕAË`%   C~YC  A¨¼ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00254    R.N-rrppp--tGSO AË`%                   Bí:Â  EF¸AË`&   CìÍC|  AfxGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00254    R.N-rrppp--tRDU AË`&                   BòÂ°½qF&° AË`ì   CXÙC AÍÇGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00670    R.N-rrppp--tBNA AË`ì                   BwwÂ²ÆF&° AË` ¾   CXÙC  AÍÇGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00670    R.N-rrppp--tBNA AË` ¾                   BüÂ´{¼F&° AË`!®   CXÙC A½PÈGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00670    R.N-rrppp--tBNA AË`!®                   B?&Â¶V0F&° AË`"   CY&fC  A°ø	GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00670    R.N-rrppp--tBNA AË`"                   BÔÂ¸)ÐF&° AË`#p   CYÙC AÅòGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00670    R.N-rrppp--tMEM AË`#p                   B\Âº  F&° AË`$B   C[&fC  A°ø	GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00670    R.N-rrppp--tDFW AË`$B                   B	KÂ¼F&° AË`%2   CZYC  A°ø	GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00670    R.N-rrppp--tDFW AË`%2                   B§Â¾  F&° AË`&   CZ¦fC  A¨¼ßGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00670    R.N-rrppp--tDFW AË`&                   BhÂ¨,`E´ùAË` F   CS3C A µGÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00255    R.N-rrppp--tATL AË` F                   B\ÂµEøRAË`"   C9C  Ad GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00560    R.N-rrppp--tRIC AË`"                   BÂ²YCäAË`"ø   CS3CX  A²¢GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00560    R.N-rrppp--tRIC AË`"ø                   BµÂªRÆE{uÃAË`$   CÓ3C AÐ7GÃOGÃO¿  GÃOGÃOÁ×
Á×
    FSL00608    R.N-rrpppp-rSDF AË`$                   BÆÂ¬jEö²AË`$ö   Cn&fC AÞ=qC\©¸GÃO¿  GÃOGÃOÁ×
Á×
    FSL00608    T.N-rrppppprSDF AË`$ö                   BôÂ¬tF£3AË`%È   CcYC  B£×CQÚGÃO¿  GÃOGÃOÁ×
Á×
    FSL00608    T.N-rrppppprSDF AË`%È                   