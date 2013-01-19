#!/bin/csh -f
#------------------------------------------------------------------------
# nmapxbms.csh
#
# This script generates 24x24 bitmaps of the GEMPAK symbols that are
# used by NMAP.
#
# This script runs the GEMPAK FAX driver to generate the symbol bitmaps
# one at a time.
#
##
# Log:
# G. Krueger/EAI	12/97
# G. Krueger/EAI	 3/98	Added ICING, HEAVY RAIN, HEAVY SNOW, and
#				SNOW with THUNDERSTORM.
# G. Krueger/EAI	 1/99	Add X and LowX specials
# T. Piper/GSC		12/99	Added wxsym05 (haze)
#------------------------------------------------------------------------

#
# Generate special symbol bitmaps:
#
set names=( high low trpstmnh hurrnh trpstmsh hurrsh xcross lowx)
set name=1
foreach spsym ( 12 13 25 26 27 28 36 38 )

echo "Generating $names[$name].xbm..."

    atest << Espsym >& /dev/null
ginitp
1
gsdeva
fax
1
$names[$name].xbm
0
24
24
gsspcl
.8
0
gspcl
D
1
$spsym
12
12
0
0
gendp
1
ex
Espsym
@ name+=1

end

#
# Generate weather symbol bitmaps:
#
foreach wxsym ( 05 10 45 51 56 61 63 65 66 71 73 75 79 80 85 95 105 201 )

set wsname="wxsym${wxsym}"
if ( $wxsym == 65 ) set wsname="hvyrain"
if ( $wxsym == 75 ) set wsname="hvysnow"
if ( $wxsym == 105 ) set wsname="modtstmsnw"

echo "Generating ${wsname}.xbm..."

    atest << Ewxsym >& /dev/null
ginitp
1
gsdeva
fax
1
${wsname}.xbm
0
24
24
gswthr
.8
0
gwthr
D
1
$wxsym
12
12
0
0
gendp
1
ex
Ewxsym

end

#
# Generate icing symbol bitmaps:
#
foreach icing ( 00 01 02 03 04 05 06 07 08 09 10)

set iname="icing${icing}"
if ( $icing == 09 ) set iname="lgtssicing"
if ( $icing == 10 ) set iname="hvyssicing"

echo "Generating ${iname}.xbm..."

    atest << Eicing >& /dev/null
ginitp
1
gsdeva
fax
1
${iname}.xbm
0
24
24
gsicng
.8
0
gicng
D
1
$icing
12
12
0
0
gendp
1
ex
Eicing

end

#
# Generate turbulence symbol bitmaps:
#
foreach turb ( 00 01 02 03 04 05 06 07 08 )

echo "Generating turb${turb}.xbm..."

    atest << Eturb >& /dev/null
ginitp
1
gsdeva
fax
1
turb${turb}.xbm
0
81
24
gsturb
.8
0
gturb
D
1
$turb
41
14
0
0
gendp
1
ex
Eturb

end

echo "Generating turb46.xbm..."

    atest << Eturb46 >& /dev/null
ginitp
1
gsdeva
fax
1
turb46.xbm
0
88
24
gsturb
.8
0
gturb
D
2
4
6
31
14
57
14
0
0
0
0
gendp
1

Eturb46

echo "Generating turb67.xbm..."

    atest << Eturb67 >& /dev/null
ginitp
1
gsdeva
fax
1
turb67.xbm
0
88
24
gsturb
.8
0
gturb
D
2
6
7
31
18
57
14
0
0
0
0
gendp
1

Eturb67

#
# Generate past weather symbol bitmaps:
#
foreach pw ( 09 )

set pwname="pw${pw}"
if ( $pw == 09 ) set pwname="pwtstorm"

echo "Generating ${pwname}.xbm..."

    atest << Epw >& /dev/null
ginitp
1
gsdeva
fax
1
${pwname}.xbm
0
24
24
gspwth
.9
0
gpwth
D
1
$pw
12
12
0
0
gendp
1
ex
Epw

end

echo "Done generating symbols."
