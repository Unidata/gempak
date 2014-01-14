#include "xmlcmn.h"

#define XSLT	"xslt"


int main( void ) 
/************************************************************************
 * TESTXML                                                              *
 *                                                                      *
 * This program tests the CGEMLIB XML functions.                        *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	10/04	initial coding                          *
 * E. Safford/SAIC	06/04	remove directory prompt for file loads  *
 * E. Safford/SAIC	11/05	add xml_count, xml_value, xml_getSubDoc *
 * E. Safford/SAIC	04/05	add xml_readTable			*
 * S. Jacobs/NCEP	 9/13	Increased the file sizes from LLSCRN	*
 *				to FILE_FULLSZ (80 to 4196)		*
 ***********************************************************************/
 {
    unsigned char	*buf, *bigStr, *xmlOut = NULL;

    FILE	*fp;

    int 	ier, nbin, bufSize;
    int		numsub, nbytes, count;
    int		elementNumber, childNumber;
    int		nfields, ii;
    long	flen;

    char	fileName[ FILE_FULLSZ ];
    char	select[ LLSCRN ];
    char	xmlFile[ FILE_FULLSZ ];
    char	xsltFile[ FILE_FULLSZ ], xsltCheckFile[ FILE_FULLSZ ];

    char	elName[ LLSCRN ], readElValue[ LLSCRN], *elValue = NULL;
    char	readChildName[ LLSCRN ], *childName = NULL;
    char	*outputValue = NULL;


    char	*elementName, readElementName[ LLSCRN ];
    char	*childValue, readChildValue[ LLSCRN ];
    char	parentPath[ LLSCRN ];
    char	**output;
    char	field[ 5 ][ LLSCRN ];

    Boolean	cont, xmlDocSet, xsltDocSet;

    xmlChar	xpathExpr[ FILE_FULLSZ ], *expr;
    xmlXPathObjectPtr	xpathObj;
    xmlDocPtr 		doc; 

 /*--------------------------------------------------------------------*/

    cont       = TRUE;
    xmlDocSet  = FALSE;
    xsltDocSet = FALSE;

    while( cont ) {
        printf( "\n\n" );
	printf( " 1 = XML_TRANSFORM\n" );
	printf( " 2 = XML_COUNT\n" );
	printf( " 3 = XML_GETSUBDOC\n" );
	printf( " 4 = XML_VALUE\n" );
	printf( " 5 = XML_READTABLE\n" );
	printf( "\n" );
	printf( "10 = xml_makeXpathExpr   11 = xml_executeXpathExpr\n" );
	printf( "20 = Load XML file       21 = Load XSTL file\n" );  
	
	printf( "\n" );
	printf( "Select a subroutine number or type EXIT: ");
	scanf( " %s", select );
        printf( "\n");

        switch( select[0] ) {
            case 'e':
	    case 'E':
                cont = False;
		break;
            default:
		numsub = atoi( select );
		break;
        }
 /*---------------------------------------------------------------------*/
        if( numsub == 1 && cont ) {

	    if( !xmlDocSet ) {
		printf("  Must load an xml document before using xml_transform\n");
            }
	    else if( !xsltDocSet ) {
		printf("  Must load an xslt document before using xml_transform\n");
            }
	    else {

                cfl_inqr( xmlFile, NULL, &flen, fileName, &ier );
                fp = fopen( xmlFile, "r");

                if( fp == NULL ) {
	            xmlDocSet = FALSE;
		    printf("  Unable to open xmlFile %s\n", xmlFile );
                }
	        else {
                    buf = (unsigned char *) malloc( flen * 
		    				sizeof( unsigned char ) );

                    cfl_read( fp, (int)flen, buf, &nbin, &ier );
                    cfl_clos( fp, &ier );

                    nbytes = xml_transform( (char *)buf, (int)flen, xsltFile, &bigStr, &ier );

		    printf( "\n" );
		    printf( "XML_TRANSORM:\n" );
                    printf( "   IRET = %d\n", ier );
		    printf( "   returned %d bytes\n", nbytes );

		    if( ier >= 0 ) {
  		        printf( "\n" );
		        printf( "%s\n", bigStr ); 
                    }
                    printf( "\n\n" );

		    if ( bigStr ) G_FREE( bigStr, unsigned char );
		    if ( buf )    G_FREE( buf, unsigned char );
                }
            }
        }
 /*---------------------------------------------------------------------*/
        else if( numsub == 2 && cont ) {

	    if( !xmlDocSet ) {
		printf("  Must load an xml document before using xml_count\n");
            }
	    else {

                cfl_inqr( xmlFile, NULL, &flen, fileName, &ier );
                fp = fopen( xmlFile, "r");

                if( fp == NULL ) {
	            xmlDocSet = FALSE;
		    printf("  Unable to open xmlFile %s\n", xmlFile );
                }
	        else {
                    buf = (unsigned char *) malloc( (flen + 1) * 
		    				sizeof( unsigned char ) ); 
                    cfl_read( fp, (int)flen, buf, &nbin, &ier );
		    buf[ nbin ] = '\0';
                    cfl_clos( fp, &ier );

                    printf( "Enter the element name:  " );
	            scanf( " %s", elName );

                    printf( "\nEnter the element number (or 0 for n/a):  " );
	            scanf( "%i", &elementNumber );

                    printf( "\nEnter the child element name (or 0 for NULL):  " );
	            scanf( "%s", readChildName );

		    if( strcmp( readChildName, "0" ) != 0 ) {
			G_MALLOC( childName, char, strlen( readChildName ) + 1,
						"testxml" );
			strcpy( childName, readChildName );
		    }
		    else {
			childName = NULL;
			childNumber = 0;
		    }

                    printf( "\nEnter the target element value (or enter 0 for NULL):  " );
	            scanf( "%s", readElValue );

		    if( strcmp( readElValue, "0" ) != 0 ) {
			G_MALLOC( elValue, char, strlen( readElValue ) + 1,
						"testxml" );
			strcpy( elValue, readElValue );
		    }


	            count = xml_count( (char *)buf, elName, elementNumber,
		    			childName, elValue, &ier );


	            printf( "\n" );
	            printf( "XML_COUNT:\n" );
	            printf( "   File      = %s\n", xmlFile );
	            printf( "   Element   = %s\n", elName );
		    if( elValue ) {
		        printf( "    of Value = %s\n", elValue );
		    }
		    if( childName ) {
		        printf( "   Child     = %s\n", childName );
		    }
		    printf( "\n" );
	            printf( "   Num Found = %d\n", count );
                    printf( "   IRET      = %d\n", ier );

		    printf( "\n" );

		    if( elValue ) {
			G_FREE( elValue, char );
			elValue = NULL;
		    }

		    strcpy( readElValue, "" );
		}
            }
	}
 /*---------------------------------------------------------------------*/
        else if( numsub == 3 && cont ) {

	    childName = NULL;
            elementName = NULL;
 
	    elementNumber = 0;
	    childNumber = 0;
            readChildName[0]   = '\0';
            readElementName[0] = '\0';

	    if( !xmlDocSet ) {
		printf("  Must load an xml document before using xml_count\n");
            }
	    else {

                cfl_inqr( xmlFile, NULL, &flen, fileName, &ier );
                fp = fopen( xmlFile, "r");

                if( fp == NULL ) {
	            xmlDocSet = FALSE;
		    printf("  Unable to open xmlFile %s\n", xmlFile );
                }
	        else {
                    buf = (unsigned char *) malloc( (flen + 1) * 
		    				sizeof( unsigned char ) );

                    cfl_read( fp, (int)flen, buf, &nbin, &ier );
		    buf[ nbin ] = '\0';
                    cfl_clos( fp, &ier );

                    printf( "Enter the element name (or 0 for NULL):  " );
	            scanf( " %s", readElementName );
	            if( strcmp( readElementName, "0" ) != 0 ) {
	                G_MALLOC( elementName, char, 
			          strlen( readElementName ) + 1, "testxml" );
                        strcpy( elementName, readElementName );
                    } 

                    printf( "\nEnter the element number (or 0 for n/a):  " );
	            scanf( "%i", &elementNumber );

                    printf( "\nEnter the child element name (or 0 for NULL):  " );
	            scanf( "%s", readChildName );

		    if( strcmp( readChildName, "0" ) != 0 ) {
			G_MALLOC( childName, char, strlen( readChildName ) + 1,
						"testxml" );
			strcpy( childName, readChildName );
		    }

		    
	            xml_getSubDoc( (char *)buf, elementName, elementNumber,
		    			childName, &xmlOut, &ier );

	            printf( "\n" );
	            printf( "XML_GETSUBDOC:\n" );
	            printf( "   File        = %s\n", xmlFile );
	            printf( "   Element     = %s\n", elementName );
                    printf( "   Element Num = %d\n", elementNumber );
		    printf( "   Child       = %s\n", childName );
		    printf( "\n" );
                    printf( "   IRET        = %d\n", ier );
		    printf( "   New XML doc = %s\n", xmlOut );

		    printf( "\n" );

		    G_FREE( xmlOut, unsigned char );
		    G_FREE( elementName, char );
		    G_FREE( childName, char );
		    G_FREE( buf, unsigned char );
		}
            }
	}
 /*---------------------------------------------------------------------*/
        else if( numsub == 4 && cont ) { 	/* XML_VALUE */

	    outputValue = NULL;
	    childName = NULL;
            elementName = NULL;
 
	    elementNumber = 0;
	    childNumber = 0;
            readChildName[0]   = '\0';
            readElementName[0] = '\0';


	    if( !xmlDocSet ) {
		printf("  Must load an xml document before using xml_value\n");
            }
	    else {

                cfl_inqr( xmlFile, NULL, &flen, fileName, &ier );
                fp = fopen( xmlFile, "r");

                if( fp == NULL ) {
	            xmlDocSet = FALSE;
		    printf("  Unable to open xmlFile %s\n", xmlFile );
                }
	        else {
                    buf = (unsigned char *) malloc( ( flen + 1 ) * 
		    				sizeof( unsigned char ) );

                    cfl_read( fp, (int)flen, buf, &nbin, &ier );
		    buf[ nbin ] = '\0';
                    cfl_clos( fp, &ier );

                    printf( "Enter the element name (or 0 for NULL):  " );
	            scanf( " %s", readElementName );
	            if( strcmp( readElementName, "0" ) != 0 ) {
	                G_MALLOC( elementName, char, 
			          strlen( readElementName ) + 1, "testxml" );
                        strcpy( elementName, readElementName );
                    } 

                    printf( "\nEnter the element number (or 0 for n/a):  " );
	            scanf( "%i", &elementNumber );

                    printf( "\nEnter the child element name (or 0 for NULL):  " );
	            scanf( "%s", readChildName );

		    if( strcmp( readChildName, "0" ) != 0 ) {
			G_MALLOC( childName, char, strlen( readChildName ) + 1,
						"testxml" );
			strcpy( childName, readChildName );

                        printf( "\nEnter the child number (or 0 for n/a):  " );
	                scanf( "%i", &childNumber );
		    }
		    else {
			childNumber = 0;
		    }


	            xml_value( (char *)buf, elementName, elementNumber, childName,
	    				childNumber, &outputValue, &ier );

	            printf( "\n" );
	            printf( "XML_VALUE:\n" );
	            printf( "   File             = %s\n", xmlFile );
	            printf( "   Element          = %s\n", elementName );
	            printf( "   Element Number   = %d\n", elementNumber );
		    printf( "   Child Element    = %s\n", childName );
	            printf( "   Child Number     = %d\n", childNumber );
		    printf( "\n" );    
	            printf( "   Value Found      = %s\n", outputValue );
                    printf( "   IRET             = %d\n", ier );

		    printf( "\n" );


		    G_FREE( elementName, char );
		    G_FREE( outputValue, char );
		    G_FREE( childName, char );
		}
            }
	}
 /*---------------------------------------------------------------------*/
        else if( numsub == 5 && cont ) {	/* XML_READTABLE */

	    parentPath[ 0 ] = '\0';


	    if( !xmlDocSet ) {
		printf("  Must load an xml table (file) before using xml_readTable\n");
		printf("  Use option 20.\n");
            }
	    else {

	        printf("  Enter the full parent element path.  This must \n" );
		printf("    start with a slash (/) and list each element down\n");
		printf("    to the parent of the desired element(s) (fields).\n");
		printf("    Example path:  /MyRoot/SomeEl/ParentEl \n" );
		printf("    \n" );
		printf("    Enter your parent path:  " );
	        scanf( " %s", parentPath );
		printf("    \n" );

		printf("  Enter the number of table fields (child elements \n");
		printf("    of the specified parent.  This test program is \n");
		printf("    limited to a range of 1 - 3):  "); 
		scanf( "%i", &nfields );
		printf("    \n" );

		while( nfields <= 0 || nfields > 3 ) {
		    printf("  Fields must be > 0 and <= 3  \n" );
		    scanf( "%i", &nfields );
                }

		switch( nfields ) {
		    case 1:
		        printf("  Enter the table field  \n" );
		        scanf(" %s", field[0] );

		        printf("  xmlFile    = %s\n", xmlFile );
		        printf("  parentPath = %s\n", parentPath );
		        printf("  nfields    = %d\n", nfields );
		        printf("  field[0]   = %s\n", field[0] );

		        xml_readTable( xmlFile, ".", parentPath, nfields, NULL,
		    		&output, &ier, field[0], NULL );
			break;

		    case 2:
		        printf("  Enter the first table field:  " );
		        scanf(" %s", field[0] );
			printf(" \n" );

			printf("  Enter the second table field:  ");
			scanf(" %s", field[ 1 ]);
			printf(" \n" );

		        printf("  xmlFile    = %s\n", xmlFile );
		        printf("  parentPath = %s\n", parentPath );
		        printf("  nfields    = %d\n", nfields );
		        printf("  field[0]   = %s\n", field[0] );
		        printf("  field[1]   = %s\n", field[1] );
			printf(" \n" );

		        xml_readTable( xmlFile, NULL, parentPath, nfields, NULL,
		    		&output, &ier, field[0], field[1], NULL );
			break;

		    case 3:
		        printf("  Enter the first table field:  " );
		        scanf(" %s", field[0] );
			printf(" \n" );

			printf("  Enter the second table field:  ");
			scanf(" %s", field[ 1 ]);
			printf(" \n" );

			printf("  Enter the second table field:  ");
			scanf(" %s", field[ 2 ]);
			printf(" \n" );

		        printf("  xmlFile    = %s\n", xmlFile );
		        printf("  parentPath = %s\n", parentPath );
		        printf("  nfields    = %d\n", nfields );
		        printf("  field[0]   = %s\n", field[0] );
		        printf("  field[1]   = %s\n", field[1] );
		        printf("  field[2]   = %s\n", field[2] );
			printf(" \n" );

		        xml_readTable( xmlFile, ".", parentPath, nfields, NULL,
		 	    &output, &ier, field[0], field[1], field[2], NULL );
			break;
		}

	        printf( "XML_READTABLE:\n" );

	        for( ii=0; ii<nfields; ii++ ) {
	            printf( " output[%d] = %s\n", ii, output[ii] );
	        }
	   
                printf( "   IRET    = %d\n", ier );
	        printf( "\n" );

	        for( ii=0; ii<nfields; ii++ ) {
		    G_FREE( output[ii], char );
	        }

	        G_FREE( output, char* );
            }
        }
 /*---------------------------------------------------------------------*/
        else if( numsub == 10 && cont ) {

            expr           = NULL;
            elementName    = NULL;
            elementNumber  = 0;
            childName      = NULL;
	    childNumber    = 0;
            childValue     = NULL;

            readElementName[0] = '\0';
	    readChildName[0]   = '\0';
	    readChildValue[0]  = '\0';

            printf( "Enter the element name (or 0 for NULL):  " );
	    scanf( " %s", readElementName );
	    if( strcmp( readElementName, "0" ) != 0 ) {
	        G_MALLOC( elementName, char, 
			strlen( readElementName ) + 1, "testxml" );
                strcpy( elementName, readElementName );
            }

            printf( "\nEnter the element number (or 0 for n/a):  " );
	    scanf( "%i", &elementNumber );

            printf( "\nEnter the child element name (or 0 for NULL):  " );
	    scanf( "%s", readChildName );
	    if( strcmp( readChildName, "0" ) != 0 ) {
	        G_MALLOC( childName, char, strlen( readChildName ) + 1,
						"testxml" );
		strcpy( childName, readChildName );

                printf( "\nEnter the child number (or 0 for n/a):  " );
	        scanf( "%i", &childNumber );

		printf( "\nEnter the child value (or 0 for NULL): " );
		scanf( "%s", readChildValue );
		if( strcmp( readChildValue, "0" ) != 0 ) {
	            G_MALLOC( childValue, char, 
		    		strlen( readChildValue ) + 1, "testxml" );
		    strcpy( childValue, readChildValue );
		}
	    }


	    xml_makeXpathExpr( elementName, elementNumber, 
	    		       childName, childNumber, childValue, &expr, &ier );

	    printf( "\n" );
	    printf( "xml_makeXpathExpr:\n" );
	    printf( "   Element Name     = %s\n", elementName );
	    printf( "   Element Number   = %d\n", elementNumber );
	    printf( "   Child Element    = %s\n", childName );
	    printf( "   Child Number     = %d\n", childNumber );
	    printf( "   Child Value      = %s\n", childValue );
	    printf( "\n" );    
	    printf( "   XPath Expression = %s\n", expr );
            printf( "   IRET             = %d\n", ier );

	    printf( "\n" );

            G_FREE( elementName, char );
	    G_FREE( childName, char );
	    G_FREE( childValue, char );
	    G_FREE( expr, xmlChar );
	}
 /*---------------------------------------------------------------------*/
        else if( numsub == 11 && cont ) {

	    if( !xmlDocSet ) {
		printf("  Must load an xml document before executing xpath expression \n");
            }
	    else {

                cfl_inqr( xmlFile, NULL, &flen, fileName, &ier );
                fp = fopen( xmlFile, "r");

                if( fp == NULL ) {
	            xmlDocSet = FALSE;
		    printf("  Unable to open xmlFile %s\n", xmlFile );
                }
	        else {
                    buf = (unsigned char *) malloc( (flen + 1) * 
		    				sizeof( unsigned char ) );

                    cfl_read( fp, (int)flen, buf, &nbin, &ier );
		    buf[ nbin ] = '\0';
                    cfl_clos( fp, &ier );

		    bufSize = strlen( (char *)buf );


		    printf("  Enter the XPath expression:  " );
		    scanf( " %s", xpathExpr );


		    xml_executeXpathExpr( (char *)buf, bufSize, xpathExpr,
		    				&doc, &xpathObj, &ier );

	            printf( "\n" );
	            printf( "xml_executeXpathExpr:\n" );
		    printf( "   xml file         = %s\n", xmlFile );
	            printf( "   XPath expression = %s\n", xpathExpr );
	            printf( "\n" );    
                    printf( "   IRET             = %d\n", ier );
		    if( xpathObj ) {
	                printf( "   returned XPath Object is non-NULL \n" );    
		    }
		    else {
	                printf( "   returned XPath Object is NULL \n" );    
		    }

	            printf( "\n" );


                    xmlXPathFreeObject( xpathObj );
                    xmlFreeDoc( doc ); 

		    G_FREE( buf, unsigned char );
                }
	    }
	}
 /*---------------------------------------------------------------------*/
        else if( numsub == 20 && cont ) {
            printf( "Enter the xml file name:  " );
	    scanf( " %s", xmlFile );

            cfl_inqr( xmlFile, NULL, &flen, xmlFile, &ier );
 	    if( ier != 0 ) {
	        printf("  Unable to find the specified xml file %s\n", xmlFile );
            }
	    else {
	        xmlDocSet = TRUE;    
            }
        }
 /*---------------------------------------------------------------------*/
        else if( numsub == 21 && cont ) {
            printf( "Enter the xslt file name:  " );
	    scanf( " %s", xsltFile );

            cfl_tinq( xsltFile, XSLT, &flen, xsltCheckFile, &ier );
 	    if( ier != 0 ) {
	        printf("  Unable to find the specified xslt file %s\n", xsltFile );
            }
	    else {
	        xsltDocSet = TRUE;    
            }

        }
 /*---------------------------------------------------------------------*/

    }

    return(0);
}
