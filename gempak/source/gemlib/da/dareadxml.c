#define DACMN_GLOBAL
#include "da.h"

int	gflnum;

/* Local functions */
static void process_elements ( xmlNode *a_node );

void da_readxml ( char *filename, int *iflno, int *iret )
/************************************************************************
 * da_readxml								*
 *									*
 * This function reads the XML configuration file. Each element of the	*
 * file is processed and stored in a structure.				*
 *									*
 * da_readxml ( filename, iret )					*
 *									*
 * Input parameters:							*
 *	filename	char*		XML file name			*
 *	iflno		int*		GEMPAK file number		*
 *									*
 * Output parameters:							*
 *	iret		int*		Return Code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 * M. James/UCAR	 3/16	Made dbserver optional to overwrite 	*
 * 				envvar EDEX_SERVER			*
 ************************************************************************/
{
    xmlDoc	*doc = NULL;
    xmlNode	*root_element = NULL;
/*---------------------------------------------------------------------*/
    *iret = 0;

    /* Set the GEMPAK file number */
    gflnum = *iflno - 1;

    /* Check the version of the library in use. */
    LIBXML_TEST_VERSION

    /* Read the XML file. */
    doc = xmlReadFile (filename, NULL, 0 );
    if ( doc == NULL ) {
	*iret = -6;
	return;
    }

    if (strlen(getenv("EDEX_SERVER")) > 0) {
	common[gflnum].dbserver = (char *)malloc(strlen(getenv("EDEX_SERVER")));
	strcpy ( common[gflnum].dbserver, getenv("EDEX_SERVER") );
    }

    /* Get the root element node and start the processing */
    root_element = xmlDocGetRootElement(doc); 
    process_elements(root_element);

    //printf(" Requesting from %s\n",common[gflnum].dbserver);

    /* Clean up the XML reader when finished */
    xmlFreeDoc ( doc );

    /* Cleanup function for the XML library. */
    xmlCleanupParser ();

    return;
}

/*=====================================================================*/

static void process_elements ( xmlNode *a_node )
/************************************************************************
 * process_elements							*
 * 									*
 * This function will process information about the current node.	*
 * 									*
 * process_elements ( a_node )						*
 *									*
 * Input parameters:							*
 *	a_node		xmlNode*	Next node to process		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ***********************************************************************/
{
    xmlNode *cur_node = NULL;
    xmlAttr *attr = NULL;

/*---------------------------------------------------------------------*/

    /* Process all nodes and their children */
    for (cur_node = a_node; cur_node; cur_node = cur_node->next) {

	/* Process the node, if it is an XML ELEMENT */
	if (cur_node->type == XML_ELEMENT_NODE) {

	    /* Save the file label and version number */
	    if ( strcmp((char *)cur_node->name,"filelabel") == 0 ) {
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"text") == 0 ) {
			common[gflnum].label = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].label, (char *)attr->children->content );
		    }
		    if ( strcmp((char *)attr->name,"version") == 0 ) {
			common[gflnum].version = atoi ((char *)attr->children->content ); 
		    }
		}
	    }

	    /* Save the file type and source */
	    if ( strcmp((char *)cur_node->name,"filetype") == 0 ) {
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"type") == 0 ) {
			FIND_KEY ( common[gflnum].type, ((char *)attr->children->content), ftype, Hash_t )
		    }
		    if ( strcmp((char *)attr->name,"source") == 0 || strcmp((char *)attr->name,"rawtext") == 0) {
			int i;
			FIND_KEY ( i, ((char *)attr->children->content), source, Hash_t )
			common[gflnum].source += i;
		    }
		}
	    }

	    /* Save the AWIPS database server name */

	    if ( strcmp((char *)cur_node->name,"dbserver") == 0 ) {
 		for (attr = cur_node->properties; attr; attr = attr->next) {
 		    if ( strcmp((char *)attr->name,"host") == 0 ) {
 			common[gflnum].dbserver = (char *)malloc(strlen((char *)attr->children->content));
 			strcpy ( common[gflnum].dbserver, (char *)attr->children->content );
 		    }
 		}
 	    }


	    /* Save the AWIPS database table name */
	    if ( strcmp((char *)cur_node->name,"dbtable") == 0 ) {
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"name") == 0 ) {
			common[gflnum].dbtable = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].dbtable, (char *)attr->children->content );
		    }
		}
	    }

	    /* Initialize the rows */
	    if ( strcmp((char *)cur_node->name,"rows") == 0 ) {
		common[gflnum].numrows = 0;
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"pyfile") == 0 ) {
			common[gflnum].pyfile_row = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].pyfile_row, (char *)attr->children->content );
		    }
		    if ( strcmp((char *)attr->name,"pymeth") == 0 ) {
			common[gflnum].pymeth_row = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].pymeth_row, (char *)attr->children->content );
		    }
		    if ( strcmp((char *)attr->name,"dbkey") == 0 ) {
			common[gflnum].dbkey_row = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].dbkey_row, (char *)attr->children->content );
		    }
		}
	    }
	    /* Save the row header information */
	    if ( strcmp((char *)cur_node->name,"row") == 0 ) {
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"name") == 0 ) {
			common[gflnum].rows[common[gflnum].numrows] = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].rows[common[gflnum].numrows], (char *)attr->children->content );
			common[gflnum].numrows++;
		    }
		}
	    }

	    /* Initialize the columns */
	    if ( strcmp((char *)cur_node->name,"columns") == 0 ) {
		common[gflnum].numcols = 0;
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"pyfile") == 0 ) {
			common[gflnum].pyfile_col = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].pyfile_col, (char *)attr->children->content );
		    }
		    if ( strcmp((char *)attr->name,"pymeth") == 0 ) {
			common[gflnum].pymeth_col = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].pymeth_col, (char *)attr->children->content );
		    }
		    if ( strcmp((char *)attr->name,"dbkey") == 0 ) {
			common[gflnum].dbkey_col = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].dbkey_col, (char *)attr->children->content );
		    }
		}
	    }
	    /* Save the column header information */
	    if ( strcmp((char *)cur_node->name,"column") == 0 ) {
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"name") == 0 ) {
			common[gflnum].cols[common[gflnum].numcols] = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].cols[common[gflnum].numcols], (char *)attr->children->content );
			common[gflnum].numcols++;
		    }
		}
	    }

	    /* Initialize the parts */
	    if ( strcmp((char *)cur_node->name,"parts") == 0 ) {
		common[gflnum].numparts = 0;
	    }
	    /* Save the part header information */
	    if ( strcmp((char *)cur_node->name,"part") == 0 ) {
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"name") == 0 ) {
			common[gflnum].parts[common[gflnum].numparts].name = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].parts[common[gflnum].numparts].name, (char *)attr->children->content );
			common[gflnum].numparts++;
		    }
		    if ( strcmp((char *)attr->name,"type") == 0 ) {
			int nprt = common[gflnum].numparts-1;
			FIND_KEY ( common[gflnum].parts[nprt].type, ((char *)attr->children->content), dtype, Hash_t )
		    }
		    if ( strcmp((char *)attr->name,"pyfile") == 0 ) {
			int nprt = common[gflnum].numparts-1;
			common[gflnum].parts[nprt].pyfile = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].parts[nprt].pyfile, (char *)attr->children->content );
		    }
		    if ( strcmp((char *)attr->name,"pymethdata") == 0 ) {
			int nprt = common[gflnum].numparts-1;
			common[gflnum].parts[nprt].pymethdata = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].parts[nprt].pymethdata, (char *)attr->children->content );
		    }
		    if ( strcmp((char *)attr->name,"pymethhdr") == 0 ) {
			int nprt = common[gflnum].numparts-1;
			common[gflnum].parts[nprt].pymethhdr = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].parts[nprt].pymethhdr, (char *)attr->children->content );
		    }
		}
	    }

	    /* Initialize the parameters */
	    if ( strcmp((char *)cur_node->name,"parameters") == 0 ) {
		common[gflnum].parts[common[gflnum].numparts-1].numparms = 0;
	    }
	    /* Save the parameter information for this part */
	    if ( strcmp((char *)cur_node->name,"parameter") == 0 ) {
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"name") == 0 ) {
			int nprt = common[gflnum].numparts-1;
			int nprm = common[gflnum].parts[nprt].numparms;
			common[gflnum].parts[nprt].parms[nprm].name = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].parts[nprt].parms[nprm].name, (char *)attr->children->content );
			common[gflnum].parts[nprt].numparms++;
		    }
		    if ( strcmp((char *)attr->name,"scale") == 0 ) {
			int nprt = common[gflnum].numparts-1;
			int nprm = common[gflnum].parts[nprt].numparms-1;
			common[gflnum].parts[nprt].parms[nprm].scale = atoi ((char *)attr->children->content ); 
		    }
		    if ( strcmp((char *)attr->name,"offset") == 0 ) {
			int nprt = common[gflnum].numparts-1;
			int nprm = common[gflnum].parts[nprt].numparms-1;
			common[gflnum].parts[nprt].parms[nprm].offset = atof ((char *)attr->children->content ); 
		    }
		    if ( strcmp((char *)attr->name,"bits") == 0 ) {
			int nprt = common[gflnum].numparts-1;
			int nprm = common[gflnum].parts[nprt].numparms-1;
			common[gflnum].parts[nprt].parms[nprm].bits = atoi ((char *)attr->children->content ); 
		    }
		    if ( strcmp((char *)attr->name,"key") == 0 ) {
			int nprt = common[gflnum].numparts-1;
			int nprm = common[gflnum].parts[nprt].numparms-1;
			common[gflnum].parts[nprt].parms[nprm].key = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].parts[nprt].parms[nprm].key, (char *)attr->children->content ); 
		    }
		}
	    }

	    /* Initialize the file headers */
	    if ( strcmp((char *)cur_node->name,"headers") == 0 ) {
		common[gflnum].numfhdrs = 0;
	    }
	    /* Save the part header information */
	    if ( strcmp((char *)cur_node->name,"header") == 0 ) {
		for (attr = cur_node->properties; attr; attr = attr->next) {
		    if ( strcmp((char *)attr->name,"name") == 0 ) {
			common[gflnum].fhdrs[common[gflnum].numfhdrs].name = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].fhdrs[common[gflnum].numfhdrs].name, (char *)attr->children->content );
			common[gflnum].numfhdrs++;
		    }
		    if ( strcmp((char *)attr->name,"type") == 0 ) {
			int nfhd = common[gflnum].numfhdrs-1;
			FIND_KEY ( common[gflnum].fhdrs[nfhd].type, ((char *)attr->children->content), dtype, Hash_t )
		    }
		    if ( strcmp((char *)attr->name,"length") == 0 ) {
			int nfhd = common[gflnum].numfhdrs-1;
			common[gflnum].fhdrs[nfhd].length = atoi ((char *)attr->children->content ); 
		    }
		    if ( strcmp((char *)attr->name,"pyfile") == 0 ) {
			int nfhd = common[gflnum].numfhdrs-1;
			common[gflnum].fhdrs[nfhd].pyfile = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].fhdrs[nfhd].pyfile, (char *)attr->children->content );
		    }
		    if ( strcmp((char *)attr->name,"pymeth") == 0 ) {
			int nfhd = common[gflnum].numfhdrs-1;
			common[gflnum].fhdrs[nfhd].pymeth = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].fhdrs[nfhd].pymeth, (char *)attr->children->content );
		    }
		    if ( strcmp((char *)attr->name,"dbkey") == 0 ) {
			int nfhd = common[gflnum].numfhdrs-1;
			common[gflnum].fhdrs[nfhd].dbkey = (char *)malloc(strlen((char *)attr->children->content));
			strcpy ( common[gflnum].fhdrs[nfhd].dbkey, (char *)attr->children->content );
		    }
		}
	    }

	}

	/* Recursively process the children of the current node */
	process_elements(cur_node->children);
    }
}
