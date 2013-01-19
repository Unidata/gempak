#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

#define MAXNUMCHK 400		/* Maximum number of memory chunks */
#define CHKSIZ (1UL<<20)	/* 1MB minimun memory chunk size */

typedef struct node_t_tag {
    size_t size;
    char *bgnptr;
    char *endptr;
    struct node_t_tag *prev;
    struct node_t_tag *next;
} node_t;
#define NODESIZE sizeof(node_t)

typedef struct {
    char *chkptr;
    node_t *free_nodes;
    node_t *used_nodes;
} chunk_t;

static chunk_t memchk[MAXNUMCHK];
static size_t numchk = 0;

/*
 * private function
 */
static void defragment ( int curchk );

/************************************************************************
 * shpmmem.c                                                            *
 *                                                                      *
 * This module contains the memory management functions.		*
 *                                                                      *
 * CONTENTS:                                                            *
 *      shp_mnew()       allocate new memory.                           *
 *      shp_mfree()      free allocated memory.                         *
 *      shp_mfreeall()   free all memory allocated by this module.    	*
 ***********************************************************************/

/*=====================================================================*/

void *shp_mnew ( size_t size )
/************************************************************************
 * shp_mnew                                                             *
 *                                                                      *
 * This function allocates size bytes and returns a pointer to the	*
 * allocated memory. The memory is cleared.				*
 *                                                                      *
 * void *shp_mnew ( size )                                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      size		size_t          Requested memory size       	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*shp_mnew	void		Pointer to the allocated memory	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   Initial coding                  	*
 * S. Jacobs/NCEP	 4/10	Increased MAXNUMCHK from 100 to 200	*
 * X. Guo/CWS            9/11   Increased MAXNUMCHK from 200 to 400     *
 ***********************************************************************/
{
    node_t *node, *free_node, *used_node;
    size_t ichk, cur_size, new_size, min_size;
    char *chkptr, *rtnptr;
    int curchk;
/*---------------------------------------------------------------------*/

    /*
     * Adjust the size to make it multiple of sizeof(size_t).
     */
    size += ( sizeof(size_t) - size % sizeof(size_t) );

    /*
     * Search the free list for a node that its size is most close to 
     * the requested size. The returned memory is within a chunk.
     */
    min_size = CHKSIZ;
    free_node = NULL;
    for ( ichk = 0; ichk < numchk; ichk++ ) {
        for ( node = memchk[ichk].free_nodes; node; node = node->next ) {
            cur_size = node->size;
            if ( cur_size < min_size && cur_size >= size ) {
                min_size = cur_size;
                free_node = node;
		curchk = ichk;
            }
        }
    }

    /*
     * No memory available, a new chunk has to be allocated.
     */
    if ( free_node == NULL ) {
        if ( ( free_node = malloc ( NODESIZE ) ) == NULL ) {
            fprintf ( stderr, " Malloc failed in shp_mnew().\n" );
            exit ( -1 );
        }
        memchk[numchk].free_nodes = free_node;

        new_size = size > CHKSIZ ? size : CHKSIZ;
        if ( ( chkptr = malloc ( new_size ) ) == NULL ) {
            fprintf ( stderr, "Malloc failed in shp_mnew().\n" );
            exit ( -1 );
        }
        memchk[numchk].chkptr = chkptr;
	free_node->size = new_size;
        free_node->bgnptr = chkptr;
        free_node->endptr = free_node->bgnptr + new_size;
        free_node->prev = NULL;
        free_node->next = NULL;

	curchk = numchk;
        numchk++;
    }

    /*
     * Got the requested memory, add it on the used list.
     */
    if ( ( used_node = malloc ( NODESIZE ) ) == NULL ) {
        fprintf ( stderr, " Malloc failed in shp_mnew().\n" );
        exit ( -1 );
    }
    used_node->size = size;
    used_node->bgnptr = free_node->bgnptr;
    used_node->endptr = used_node->bgnptr + size;
    used_node->prev = NULL;
    used_node->next = NULL;
    used_node->next = memchk[curchk].used_nodes;
    if ( memchk[curchk].used_nodes != NULL ) {
        memchk[curchk].used_nodes->prev = used_node;
    }
    memchk[curchk].used_nodes = used_node;
    rtnptr = (void *)(used_node->bgnptr);
    memset ( rtnptr, 0, size );

    /*
     * Update the free node.
     */
    free_node->size -= size;
    if ( free_node->size == (size_t)0 ) {
	/*
	 * No available memory associated with this node, free it.
	 */
        if ( free_node->prev != NULL ) {
	    free_node->prev->next = free_node->next;
	} else {
	    memchk[curchk].free_nodes = free_node->next;
	    if ( memchk[curchk].free_nodes != NULL ) {
	        memchk[curchk].free_nodes->prev = NULL;
	    }
	}
	if ( free_node->next != NULL ) {
	    free_node->next->prev = free_node->prev;
	}

	memset ( free_node, 0, NODESIZE );
	free ( free_node );
    } else {
        free_node->bgnptr += size;
    }

    return rtnptr;
}

void shp_mfree ( void *ptr )
/************************************************************************
 * shp_mfree                                                            *
 *                                                                      *
 * This function frees the memory space pointed to by ptr, which must	*
 * have been returned by a previous call to shp_mnew().			*
 *                                                                      *
 * shp_mfree ( ptr )                                            	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ptr		void            Pointe to the memroy        	*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    node_t *node, *cur_node;
    int curchk, fnd;
    size_t ii;
/*---------------------------------------------------------------------*/
    /*
     * Search for the pointer in the used list.
     */
    fnd = G_FALSE;
    for ( ii = 0; ii < numchk && !fnd; ii++ ) {
        for ( node = memchk[ii].used_nodes; node; node = node->next ) {
	    if ( (char *)ptr == node->bgnptr ) {
		fnd = G_TRUE;
	        curchk = ii;
		cur_node = node;
		break;
	    }
        }
    }
    if ( !fnd ) {
        fprintf ( stderr, "Pointer not found in the used list.\n" );
	exit ( -1 );
    }

    /*
     * Move the node from used list to free list.
     */
    memset ( cur_node->bgnptr, 0, cur_node->size );
    if ( cur_node->prev != NULL ) {
        cur_node->prev->next = cur_node->next;
    } else {
        memchk[curchk].used_nodes = cur_node->next;
	if ( memchk[curchk].used_nodes != NULL ) {
            memchk[curchk].used_nodes->prev = NULL;
	}
    }
    if ( cur_node->next != NULL ) {
        cur_node->next->prev = cur_node->prev;
    }
    cur_node->next = memchk[curchk].free_nodes;
    if ( memchk[curchk].free_nodes != NULL ) {
        memchk[curchk].free_nodes->prev = cur_node;
    }
    memchk[curchk].free_nodes = cur_node;
    memchk[curchk].free_nodes->prev = NULL;

    defragment ( curchk );
}

void shp_mfreeall ( void ) 
/************************************************************************
 * shp_mfreeall                                                         *
 *                                                                      *
 * This function frees up all of the memory allocated by shp_mnew().	*
 *                                                                      *
 * shp_mfreeall( void )                                       		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    node_t *node;
    size_t ii;
/*---------------------------------------------------------------------*/

    for ( ii = 0; ii < numchk; ii++ ) {
        while ( memchk[ii].free_nodes ) {
	    node = memchk[ii].free_nodes;
	    memchk[ii].free_nodes = memchk[ii].free_nodes->next;
            free ( node ); 
        }
        while ( memchk[ii].used_nodes ) {
	    node = memchk[ii].used_nodes;
	    memchk[ii].used_nodes = memchk[ii].used_nodes->next;
            free ( node ); 
        }
        free ( memchk[ii].chkptr );
    }
}

static void defragment ( int curchk )
/************************************************************************
 * defragment                                                           *
 *                                                                      *
 * This function defragments freed memory.				*
 *                                                                      *
 * defragment ( curchk )                                        	*
 *                                                                      *
 * Input parameters:                                                    *
 *      curchk		int		Chunk index			*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04           Initial coding                  *
 ***********************************************************************/
{
    node_t *node1, *node2, *tmp_node;
    int fwd;

/*---------------------------------------------------------------------*/

    node1 = memchk[curchk].free_nodes;
    while ( node1 != NULL ) {
	node2 = memchk[curchk].free_nodes;
	while ( node2 != NULL ) {
            fwd = G_FALSE;
	    if ( node1 != node2 && 
	         ( node1->endptr == node2->bgnptr ||
	           node2->endptr == node1->bgnptr ) ) {
		node1->size += node2->size;
                if ( node1->endptr == node2->bgnptr ) {
	            node1->endptr = node2->endptr;
	        } else if ( node2->endptr == node1->bgnptr ) {
	            node1->bgnptr = node2->bgnptr;
	        }
		if ( node2->prev != NULL ) {
	            node2->prev->next = node2->next;
		} else {
		    memchk[curchk].free_nodes = node2->next;
		    if ( memchk[curchk].free_nodes != NULL ) {
		        memchk[curchk].free_nodes->prev = NULL;
		    }
		}
		if ( node2->next != NULL ) {
	            node2->next->prev = node2->prev;
		}
		tmp_node = node2;
		node2 = node2->next;
		fwd = G_TRUE;
		memset ( tmp_node, 0, NODESIZE );
	        free ( tmp_node );
	    }

	    if ( ! fwd ) {
	        node2 = node2->next;
	    }
        }
	
	node1 = node1->next;
    }
}
