#define GC_DEBUG

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <gc.h>
#include "hashmap.h"

static
void gc_finalizer (void *obj, void *client_data) 
{
    static int count = 1;
    printf ("free: %d\n", count++);
}

int64_t print_int (int64_t n)
{
    printf ("%" PRId64 "\n", n);
    return 0;
}

void *gc_malloc (long size) 
{
    return GC_MALLOC (size);
}

void gc_init ()
{
    GC_INIT ();
}

void gc_register_finalizer (void *obj)
{
    GC_REGISTER_FINALIZER (obj, &gc_finalizer, NULL, NULL, NULL);
}

void gc_collect ()
{
    GC_gcollect ();
}

void *hashmap_init ()
{
    void *p = gc_malloc (sizeof (struct hashmap_s));
    hashmap_create (2, (struct hashmap_s *) p);

    return (void *) p;
}

void hashmap_insert (void *ptr, char *key, void *value)
{
    hashmap_put ((struct hashmap_s *) ptr, key, strlen (key), value);
} 

void *hashmap_lookup (void *ptr, char *key)
{
    return hashmap_get ((struct hashmap_s *) ptr, key, strlen (key));
} 
