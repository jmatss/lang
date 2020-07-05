#ifndef LIST
#define LIST
#endif /* LIST */

#include <stddef.h>

void list_add(void *item);
void* list_get(size_t index);
void list_remove();