#ifndef ARRAY_LIST
#define ARRAY_LIST
#endif /* ARRAY_LIST */

#include <stdint.h>

typedef struct ArrayList ArrayList;

ArrayList *array_list_create();
ArrayList *array_list_create_with_capacity(uint64_t capacity);
void array_list_destroy(ArrayList *array_list);
void array_list_add(ArrayList *array_list, void *item);
void *array_list_get(ArrayList *array_list, uint64_t index);
void *array_list_replace(ArrayList *array_list, uint64_t index, void *item);
void *array_list_remove(ArrayList *array_list, uint64_t index);
void array_list_remove_sorted(ArrayList *array_list, uint64_t index);
uint64_t array_list_count(ArrayList *array_list);
