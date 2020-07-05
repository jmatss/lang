#include <stdlib.h>
#include "array_list.h"

#define DEFAULT_CAPACITY ((uint64_t)16)
#define POINTER_SIZE sizeof(void*)

typedef struct ArrayList
{
    void **items;
    uint64_t position;
    uint64_t capacity;
} ArrayList;

ArrayList *array_list_create()
{
    return array_list_create_with_capacity(DEFAULT_CAPACITY);
}

ArrayList *array_list_create_with_capacity(uint64_t capacity)
{
    ArrayList *array_list = malloc(sizeof(ArrayList));
    if (array_list == NULL)
    {
        return NULL;
    }

    void *items = malloc(POINTER_SIZE * capacity);
    if (items == NULL)
    {
        free(array_list);
        return NULL;
    }

    array_list->items = &items;
    array_list->position = 0;
    array_list->capacity = capacity;

    return array_list;
}

void array_list_destroy(ArrayList *array_list)
{
    if (array_list != NULL)
    {
        if (array_list->items != NULL)
        {
            free(array_list->items);
        }

        free(array_list);
    }
}

static int64_t increase_capacity(ArrayList *array_list)
{
    uint64_t new_capacity = array_list->capacity * 2;

    void *re_items = realloc(array_list->items, new_capacity);
    if (re_items == NULL)
    {
        return -1;
    }

    array_list->items = re_items;
    array_list->capacity = new_capacity;

    return 0;
}

void array_list_add(ArrayList *array_list, void *item)
{
    if (array_list == NULL || item == NULL)
    {
        return NULL;
    }

    if (array_list->position >= array_list->capacity)
    {
        // result < 0 => failure to increase size.
        int64_t result = increase_capacity(array_list);
        if (result < 0)
        {
            return NULL;
        }
    }

    array_list->items[array_list->position++] = item;
}

void* array_list_get(ArrayList *array_list, uint64_t index)
{
    if (array_list == NULL)
    {
        return NULL;
    }

    if (index >= array_list->position)
    {
        return NULL;
    }

    return array_list->items[index];
}

void *array_list_replace(ArrayList *array_list, uint64_t index, void *item)
{
    if (array_list == NULL || item == NULL)
    {
        return NULL;
    }

    if (index >= array_list->position)
    {
        return NULL;
    }

    void *old_item = array_list->items[index];
    array_list->items[index] = item;

    return old_item;
}

static void swap(ArrayList *array_list, uint64_t first, uint64_t second)
{
    if (first >= array_list->position || second >= array_list->position)
    {
        return;
    }

    void *tmp = array_list->items[first];
    array_list->items[first] = array_list->items[second];

}

// Removes the item at index "index" from the array_list and moves the last
// element to the specified "index" that is now unnused.
void *array_list_remove(ArrayList *array_list, uint64_t index)
{
    void *item = array_list_get(array_list, index);
    if (item == NULL)
    {
        return NULL;
    }

    return item;
}

void array_list_remove_sorted(ArrayList *array_list, uint64_t index)
{
    if (array_list == NULL)
    {
        return NULL;
    }

    if (index >= array_list->position)
    {
        return NULL;
    }

    return array_list[index];
}

uint64_t array_list_count(ArrayList *array_list);