#include <stdint.h>
void* increment_pointer(void *ptr, int64_t number_of_bytes)
{
        return (void*)((char*)ptr + number_of_bytes);
}
