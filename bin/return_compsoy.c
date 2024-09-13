#include <stdio.h>

int search(int start, int stop, int (*predicate)(int)) {
  int index = start;

  printf("Starting: Searching until: %d\n", stop);

Loop:
  // Exit the loop if we have reached the end.
  if (index >= stop)
    goto Done;

  // Check if we have found what we are looking for. Early return.
  if (predicate(index)) {
    printf("EARLY RETURN!\n");
    return index;
  }

  // "Body" of the loop
  printf("Looping: %d\n", index);
  index++;

  // Continue looping
  goto Loop;

Done:
  printf("Done!\n");
  return -1;
}
