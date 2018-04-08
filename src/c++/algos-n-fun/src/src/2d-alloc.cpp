/* Copyright (C) 2018 by samuelrivas@gmail.com

   Allocate a 2d array with a single call to malloc, still allowing table[i][j]
   indexing
*/
#include <cstdlib>
#include <cstdio>

using std::malloc;

int** _2dAlloc(int rows, int columns) {
  int** buff = reinterpret_cast<int**>(malloc(rows * sizeof(int*) +
                                              rows * columns * sizeof(int)));

  int *data = reinterpret_cast<int*>(buff + rows);

  for (int i = 0; i < rows; i++) {
    buff[i] = data + i * columns;
  }
  return buff;
}

int main(void) {
  int** table = _2dAlloc(2, 3);

  table[0][0] = 1;
  table[0][1] = 2;
  table[0][2] = 3;
  table[1][0] = 4;
  table[1][1] = 5;
  table[1][2] = 6;

  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 3; j++) {
      printf("%2d", table[i][j]);
    }
    printf("\n");
  }

  free(table);
  return 0;
}
