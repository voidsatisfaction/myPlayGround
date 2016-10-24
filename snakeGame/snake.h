#include <iostream>
#include <vector>
#include <ncurses.h>
#include <cstdlib>
#include <unistd.h>
using namespace std;
#ifndef SNAKE_H
#define SNAKE_H

struct snakepart{
  int x,y;
  snakepart(int col,int row);
  snakepart();
};

class snakeclass{
  int points,del,maxwidth,maxheight;
  char direction,partchar,oldalchar,foo;
  bool get;
  snakepart food;
  vector<snakepart> snake;

  void putfood();
  bool collision();
  void movesnake();
public:
  snakeclass();
  ~snakeclass();
  void start();
};

#endif