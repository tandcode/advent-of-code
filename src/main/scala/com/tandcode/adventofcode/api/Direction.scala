package com.tandcode.adventofcode.api

enum Direction:
  case Up, Left, Down, Right

  def turnRight: Direction = this match
    case Up => Right
    case Left => Up
    case Down => Left
    case Right => Down

  def turnLeft: Direction = this match
    case Up => Left
    case Left => Down
    case Down => Right
    case Right => Up