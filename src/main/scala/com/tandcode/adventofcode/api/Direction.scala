package com.tandcode.adventofcode.api

enum Direction:
  case Up, Left, Down, Right

  def turnRight: Direction = this match
    case Up => Right
    case Left => Up
    case Down => Left
    case Right => Down