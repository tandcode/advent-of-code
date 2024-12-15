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
    
  def turnAround: Direction = this match
    case Up => Down
    case Left => Right
    case Down => Up
    case Right => Left


object Direction:
  
  def apply(c: Char): Direction = c match
    case '^' => Up
    case '>' => Right
    case 'v' => Down
    case '<' => Left
    
end Direction