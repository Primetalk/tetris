# Tetris

A tutorial tetris implementation in Scala.js.
The goal of this tutorial is to demonstrate a good way 
of using Scala.

## Step 1. Bootstrap Scala.js project

See http://www.scala-js.org/doc/tutorial/basic/ 
for the introduction of Scala.js.

## Step 2. Direct rules modelling

We take rules of the game and translate them literally 
to Scala version.

Notes and nuances:
 - we have two representations of Tetriminos - the list of relative points
   and the list of rows.
 - rotation can also represent an angle. We have 4 rotations and 4 angles. 
   we may consider an angle to be a rotation from the original position.
 - velocity/speed is represented by time that it takes to pass a single row.

## Step 3. Side story about vectors and rotations

Introduce `Rotations` trait.

## Step 4. Algorithms

Data structures help or not help writing algorithms.

 - conversion of points to rows. One data structure that is convenient to rotations
   to another data structure that is convenient to moves and the rest of game logic
 - collision detection
 - merge of board and shape, "bake" the board
  
Notes: tailrec

Also No mutability!

## Step 5. GameMechanics

Handling events, moves, new board state.
No mutability.

## Step 6. Controller

The first `var` - game state.

## Step 7. Rendering

Mutable external context

