# 2048 Haskell
Based on [the 2048 game.](http://2048game.com/)

I made this project to get some much needed practice with Haskell for an exam. The code is not commented (sorry), but the function names themselves should be rather self explanitory. However, in the section below is the basic idea of the implementation. 
## How it's implemented: 
* The game board consists of a 4x4 matrix: <br/> **[0, 0, 0, 0] <br/> [0, 0, 0, 0] <br/> [0, 0, 0, 0] <br/> [0, 0, 0, 0]**
* Upon moving in the left direction, elements are moved or merged to the left in a row like this: 
   <br/>     **[0, 0, 2, 2] => INPUT (left) => [2, 2, 0, 0] => MERGE => [4, 0, 0, 0]**
* An element can move if there's a zero to the left of it (happens recursively). An element is merged if it "bumps into" another element of equal value to the left of it.
* The other directions (up, down, right) also moves and merges to the left, but transposes the matrix before the operation. In that way, only logic for moving to the left needs to be implemented and can be reused with every direction.
* The game board is rendered in the terminal, using clearing and redrawing after each update (input).
