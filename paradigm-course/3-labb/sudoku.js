// A suduko puzzle is a 9x9 array with numbers from 0-9.
// Some examples:

var diagonal = [
  [1, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 2, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 3, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 4, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 5, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 6, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 7, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 8, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 9],
];

var easy = [
  [0, 2, 0, 4, 5, 6, 7, 8, 9],
  [4, 5, 7, 0, 8, 0, 2, 3, 6],
  [6, 8, 9, 2, 3, 7, 0, 4, 0],
  [0, 0, 5, 3, 6, 2, 9, 7, 4],
  [2, 7, 4, 0, 9, 0, 6, 5, 3],
  [3, 9, 6, 5, 7, 4, 8, 0, 0],
  [0, 4, 0, 6, 1, 8, 3, 9, 7],
  [7, 6, 1, 0, 4, 0, 5, 2, 8],
  [9, 3, 8, 7, 2, 5, 0, 6, 0],
];

var medium = [
  [0, 4, 0, 0, 0, 2, 0, 1, 9],
  [0, 0, 0, 3, 5, 1, 0, 8, 6],
  [3, 1, 0, 0, 9, 4, 7, 0, 0],
  [0, 9, 4, 0, 0, 0, 0, 0, 7],
  [0, 0, 0, 0, 0, 0, 0, 0, 0],
  [2, 0, 0, 0, 0, 0, 8, 9, 0],
  [0, 0, 9, 5, 2, 0, 0, 4, 1],
  [4, 2, 0, 1, 6, 9, 0, 0, 0],
  [1, 6, 0, 8, 0, 0, 0, 7, 0],
];

var hard = [
  [0, 2, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 6, 0, 0, 0, 0, 3],
  [0, 7, 4, 0, 8, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 3, 0, 0, 2],
  [0, 8, 0, 0, 4, 0, 0, 1, 0],
  [6, 0, 0, 5, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 1, 0, 7, 8, 0],
  [5, 0, 0, 0, 0, 9, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 4, 0],
];

var almostsolved = [
  [0, 2, 3, 4, 5, 6, 7, 8, 9],
  [4, 5, 7, 1, 8, 9, 2, 3, 6],
  [6, 8, 9, 2, 3, 7, 1, 4, 5],
  [8, 1, 5, 3, 6, 2, 9, 7, 4],
  [2, 7, 4, 8, 9, 1, 6, 5, 3],
  [3, 9, 6, 5, 7, 4, 8, 1, 2],
  [5, 4, 2, 6, 1, 8, 3, 9, 7],
  [7, 6, 1, 9, 4, 3, 5, 2, 8],
  [9, 3, 8, 7, 2, 5, 4, 6, 1],
];

var solved = [
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [4, 5, 7, 1, 8, 9, 2, 3, 6],
  [6, 8, 9, 2, 3, 7, 1, 4, 5],
  [8, 1, 5, 3, 6, 2, 9, 7, 4],
  [2, 7, 4, 8, 9, 1, 6, 5, 3],
  [3, 9, 6, 5, 7, 4, 8, 1, 2],
  [5, 4, 2, 6, 1, 8, 3, 9, 7],
  [7, 6, 1, 9, 4, 3, 5, 2, 8],
  [9, 3, 8, 7, 2, 5, 4, 6, 1],
];

// Return an empty sudoku
function emptySudoku() {
  return [
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
  ];
}

// Convert an array of arrays to a string keeping track of the inner
// arrays.
function arrayToString(arr) {
  let out = "";
  for (let i in arr) {
    out += "[" + arr[i] + "]" + "<br>";
  }
  return out;
}

// Compute all 27 blocks of a sudoku. Returns all 9 rows, 9 cols and 9
// 3x3 blocks
function blocks(sud) {
  const len = sud.length;
  // rows
  let out = [];
  for (let j = 0; j < len; j++) {
    out.push(sud[j]);
  }

  // cols
  for (let j = 0; j < len; j++) {
    let column = [];
    for (let i = 0; i < len; i++) {
      column.push(sud[i][j]);
    }
    out.push(column);
  }

  // 3x3 blocks
  for (let j = 0; j < 3; j++) {
    let bunchoblocks = [[], [], []];

    // adding elements to each block
    for (let i = 0; i < 3; i++) {
      for (let c = 0; c < 3; c++) {
        for (let n = 0; n < 3; n++) {
          bunchoblocks[i].push(sud[c + 3 * j][n + 3 * i]);
        }
      }
    }
    for (let x in bunchoblocks) {
      out.push(bunchoblocks[x]);
    }
  }
  return out;
}

// Check that a block is valid (i.e. that it contains no duplicate
// number 1-9, but multiple 0 is fine).
function isValidBlock(b) {
  const sortedb = [...b];
  sortedb.sort();
  for (let i = 0; i < b.length - 1; i++) {
    if (sortedb[i] != 0 && sortedb[i] == sortedb[i + 1]) {
      return false;
    }
  }
  return true;
}

// Check that a sudoku is valid by checking that all of its blocks are
// valid.
function isValid(sud) {
  let blocksofsud = blocks(sud);
  console.log(blocksofsud);
  for (let i in blocksofsud) {
    // check each block if they are valid
    let single = blocksofsud[i];
    if (isValidBlock(single) == false) {
      return false; // if a single block isn't valid, return false
    }
  }
  return true; // only returns true if all blocks are valid.
}

// Check if a sudoku is solved by checking that there are no zeroes
// and that it is valid.
function isSolved(sud) {
  let blocksofsud = blocks(sud);

  for (let i in blocksofsud) {
    // check that each block contains no zeroes
    if (blocksofsud[i].includes(0)) {
      return false;
    }
  }
  if (isValid(sud) == false) {
    // check that sud is valid
    return false;
  }
  return true; // return true if above two checks pass.
}

// Draw the start sudoku
function init(sud) {}

// What to do when the user clicks on a tile (not called "click"
// because that caused some problems with some browsers)
function klick() {}

// Fetch the current sudoku
function getSudoku() {}

//console.log(arrayToString(solved));

//console.log(arrayToString(blocks(solved)));
