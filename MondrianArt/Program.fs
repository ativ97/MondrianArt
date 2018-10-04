//
// F# program to generate random Mondrian Art.
//
// <<Ativ Aggarwal>>
// U. of Illinois, Chicago
// CS 341, Spring 2018
// Project #05
//

#light

//
// randomInt LB UB
//
// generates random integer in range LB..UB, inclusive.
//
// NOTE: if you want repeatable random numbers for testing,
// uncomment "let seed = 0".  If you want random images 
// every time, uncomment the other "let seed = ..." line.
//
let seed = System.DateTime.Now.Millisecond
//let seed = 0
let ranInt = new System.Random(seed)
let randomInt LB UB =
  if LB <= UB then
    ranInt.Next(LB, UB+1)
  else
    LB
  //ranInt.Next(LB, UB+1)


//
// randomRect
//
// An example of generating a random-colored rectangle
// in HTML SVG format.
//
let randomRect x1 y1 x2 y2 =
   let r= randomInt 0 100
   if r<9 then
    let red= 255
    let green= 0
    let blue= 0
    let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string (x2-x1+1)) + 
       " height=" + (string (y2-y1+1)) + 
       " stroke=\"None\"" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
    html
   elif r<17 then
    let red= 0
    let green= 191
    let blue= 255
    let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string (x2-x1+1)) + 
       " height=" + (string (y2-y1+1)) + 
       " stroke=\"None\"" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
    html
   elif r<25 then
    let red= 255
    let green= 255
    let blue= 0
    let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string (x2-x1+1)) + 
       " height=" + (string (y2-y1+1)) + 
       " stroke=\"None\"" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
    html
   else 
    let red= 255
    let green= 255
    let blue= 255
    let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string (x2-x1+1)) + 
       " height=" + (string (y2-y1+1)) + 
       " stroke=\"None\"" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
    html

let linefunc x1 x2 y1 y2= 
    let html= 
        "<line x1 =" + (string x1) +
        " y1 =" + (string y1) +
        " x2 =" + (string x2) + 
        " y2 =" + (string y1) + 
        " stroke =\"black\" ;stroke-width=2 /> \n" +
        "<line x1 =" + (string x1) +
        " y1 =" + (string y1) +
        " x2 =" + (string x1) + 
        " y2 =" + (string y2) + 
        " stroke=\"black\" ;stroke-width=2 /> \n" +
        "<line x1 =" + (string x2) +
        " y1 =" + (string y1) +
        " x2 =" + (string x2) + 
        " y2 =" + (string y2) + 
        " stroke=\"black\" ;stroke-width=2 /> \n" +
        "<line x1 =" + (string x1) +
        " y1 =" + (string y2) +
        " x2 =" + (string x2) + 
        " y2 =" + (string y2) + 
        " stroke=\"black\" ;stroke-width=2 /> \n"
    html
//
// _mondrian x1 y1 x2 y2 canvasWidth canvasHeight
//
// Recursive helper function that randomly generates an image
// for the area denoted by the rectange (x1,y1) and (x2,y2),
// where (x1,y1) is the upper-left corner and (x2,y2) is the 
// lower-right corner.  The image is in HTML SVG format.
//
let rec _mondrian x1 y1 x2 y2 canvasWidth canvasHeight = 
  let width= (x2-x1+1)
  let height= (y2-y1+1)
  let split1= randomInt (x1+ (33*width/100)) (x1+ (67*width/100))
  let split2= randomInt (y1+ (33*height/100)) (y1+ (67*height/100))

  if width > (canvasWidth/2) && height > (canvasHeight/2) then
    _mondrian x1 y1 split1 split2 canvasWidth canvasHeight +
    _mondrian split1 y1 x2 split2 canvasWidth canvasHeight +
    _mondrian x1 split2 split1 y2 canvasWidth canvasHeight +
    _mondrian split1 split2 x2 y2 canvasWidth canvasHeight
  elif width > (canvasWidth/2) then
    _mondrian x1 y1 split1 y2 canvasWidth canvasHeight +
    _mondrian split1 y1 x2 y2 canvasWidth canvasHeight
  elif(height>canvasHeight/2) then
    _mondrian x1 y1 x2 split2 canvasWidth canvasHeight +
     _mondrian x1 split2 x2 y2 canvasWidth canvasHeight
   else
    //let w= (float width)*1.5
    //let h= (float height)*1.5
    let ranW= randomInt 120 (width*3/2)
    let ranH= randomInt 120 (height*3/2)
    if (ranW<width && ranH<height) then
      _mondrian x1 y1 split1 split2 canvasWidth canvasHeight +
        _mondrian split1 y1 x2 split2 canvasWidth canvasHeight +
        _mondrian x1 split2 split1 y2 canvasWidth canvasHeight +
        _mondrian split1 split2 x2 y2 canvasWidth canvasHeight
    elif (ranW<width) then
      _mondrian x1 y1 split1 y2 canvasWidth canvasHeight +
      _mondrian split1 y1 x2 y2 canvasWidth canvasHeight
    elif (ranH<height) then
      _mondrian x1 y1 x2 split2 canvasWidth canvasHeight +
      _mondrian x1 split2 x2 y2 canvasWidth canvasHeight
    else
       let html= randomRect x1 y1 x2 y2 + linefunc x1 x2 y1 y2
       html


//
// mondrian canvasWidth canvasHeight
//
// Randomly generates an image in the spirit of Piet Mondrian.
// Returns an HTML document containing an SVG image of the given
// canvas width and height.  
//
// SVG: https://www.w3schools.com/html/html5_svg.asp
//
let mondrian canvasWidth canvasHeight = 
  let prefix = "<html>\n<head></head>\n<body>\n" +
               "<svg width=\"" + (string canvasWidth) + 
               "\" height=\"" + (string canvasHeight) + "\">\n"
  //
  let image = _mondrian 0 0 (canvasWidth-1) (canvasHeight-1) canvasWidth canvasHeight
  //
  let suffix = "</svg>\n</body>\n</html>\n"
  let html = prefix + image + suffix
  html


//
// main:
//
[<EntryPoint>]
let main argv =
  printfn "** Starting **"
  //
  let width = 1024
  let height = 768
  let filename = "..\\..\\..\\mondrian.html"  // same folder as F# code:
  //
  printfn "** Generating image... "
  let html = mondrian width height
  System.IO.File.WriteAllText(filename, html) 
  //
  printfn "** Done **"
  0
