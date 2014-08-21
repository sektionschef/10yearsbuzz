//Libraries
import deadpixel.keystone.*; //keystone library


//variables
String svg_path = "../canvas.svg"; //path to svg of canvas
int element_count = 3;// number of elements in svg, mind that the loop starts at 0
int width = 1200; //width of canvas - 120*5
int height = 800; //height of canvas - 80*5

String table_path = "../letitallbe.bak.csv"; //path to table with colors
int row_count = 0; //starting line of color csv table
//int[] element_names = new int[17]; // create array with element ids of svg
int[] hue = new int[element_count]; //array for colour levels
int whiteout = 100; //0-255 more brightness for the whole image

//temp
String[] elementos = {"e1","e2","e3"};


//Objects
Keystone ks; //keystone
CornerPinSurface surface; //keystone
Table scheme; //table, load csv

PGraphics offscreen;

//shapes
PShape canvas;
//PShape[] element = new PShape[scheme.getColumnCount()]; //declare them all at once - column count not possible because table not loaded at this stage
PShape[] element = new PShape[element_count]; //declare them all at once - Achtung 


//
///////////////////////////////// SETUP ////////////////////////////////
//
void setup() {
 
  size(displayWidth, displayHeight,P3D); //P3D important for keystone, since it relies on texture mapping to deform; fill screen
  ks = new Keystone(this);
  surface = ks.createCornerPinSurface(width, height, 20); //height, width, distance grid
 
  // We need an offscreen buffer to draw the surface we
    // want projected
    // note that we're matching the resolution of the
    // CornerPinSurface.
    // (The offscreen buffer can be P2D or P3D)
  offscreen = createGraphics(displayWidth, displayHeight, P3D);


  //get svg elements
  canvas = loadShape( svg_path ); //load the svg
  //rect_a = canvas.getChild("e1"); //archive for getting child
  for (int i = 0; i < element_count; i++) { //get all the children at once
     element[i] = canvas.getChild(elementos[i]); // Initialize each object with the ID of the svg; convert it to string so it is accepted
     //element[i].scale(0.5);// scale, which percentage
     //println(elementos[i]+ "element");
  }
   
  
  scheme = loadTable( table_path, "header");
  println(scheme.getRowCount() + " total rows in table"); //debug, Anzahl Rows
}

//
///////////////////////////////// DRAW ////////////////////////////////
//
void draw() {
  
    background(0); //background black, so there is nothing in the projection
 
 

  TableRow axel_row = scheme.getRow(row_count%scheme.getRowCount()); //initialize a single row manually chosen, use the modulo to restrict the row_count not exceeding the row count
  //println(axel_row); //debug
  for (int i = 5; i < element_count; i++) { //for each element  
    hue[i-2] = unhex(axel_row.getString(i)); //get value for each element and write it in an array; getString for unhexing; mode is ARGB! so put "ff in front for full colour (in format "ff"+"2d495e") 
   println("hue: " + hue[i]); //debug
  }
  
  
  offscreen.beginDraw();
  
  for (int i = 0; i < element_count; i++) {           
      element[i].disableStyle();
      offscreen.fill(hue[i]);
      offscreen.noStroke(); 
      offscreen.shape( element[i], 0 ,125); //552, 122 oder 0px deviance, no idea why - probably because of rescaling from 1000 to 500; for offscreen (keystoning) it takes 0 instead of -552px for y)
  }  
  
  
  // add a white rectengular for softening the colours in total, transparency value = whiteout
  offscreen.fill(255,255,255,whiteout);
  //println(whiteout);
  offscreen.noStroke();
  offscreen.rect(0,0,width,height);
  
  
  // Convert the mouse coordinate into surface coordinates
  // this will allow you to use mouse events inside the 
  // surface from your screen. 
  PVector surfaceMouse = surface.getTransformedMouse();
  
  offscreen.endDraw();
 
  // render the scene, transformed using the corner pin surface
  surface.render(offscreen);     
 
}


// MOUSE INSTEAD OF ARDUINO
void mousePressed(){ // pressing the mouse
  //row_count+=1; 
  //println(row_count); 
}

void keyPressed() { //function for keystone
  switch(key) {
  case 'c':
    // enter/leave calibration mode, where surfaces can be warped 
    // and moved
    ks.toggleCalibration();
    break;

  case 'l':
    // loads the saved layout
    ks.load();
    break;

  case 's':
    // saves the layout
    ks.save();
    break;

  case 'w':
    whiteout+=10;
    break;
  case 'b':
    whiteout-=10;
    break;
  }   
}

