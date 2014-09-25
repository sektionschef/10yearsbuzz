//Libraries
import deadpixel.keystone.*; //keystone library


//variables
int row_start = 480; // start time  - 480
int row_end = 1200; //end time - 1200
int row_count = row_start; //starting line of color csv table - equals start time - 480

String svg_path = "canvas.svg"; //path to svg of canvas
int element_count = 133;// number of elements in svg, mind that the loop starts at 0 - 133 elements in total

//temp
String[] elementos = new String[element_count];

int width = 800; //width of canvas - 120
int height = 500; //height of canvas - 80


String table_path = "lightup.csv"; //path to table with colors per element (columns) for each state (rows)
//int[] element_names = new int[17]; // create array with element ids of svg
int[] hue = new int[element_count]; //array for colour levels
int whiteout = 100; //0-255 more brightness for the whole image

PFont Font_bold;
PFont Font_normal;

String clock = "00:00"; // variable showing the time as strng
int clock_sec =0; // number of seconds of the day (from the csv)
int time; //time from start, for the delay
int wait = 5000; //delay for reach cycle 
// 2500 milliseconds for doing one day in 60 Minutes
// 1250 milliseconds for one day in 30 Minutes
// 1000 one per second
// 500 fTageszeit 
//Objects
Keystone ks; //keystone
CornerPinSurface surface; //keystone
PGraphics offscreen;

CornerPinSurface surface2;
PGraphics offscreen2;

CornerPinSurface surface3;
PGraphics offscreen3;

Table scheme; //table, load csv


//shapes
PShape canvas;
//PShape[] element = new PShape[scheme.getColumnCount()]; //declare them all at once - column count not possible because table not loaded at this stage
PShape[] element = new PShape[element_count]; //declare them all at once - Achtung 
PShape ramen; //get the ramen of the svg


//
///////////////////////////////// SETUP ////////////////////////////////
//
void setup() {
 
  size(displayWidth, displayHeight,P3D); //P3D important for keystone, since it relies on texture mapping to deform; fill screen
  ks = new Keystone(this);
  surface = ks.createCornerPinSurface(width, height, 20); //height, width, distance grid

  surface2 = ks.createCornerPinSurface(800, 70, 20);
  surface2.moveTo(200, 0);

 
  surface3 = ks.createCornerPinSurface(400, 70, 20);
  surface3.moveTo(200, 500);

  // We need an offscreen buffer to draw the surface we
    // want projected
    // note that we're matching the resolution of the
    // CornerPinSurface.
    // (The offscreen buffer can be P2D or P3D)
  offscreen = createGraphics(displayWidth, displayHeight, P3D);
  offscreen2 = createGraphics(800, 70, P3D); //matching the Corner Pin Surface
  offscreen3 = createGraphics(400, 70, P3D);

  time = millis();//store the current time


for (int x = 0; x < element_count; x++) {
  //String[] elementos = {"e1","e2","e3","e4","e5","e6","e7","e8"};
  elementos[x] = "e"+(x+1);
  println(elementos[x]);
}

  //get svg elements
  canvas = loadShape( svg_path ); //load the svg
  //rect_a = canvas.getChild("e1"); //archive for getting child
  for (int i = 0; i < element_count; i++) { //get all the children at once
     element[i] = canvas.getChild(elementos[i]); // Initialize each object with the ID of the svg; convert it to string so it is accepted
     //element[i].scale(1.5);// scale, which percentage
     println(elementos[i]+ " element");
  }

  ramen = canvas.getChild("ramen");
  
  Font_normal = createFont("Liberation Sans Regular", 16); 
  Font_bold = createFont("Liberation Sans Bold", 16); 

  
  scheme = loadTable( table_path, "header");
  println(scheme.getRowCount() + " total rows in table"); //debug, Anzahl Rows
}

//
///////////////////////////////// DRAW ////////////////////////////////
//
void draw() {
  background(0); //background black, so there is nothing in the projection
 
  TableRow axel_row = scheme.getRow(row_count%scheme.getRowCount()); //initialize a single row manually chosen, use the modulo to restrict the row_count not exceeding the row count
// println(axel_row); //debug


  if(millis() - time >= wait){ //delay loop
    //println("tick");

    if(row_count+1 > row_end) { //if end is reach start all over
      row_count=row_start-1;
//      println(row_count);
    }

    row_count+=1; //for the whole cycle - beginning to end
    
    time = millis();//update the stored time
  

    clock_sec = axel_row.getInt(1); //get the seconds from the csv and format them to clock-style
    clock = nf(clock_sec/60,2,0) + ":" + nf(clock_sec%60,2,0); 
    //println("Clock: " + clock);
  }

  // get the values for each row
  for (int i = 0; i < element_count; i++) { //for each element  
    hue[i] = unhex(axel_row.getString(i+2)); //get value from each column and write it in an array; getString for unhexing; mode is ARGB! so put "ff in front for full colour (in format "ff"+"2d495e") 
    //println("zahl: "+ i + "hue: " + hue[i]); //debug
  }
  
  
  offscreen.beginDraw();
  
  // create a background rect so transparency works, otherwise the transparency of 0 is not fully transparent
  
  offscreen.smooth(8); 
  offscreen.fill(0);
  offscreen.noStroke();
  offscreen.rect(0,0,width,height);


  for (int i = 0; i < element_count; i++) {               
      element[i].disableStyle();
      offscreen.fill(hue[i]);
      offscreen.noStroke(); 
      offscreen.shape( element[i], 0 ,0); //552, 122 oder 0px deviance, no idea why - probably because of rescaling from 1000 to 500; for offscreen (keystoning) it takes 0 instead of -552px for y)
  }  


  // add a white rectengular for softening the colours in total, transparency value = whiteout
  //offscreen.fill(255,255,255,whiteout);
  //println(whiteout);
  //offscreen.noStroke();
  //offscreen.rect(0,0,width,height);
  
  //add the frame - border of the canvas
  ramen.disableStyle(); 
  offscreen.fill(255,255,255,whiteout);
  offscreen.noStroke();
  offscreen.shape(ramen, 0, 0);

  // Convert the mouse coordinate into surface coordinates
  // this will allow you to use mouse events inside the 
  // surface from your screen. 
  PVector surfaceMouse = surface.getTransformedMouse();
  
  offscreen.endDraw();

  offscreen2.beginDraw();
  // create the text box with the clock
    offscreen2.smooth(8);
    offscreen2.fill(0);
    offscreen2.noStroke();
    offscreen2.rect(0,0,800,70);
    offscreen2.textFont(Font_normal);
    offscreen2.textSize(16);
    offscreen2.fill(255); 
    offscreen2.text("Woran arbeiteten ambuzzador Gehirne die letzten Jahre exakt um " + clock + " Uhr?", 5, 30); //it is important to overwrite the text in each void draw loop - \n for newline
    offscreen2.fill(unhex("ff1eaa15"));
    offscreen2.text("\nBeratung", 5, 30); //newline
    offscreen2.fill(unhex("ff4abdf6"));
    offscreen2.text("\nJFX", 90, 30);
    offscreen2.fill(unhex("ff0003ff"));
    offscreen2.text("\nRed. Betreuung", 130, 30);
    offscreen2.fill(unhex("fff7060c"));
    offscreen2.text("\nProjektmanagement", 260, 30);
    offscreen2.fill(unhex("fff21aec"));
    offscreen2.text("\nContent Creation", 440, 30);
    offscreen2.fill(unhex("ffffd600"));
    offscreen2.text("\nKonzeption", 590, 30);
  offscreen2.endDraw();



  offscreen3.beginDraw();
  // create the text box with the clock
    offscreen3.smooth(8);
    offscreen3.fill(0);
    offscreen3.noStroke();
    offscreen3.rect(0,0,400,70);
    offscreen3.textFont(Font_normal);
    offscreen3.textSize(12);
    offscreen3.fill(255); 
    offscreen3.text("Stefan Schwaha - @sektionschef", 5, 30); //it is important to overwrite the text in each void draw loop - \n for newline
  offscreen3.endDraw();




  // render the scene, transformed using the corner pin surface
  surface.render(offscreen);     
  surface2.render(offscreen2);     
  surface3.render(offscreen3);
}


// MOUSE INSTEAD OF ARDUINO
/*
void mousePressed(){ // pressing the mouse
  row_count+=1; 
  //println("row count: " + row_count); 
}
*/

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
    if (whiteout < 255) {
      whiteout+=15;
      println("whiteout: " + whiteout);
    }
      break;
  
  case 'b':
    if (whiteout > 0) {
      whiteout-=15;
      println("whiteout: " + whiteout);
    }  
    break;
  }   
}


