// ===============================================
// LED Matrix Studio MAX72xx code (4x 8x8 displays)
//
// Render a scrolly animation across a 4x display
// these can be found an ebay for a few £/$
//
// LED Matrix Studio, new single colour, 8 high by any width (32 minimum)
//
// Draw your message/pattern on the matrix and export using
// the Export Code option.
//
// See LED_Matrix.leds in the \codetemplates\ folder.
//
// https://sourceforge.net/projects/led-matrix-studio/
// 
// ===============================================

// this should be included with your arduino install
// if it's not installed then add it from the Library Manager:
//     Sketch->Include Library->Library Manger
// or 
// Download LedControlMS library from here:
// http://www.instructables.com/id/LED-Matrix-with-Arduino/step2/Arduino-Library/
#include "LedControlMS.h"

// pin 12 is connected to DataIn 
// pin 11 is connected to CLK 
// pin 10 is connected to LOAD / CS
//
LedControl lc = LedControl(12, 11, 10, 1);

unsigned long delaytime = 100;

{$LMS_MATRIX_DATA$}

int columnCount = {$LMS_COUNT$};

// used as a frame store for each [device][row]
int buffer[4][8];

int powers[8] = {1, 2, 4, 8, 16, 32, 64, 128};

byte idx = 0;
byte dir = 0;

// ===================================================================

void setup() 
{
    //we have already set the number of devices when we created the LedControl
    int devices = lc.getDeviceCount(); // this demo expects 4 :)

    //we have to init all devices in a loop
    for (int address = 0; address < devices; address++) 
    {
      // The MAX72XX is in power-saving mode on startup
      lc.shutdown(address,false);

      // Set the brightness to a medium values
      lc.setIntensity(address,8);

      // and clear the display
      lc.clearDisplay(address);
    }
}

void loop()
{ 
    buildBuffer();
    writeToMatrix();
  
    delay(delaytime);
  
    if (dir == 0)
    {
        if (idx != ({$LMS_COUNT$} - 32))
        {
            idx++;  
        }
        else
        {
            dir = 1;
        }
    }  
    else
    {
        if (idx != 0)
        {
            idx--;
        }
        else
        {
            dir = 0;
        }
    }
}

void writeToMatrix()
{
    for (int t = 0; t < 4; t++)
    {
        for (int z = 0; z < 8; z++)
        {
            lc.setRow(t, z, buffer[t][z]);
        }
    }
}


// converts column data to rows
// MUCH faster to write out rows than columns
// this converts the column data in our data array 
// in to row data for output
void buildBuffer()
{
    for (int t = 0; t < 4; t++)
    {
        for (int r = 0; r < 8; r++)
        {
            buffer[t][r] = 0;
        }
    
        for (int z = 0; z < 8; z++)
        {
            int column = somearray[idx + (t * 8) + z];

            for (int r = 0; r < 8; r++)
            {
                if (column & powers[7 - r])
                {
                    buffer[t][r] = buffer[t][r] | (powers[z]);
                }
            }
        }
    }
}