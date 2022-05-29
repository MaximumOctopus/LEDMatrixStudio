//
// Paul Alan Freshney :: paul@freshney.org
//
// https://sourceforge.net/projects/led-matrix-studio/
//
// January 9th 2022
//
//

#include "LedControlMS.h"

// this is designed for the cheap 4x 8x8 digit display from ebay!
//
//  NODEMCU V3 12E pinouts:
//
//  GPIO16 (pin D0) is connected to DIN (DataIN0
//  GPIO5  (pin D1) is connected to CLK (Clock)
//  GPIO4  (pin D2) is connected to CS  (Load)
//

LedControl lc=LedControl(16, 5, 4, 4);

unsigned long delaytime=100;

// used as a frame store for each [device][row]
int buffer[4][8];

int powers[8] = {1, 2, 4, 8, 16, 32, 64, 128};

{$LMS_MATRIX_DATA$}

byte idx = 0;
byte dir = 0;

// ===================================================================

void setup() 
{
  //we have already set the number of devices when we created the LedControl
  int devices = lc.getDeviceCount();
  
  //we have to init all devices in a loop
  for(int address = 0;address < devices; address++) 
  {
    /*The MAX72XX is in power-saving mode on startup*/
    lc.shutdown(address, false);
    /* Set the brightness to a medium values */
    lc.setIntensity(address, 8);
    /* and clear the display */
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
    if (idx != {$LMS_MATRIX_WIDTH$} - 32 - 1)
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
      int column = ledarray[idx + (t * 8) + z];

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
