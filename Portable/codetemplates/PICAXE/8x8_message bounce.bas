' =======================================================
' = Optimised 8x8 Matrix driver                         =
' =                                                     =
' = (c) Paul Alan Freshney 2009 (added to LMS  in 2022) =
' =                                                     =
' = July 15th 2009 (January 9th 2022)                   =
' =======================================================

setfreq m8

symbol dataout     = 0
symbol latchout    = 1
symbol clk	       = 2

symbol outbyte     = b0
symbol ncounter    = b1
symbol datapointer = b2
symbol tempbyte    = b4
symbol rowbyte     = b5
symbol charrowdata = b6
symbol dcounter    = b7
symbol direction   = b8 ' 0 = forwards, 1 = backwards

EEPROM (255, 255, 255, 255)
{$LMS_MATRIX_DATA$}
EEPROM (255, 255)           

symbol lastoutputbyte = {$LMS_BYTES$} + 1             ' (last EEPROM byte) - 5

init:
	pause 100
	
	gosub test
	
	datapointer = 0
	dcounter    = 0	
	direction   = 0
	low latchout

main:              
	charrowdata = datapointer  	   
	   	    
	read charrowdata, outbyte
	spiout clk, dataout, MSBFirst_H, (outbyte, 128)
	  
	high latchout
	low  latchout

	' ========================================================
	
	inc charrowdata
	   	    
	read charrowdata, outbyte
	spiout clk, dataout, MSBFirst_H, (outbyte, 64)
	  
	high latchout
	low  latchout
	  
	' ========================================================
	
	inc charrowdata  	   
	   	    
	read charrowdata, outbyte
	spiout clk, dataout, MSBFirst_H, (outbyte, 32)
	  
	high latchout
	low  latchout
	  
	' ========================================================
	
	inc charrowdata
	   	    
	read charrowdata, outbyte
	spiout clk, dataout, MSBFirst_H, (outbyte, 16)
	  
	high latchout
	low  latchout
	  
	' ========================================================
	
	inc charrowdata   	   
	   	    
	read charrowdata, outbyte
	spiout clk, dataout, MSBFirst_H, (outbyte, 8)
	  
	high latchout
	low  latchout
	  
	' ========================================================
	
	inc charrowdata
	   	    
	read charrowdata, outbyte
	spiout clk, dataout, MSBFirst_H, (outbyte, 4)
  
	high latchout
	low  latchout
	  
	' ========================================================
	 
	inc charrowdata  	   
	   	    
	read charrowdata, outbyte
	spiout clk, dataout, MSBFirst_H, (outbyte, 2)
	  
	high latchout
	low  latchout
	  
	' ========================================================
	
	inc charrowdata
	   	    
	read charrowdata, outbyte
	spiout clk, dataout, MSBFirst_H, (outbyte, 1)
	  
	high latchout
	low  latchout
	  
	' ========================================================							
	
	inc dcounter	
	
	if dcounter=5 then   	  
    	  if direction=0 then
          inc datapointer
        else
          dec datapointer
        endif
    	      	  
    	  dcounter = 0
    	  
    	  if datapointer=lastoutputbyte then
    	    direction = 1
    	  elseif datapointer=3 then
    	    direction = 0
    	  endif
	endif	
	
	goto main
	
' simple test pattern to check all the LEDs work!
test:      
    	for dcounter = 0 to 1
        
        rowbyte = 128
    	
        for ncounter = 0 to 7
	   	  
	    spiout clk, dataout, MSBFirst_L, (rowbyte)

  	    outbyte = not rowbyte
    	    spiout clk, dataout, MSBFirst_L, (outbyte)	  
	  	  
	  
  	    high latchout
   	    low  latchout
	  
	    rowbyte = rowbyte >> 1 
	  
	    pause 150
	  
	  next ncounter
	  
	next dcounter
	
	return