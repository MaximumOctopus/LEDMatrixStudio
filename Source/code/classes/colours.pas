// ===================================================================
//
// (c) Paul Alan Freshney 2012-2022
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://github.com/MaximumOctopus/LEDMatrixStudio
//
// Please do not modifiy this comment section
//
// ===================================================================

unit colours;


interface


type
  TColours = record
    HasData        : boolean;

    DrawColours    : array[1..3] of integer;   // left, middle, right
    CustomColours  : array[0..15] of integer;

    PaletteHistory : array[0..27] of integer;
  end;


implementation


end.
