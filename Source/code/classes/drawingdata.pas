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

unit drawingdata;


interface


uses System.Types;


type
  TDrawMode = (dmNone,
               dmFilledBox, dmEmptyBox,
               dmLine, dmFont,
               dmEmptyCircle, dmFilledCircle,
               dmRandom, dmMulti, dmPicker,
               dmCopy, dmPaste,
               dmGradientBrush,
               dmFloodFill,
               dmSpiral, dmRing, dmSplitRing, dmPetals, dmGrid, dmPyramid, dmLeftTriangle, dmRightTriangle);

  TDrawPoint = (dpNone, dpFirst, dpLast);


  TDrawData = record
                Mode         : TDrawMode;
                Point        : integer;
                Colour       : integer;
                Coords       : array[0..1] of TPoint;
                Special      : integer;

                CopyPos      : TPoint;

                ParameterMin : integer;
                ParameterMax : integer;
                Parameter    : integer;

                SinglePoint  : boolean;              // this draw mode only requires a single click to render
              end;


const
  CDrawPointNone  = 0;
  CDrawPointFirst = 1;
  CDrawPointLast  = 2;


implementation


end.
