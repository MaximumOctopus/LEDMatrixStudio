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

unit projectsettings;


interface


uses matrixconstants;


type
  TProjectSettings = record
                       Valid            : boolean;

                       MatrixMode       : TMatrixMode;
                       Width            : integer;
                       Height           : integer;
                       Clear            : boolean;
                       Special          : integer;
                       SizeType         : boolean;
                       Pixel            : TPixelShape;
                       CustomShape      : TCustomShape;
                       CustomShapeParam : integer;

                       Background       : integer;
                     end;


implementation


end.
