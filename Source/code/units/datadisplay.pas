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


unit datadisplay;


interface


uses matrixconstants;


type
  TDataDisplay     = record
                       HexFormat    : THexFormat;
                       HexPrefix    : THexPrefix;
                       BinaryPrefix : TBinaryPrefix;
                       PadFormat    : TPadFormat;
                       Brackets     : TBracketStyle;
                     end;


implementation


end.
