// ===================================================================
//
// (c) Paul Alan Freshney 2012-2023
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://github.com/MaximumOctopus/LEDMatrixStudio
//
// Please do not modifiy this comment section
//
// ===================================================================

unit previewsettings;


interface


uses matrixconstants;


type
  TPreviewOptions  = record
                       Enabled   : boolean;
                       Size      : integer;
                       View      : TViewShape;
                       Void      : integer;
                       Offset    : integer;
                       Direction : boolean;
                       Popout    : boolean;
                     end;


implementation


end.
