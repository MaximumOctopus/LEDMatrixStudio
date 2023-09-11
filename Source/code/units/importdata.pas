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

unit importdata;


interface


uses colours, matrixconstants;


type
  TImportDataPreview = record
                         Enabled           : boolean;

                         Size              : integer;
                         View              : TViewShape;
                         Void              : integer;

                         IncrementRadially : boolean;

                         Popout            : boolean;

                         Offset            : integer;
                         OffsetDirection   : boolean;
                       end;


  TImportData = record
                  ImportOk           : boolean;
                  ErrorString        : string;

                  Source             : integer;
                  SourceLSB          : integer;
                  SourceDirection    : integer;
                  PadMode            : TPadFormat;
                  HexFormat          : THexFormat;
                  HexOutput          : THexPrefix;
                  Brackets           : TBracketStyle;
                  MatrixMode         : TMatrixMode;
                  NewWidth           : integer;
                  NewHeight          : integer;
                  NewFrames          : integer;
                  MaxFrames          : integer;
                  ASCIIIndex         : integer;
                  FontMode           : boolean;

                  RGBImport          : boolean;
                  BackgroundColour   : integer;

                  RGBBrightness      : integer;

                  StartFrame         : integer;
                  EndFrame           : integer;

                  Preview            : TImportDataPreview;

                  AutomationFileName : string;

                  Colours            : TColours;
                end;


implementation


end.
