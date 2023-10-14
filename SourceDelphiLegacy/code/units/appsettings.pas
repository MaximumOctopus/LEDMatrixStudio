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


unit appsettings;


interface


uses exportoptions, matrixconstants;


type
  TAppSettings   = record
                     DataFilename           : string;
                     ASCIIIndex             : integer;
                     PadMode                : TPadFormat;
                     PadModeHexCol          : integer;
                     PadModeHexRow          : integer;
                     PadModeDecCol          : integer;
                     PadModeDecRow          : integer;
                     HexPrefix              : string;
                     BinaryPrefix           : string;
                     OpenBracket            : string;
                     CloseBracket           : string;
                     LastExport             : TExportOptions;

                     LastSaveLocation       : string;
                     LastLoadLocation       : string;
                     LastAutomationFileName : string;

                     CustomSpeed            : integer;

                     BackgroundColour       : integer;

                     ExportUpdateMaxPixels  : integer;
                     ExportPreviewSize      : integer;

                     Language               : string;
                   end;


implementation


end.
