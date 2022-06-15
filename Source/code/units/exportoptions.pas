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

unit exportoptions;


interface


type
  TNumberFormat       = (nfDecimal, nfBinary, nfHex);
  TNumberSize         = (ns8Bit, ns16bit, ns32bit, ns8bitSwap, ns16bitSwap, ns64bit, nsRGB8bit, nsRGB32bit);
  TReadSource         = (rsColumns, rsRows);
  TInputOrientation   = (ioTopBottomLeftRight, ioBottomTopRightLeft, ioSure24x16);
  TRGBMode            = (cmRGB, cmBGR, cmGRB, cmBRG, cmRGBSimple);
  TLSB                = (llTopLeft, llBottomRight);
  TExportSource       = (esNone, esAnimation, esUserMemories);
  TExportLanguage     = (elCSV, elPICAXE, elC1Dim, elC2Dim, elCFastLED, elPython1Dim, elPython2Dim, elMicrochip, elPascal, elSpecial);
  TLineContent        = (lcRowCol, lcFrame, lcBytes);
  TBinaryFileContents = (bfEntireAnimation, bfSingleFrame);


  TExportOptions = record
                     Valid                 : boolean;

                     IncludePreamble       : boolean;
                     CleanMode             : boolean; // True = exclude everything from data output except the data!
                     ExportMode            : TExportSource;
                     StartFrame            : integer;
                     EndFrame              : integer;
                     SelectiveStart        : integer;
                     SelectiveEnd          : integer;
                     Source                : TReadSource;
                     Orientation           : TInputOrientation;
                     ScanDirection         : integer;
                     LSB                   : TLSB;
                     Language              : TExportLanguage;
                     NumberFormat          : TNumberFormat;
                     NumberSize            : TNumberSize;
                     LineContent           : TLineContent;
                     LineCount             : integer;
                     FontMode              : boolean;
                     Optimise              : boolean;
                     MinWidth              : integer;
                     MaxWidth              : integer;
                     MinHeight             : integer;
                     MaxHeight             : integer;
                     RGBEnabled            : boolean;
                     RGBMode               : TRGBMode;
                     RGBChangePixels       : boolean;
                     RGBChangeColour       : integer;
                     RGBBrightness         : integer;
                     Description           : string;
                     Information           : string;
                     DataPadding           : string;

                     BinarySource          : TReadSource;
                     BinaryOrientation     : TInputOrientation;
                     BinaryScanDirection   : integer;
                     BinaryLSB             : TLSB;
                     BinaryRGBMode         : TRGBMode;
                     BinaryRGBChangePixels : boolean;
                     BinaryRGBChangeColour : integer;
                     BinaryRGBBrightness   : integer;
                     BinaryNumberSize      : TNumberSize;
                     BinaryFileContents    : TBinaryFileContents;

                     Examples              : boolean; // include code example to output (BETA!)
                   end;


implementation


end.
