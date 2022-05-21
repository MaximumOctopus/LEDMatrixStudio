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

unit utility;


interface


uses ExtCtrls, shellapi, SysUtils, Windows, classes, dialogs, System.UITypes,

     exportoptions, systemsettings,

     thematrix, languagehandler,

     matrixconstants, xglobal;


type
  TDataOut = record
               count : integer;
               data  : array[0..7] of string;
             end;


  TDataOutDisplay = record
                      Text : string;
                      ColumnData : array[0.._MaxHeight] of string;
                      RowData    : array[0.._MaxHeight] of string;
                    end;


  TUtility = class
    class function  ValidateNumber(s : string; max : integer): boolean;
    class function  HexToInt(const s : string): Int64;
    class function  HexToByte(const s : string): byte;

    class function  RGBPlusInteger(aWindowsFormatColour, aBrightness : integer): string;
    class function  RGBColourNumberFormat(aNumberFormat : TNumberFormat; aNybbles : integer; aColour : LongWord): string;
    class function  RGBConvertTo(rgb : LongWord; convertmode : TRGBMode; lsblocation : TLSB; aBrightness : integer): LongWord;
    class function  RGBConvertToSplit(rgb : LongWord; convertmode : TRGBMode; aBrightness : integer; aNumberFormat : TNumberFormat; aPrefix, aSpacer : string): string;
    class function  RGB3BPPFormatOutput(r, g, b : integer; aConvertMode : TRGBMode; aNumberFormat : TNumberFormat; aNumberSize : TNumberSize; aBrightness : integer; aPrefix, aSpacer : string): string;

    class function  IntegerToBinary(ns : integer; anumber : int64): string;

    class function  CreatedDate: string;
    class function  GetFilePrefix(aFileName : string): string;
    class function  GetFileNameNoExt(aFileName : string): string;

    class procedure ClearTExportOptions(aIsRGB : boolean; var aEEO : TExportOptions);

    class function  RemoveExtension(s : string): string;

    class function  ExecuteFile(bob : THandle; const FileName, Params, DefaultDir: string): THandle;

    class function  GetAutoSaveName: string;

    class function  IsAlphaNumeric(a : string): boolean;

    class function  PadString(aChar : char; aCount : integer): string;
    class function  PadToLength(aInput : string; aCount : integer): string;
    class function  PadZeroes(aInput : string; aCount : integer): string;

    class function  BoolToInt(aInput : boolean): integer;
    class function  IntToBool(aInput : integer): boolean;

    class function  GetTypeName(aMatrixMode : TMatrixMode): string;

    class function  GetDate: string;
    class function  GetTime: string;
  end;


const
  LEDStudioDate      = 'May 21st 2022';

  {$ifdef CPUX64}
     LEDStudioVersion   = '0.10.8 (x64)';
  {$else}
     LEDStudioVersion   = '0.10.8 (x32)';
  {$endif}

  BiColoursLSBLeft   : array[0..3] of string = ('00', '01', '10', '11');
  BiColoursLSBRight  : array[0..3] of string = ('00', '10', '01', '11');

  NumberSizes        : array[0..7] of integer = (7, 15, 31, 7, 15, 63, 7, 31);
  NumberPadding      : array[0..7] of integer = (2,  4,  8, 2,  4, 16, 2,  8);


var
 MatrixMain  : TTheMatrix;

 LMSSettings : TSystemSettingsObject;


implementation


class function TUtility.PadString(aChar : char; aCount : integer): string;
 begin
  Result := '';

  while Length(Result) < aCount do
    Result := Result + aChar;
end;


class function TUtility.PadToLength(aInput : string; aCount : integer): string;
begin
  Result := aInput;

  while Length(Result) < aCount do
    Result := Result + ' ';
end;


class function TUtility.PadZeroes(aInput : string; aCount : integer): string;
begin
  Result := aInput;

  while Length(Result) < aCount do
    Result := '0' + Result;
end;


class function TUtility.ValidateNumber(s : string; max : integer): boolean;
var
  t : integer;

begin
  Result := True;

  for t := 1 to length(s) do begin
    if not((Ord(s[t]) >= 48) and (Ord(s[t]) <= 57)) then
      Result := False;
  end;

  if Result then begin
    if max <> -1 then begin
      if StrToInt(s) > max then
        Result := False;
    end;
  end;
end;


class function TUtility.CreatedDate: string;
begin
  Result := GLanguageHandler.Text[kDate] + ' : ' + GetDate;
end;


class function TUtility.HexToInt(const s : string): Int64;
var
  i : integer;
  digit : integer;

begin
  Result := 0;

  for i := 1 to length(s) do begin
    case Ord(s[i]) of
      48..57 : digit := StrToInt(s[i]);
      65..70 : digit := Ord(s[i]) - 55;
    else
      MessageDlg('ERROR: "' + s[i] + '"', mtError, [mbOK], 0);
      digit := 0;
    end;

    Result := Result + (digit * powers16[length(s) - i]);
  end;
end;


class function TUtility.HexToByte(const s : string): byte;
var
  i : integer;
  digit : integer;

begin
  Result := 0;

  for i := 1 to length(s) do begin
    case Ord(s[i]) of
      48..57 : digit := StrToInt(s[i]);
      65..70 : digit := Ord(s[i]) - 55;
    else
      MessageDlg('ERROR: "' + s[i] + '"', mtError, [mbOK], 0);
      digit := 0;
    end;

    Result := Result + (digit * powers16[length(s) - i]);
  end;
end;


class function TUtility.RGBPlusInteger(aWindowsFormatColour, aBrightness : integer): string;
begin
  Result := '0x' + IntToHex(RGBConvertTo(aWindowsFormatColour, cmRGB, llBottomRight, aBrightness), 6) + ' (' + IntToStr(aWindowsFormatColour) + ')';
end;


class function TUtility.RGBConvertTo(rgb : LongWord; convertmode : TRGBMode; lsblocation : TLSB; aBrightness : integer): LongWord;
var
  xR : LongWord;
  xG : LongWord;
  xB : LongWord;
  xT : LongWord;
  t : integer;

begin
  xR := (rgb and $0000ff);         // Windows colour structure = BGR
  xB := (rgb and $ff0000) shr 16;
  xG := (rgb and $00ff00) shr 8;

  if (aBrightness <> 100) then begin
    xR := Round((aBrightness / 100) * xR);
    xG := Round((aBrightness / 100) * xG);
    xB := Round((aBrightness / 100) * xB);
  end;

  case convertmode of
    cmRGB : xT := (xR shl 16) + (xG shl 8) + xB;
    cmBGR : xT := (xB shl 16) + (xG shl 8) + xR;
    cmGRB : xT := (xG shl 16) + (xR shl 8) + xB;
    cmBRG : xT := (xB shl 16) + (xR shl 8) + xG;
  else
    xT := 0;
  end;

  if lsblocation = llTopLeft then begin // flip bit order
    Result := 0;

    for t := 0 to 23 do
      if (xT and powers[t]) = powers[t] then
        Result := Result + powers[31 - t];
  end
  else
    Result := xT;
end;


class function TUtility.RGBColourNumberFormat(aNumberFormat : TNumberFormat; aNybbles : integer; aColour : LongWord): string;
var
  lBits : integer;

begin
  case aNumberFormat of
    nfDecimal : Result := IntToStr(aColour);
    nfBinary  : begin
                  lBits := (aNybbles * 4) - 1;

                  Result := IntegerToBinary(lBits, aColour);
                end;
    nfHex     : Result := IntToHex(aColour, aNybbles);
  else
    Result := 'error ' + IntToStr(Ord(aNumberFormat));
  end;
end;


// converts windows format colour to separate R G B values
// eg ff0000 (blue)
// => 00 00 ff
class function TUtility.RGBConvertToSplit(rgb : LongWord; convertmode : TRGBMode; aBrightness : integer; aNumberFormat : TNumberFormat; aPrefix, aSpacer : string): string;
var
  xR : LongWord;
  xG : LongWord;
  xB : LongWord;

begin
  xR := (rgb and $0000ff);         // Windows colour structure = BGR
  xB := (rgb and $ff0000) shr 16;
  xG := (rgb and $00ff00) shr 8;

  if (aBrightness <> 100) then begin
    xR := Round((aBrightness / 100) * xR);
    xG := Round((aBrightness / 100) * xG);
    xB := Round((aBrightness / 100) * xB);
  end;

   // (cmRGB, cmBGR, cmGRB, cmBRG);

  case convertmode of
    cmRGB       : Result := aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xR) + aSpacer +
                            aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xG) + aSpacer +
                            aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xB) + aSpacer;
    cmBGR       : Result := aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xB) + aSpacer +
                            aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xG) + aSpacer +
                            aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xR) + aSpacer;
    cmGRB       : Result := aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xG) + aSpacer +
                            aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xR) + aSpacer +
                            aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xB) + aSpacer;
    cmBRG       : Result := aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xB) + aSpacer +
                            aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xR) + aSpacer +
                            aPrefix + RGBColourNumberFormat(aNumberFormat, 2, xG) + aSpacer;
    cmRGBSimple : Result := IntToStr(xR) + aSpacer +IntToStr(xG) + aSpacer + IntToStr(xB);
  else
    Result := aPrefix + '00' + aSpacer + aPrefix + '00' + aSpacer + aPrefix + '00' + aSpacer;   // !
  end;
end;


class function TUtility.RGB3BPPFormatOutput(r, g, b : integer; aConvertMode : TRGBMode; aNumberFormat : TNumberFormat; aNumberSize : TNumberSize; aBrightness : integer; aPrefix, aSpacer : string): string;
begin
  if aNumberSize = nsRGB8bit then begin
    case aConvertMode of
      cmRGB : Result := aPrefix + RGBColourNumberFormat(aNumberFormat, 2, r) + aSpacer +
                                  aPrefix + RGBColourNumberFormat(aNumberFormat, 2, g) + aSpacer +
                                  aPrefix + RGBColourNumberFormat(aNumberFormat, 2, b) + aSpacer;
      cmBGR : Result := aPrefix + RGBColourNumberFormat(aNumberFormat, 2, b) + aSpacer +
                                  aPrefix + RGBColourNumberFormat(aNumberFormat, 2, g) + aSpacer +
                                  aPrefix + RGBColourNumberFormat(aNumberFormat, 2, r) + aSpacer;
      cmGRB : Result := aPrefix + RGBColourNumberFormat(aNumberFormat, 2, g) + aSpacer +
                                  aPrefix + RGBColourNumberFormat(aNumberFormat, 2, r) + aSpacer +
                                  aPrefix + RGBColourNumberFormat(aNumberFormat, 2, b) + aSpacer;
      cmBRG : Result := aPrefix + RGBColourNumberFormat(aNumberFormat, 2, b) + aSpacer +
                                  aPrefix + RGBColourNumberFormat(aNumberFormat, 2, r) + aSpacer +
                                  aPrefix + RGBColourNumberFormat(aNumberFormat, 2, g) + aSpacer;
    else
      Result := 'errorFO';
    end;
  end
  else begin
    case aConvertMode of
      cmRGB : Result := aPrefix + RGBColourNumberFormat(nfHex, 2, r) + RGBColourNumberFormat(nfHex, 2, g) + RGBColourNumberFormat(nfHex, 2, b) + aSpacer;
      cmBGR : Result := aPrefix + RGBColourNumberFormat(nfHex, 2, b) + RGBColourNumberFormat(nfHex, 2, g) + RGBColourNumberFormat(nfHex, 2, r) + aSpacer;
      cmGRB : Result := aPrefix + RGBColourNumberFormat(nfHex, 2, g) + RGBColourNumberFormat(nfHex, 2, r) + RGBColourNumberFormat(nfHex, 2, b) + aSpacer;
      cmBRG : Result := aPrefix + RGBColourNumberFormat(nfHex, 2, b) + RGBColourNumberFormat(nfHex, 2, r) + RGBColourNumberFormat(nfHex, 2, g) + aSpacer;
    end;
  end;
end;


class function TUtility.IntegerToBinary(ns : integer; anumber : int64): string;
var
  i : integer;

begin
  Result := '';

  for i := 1 to ns + 1 do
    Result := Result + '0';

  for i := 0 to ns do begin
    if (anumber and powers[i]) = powers[i] then
      Result[(ns - i) + 1] := '1';
  end;
end;


class function TUtility.RemoveExtension(s : string): string;
var
  idx,t : integer;

begin
  idx    := -1;
  Result := s;

  for t := 1 to length(s) do
    if s[t] = '.' then idx := t;

  if idx <> -1 then
    Result := Copy(s, 1, idx - 1);
end;


class procedure TUtility.ClearTExportOptions(aIsRGB : boolean; var aEEO : TExportOptions);
begin
  aEEO.IncludePreamble := True;              //
  aEEO.CleanMode       := False;             // True = exclude everything from data output except the data!
  aEEO.ExportMode      := esNone;
  aEEO.StartFrame      := 1;
  aEEO.EndFrame        := 1;
  aEEO.Source          := rsRows;
  aEEO.Orientation     := ioTopBottomLeftRight;
  aEEO.ScanDirection   := scanRowLeftToRight;
  aEEO.LSB             := llBottomRight;
  aEEO.Language        := elCSV;
  aEEO.NumberFormat    := nfHex;
  aEEO.NumberSize      := ns8Bit;            //
  aEEO.LineContent     := lcRowCol;
  aEEO.LineCount       := 10;
  aEEO.FontMode        := False;
  aEEO.Optimise        := False;
  aEEO.MinWidth        := -1;
  aEEO.MaxWidth        := -1;
  aEEO.MinHeight       := -1;
  aEEO.MaxHeight       := -1;

  if aIsRGB then begin
    aEEO.RGBEnabled      := True;
    aEEO.RGBMode         := cmRGB;
    aEEO.RGBChangePixels := False;
    aEEO.RGBChangeColour := $00000000;
  end
  else begin
    aEEO.RGBEnabled      := False;
    aEEO.RGBMode         := cmRGB;
    aEEO.RGBChangePixels := False;
    aEEO.RGBChangeColour := $00000000;
  end;

  aEEO.RGBBrightness   := 100;

  aEEO.Description     := '';
  aEEO.DataPadding     := '';

  // == binary output settings =================================================

  aEEO.BinarySource          := rsRows;
  aEEO.BinaryOrientation     := ioTopBottomLeftRight;
  aEEO.BinaryScanDirection   := scanRowLeftToRight;
  aEEO.BinaryLSB             := llBottomRight;

  if aIsRGB then begin
    aEEO.BinaryRGBMode         := cmRGB;
    aEEO.BinaryRGBChangePixels := False;
    aEEO.BinaryRGBChangeColour := $00000000;
  end
  else begin
    aEEO.BinaryRGBMode         := cmRGB;
    aEEO.BinaryRGBChangePixels := False;
    aEEO.BinaryRGBChangeColour := $00000000;
  end;

  aEEO.BinaryRGBBrightness   := 100;

  aEEO.BinaryNumberSize      := ns8Bit;

  aEEO.BinaryFileContents    := bfEntireAnimation;
end;


class function TUtility.ExecuteFile(bob : THandle; const FileName, Params, DefaultDir: string): THandle;
var
  zFileName, zParams, zDir: array[0..254] of Char;

begin
  Result := ShellExecute(bob, nil, StrPCopy(zFileName, FileName), StrPCopy(zParams, Params), StrPCopy(zDir, DefaultDir), SW_SHOW);
end;


class function TUtility.GetAutoSaveName: string;
var
  lYear, lMonth, lDay : word;
  lHour, lMin, lSec, lMSec : word;

begin
  DecodeDate(Now, lYear, lMonth, lDay);
  DecodeTime(Now, lHour, lMin, lSec, lMSec);

  Result := 'autosave_' + IntToStr(lYear);

  if lMonth < 10 then
    Result := Result + '0' + IntToStr(lMonth)
  else
    Result := Result + IntToStr(lMonth);

  if lDay < 10 then
    Result := Result + '0' + IntToStr(lDay) + '_'
  else
    Result := Result + IntToStr(lDay) + '_';

  if lHour < 10 then
    Result := Result + '0' + IntToStr(lHour)
  else
    Result := Result + IntToStr(lHour);

  if lMin < 10 then
    Result := Result + '0' + IntToStr(lMin)
  else
    Result := Result + IntToStr(lMin);

  Result := Result + '.leds';
end;



class function TUtility.GetFilePrefix(aFileName : string): string;
var
  lFileName : string;
  t,idx : integer;

begin
  lFileName := ExtractFileName(aFileName);

  idx := -1;

  for t := length(lFileName) downto 1 do
    if lFileName[t] = '.' then
      idx := t;

  if idx <> -1 then
    Result := Copy(lFileName, 1, idx - 1)
  else
    Result := aFileName;
end;


class function TUtility.GetFileNameNoExt(aFileName : string): string;
var
  t,idx : integer;

 begin
  idx := -1;

  for t := length(aFileName) downto 1 do
    if aFileName[t] = '.' then begin
      idx := t;

      Break;
    end;

  if idx <> -1 then
    Result := Copy(aFileName, 1, idx - 1)
  else
    Result := aFileName;
end;


class function TUtility.IsAlphaNumeric(a : string): boolean;
begin
  Result := False;

  if ((ord(a[1]) >= 48) and (ord(a[1]) <= 57)) then    // 0..9
    Result := True;

  if ((ord(a[1]) >= 65) and (ord(a[1]) <= 90)) then    // A..Z
    Result := True;

  if ((ord(a[1]) >= 97) and (ord(a[1]) <= 122)) then   // a..z
    Result := True;
end;


class function TUtility.BoolToInt(aInput : boolean): integer;
begin
  if aInput then
    Result := 1
  else
    Result := 0;
end;


class function TUtility.IntToBool(aInput : integer): boolean;
begin
  if aInput = 0 then
    Result := False
  else
    Result := True;
end;


class function TUtility.GetTypeName(aMatrixMode : TMatrixMode): string;
begin
  case aMatrixMode of
    mtMono         : Result := GLanguageHandler.Text[kSingleColour];
    mtBiSequential : Result := GLanguageHandler.Text[kBiColourSequential];
    mtBiBitPlanes  : Result := GLanguageHandler.Text[kBiColourBitplanes];
    mtRGB          : Result := GLanguageHandler.Text[kRGB];
    mtRGB3BPP      : Result := GLanguageHandler.Text[kRGB3BPP];
  else
    Result := 'unknown :(';
  end;
end;


class function TUtility.GetDate: string;
var
  mm, dd, yy : word;

begin
  DecodeDate(Now, yy, mm, dd);

  if dd < 10 then
    Result := '0' + IntToStr(dd) + '/'
  else
    Result := Result + IntToStr(dd) + '/';

  if mm < 10 then
    Result := Result + '0' + IntToStr(mm) + '/'
  else
    Result := Result + IntToStr(mm) + '/';

  Result := Result + IntToStr(yy);
end;


class function TUtility.GetTime: string;
var
  hh, mm, ss, sss : word;

begin
  DecodeTime(Now, hh, mm, ss, sss);

  if (hh < 10) then
    Result := '0' + IntToStr(hh)
  else
    Result := IntToStr(hh);

  Result := Result + ':';

  if (mm < 10) then
    Result := '0' + IntToStr(mm)
  else
    Result := IntToStr(mm);

  Result := Result + '.';

  if (ss < 10) then
    Result := '0' + IntToStr(ss)
  else
    Result := IntToStr(ss);
end;


end.
