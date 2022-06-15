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

unit profilehandler;


interface


uses SysUtils, Vcl.StdCtrls,

     fileconstants,

     utility,

     exportoptions;


type
  TLoadProfile     = (lpUnknown, lpDataBegin, lpDataEnd,
                      lpSource, lpOrientation, lpLSB, lpLanguage, lpNumberFormat, lpNumberSize, lpScanDirection,
                      lpLineContent, lpLineCount,
                      lpRGBMode, lpRGBChangePixels, lpRGBChangeColour, lpRGBBrightness,
                      lpMinWidth, lpMaxWidth, lpMinHeight, lpMaxHeight,
                      lpInformation,
                      lpBinarySource, lpBinaryOrientation, lpBinaryLSB, lpBinaryScanDirection,
                      lpBinaryRGBMode, lpBinaryRGBChangePixels, lpBinaryRGBChangeColour, lpBinaryRGBBrightness,
                      lpBinaryFileContents);

  TProfileHandler = class
    class function LoadExportProfileFromFile(fn : string): TExportOptions;
    class function SaveExportProfile(aFileName : string; aIsRGB : boolean; aEEO : TExportOptions): boolean;
    class function DeleteExportProfile(aFileName : string): boolean;
    class function GetExportProfilesList(aPath : string; var aProfileList : TComboBox): boolean;
  end;


implementation


class function TProfileHandler.LoadExportProfileFromFile(fn : string): TExportOptions;
var
  tf : TextFile;
  s, v : string;

 function parameterType(s : string): TLoadProfile;
  begin
   if s[1] = kDataBlockStart then
     Result := lpDataBegin
   else if s[1] = kDataBlockEnd then
     Result := lpDataEnd
   else if s[1] = kExportSource then
     Result := lpSource
   else if s[1] = kExportOrientation then
     Result := lpOrientation
   else if s[1] = kExportLSB then
     Result := lpLSB
   else if s[1] = kExportLanguage then
     Result := lpLanguage
   else if s[1] = kExportNumberFormat then
     Result := lpNumberFormat
   else if s[1] = kExportNumberSize then
     Result := lpNumberSize
   else if s[1] = kExportScanDirection then
     Result := lpScanDirection
   else if s[1] = kExportLineContent then
     Result := lpLineContent
   else if s[1] = kExportLineCount then
     Result := lpLineCount
   else if s[1] = kExportRGBMode then
     Result := lpRGBMode
   else if s[1] = kExportRGBChangePixels then
     Result := lpRGBChangePixels
   else if s[1] = kExportRGBChangeColour then
     Result := lpRGBChangeColour
   else if s[1] = kExportRGBBrightness then
     Result := lpRGBBrightness
   else if s[1] = kExportMinWidth then
     Result := lpMinWidth
   else if s[1] = kExportMaxWidth then
     Result := lpMaxWidth
   else if s[1] = kExportMinHeight then
     Result := lpMinHeight
   else if s[1] = kExportMaxHeight then
     Result := lpMaxHeight
   else if s[1] = kExportInformation then
     Result := lpInformation

   else if s[1] = kExportBinarySource then
     Result := lpBinarySource
   else if s[1] = kExportBinaryOrientation then
     Result := lpBinaryOrientation
   else if s[1] = kExportBinaryLSB then
     Result := lpBinaryLSB
   else if s[1] = kExportBinaryScanDirection then
     Result := lpBinaryScanDirection
   else if s[1] = kExportBinaryRGBMode then
     Result := lpBinaryRGBMode
   else if s[1] = kExportBinaryRGBChangePixels then
     Result := lpBinaryRGBChangePixels
   else if s[1] = kExportBinaryRGBChangeColour then
     Result := lpBinaryRGBChangeColour
   else if s[1] = kExportBinaryRGBBrightness then
     Result := lpBinaryRGBBrightness
   else if s[1] = kExportBinaryFileContents then
     Result := lpBinaryFileContents
   else
     Result := lpUnknown;
 end;

begin
  Result.Valid := False;

  if FileExists(fn) then begin
    Result.RGBBrightness := 100;
    Result.ExportMode    := esAnimation;

    AssignFile(tf, fn);
    Reset(tf);

    while not(eof(tf)) do begin
      Readln(tf, s);

      if s <> '' then begin
        v := Copy(s, 3, length(s) - 2);

        case parameterType(s) of
          lpDataBegin       : if pos('RGB', UpperCase(s)) <> 0 then
                                Result.RGBEnabled := True
                              else
                                Result.RGBEnabled := False;
          lpDataEnd               : {};
          lpSource                : Result.Source                := TReadSource(StrToInt(v));
          lpOrientation           : Result.Orientation           := TInputOrientation(StrToInt(v));
          lpLSB                   : Result.LSB                   := TLSB(StrToInt(v));
          lpLanguage              : Result.Language              := TExportLanguage(StrToInt(v));
          lpNumberFormat          : Result.NumberFormat          := TNumberFormat(StrToInt(v));
          lpNumberSize            : Result.NumberSize            := TNumberSize(StrToInt(v));
          lpScanDirection         : Result.ScanDirection         := StrToInt(v);
          lpLineContent           : Result.LineContent           := TLineContent(StrToInt(v));
          lpLineCount             : Result.LineCount             := StrToInt(v);

          lpRGBMode               : Result.RGBMode               := TRGBMode(StrToInt(v));
          lpRGBChangePixels       : Result.RGBChangePixels       := StrToBool(v);
          lpRGBChangeColour       : Result.RGBChangeColour       := StrToInt(v);
          lpRGBBrightness         : Result.RGBBrightness         := StrToInt(v);

          lpMinWidth              : Result.MinWidth              := StrToInt(v);
          lpMaxWidth              : Result.MaxWidth              := StrToInt(v);
          lpMinHeight             : Result.MinHeight             := StrToInt(v);
          lpMaxHeight             : Result.MaxHeight             := StrToInt(v);

          lpInformation           : Result.Information           := v;

          lpBinarySource          : Result.BinarySource          := TReadSource(StrToInt(v));
          lpBinaryOrientation     : Result.BinaryOrientation     := TInputOrientation(StrToInt(v));
          lpBinaryLSB             : Result.BinaryLSB             := TLSB(StrToInt(v));
          lpBinaryScanDirection   : Result.BinaryScanDirection   := StrToInt(v);

          lpBinaryRGBMode         : Result.BinaryRGBMode         := TRGBMode(StrToInt(v));
          lpBinaryRGBChangePixels : Result.BinaryRGBChangePixels := StrToBool(v);
          lpBinaryRGBChangeColour : Result.BinaryRGBChangeColour := StrToInt(v);
          lpBinaryRGBBrightness   : Result.BinaryRGBBrightness   := StrToInt(v);
          lpBinaryFileContents    : Result.BinaryFileContents    := TBinaryFileContents(StrToInt(v));
        end;
      end;
    end;

    CloseFile(tf);

    Result.Valid := True;
  end;
end;


class function TProfileHandler.SaveExportProfile(aFileName : string; aIsRGB : boolean; aEEO : TExportOptions): boolean;
var
  tf : TextFile;

begin
  Result := True;

  AssignFile(tf, aFileName);
  Rewrite(tf);

  if aIsRGB then
    writeln(tf, '{RGB')
  else
    writeln(tf, kDataBlockStart);

  writeln(tf, kExportSource +          ':' + IntToStr(Ord(aEEO.Source)));
  writeln(tf, kExportOrientation +     ':' + IntToStr(Ord(aEEO.Orientation)));
  writeln(tf, kExportLSB +             ':' + IntToStr(Ord(aEEO.LSB)));
  writeln(tf, kExportLanguage +        ':' + IntToStr(Ord(aEEO.Language)));
  writeln(tf, kExportNumberFormat +    ':' + IntToStr(Ord(aEEO.NumberFormat)));
  writeln(tf, kExportNumberSize +      ':' + IntToStr(Ord(aEEO.NumberSize)));
  writeln(tf, kExportScanDirection +   ':' + IntToStr(aEEO.ScanDirection));
  writeln(tf, kExportLineContent +     ':' + IntToStr(Ord(aEEO.LineContent)));
  writeln(tf, kExportLineCount +       ':' + IntToStr(aEEO.LineCount));
  writeln(tf, kExportRGBMode +         ':' + IntToStr(Ord(aEEO.RGBMode)));
  writeln(tf, kExportRGBChangePixels + ':' + BoolToStr(aEEO.RGBChangePixels));
  writeln(tf, kExportRGBChangeColour + ':' + IntToStr(aEEO.RGBChangeColour));
  writeln(tf, kExportRGBBrightness +   ':' + IntToStr(aEEO.RGBBrightness));

  // binary export options

  writeln(tf, kExportBinarySource +          ':' + IntToStr(Ord(aEEO.BinarySource)));
  writeln(tf, kExportBinaryOrientation   +   ':' + IntToStr(Ord(aEEO.BinaryOrientation)));
  writeln(tf, kExportBinaryLSB +             ':' + IntToStr(Ord(aEEO.BinaryLSB)));
  writeln(tf, kExportBinaryScanDirection +   ':' + IntToStr(aEEO.BinaryScanDirection));
  writeln(tf, kExportBinaryRGBMode +         ':' + IntToStr(Ord(aEEO.BinaryRGBMode)));
  writeln(tf, kExportBinaryRGBChangePixels + ':' + BoolToStr(aEEO.BinaryRGBChangePixels));
  writeln(tf, kExportBinaryRGBChangeColour + ':' + IntToStr(aEEO.BinaryRGBChangeColour));
  writeln(tf, kExportBinaryRGBBrightness +   ':' + IntToStr(aEEO.BinaryRGBBrightness));
  writeln(tf, kExportBinaryFileContents +    ':' + IntToStr(Ord(aEEO.BinaryFileContents)));
  writeln(tf, kDataBlockEnd);

  CloseFile(tf);
end;


class function TProfileHandler.DeleteExportProfile(aFileName : string): boolean;
begin
  Result := DeleteFile(aFileName);
end;


class function TProfileHandler.GetExportProfilesList(aPath : string; var aProfileList : TComboBox): boolean;
var
  s : string;
  searchResult : TSearchRec;

begin
  Result := True;

  aProfileList.Clear;

  if FindFirst(aPath, faAnyFile, searchResult) = 0 then begin
    repeat
      s := TUtility.RemoveExtension(searchResult.Name);

      aProfileList.Items.Add(s);
    until FindNext(searchResult) <> 0;

    FindClose(searchResult);
  end;
end;


end.
