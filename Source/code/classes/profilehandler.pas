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
   if s[1] = '{' then
     Result := lpDataBegin
   else if s[1] = '}' then
     Result := lpDataEnd
   else if s[1] = 'a' then
     Result := lpSource
   else if s[1] = 'b' then
     Result := lpOrientation
   else if s[1] = 'c' then
     Result := lpLSB
   else if s[1] = 'd' then
     Result := lpLanguage
   else if s[1] = 'e' then
     Result := lpNumberFormat
   else if s[1] = 'f' then
     Result := lpNumberSize
   else if s[1] = 'g' then
     Result := lpScanDirection
   else if s[1] = 'h' then
     Result := lpLineContent
   else if s[1] = 'i' then
     Result := lpLineCount
   else if s[1] = 'r' then
     Result := lpRGBMode
   else if s[1] = 's' then
     Result := lpRGBChangePixels
   else if s[1] = 't' then
     Result := lpRGBChangeColour
   else if s[1] = 'u' then
     Result := lpRGBBrightness
   else if s[1] = 'v' then
     Result := lpMinWidth
   else if s[1] = 'w' then
     Result := lpMaxWidth
   else if s[1] = 'y' then
     Result := lpMinHeight
   else if s[1] = 'z' then
     Result := lpMaxHeight
   else if s[1] = '!' then
     Result := lpInformation

   else if s[1] = '1' then
     Result := lpBinarySource
   else if s[1] = '2' then
     Result := lpBinaryOrientation
   else if s[1] = '3' then
     Result := lpBinaryLSB
   else if s[1] = '4' then
     Result := lpBinaryLSB
   else if s[1] = '5' then
     Result := lpBinaryRGBMode
   else if s[1] = '6' then
     Result := lpBinaryRGBChangePixels
   else if s[1] = '7' then
     Result := lpBinaryRGBChangeColour
   else if s[1] = '8' then
     Result := lpBinaryRGBBrightness
   else if s[1] = '9' then
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
    writeln(tf, '{');

  writeln(tf, 'a:' + IntToStr(Ord(aEEO.Source)));
  writeln(tf, 'b:' + IntToStr(Ord(aEEO.Orientation)));
  writeln(tf, 'c:' + IntToStr(Ord(aEEO.LSB)));
  writeln(tf, 'd:' + IntToStr(Ord(aEEO.Language)));
  writeln(tf, 'e:' + IntToStr(Ord(aEEO.NumberFormat)));
  writeln(tf, 'f:' + IntToStr(Ord(aEEO.NumberSize)));
  writeln(tf, 'g:' + IntToStr(aEEO.ScanDirection));
  writeln(tf, 'h:' + IntToStr(Ord(aEEO.LineContent)));
  writeln(tf, 'i:' + IntToStr(aEEO.LineCount));
  writeln(tf, 'r:' + IntToStr(Ord(aEEO.RGBMode)));
  writeln(tf, 's:' + BoolToStr(aEEO.RGBChangePixels));
  writeln(tf, 't:' + IntToStr(aEEO.RGBChangeColour));
  writeln(tf, 'u:' + IntToStr(aEEO.RGBBrightness));

  // binary export options

  writeln(tf, '1:' + IntToStr(Ord(aEEO.BinarySource)));
  writeln(tf, '2:' + IntToStr(Ord(aEEO.BinaryOrientation)));
  writeln(tf, '3:' + IntToStr(Ord(aEEO.BinaryLSB)));
  writeln(tf, '4:' + IntToStr(aEEO.BinaryScanDirection));
  writeln(tf, '5:' + IntToStr(Ord(aEEO.BinaryRGBMode)));
  writeln(tf, '6:' + BoolToStr(aEEO.BinaryRGBChangePixels));
  writeln(tf, '7:' + IntToStr(aEEO.BinaryRGBChangeColour));
  writeln(tf, '8:' + IntToStr(aEEO.BinaryRGBBrightness));
  writeln(tf, '9:' + IntToStr(Ord(aEEO.BinaryFileContents)));
  writeln(tf, '}');

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
