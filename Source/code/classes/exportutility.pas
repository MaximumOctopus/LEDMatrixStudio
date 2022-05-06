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


unit exportutility;


interface


uses System.Classes, System.SysUtils, dialogs,

     exportoptions, matrix,

     xglobal, utility,

     languagehandler;



type
  TExportUtility = class
    class procedure AddContentByFrame(teo : TExportOptions; s : string; frame : integer; var aOutput : TStringList);
    class procedure AddContentByRowCol(teo : TExportOptions; s : string; var aOutput : TStringList);

    class function  GetRowData(hexmode : boolean; direction, aFrame, rowid : integer): string;
    class function  GetColumnData(hexmode : boolean; direction, aFrame, colid : integer): string;

    class function  GetCommentCharacter(aLanguage : TExportLanguage): string;
    class function  GetLanguage(teo : TExportOptions; aIncludeComment : boolean): string;
    class function  GetLineContent(teo : TExportOptions; aIncludeComment : boolean): string;
    class function  GetLSB(teo : TExportOptions; aIncludeComment : boolean): string;
    class function  GetNumberFormat(aLanguage : TExportLanguage; aNumberFormat : TNumberFormat): string;
    class function  GetNumberSize(aLanguage : TExportLanguage; aNumberSize : TNumberSize; aIncludeComment : boolean): string;
    class function  GetOrientation(teo : TExportOptions; aIncludeComment : boolean): string;
    class procedure GetPreamble(teo : TExportOptions; var aOutput : TStringList; aSimple : boolean);
    class function  GetExampleCodeDisclaimer(teo : TExportOptions): string;
    class function  GetRGBMode(teo : TExportOptions; aIncludeComment : boolean): string;
    class function  GetRGBBrightness(teo : TExportOptions; aIncludeComment : boolean): string;
    class function  GetScanDirection(teo : TExportOptions; aIncludeComment : boolean): string;
    class function  GetSource(aLanguage : TExportLanguage; aSaveType : TReadSource): string;
    class procedure GetSpacerLine(aLanguage : TExportLanguage; var aOutput : TStringList);
    class function  GetSingleVariableStatement(aLanguage : TExportLanguage; aNumberSize : TNumberSize): string;
    class function  GetVariableID(aLanguage : TExportLanguage): string;
    class function  GetVariableIDFrameIn(aLanguage : TExportLanguage; aFrame : integer): string;
    class function  GetVariableIDFrameOut(aLanguage : TExportLanguage): string;
    class function  GetVariableType(aLanguage : TExportLanguage; aNumberSize : TNumberSize): string;

    class function  GetPadding(aLanguage : TExportLanguage; aVariableDefinition : integer): string;

    class function  TitleWithComments(aTitle : string; aLanguage : TExportLanguage; aIncludeComment : boolean): string;
  end;


implementation


class function TExportUtility.TitleWithComments(aTitle : string; aLanguage : TExportLanguage; aIncludeComment : boolean): string;
begin
  if aIncludeComment then
    Result := GetCommentCharacter(aLanguage) + aTitle
  else
    Result := '';
end;


class procedure TExportUtility.AddContentByFrame(teo : TExportOptions; s : string; frame : integer; var aOutput : TStringList);
var
  m : string;

begin
  if (s = '') then
    Exit;

  m := Copy(s, 1, length(s) - 2); // trims last (and unnecessary) ", " from data

  if teo.FontMode then begin
    case teo.Language of
      elCSV        : aOutput.Add(LMSSettings.App.OpenBracket + m + LMSSettings.App.CloseBracket + ';  // ' + Chr(frame + teo.StartFrame - 1) + ' ASCII ' + IntToStr(frame + teo.StartFrame-1));
      elPICAXE     : aOutput.Add('EEPROM (' + m + ')  ; ' + Chr(frame + teo.StartFrame - 1) + ' ASCII ' + IntToStr(frame + teo.StartFrame - 1));
      elC1Dim      : aOutput.Add(teo.DataPadding + m + '  // ' + Chr(frame + teo.StartFrame - 1) + ' ASCII ' + IntToStr(frame + teo.StartFrame - 1));
      elC2Dim      : aOutput.Add(teo.DataPadding + '{' + m + '},  // ' + Chr(frame + teo.StartFrame - 1) + ' ASCII ' + IntToStr(frame + teo.StartFrame - 1));
      elCFastLED   : aOutput.Add(teo.DataPadding + s + '  // ' + Chr(frame + teo.StartFrame - 1) + ' ASCII ' + IntToStr(frame + teo.StartFrame - 1));
      elPython1Dim : aOutput.Add(teo.DataPadding + s + '  # ' + Chr(frame + teo.StartFrame - 1) + ' ASCII ' + IntToStr(frame + teo.StartFrame - 1));
      elPython2Dim : aOutput.Add(teo.DataPadding + '[' + m + '],  # ' + Chr(frame + teo.StartFrame - 1) + ' ASCII ' + IntToStr(frame + teo.StartFrame - 1));
      elMicrochip  : aOutput.Add('dt ' + m + ' ; ' + Chr(frame + teo.StartFrame - 1) + ' ASCII ' + IntToStr(frame + teo.StartFrame - 1));
      elPascal     : aOutput.Add('matrixdata : array[0..__LEDCount] of integer = (' + m + ');');
      elSpecial    : aOutput.Add(s);
    end;
  end
  else begin
    case teo.Language of
      elCSV        : aOutput.Add(LMSSettings.App.OpenBracket + m + LMSSettings.App.CloseBracket + ';  // ' + teo.Description + ' ' + IntToStr(frame));
      elPICAXE     : aOutput.Add('EEPROM (' + m + ')  ; ' + teo.Description + ' ' + IntToStr(frame));
      elC1Dim      : aOutput.Add(teo.DataPadding + m + '  // ' + teo.Description + ' ' + IntToStr(frame));
      elC2Dim      : aOutput.Add(teo.DataPadding + '{' + m + '},  // ' + teo.Description + ' ' + IntToStr(frame));
      elCFastLED   : aOutput.Add(teo.DataPadding + m + '  // ' + teo.Description + ' ' + IntToStr(frame));
      elPython1Dim : aOutput.Add(teo.DataPadding + s + '  # ' + teo.Description + ' ' + IntToStr(frame));
      elPython2Dim : aOutput.Add(teo.DataPadding + '[' + m + '],  # ' + teo.Description + ' ' + IntToStr(frame));
      elMicrochip  : aOutput.Add('dt ' + m + ' ; ' + teo.Description + ' ' + IntToStr(frame));
      elPascal     : aOutput.Add('matrixdata : array[0..__LEDCount] of integer = (' + m + ');');
      elSpecial    : aOutput.Add(s);
    end;
  end;
end;


class procedure TExportUtility.AddContentByRowCol(teo : TExportOptions; s : string; var aOutput : TStringList);
var
  m : string;

begin
  if (s = '') then
    Exit;

  m := Copy(s, 1, length(s) - 2);

  case teo.Language of
    elCSV        : aOutput.Add(LMSSettings.App.OpenBracket + m + LMSSettings.App.CloseBracket + ';');
    elPICAXE     : aOutput.Add('EEPROM (' + m + ')');
    elC1Dim      : aOutput.Add(teo.DataPadding + s);
    elC2Dim      : aOutput.Add(teo.DataPadding + '{' + m + '},');
    elCFastLED   : aOutput.Add(teo.DataPadding + s);
    elPython1Dim : aOutput.Add(teo.DataPadding + s);
    elPython2Dim : aOutput.Add(teo.DataPadding + '[' + m + '],');
    elMicrochip  : aOutput.Add('dt ' + m);
    elPascal     : aOutput.Add('matrixdata : array[0..__LEDCount] of integer = (' + m + ');');
    elSpecial    : aOutput.Add(s);
  end;
end;


class function TExportUtility.GetRowData(hexmode : boolean; direction, aFrame, rowid : integer): string;
var
  lColumn : integer;
  mydata : int64;

begin
  Result := '';
  mydata := 0;

  for lColumn := 0 to MatrixMain.Matrix.Width - 1 do begin
    if MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[lColumn, rowid] = 1 then begin
      if direction = 0 then
        mydata := mydata + (powers[lColumn])
      else
        mydata := mydata + (powers[MatrixMain.Matrix.Width - lColumn]);
    end;
  end;

  if hexmode then
    Result := IntToHex(mydata, LMSSettings.App.PadModeHexRow)
  else
    Result := IntToStr(mydata);
end;


class function TExportUtility.GetColumnData(hexmode : boolean; direction, aFrame, colid : integer): string;
var
  lRow : integer;
  mydata : int64;

begin
  Result := '';
  mydata := 0;

  for lRow := 0 to MatrixMain.Matrix.Height - 1 do begin
    if MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[colid, lRow] = 1 then begin
      if direction = 0 then
        mydata := mydata + (powers[lRow])
      else
        mydata := mydata + (powers[MatrixMain.Matrix.Height - lRow]);
    end;
  end;

  if hexmode then
    Result := IntToHex(mydata, LMSSettings.App.PadModeHexCol)
  else
    Result := IntToStr(mydata);
end;


class function TExportUtility.GetCommentCharacter(aLanguage : TExportLanguage): string;
 begin
  case aLanguage of
    elCSV        : Result := '// ';
    elPICAXE     : Result := '; ';
    elC1Dim,
    elC2Dim,
    elCFastLED   : Result := '// ';
    elPython1Dim,
    elPython2Dim : Result := '# ';
    elMicrochip  : Result := '; ';
    elPascal     : Result := '// ';
  else
    Result := '';
  end;
end;


class function TExportUtility.GetLanguage(teo : TExportOptions; aIncludeComment : boolean): string;
var
  cc : string;

begin
  cc := TitleWithComments(GLanguageHandler.Text[kLanguage] + ' : ', teo.Language, aIncludeComment);

  case teo.Language of
    elCSV        : Result := cc + GLanguageHandler.Text[kExportCommaSeparated];
    elPICAXE     : Result := cc + GLanguageHandler.Text[kExportPICAXEEEPROM];
    elC1Dim      : Result := cc + GLanguageHandler.Text[kExportCCpp1Dimensional];
    elC2Dim      : Result := cc + GLanguageHandler.Text[kExportCCpp2Dimensional];
    elCFastLED   : Result := cc + GLanguageHandler.Text[kExportCCppFastLED];
    elPython1Dim : Result := cc + GLanguageHandler.Text[kExportPython1Dimensional];
    elPython2Dim : Result := cc + GLanguageHandler.Text[kExportPython2Dimensional];
    elMicrochip  : Result := cc + GLanguageHandler.Text[kExportMicrochip];
    elPascal     : Result := cc + GLanguageHandler.Text[kExportPascal];
  else
    Result := cc + 'UNKNOWN!! (' + IntToStr(Ord(teo.Language)) + ')';
  end;
end;


class function TExportUtility.GetLineContent(teo : TExportOptions; aIncludeComment : boolean): string;
var
  cc : string;

begin
  cc := TitleWithComments('Line   : ', teo.Language, aIncludeComment);

  case teo.LineContent of
    lcRowCol : case teo.Source of
                 rsColumns : Result := cc + GLanguageHandler.Text[kColumn];
                 rsRows    : Result := cc + GLanguageHandler.Text[kRow];
               end;
    lcFrame  : Result := cc + GLanguageHandler.Text[kAnimationFrame];
    lcBytes  : Result := cc + IntToStr(teo.LineCount) + ' ' + GLanguageHandler.Text[kBytes];
  else
    Result := '';
  end;
end;


class function TExportUtility.GetLSB(teo : TExportOptions; aIncludeComment : boolean): string;
var
  cc : string;

begin
  cc := TitleWithComments('Bits   : ', teo.Language, aIncludeComment);

  case teo.Source of
    rsColumns : case teo.LSB of
                  llTopLeft     : Result := cc + GLanguageHandler.Text[kLSBatTop];
                  llBottomRight : Result := cc + GLanguageHandler.Text[kMSBatTop];
                end;
    rsRows    : case teo.LSB of
                  llTopLeft     : Result := cc + GLanguageHandler.Text[kLSBatLeft];
                  llBottomRight : Result := cc + GLanguageHandler.Text[kMSBatLeft];
                end;
  else
    Result := '';
  end;
end;


class function TExportUtility.GetNumberFormat(aLanguage : TExportLanguage; aNumberFormat : TNumberFormat): string;
begin
  case aLanguage of
    elCSV        : case aNumberFormat of
                     nfBinary : Result := '%';
                     nfHex    : Result := '$';
                   end;
    elPICAXE     : case aNumberFormat of
                     nfBinary : Result := '%';
                     nfHex    : Result := '$';
                   end;
    elC1Dim,
    elC2Dim      : case aNumberFormat of
                     nfBinary : Result := '0B';
                     nfHex    : Result := '0x';
                   end;
    elCFastLED   : case aNumberFormat of
                     nfBinary : Result := '0B';
                     nfHex    : Result := '0x';
                   end;
    elPython1Dim,
    elPython2Dim : case aNumberFormat of
                     nfBinary : Result := '0B';
                     nfHex    : Result := '0x';
                   end;
    elMicrochip  : case aNumberFormat of
                     nfBinary : Result := '%';
                     nfHex    : Result := '0x';
                   end;
    elPascal     : case aNumberFormat of
                     nfBinary : Result := '%';
                     nfHex    : Result := '$';
                   end;
  else
    Result := '';
  end;
end;


class function TExportUtility.GetNumberSize(aLanguage : TExportLanguage; aNumberSize : TNumberSize; aIncludeComment : boolean): string;
var
  cc : string;

begin
  cc := TitleWithComments('Size   : ', aLanguage, aIncludeComment);

  case aNumberSize of
    ns8Bit      : Result := cc + GLanguageHandler.Text[k8Bits];
    ns16bit     : Result := cc + GLanguageHandler.Text[k16Bits];
    ns32bit     : Result := cc + GLanguageHandler.Text[k32Bits];
    ns8bitSwap  : Result := cc + GLanguageHandler.Text[k8BitsNybblesSwapped];
    ns16bitSwap : Result := cc + GLanguageHandler.Text[k16BitsBytesSwapped];
    ns64bit     : Result := cc + GLanguageHandler.Text[k64Bits];
    nsRGB8bit   : Result := cc + GLanguageHandler.Text[k8Bits];
    nsRGB32bit  : Result := cc + GLanguageHandler.Text[k32Bits];
  else
    Result := '';
  end;
end;


class function TExportUtility.GetOrientation(teo : TExportOptions; aIncludeComment : boolean): string;
var
  cc : string;

begin
  cc := TitleWithComments('Order  : ', teo.Language, aIncludeComment);

  case teo.Source of
    rsColumns : case teo.Orientation of
                  ioTopBottomLeftRight : Result := cc + GLanguageHandler.Text[kLeftToRight];
                  ioBottomTopRightLeft : Result := cc + GLanguageHandler.Text[kRightToLeft];
                  ioSure24x16          : Result := cc + GLanguageHandler.Text[kSure24x16];
                else
                  Result := '';
                end;
    rsRows    : case teo.Orientation of
                  ioTopBottomLeftRight : Result := cc + GLanguageHandler.Text[kTopToBottom];
                  ioBottomTopRightLeft : Result := cc + GLanguageHandler.Text[kBottomToTop];
                else
                  Result := '';
                end;
  end;
end;


class procedure TExportUtility.GetPreamble(teo : TExportOptions; var aOutput : TStringList; aSimple : boolean);
var
  cc : string;

begin
  cc := GetCommentCharacter(teo.Language);

  aOutput.Add(cc + '=================================================================');
  aOutput.Add(cc + 'LED Matrix Studio - (c) Paul A Freshney 2021');
  aOutput.Add(cc);
  aOutput.Add(cc + 'www.MaximumOctopus.com');
  aOutput.Add(cc + 'www.MaximumOctopus.com/electronics/builder.htm');
  aOutput.Add(cc);
  aOutput.Add(cc + TUtility.CreatedDate);

  if not(aSimple) then begin

    if MatrixMain.Matrix.Comment <> '' then begin
      aOutput.Add(cc);
      aOutput.Add(cc + '-----------------------------------------------------------------');
      aOutput.Add(cc + GLanguageHandler.Text[kComment] + ':');
      aOutput.Add(cc + ' ' + MatrixMain.Matrix.Comment);
    end;
    if LMSSettings.App.DataFilename <> '' then begin
      aOutput.Add(cc);
      aOutput.Add(cc + '-----------------------------------------------------------------');
      aOutput.Add(cc + GLanguageHandler.Text[kOriginalFile] + ':');
      aOutput.Add(cc + ' ' + LMSSettings.App.DataFilename);
    end;

  end;

  aOutput.Add(cc);
  aOutput.Add(cc + '=================================================================');
  aOutput.Add(cc);

  if not(aSimple) then begin

    if teo.ExportMode = esAnimation then begin
      if teo.FontMode then
        aOutput.Add(cc + GLanguageHandler.Text[kFontCharacters] + ' ' + IntToStr(teo.StartFrame) + ' ' + GLanguageHandler.Text[kTo] + ' ' + IntToStr(teo.StartFrame + 95))
      else begin
        if teo.StartFrame = teo.EndFrame then
          aOutput.Add(cc + GLanguageHandler.Text[kAnimationFrame] + ' #' + IntToStr(teo.StartFrame))
        else
          aOutput.Add(cc + GLanguageHandler.Text[kAnimationFrame] + ' #' + IntToStr(teo.StartFrame) + ' ' + GLanguageHandler.Text[kTo] + ' #' + IntToStr(teo.EndFrame));

        if teo.Source = rsRows then
          aOutput.Add(cc + GLanguageHandler.Text[kRows] + ' #' + IntToStr(teo.SelectiveStart) + ' - #' + IntToStr(teo.SelectiveEnd))
        else
          aOutput.Add(cc + GLanguageHandler.Text[kColumns] + ' #' + IntToStr(teo.SelectiveStart) + ' - #' + IntToStr(teo.SelectiveEnd));
      end;
    end
    else begin
      aOutput.Add(cc + GLanguageHandler.Text[kMemoryBuffers] + ' #' + IntToStr(teo.StartFrame + 1) + ' ' + GLanguageHandler.Text[kTo] + ' #' + IntToStr(teo.EndFrame + 1));
    end;

    aOutput.Add(cc);
    aOutput.Add(cc + '=================================================================');
    aOutput.Add(cc);

    aOutput.Add(GetSource(teo.Language, teo.Source));
    aOutput.Add(GetLineContent(teo, True));
    aOutput.Add(GetLSB(teo, True));
    aOutput.Add(GetOrientation(teo, True));
    aOutput.Add(GetScanDirection(teo, True));

    if teo.RGBEnabled then begin
      aOutput.Add(cc);
      aOutput.Add(GetRGBMode(teo, True));
      aOutput.Add(GetRGBBrightness(teo, True));
    end
    else
      aOutput.Add(GetNumberSize(teo.Language, teo.NumberSize, True));
  end;
end;


class function TExportUtility.GetExampleCodeDisclaimer(teo : TExportOptions): string;
var
  cc : string;

begin
  cc := GetCommentCharacter(teo.Language);

  Result := #13#10;
  Result := Result + cc + '=================================================================' + #13#10;
  Result := Result + cc + '== Example code, use as a template, may not function 100%  ======' + #13#10;
  Result := Result + cc + '=================================================================' + #13#10;
  Result := Result + #13#10;
end;


class function TExportUtility.GetRGBMode(teo : TExportOptions; aIncludeComment : boolean): string;
var
  cc : string;

begin
  cc := TitleWithComments('Colour Format: ', teo.Language, aIncludeComment);

  case teo.RGBMode of
    cmRGB : Result := cc + 'RGB';
    cmBGR : Result := cc + 'BGR';
    cmGRB : Result := cc + 'GRB';
    cmBRG : Result := cc + 'BRG';
  end;
end;


class function TExportUtility.GetRGBBrightness(teo : TExportOptions; aIncludeComment : boolean): string;
var
  cc : string;

begin
  cc := TitleWithComments('Brightness   : ', teo.Language, aIncludeComment);

  if (teo.RGBBrightness <= 0) then
    Result := cc + IntToStr(teo.RGBBrightness) + '% - ' + GLanguageHandler.Text[kAreYouSure]
  else
    Result := cc + IntToStr(teo.RGBBrightness) + '%';
end;


class function TExportUtility.GetScanDirection(teo : TExportOptions; aIncludeComment : boolean): string;
var
  cc : string;

begin
  cc := TitleWithComments('Scan   : ', teo.Language, aIncludeComment);

  case teo.Source of
    rsColumns : case teo.ScanDirection of
                  scanColTopToBottom : Result := cc + GLanguageHandler.Text[kTopToBottom];
                  scanColBottomToTop : Result := cc + GLanguageHandler.Text[kBottomToTop];
                  scanColAltDownUp   : Result := cc + GLanguageHandler.Text[kAlternateDownUp];
                  scanColAltUpDown   : Result := cc + GLanguageHandler.Text[kAlternateUpDown];
                else
                  Result := '';
                end;
    rsRows    : case teo.ScanDirection of
                  scanRowLeftToRight  : Result := cc + GLanguageHandler.Text[kLeftToRight];
                  scanRowRightToLeft  : Result := cc + GLanguageHandler.Text[kRightToLeft];
                  scanRowAltLeftRight : Result := cc + GLanguageHandler.Text[kAlternateLeftRight];
                  scanRowAltRightLeft : Result := cc + GLanguageHandler.Text[kAlternateRightLeft];
                else
                  Result := '';
                end;
  end;
end;


class function TExportUtility.GetSource(aLanguage : TExportLanguage; aSaveType : TReadSource): string;
var
  cc : string;

begin
  cc := GetCommentCharacter(aLanguage) + 'Source : ';

  case aSaveType of
    rsColumns : Result := cc + GLanguageHandler.Text[kColumns];
    rsRows    : Result := cc + GLanguageHandler.Text[kRows];
  end;
end;


class procedure TExportUtility.GetSpacerLine(aLanguage : TExportLanguage; var aOutput : TStringList);
var
  cc : string;

begin
  cc := GetCommentCharacter(aLanguage);

  aOutput.Add(cc);
  aOutput.Add(cc + '=================================================================');
  aOutput.Add(cc);
end;


// only used when each line of data output includes a variable definition
// OR
// when there is only a single variable definition of the entire outpu

class function TExportUtility.GetSingleVariableStatement(aLanguage : TExportLanguage; aNumberSize : TNumberSize): string;
begin
  Result :=  GetVariableType(aLanguage, aNumberSize) + GetVariableID(aLanguage);
end;


class function TExportUtility.GetVariableID(aLanguage : TExportLanguage): string;
begin
  case aLanguage of
    elCSV,
    elPICAXE     : Result := '';
    elC1Dim      : Result := 'ledarray[] = {';
    elC2Dim      : Result := 'ledarray[][] = {';
    elCFastLED   : Result := '';
    elPython1Dim : Result := 'ledarray[] = [';
    elPython2Dim : Result := 'ledarray[][] = [';
    elMicrochip  : Result := '';
    elPascal     : Result := '';
    elSpecial    : Result := '';
  else
    Result := '';
  end
end;


class function TExportUtility.GetVariableIDFrameIn(aLanguage : TExportLanguage; aFrame : integer): string;
begin
  case aLanguage of
    elC1Dim,
    elCFastLED : Result := 'const long ledarray' + IntToStr(aFrame - 1) + '[] PROGMEM = {';
  else
    Result := '';
  end
end;


class function TExportUtility.GetVariableIDFrameOut(aLanguage : TExportLanguage): string;
begin
  case aLanguage of
    elC1Dim,
    elCFastLED : Result := '};';
  else
    Result := '';
  end
end;


class function TExportUtility.GetVariableType(aLanguage : TExportLanguage; aNumberSize : TNumberSize): string;
begin
  case aLanguage of
    elCSV,
    elPICAXE      : Result := '';
    elC1Dim,
    elC2Dim       : case aNumberSize of
                      ns8Bit      : Result := 'byte ';
                      ns16bit     : Result := 'word ';
                      ns32bit     : Result := 'long ';
                      ns8bitSwap  : Result := 'byte ';
                      ns16bitSwap : Result := 'word ';
                      ns64bit     : Result := 'uint64_t ';
                      nsRGB8bit   : Result := 'byte ';
                      nsRGB32bit  : Result := 'long ';
                    end;
    elCFastLED    : {};
    elPython1Dim,
    elPython2Dim  : Result := '';
    elMicrochip   : Result := '';
    elPascal      : Result := '';
    elSpecial     : Result := '';
  else
    Result := '';
  end;
end;


class function TExportUtility.GetPadding(aLanguage : TExportLanguage; aVariableDefinition : integer): string;
begin
  if (aVariableDefinition <> 0) then
    Result := TUtility.PadString(' ', aVariableDefinition)
  else begin
    Result := #9#9#9#9; // four tabs, customisable soon...
  end;
end;


end.
