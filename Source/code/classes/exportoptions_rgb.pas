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


unit exportoptions_rgb;


interface


uses System.SysUtils, System.Classes, dialogs, System.UITypes,

     languagehandler,

     matrix, matrixdead,

     exportutility,

     exportoptions, utility,

     xglobal, matrixconstants;


type
  TExportRGB = class
    class function  CreateExportAnimationRGB(teo : TExportOptions; var aOutput : TStringList; var entrycount : integer; var aUniqueItems : TStringList): boolean;

    class function  ExportColumnDataRGB(prefix : string; teo : TExportOptions; aFrame, aColId : integer; spacingchar : string): TDataOut;
    class function  ExportRowDataRGB(prefix : string; teo : TExportOptions; aFrame, aRowId : integer; spacingchar : string): TDataOut;
  end;


implementation


class function TExportRGB.CreateExportAnimationRGB(teo : TExportOptions; var aOutput : TStringList; var entrycount : integer; var aUniqueItems : TStringList): boolean;
var
  s, vartype, spacingstring  : string;
  x, y, t, i : integer;
  prefix, cdescription : string;
  tdo : TDataOut;
  matrixdata : array[0.._MaxHeight] of string;

  procedure AddContentByRowCol(s : string);
  var
    m : string;

  begin
    if (s = '') then
      Exit;

    m := Copy(s, 1, length(s) - 2); // trims last (and unnecessary) ", " from data

    case teo.Language of
      elCSV        : aOutput.Add(LMSSettings.App.OpenBracket + m + LMSSettings.App.CloseBracket + ';');
      elPICAXE     : aOutput.Add('EEPROM (' + m + ')');
      elC1Dim      : aOutput.Add(teo.DataPadding + s);
      elC2Dim      : aOutput.Add(teo.DataPadding + '{' + s + '},');
      elCFastLED   : aOutput.Add(teo.DataPadding + s);
      elPython1Dim : aOutput.Add(teo.DataPadding + s);
      elPython2Dim : aOutput.Add(teo.DataPadding + '[' + s + '],');
      elMicrochip  : aOutput.Add('dt ' + m);
      elPascal     : aOutput.Add('matrixdata : array[0..__LEDCount] of integer = (' + m + ');');
      elSpecial    : aOutput.Add(s);
    end;
  end;

  function ProcessUnique(s : string): string;
   var
    t : integer;

   begin
    if aUniqueItems.Count = 0 then
      Result := s
    else begin

      for t:= 0 to aUniqueItems.Count - 1 do begin
        s := StringReplace(s, aUniqueItems[t], IntToStr(t), [rfReplaceAll]);
      end;

      Result := s;
    end;
  end;


begin
  Result := True;

  entrycount := 0; // total of all entries added to data variable in output

  // ===========================================================================
  prefix := TExportUtility.GetNumberFormat(teo.Language, teo.NumberFormat);

  if teo.CleanMode then begin
    spacingstring := ' ';

    teo.Language  := elSpecial;
  end
  else
    spacingstring := ', ';

  // ===========================================================================

  if teo.IncludePreamble then begin

    if teo.ExportMode = esAnimation then
      cdescription := GLanguageHandler.Text[kFrame]
    else
      cdescription := GLanguageHandler.Text[kMemory];

    // =========================================================================

    TExportUtility.GetPreamble(teo, aOutput, False);

    TExportUtility.GetSpacerLine(teo.Language, aOutput);
    aOutput.Add('');
  end;

  // =========================================================================
  // =========================================================================
  // =========================================================================

  vartype := TExportUtility.GetSingleVariableStatement(teo.Language, teo.NumberSize);

  if vartype <> '' then begin
    aOutput.Add(vartype);
  end;

  teo.DataPadding := TExportUtility.GetPadding(teo.Language, length(vartype));

  // ===========================================================================
  // ===========================================================================

  for t := teo.StartFrame to teo.EndFrame do begin

    if (teo.language = elCFastLED) then
      aOutput.Add(TExportUtility.GetVariableIDFrameIn(teo.language, t));

    // =========================================================================

    for i := 0 to _MaxHeight do
      matrixdata[i] := '';

    if teo.Source = rsRows then begin
      for y := teo.SelectiveStart - 1 to teo.SelectiveEnd - 1 do begin
        tdo := ExportRowDataRGB(prefix, teo, t, y, spacingstring);

        matrixdata[y] := ProcessUnique(tdo.data[0]);

        inc(entrycount, tdo.count);
      end;
    end;

    if teo.Source = rsColumns then begin
      for x := teo.SelectiveStart - 1 to teo.SelectiveEnd - 1 do begin
        tdo := ExportColumnDataRGB(prefix, teo, t, x, spacingstring);

        matrixdata[x] := ProcessUnique(tdo.data[0]);

        inc(entrycount, tdo.count);
      end;
    end;

    // ===========================================================================
    // ===========================================================================
    // row data
    // ===========================================================================
    // ===========================================================================

    if teo.Source = rsRows then begin
      if teo.orientation = ioTopBottomLeftRight then begin
        s := '';

        for y := 0 to MatrixMain.Matrix.Height - 1 do begin
          case teo.LineContent of
            lcRowCol : AddContentByRowCol(matrixdata[y]);
            lcFrame  : s := s + matrixdata[y];
          end;
        end;

        if teo.LineContent = lcFrame then
           TExportUtility.AddContentByFrame(teo, s, t, aOutput);
      end
      else begin
        s := '';

        for y := MatrixMain.Matrix.Height - 1 downto 0 do begin
          case teo.LineContent of
            lcRowCol : AddContentByRowCol(matrixdata[y]);
            lcFrame  : s := s + matrixdata[y];
          end;
        end;

        if teo.LineContent = lcFrame then
           TExportUtility.AddContentByFrame(teo, s, t, aOutput);
      end;
    end;

    // ===========================================================================
    // col data
    // ===========================================================================

    if teo.Source = rsColumns then begin
      case teo.orientation of
        ioTopBottomLeftRight,
        ioBottomTopRightLeft : begin
                if teo.orientation = ioTopBottomLeftRight then begin
                  s := '';

                  for x := teo.SelectiveStart - 1 to teo.SelectiveEnd - 1 do begin
                    case teo.LineContent of
                      lcRowCol : AddContentByRowCol(matrixdata[x]);
                      lcFrame  : s := s + matrixdata[x];
                    end;
                  end;

                  if teo.LineContent = lcFrame then
                    TExportUtility.AddContentByFrame(teo, s, t, aOutput);
                end
                else begin
                  s := '';

                  for x := teo.SelectiveEnd - 1 downto teo.SelectiveStart - 1 do begin
                    case teo.LineContent of
                       lcRowCol : AddContentByRowCol(matrixdata[x]);
                       lcFrame  : s := s + matrixdata[x];
                    end;
                  end;

                  if teo.LineContent = lcFrame then
                    TExportUtility.AddContentByFrame(teo, s, t, aOutput);
                end;
              end;
        ioSure24x16 : begin
              s := GLanguageHandler.Text[kSure24x16BoardNotAvailableInRGBMode]; // sure 2416 not available in RGB!!
            end;
      end;
    end;

    // =========================================================================

    if (teo.language = elCFastLED) then begin
      aOutput.Add(TExportUtility.GetVariableIDFrameOut(teo.language));

      aOutput.Add('');
    end;
  end;

  case teo.language of
    elC1Dim,
    elC2Dim       : aOutput.Add(teo.DataPadding + '};');
    elCFastLED    : {};
    elPython1Dim,
    elPython2Dim  : aOutput.Add(teo.DataPadding + ']');
  end;

  if teo.IncludePreamble then begin
    TExportUtility.GetSpacerLine(teo.Language, aOutput);
  end;
end;


class function TExportRGB.ExportColumnDataRGB(prefix : string; teo : TExportOptions; aFrame, aColId : integer; spacingchar : string): TDataOut;
var
  output : string;
  lScanDirection : integer;
  lRowPixel : integer;
  lMatrixData : TMatrix;

begin
  Result.count   := 0;
  lScanDirection := teo.ScanDirection;

  MatrixMain.CreateMatrixMerge;

  // ===========================================================================

  if (teo.ExportMode = esAnimation) then begin
    if (MatrixMain.MatrixLayers.Count = 1) then
      lMatrixData := MatrixMain.MatrixLayers[0].Frames[aFrame]
    else begin
      MatrixMain.BuildMergedFrame(aFrame, 0);

      lMatrixData := MatrixMain.MatrixMerge;
    end;
  end
  else
    lMatrixData := MatrixMain.MatrixUser[aFrame];

  // ===========================================================================

  if teo.Orientation = ioTopBottomLeftRight then begin
    case lScanDirection of
      scanColAltDownUp : if odd(aColId) then
                           lScanDirection := scanColBottomToTop
                         else
                           lScanDirection := scanColTopToBottom;
      scanColAltUpDown : if odd(aColId) then
                           lScanDirection := scanColTopToBottom
                         else
                           lScanDirection := scanColBottomToTop;
    end;
  end
  else if teo.Orientation = ioBottomTopRightLeft then begin
    case lScanDirection of
      scanColAltDownUp : if odd(MatrixMain.Matrix.Width - aColId - 1) then
                           lScanDirection := scanColBottomToTop
                         else
                           lScanDirection := scanColTopToBottom;
      scanColAltUpDown : if odd(MatrixMain.Matrix.Width - aColId - 1) then
                           lScanDirection := scanColTopToBottom
                         else
                           lScanDirection := scanColBottomToTop;
    end;
  end;

  // ===========================================================================

  if lScanDirection = scanColTopToBottom then begin             // top to bottom
    for lRowPixel := 0 to MatrixMain.Matrix.Height - 1 do begin
      if MatrixMain.MatrixDead.Grid[aColId, lRowPixel] = ptNormal then begin
        if teo.NumberSize = nsRGB8bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[aColId, lRowPixel] = MatrixMain.RGBBackground) then
            output := output + TUtility.RGBConvertToSplit(teo.RGBChangeColour, teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, prefix, spacingchar)
          else
            output := output + TUtility.RGBConvertToSplit(lMatrixData.Grid[aColId, lRowPixel], teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, prefix, spacingchar);

          inc(Result.Count, 3);
        end
        else if teo.NumberSize = nsRGB32bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[aColId, lRowPixel] = MatrixMain.RGBBackground) then
            output := output + prefix + TUtility.RGBColourNumberFormat(teo.NumberFormat, 8, TUtility.RGBConvertTo(teo.RGBChangeColour, teo.RGBMode, teo.LSB,  teo.RGBBrightness))
          else
            output := output + prefix + TUtility.RGBColourNumberFormat(teo.NumberFormat, 8, TUtility.RGBConvertTo(lMatrixData.Grid[aColId, lRowPixel], teo.RGBMode, teo.LSB,  teo.RGBBrightness));

          output := output + spacingchar;

          inc(Result.Count);
        end;
      end;
    end;
  end
  else if lScanDirection = scanColBottomToTop then begin        // bottom to top
    for lRowPixel := MatrixMain.Matrix.Height - 1 downto 0 do begin
      if MatrixMain.MatrixDead.Grid[aColId, lRowPixel] = ptNormal then begin
        if teo.NumberSize = nsRGB8bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[aColId, lRowPixel] = MatrixMain.RGBBackground) then
            output := output + TUtility.RGBConvertToSplit(teo.RGBChangeColour, teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, prefix, spacingchar)
          else
            output := output + TUtility.RGBConvertToSplit(lMatrixData.Grid[aColId, lRowPixel], teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, prefix, spacingchar);

          inc(Result.Count, 3);
        end
        else if teo.NumberSize = nsRGB32bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[aColId, lRowPixel] = MatrixMain.RGBBackground) then
            output := output + prefix + TUtility.RGBColourNumberFormat(teo.NumberFormat, 8, TUtility.RGBConvertTo(teo.RGBChangeColour, teo.RGBMode, teo.LSB, teo.RGBBrightness))
          else
            output := output + prefix + TUtility.RGBColourNumberFormat(teo.NumberFormat, 8, TUtility.RGBConvertTo(lMatrixData.Grid[aColId, lRowPixel], teo.RGBMode, teo.LSB, teo.RGBBrightness));

          output := output + spacingchar;

          inc(Result.Count);
        end;
      end;
    end;
  end;

  // ===========================================================================

  MatrixMain.FreeMatrixMerge;

  // ===========================================================================

  Result.data[0] := output;
end;


class function TExportRGB.ExportRowDataRGB(prefix : string; teo : TExportOptions; aFrame, aRowId : integer; spacingchar : string): TDataOut;
var
  lOutput : string;
  lScanDirection : integer;
  lColumnPixel : integer;
  lMatrixData : TMatrix;

begin
  Result.count   := 0;
  lOutput        := '';
  lScanDirection := teo.ScanDirection;

  MatrixMain.CreateMatrixMerge;

  // ===========================================================================

  if (teo.ExportMode = esAnimation) then begin
    if (MatrixMain.MatrixLayers.Count = 1) then
      lMatrixData := MatrixMain.MatrixLayers[0].Frames[aFrame]
    else begin
      MatrixMain.BuildMergedFrame(aFrame, 0);

      lMatrixData := MatrixMain.MatrixMerge;
    end;
  end
  else
    lMatrixData := MatrixMain.MatrixUser[aFrame];

  // ===========================================================================

  if teo.Orientation = ioTopBottomLeftRight then begin
    case lScanDirection of
      scanRowAltLeftRight : if odd(aRowId) then
                                lScanDirection := scanRowRightToLeft
                              else
                                lScanDirection := scanRowLeftToRight;
      scanRowAltRightLeft : if odd(aRowId) then
                                lScanDirection := scanRowLeftToRight
                              else
                                lScanDirection := scanRowRightToLeft;
    end;
  end
  else if teo.Orientation = ioBottomTopRightLeft then begin
    case lScanDirection of
      scanRowAltLeftRight : if odd(MatrixMain.Matrix.Height - aRowId - 1) then
                                lScanDirection := scanRowRightToLeft
                              else
                                lScanDirection := scanRowLeftToRight;
      scanRowAltRightLeft : if odd(MatrixMain.Matrix.Height - aRowId - 1) then
                                lScanDirection := scanRowLeftToRight
                              else
                                lScanDirection := scanRowRightToLeft;
    end;
  end
  else
    MessageDlg('Error, unknown orientation' + IntToStr(Ord(teo.Orientation)), mtError, [mbOK], 0);

  // ===========================================================================

  if lScanDirection = scanRowLeftToRight then begin        // left to right
    for lColumnPixel := 0 to MatrixMain.Matrix.Width - 1 do begin
      if MatrixMain.MatrixDead.Grid[lColumnPixel, aRowId] = ptNormal then begin
        if teo.NumberSize = nsRGB8bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[lColumnPixel, aRowId] = MatrixMain.RGBBackground) then
            lOutput := lOutput + TUtility.RGBConvertToSplit(teo.RGBChangeColour, teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, prefix, spacingchar)
          else
            lOutput := lOutput + TUtility.RGBConvertToSplit(lMatrixData.Grid[lColumnPixel, aRowId], teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, prefix, spacingchar);

          inc(Result.Count, 3);
        end
        else if teo.NumberSize = nsRGB32bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[lColumnPixel, aRowId] = MatrixMain.RGBBackground) then
            lOutput := lOutput + prefix + TUtility.RGBColourNumberFormat(teo.NumberFormat, 8, TUtility.RGBConvertTo(teo.RGBChangeColour, teo.RGBMode, teo.LSB, teo.RGBBrightness))
          else
            lOutput := lOutput + prefix + TUtility.RGBColourNumberFormat(teo.NumberFormat, 8, TUtility.RGBConvertTo(lMatrixData.Grid[lColumnPixel, aRowId], teo.RGBMode, teo.LSB, teo.RGBBrightness));

          lOutput := lOutput + spacingchar;

          inc(Result.Count);
        end;
      end;
    end;
  end
  else if lScanDirection = scanRowRightToLeft then begin        // right to left
    for lColumnPixel := MatrixMain.Matrix.Width - 1 downto 0 do begin
      if MatrixMain.MatrixDead.Grid[lColumnPixel, aRowId] = ptNormal then begin
        if teo.NumberSize = nsRGB8bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[lColumnPixel, aRowId] = MatrixMain.RGBBackground) then
            lOutput := lOutput + TUtility.RGBConvertToSplit(teo.RGBChangeColour, teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, prefix, spacingchar)
          else
            lOutput := lOutput + TUtility.RGBConvertToSplit(lMatrixData.Grid[lColumnPixel, aRowId], teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, prefix, spacingchar);

          inc(Result.Count, 3);
        end
        else if teo.NumberSize = nsRGB32bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[lColumnPixel, aRowId] = MatrixMain.RGBBackground) then
            lOutput := lOutput + prefix + TUtility.RGBColourNumberFormat(teo.NumberFormat, 8, TUtility.RGBConvertTo(teo.RGBChangeColour, teo.RGBMode, teo.LSB, teo.RGBBrightness))
          else
            lOutput := lOutput + prefix + TUtility.RGBColourNumberFormat(teo.NumberFormat, 8, TUtility.RGBConvertTo(lMatrixData.Grid[lColumnPixel, aRowId], teo.RGBMode, teo.LSB, teo.RGBBrightness));

          lOutput := lOutput + spacingchar;

          inc(Result.Count);
        end;
      end;
    end;
  end;

  // ===========================================================================

  MatrixMain.FreeMatrixMerge;

  // ===========================================================================

  Result.data[0] := lOutput;
end;


end.
