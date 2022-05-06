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


unit exportoptions_monobi;


interface


uses System.SysUtils, System.Classes, dialogs,

     languagehandler,

     matrix, matrixdead,

     exportutility,

     exportoptions, utility,

     xglobal, matrixconstants;



type
  TExportMonoBi = class
    class function CreateExportAnimation(teo : TExportOptions; var aOutput : TStringList; var aEntryCount : integer; var aUniqueItems : TStringList): boolean;

    class function ExportColumnData(teo : TExportOptions; aFrame, aColId : integer; spacingchar : string): TDataOut;
    class function ExportRowData(teo : TExportOptions; aFrame, aRowId : integer; spacingchar : string): TDataOut;
  end;


implementation


class function TExportMonoBi.CreateExportAnimation(teo : TExportOptions; var aOutput : TStringList; var aEntryCount : integer; var aUniqueItems : TStringList): boolean;
var
  vartype, spacingstring : string;
  x, y, t, i, lc, rc, z : integer;
  prefix, op, cdescription : string;
  tdo : TDataOut;
  lMatrixData : array[0.._MaxHeight] of TStringList;
  lStart, lEnd, lInc : integer;

  procedure AddContentBySize(s : string; frame, rowcount : integer);
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
      elC2Dim      : if (rowcount = 0) then
                       aOutput.Add(teo.DataPadding + '{' + m + ',  // ' + cdescription + ' ' + IntToStr(frame))
                     else if (rowcount = -1) then
                       aOutput.Add(teo.DataPadding + ' ' + m + '},')
                     else
                       aOutput.Add(teo.DataPadding + ' ' + s);
      elCFastLED   : aOutput.Add(teo.DataPadding + s);
      elPython1Dim : aOutput.Add(teo.DataPadding + s + '');
      elPython2Dim : if (rowcount = 0) then
                       aOutput.Add(teo.DataPadding + '[' + s + '  # ' + cdescription + ' ' + IntToStr(frame))
                     else if (rowcount = -1) then
                       aOutput.Add(teo.DataPadding + ' ' + m + '],')
                     else
                       aOutput.Add(' ' + s);
      elMicrochip  : aOutput.Add('dt ' + m);
      elPascal     : aOutput.Add('data : array[0..__LEDCount] of integer = (' + m + ');');
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

  for t := 0 to _MaxHeight do
    lMatrixData[t] := TStringList.Create;

  aEntryCount := 0; // total of all entries added to data variable in output

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

  op := '';
  lc := 0;
  rc := 0;

  vartype := TExportUtility.GetVariableType(teo.Language, teo.NumberSize) +
             TExportUtility.GetVariableID(teo.Language);

  if vartype <> '' then begin
    aOutput.Add(vartype);
  end;

  teo.DataPadding := TUtility.PadString(' ', length(vartype));

  aEntryCount := 0; // total of all entries added to data variable in output

  // =========================================================================
  // =========================================================================

  for t := teo.StartFrame to teo.EndFrame do begin

    if (teo.language = elCFastLED) then
      aOutput.Add(TExportUtility.GetVariableIDFrameIn(teo.language, t));

    // =========================================================================

    for i := 0 to _MaxHeight do
      lMatrixData[i].Clear;

    if teo.Source = rsRows then begin
      for y := teo.SelectiveStart - 1 to teo.SelectiveEnd - 1 do begin
        tdo := ExportRowData(teo, t, y, spacingstring);

        for i := 0 to 7 do begin
          if tdo.data[i] <> '' then begin
            lMatrixData[y].Add(ProcessUnique(prefix + tdo.data[i]) + spacingstring)
          end;
        end;

        inc(aEntryCount, tdo.Count);
      end;
    end;

    if teo.Source = rsColumns then begin
      for x := teo.SelectiveStart - 1 to teo.SelectiveEnd - 1 do begin
        tdo := ExportColumnData(teo, t, x, spacingstring);

        for i := 0 to 7 do begin
          if tdo.data[i] <> '' then begin
            lMatrixData[x].Add(ProcessUnique(prefix + tdo.data[i]) + spacingstring)
          end;
        end;

        inc(aEntryCount, tdo.Count);
      end;
    end;

    // ===========================================================================
    // ===========================================================================
    // row data
    // ===========================================================================
    // ===========================================================================

    if (teo.LineContent) <> lcBytes then // maintain data when saving in blocks
      op := '';

    if teo.Source = rsRows then begin
      if teo.orientation = ioTopBottomLeftRight then begin
        lStart := teo.SelectiveStart - 1;
        lEnd   := teo.SelectiveEnd - 1;
        lInc   := 1;
      end
      else begin
        lStart := teo.SelectiveEnd - 1;
        lEnd   := teo.SelectiveStart - 1;
        lInc   := -1;
      end;

      y := lStart;

      while y <> 999 do begin
        if teo.LineContent = lcRowCol then
          op := '';

        for z := 0 to lMatrixData[y].Count - 1 do begin
          op := op + lMatrixData[y][z];

          if teo.LineContent = lcBytes then begin
            inc(lc);

            if lc = teo.LineCount then begin
              AddContentBySize(op, t, rc);

              lc := 0;
              op := '';
              inc(rc);
            end;
          end;
        end;

        if teo.LineContent = lcRowCol then begin
          TExportUtility.AddContentByRowCol(teo, op, aOutput);
        end;

        inc(y, lInc);

        if teo.orientation = ioTopBottomLeftRight then begin
          if (y > lEnd) or (y < 0) then
            y := 999
        end
        else begin
          if (y < lEnd) or (y < 0) then
            y := 999
        end;
      end;

      case teo.LineContent of
        lcFrame : TExportUtility.AddContentByFrame(teo, op, t, aOutput);
        lcBytes : begin
                    case teo.Language of
                      elC2Dim,
                      elPython2Dim : begin
                                       AddContentBySize(op, t, -1);

                                       op := '';
                                       lc := 0;
                                       rc := 0;
                                     end;
                    end;
                  end;
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
                  lStart := teo.SelectiveStart - 1;
                  lEnd   := teo.SelectiveEnd - 1;
                  lInc   := 1;
                end
                else begin
                  lStart := teo.SelectiveEnd - 1;
                  lEnd   := teo.SelectiveStart - 1;
                  lInc   := -1;
                end;

                y := lStart;

                while y <> 999 do begin
                  if teo.LineContent = lcRowCol then
                    op := '';

                  for z := 0 to lMatrixData[y].Count - 1 do begin
                    op := op + lMatrixData[y][z];

                    if teo.LineContent = lcBytes then begin
                      inc(lc);

                      if lc = teo.LineCount then begin
                        AddContentBySize(op, t, rc);

                        lc := 0;
                        op := '';
                        inc(rc);
                      end;
                    end;
                  end;

                  if teo.LineContent = lcRowCol then begin
                    TExportUtility.AddContentByRowCol(teo, op, aOutput);
                  end;

                  inc(y, lInc);

                  if teo.orientation = ioTopBottomLeftRight then begin
                    if (y > lEnd) or (y < 0) then
                      y := 999
                  end
                  else begin
                    if (y < lEnd) or (y < 0) then
                      y := 999
                  end;
                end;
              end;
        ioSure24x16 : begin
              for y := 7 downto 0 do begin
                for z := 0 to lMatrixData[y].Count - 1 do
                  op := op + lMatrixData[y][z];// + spacingstring;
              end;

              for y := 15 downto 8 do begin
                for z := 0 to lMatrixData[y].Count - 1 do
                  op := op + lMatrixData[y][z];// + spacingstring;
              end;

              for y := 23 downto 16 do begin
                for z := 0 to lMatrixData[y].Count - 1 do
                  op := op + lMatrixData[y][z];// + spacingstring;
              end;
            end;
      end;

      case teo.LineContent of
        lcFrame : TExportUtility.AddContentByFrame(teo, op, t, aOutput);
        lcBytes : begin
                    case teo.Language of
                      elC2Dim,
                      elPython2Dim : begin
                                       AddContentBySize(op, t, -1);

                                       op := '';
                                       lc := 0;
                                       rc := 0;
                                     end;
                    end;
                  end;
      end;
    end;

    if (teo.language = elCFastLED) then begin
      aOutput.Add(TExportUtility.GetVariableIDFrameOut(teo.language));

      aOutput.Add('');
    end;
  end;

  case teo.LineContent of
    lcBytes : if op <> '' then AddContentBySize(op, 0, rc);
  end;

  case teo.language of
    elC1Dim,
    elC2Dim       : aOutput.Add(teo.DataPadding + '};');
    elCFastLED      : {};
    elPython1Dim,
    elPython2Dim  : aOutput.Add(teo.DataPadding + ']');
  end;

  for t := 0 to _MaxHeight do
    lMatrixData[t].Free;

  if teo.IncludePreamble then begin
    TExportUtility.GetSpacerLine(teo.Language, aOutput);
  end;
end;


class function TExportMonoBi.ExportColumnData(teo : TExportOptions; aFrame, aColId : integer; spacingchar : string): TDataOut;
var
  s : string;
  nsbits, nspads : integer;
  bitcounter, dataindex, y, lScanDirection : integer;
  lInternalNumber : array[0..7] of Int64;
  lMatrixData : TMatrix;

begin
  Result.Count := 0;

  MatrixMain.CreateMatrixMerge;

  for y := 0 to 7 do begin
    lInternalNumber[y] := -1;
    Result.data[y] := '';
  end;

  nsbits := NumberSizes[Ord(teo.NumberSize)];
  nspads := NumberPadding[Ord(teo.NumberSize)];

  lScanDirection := teo.ScanDirection;

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

  bitcounter := 0;
  dataindex  := 0;
  lInternalNumber[dataindex] := 0;

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

  if lScanDirection = scanColTopToBottom then begin
    for y := 0 to MatrixMain.Matrix.Width - 1 do begin
      if MatrixMain.MatrixDead.Grid[aColId, y] = ptNormal then begin
        if lMatrixData.Grid[aColId, y] = 1 then begin
          if teo.LSB = llTopLeft then
            lInternalNumber[dataindex] := lInternalNumber[dataindex] + (powers[bitcounter])
          else
            lInternalNumber[dataindex] := lInternalNumber[dataindex] + (powers[nsbits - bitcounter]);
        end;

        if bitcounter = nsbits then begin
          bitcounter := 0;
          inc(dataindex);

          if (y <> MatrixMain.Matrix.Height - 1) then
            lInternalNumber[dataindex] := 0;
        end
        else
          inc(bitcounter);
      end;
    end;
  end
  else if lScanDirection = scanColBottomToTop then begin
    for y := MatrixMain.Matrix.Height - 1 downto 0 do begin
      if MatrixMain.MatrixDead.Grid[aColId, y] = ptNormal then begin
        if lMatrixData.Grid[aColId, y] = 1 then begin
          if teo.LSB = llTopLeft then
            lInternalNumber[dataindex] := lInternalNumber[dataindex] + (powers[bitcounter])
          else
            lInternalNumber[dataindex] := lInternalNumber[dataindex] + (powers[nsbits - bitcounter]);
        end;

        if bitcounter = nsbits then begin
          bitcounter := 0;
          inc(dataindex);

          if (y <> 0) then
            lInternalNumber[dataindex] := 0;
        end
        else
          inc(bitcounter);
      end;
    end;
  end;

  // ===========================================================================

  for y := 0 to 7 do begin
    if lInternalNumber[y] <> -1 then begin
      inc(Result.count);

      case teo.NumberSize of
        ns8bitSwap  : begin // swap nybbles
                        s := IntToHex(lInternalNumber[y], 2);

                        lInternalNumber[y] := TUtility.HexToInt(s[2] + s[1]);
                      end;
        ns16bitSwap : begin // swap bytes
                        s := IntToHex(lInternalNumber[y], 4);

                        lInternalNumber[y] := TUtility.HexToInt(s[3] + s[4] + s[1] + s[2]);
                      end;
      end;

      case teo.NumberFormat of
        nfDecimal : Result.data[y] := IntToStr(lInternalNumber[y]);
        nfBinary  : Result.data[y] := TUtility.IntegerToBinary(nsbits, lInternalNumber[y]);
        nfHex     : Result.data[y] := IntToHex(lInternalNumber[y], nspads);
      end;
    end;
  end;

  // ===========================================================================

  MatrixMain.FreeMatrixMerge;
end;


class function TExportMonoBi.ExportRowData(teo : TExportOptions; aFrame, aRowId : integer; spacingchar : string): TDataOut;
var
  s : string;
  nsbits, nspads : integer;
  bitcounter, dataindex, x, lScanDirection : integer;
  lInternalNumber : array[0..7] of Int64;
  lMatrixData : TMatrix;

begin
  Result.Count := 0;

  MatrixMain.CreateMatrixMerge;

  for x := 0 to 7 do begin
    lInternalNumber[x] := -1;
    Result.data[x]     := '';
  end;

  nsbits := NumberSizes[Ord(teo.NumberSize)];
  nspads := NumberPadding[Ord(teo.NumberSize)];

  lScanDirection := teo.ScanDirection;

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

  bitcounter := 0;
  dataindex  := 0;
  lInternalNumber[dataindex] := 0;

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
  end;

  // ===========================================================================

  if lScanDirection = scanRowLeftToRight then begin // left to right
    for x := 0 to MatrixMain.Matrix.Width - 1 do begin
      if MatrixMain.MatrixDead.Grid[x, aRowId] = ptNormal then begin
        if lMatrixData.Grid[x, aRowId] = 1 then begin
          if teo.LSB = llTopLeft then
            lInternalNumber[dataindex] := lInternalNumber[dataindex] + (powers[bitcounter])
          else
            lInternalNumber[dataindex] := lInternalNumber[dataindex] + (powers[nsbits - bitcounter]);
        end;

        if bitcounter = nsbits then begin
          bitcounter := 0;
          inc(dataindex);

          if (x <> MatrixMain.Matrix.Width - 1) then
            lInternalNumber[dataindex] := 0;
        end
        else
          inc(bitcounter);
      end;
    end;
  end
  else if lScanDirection = scanRowRightToLeft then begin                  // right to left
    for x := MatrixMain.Matrix.Width - 1 downto 0 do begin
      if MatrixMain.MatrixDead.Grid[x, aRowId] = ptNormal then begin
        if lMatrixData.Grid[x, aRowId] = 1 then begin
          if teo.LSB = llTopLeft then
            lInternalNumber[dataindex] := lInternalNumber[dataindex] + (powers[bitcounter])
          else
            lInternalNumber[dataindex] := lInternalNumber[dataindex] + (powers[nsbits - bitcounter]);
        end;

        if bitcounter=nsbits then begin
          bitcounter := 0;
          inc(dataindex);

          if (x <> 0) then
            lInternalNumber[dataindex] := 0;
        end
        else
          inc(bitcounter);
      end;
    end;
  end;

  // ===========================================================================

  for x := 0 to 7 do begin
    if lInternalNumber[x] <> -1 then begin
      inc(Result.count);

      case teo.NumberSize of
        ns8bitSwap  : begin // swap nybbles
                        s := IntToHex(lInternalNumber[x], 2);

                        lInternalNumber[x] := TUtility.HexToInt(s[2] + s[1]);
                      end;
        ns16bitSwap : begin // swap bytes
                        s := IntToHex(lInternalNumber[x], 4);

                        lInternalNumber[x] := TUtility.HexToInt(s[3] + s[4] + s[1] + s[2]);
                      end;
      end;

      case teo.NumberFormat of
        nfDecimal : Result.data[x] := IntToStr(lInternalNumber[x]);
        nfBinary  : Result.data[x] := TUtility.IntegerToBinary(nsbits, lInternalNumber[x]);
        nfHex     : Result.data[x] := IntToHex(lInternalNumber[x], nspads);
      end;
    end;
  end;

  // ===========================================================================

  MatrixMain.FreeMatrixMerge;
end;


end.
