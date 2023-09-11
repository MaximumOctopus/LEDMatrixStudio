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

    // used only for quick GUI-based output
    class function SimpleExportMono(aFrame, aSourceLSB, aSource, aSourceDirection : integer; aHexFormat, aCombineNibbles, aSourceDisplayVisible : boolean): TDataOutDisplay;
    class function SimpleExportBiSequential(aFrame, aSourceLSB, aSource, aSourceDirection : integer; aHexFormat, aSourceDisplayVisible : boolean): TDataOutDisplay;
    class function SimpleExportBiBitplanes(aFrame, aSourceLSB, aSource, aSourceDirection : integer; aHexFormat, aSourceDisplayVisible : boolean): TDataOutDisplay;
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

        for i := 0 to tdo.Count - 1 do begin
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

        for i := 0 to tdo.Count - 1 do begin
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
    elCFastLED    : {};
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
  lInternalNumber : array[0..DataOutDataMax] of Int64;
  lMatrixData : TMatrix;

begin
  Result.Count := 0;

  MatrixMain.CreateMatrixMerge;

  for y := 0 to DataOutDataMax do begin
    lInternalNumber[y] := -1;
    Result.Data[y] := '';
  end;

  Result.Count := 0;

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

  Result.Count := DataIndex;

  // ===========================================================================

  for y := 0 to Result.Count - 1 do begin
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
  lInternalNumber : array[0..DataOutDataMax] of Int64;
  lMatrixData : TMatrix;

begin
  Result.Count := 0;

  MatrixMain.CreateMatrixMerge;

  for x := 0 to DataOutDataMax do begin
    lInternalNumber[x] := -1;
    Result.Data[x]     := '';
  end;

  Result.Count := 0;

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

        if bitcounter = nsbits then begin
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

  Result.Count := dataindex;

  // ===========================================================================

  for x := 0 to Result.Count - 1 do begin
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


class function TExportMonoBi.SimpleExportMono(aFrame, aSourceLSB, aSource, aSourceDirection : integer; aHexFormat, aCombineNibbles, aSourceDisplayVisible : boolean): TDataOutDisplay;
var
  x, y, mydata : integer;
  s : string;

begin
  for y := 0 to MatrixMain.Matrix.Height - 1 do begin
    mydata := 0;

    for x := 0 to MatrixMain.Matrix.Width - 1 do begin
      if MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, y] = 1 then begin
        if (aSourceLSB = 0) then
          mydata := mydata + (powers[x])
        else
          mydata := mydata + (powers[MatrixMain.Matrix.Width - x - 1])
       end;
    end;

    if (aHexFormat) then
      s := IntToHex(mydata, LMSSettings.App.PadModeHexRow)
    else
      s := IntToStr(mydata);

    Result.RowData[y] := s;
  end;

  for x := 0 to MatrixMain.Matrix.Width - 1 do begin
    mydata := 0;

    for y := 0 to MatrixMain.Matrix.Height - 1 do begin
      if MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, y] = 1 then begin
        if (aSourceLSB = 0) then
          mydata := mydata + (powers[y])
        else
          mydata := mydata + (powers[MatrixMain.Matrix.Height - y - 1]);
      end;
    end;

    if (aHexFormat) then
      s := IntToHex(mydata, LMSSettings.App.PadModeHexCol)
    else
      s := IntToStr(mydata);

    Result.ColumnData[x] := s;
  end;

  // ===========================================================================
  // Need to display anything?
  // ===========================================================================

  if not(aSourceDisplayVisible) then exit;

  s := '';

  if (aSource = 0) then begin

    // =================================================================
    // Row data
    // =================================================================

    if (aSourceDirection = 0) then begin
      s := LMSSettings.App.OpenBracket;

      for y := 0 to MatrixMain.Matrix.Height - 2 do begin
        s := s + LMSSettings.App.HexPrefix + Result.RowData[y] + ', ';
      end;

      s := s + LMSSettings.App.HexPrefix + Result.RowData[MatrixMain.Matrix.Height - 1] + LMSSettings.App.CloseBracket;
    end
    else begin
      s := LMSSettings.App.OpenBracket;

      for y := MatrixMain.Matrix.Height - 1 downto 1 do begin
        s := s + LMSSettings.App.HexPrefix + Result.RowData[y] + ', ';
      end;

      s := s + LMSSettings.App.HexPrefix + Result.RowData[0] + LMSSettings.App.CloseBracket;
    end;

    Result.Text := s;
  end
  else begin

    // =================================================================
    // Column data
    // =================================================================

    case aSourceDirection of
      0 : begin
            s := LMSSettings.App.OpenBracket;

            if (aCombineNibbles) then begin
              x := 0;

              while x <= MatrixMain.Matrix.Width - 2 do begin
                s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + Result.ColumnData[x + 1] + ', ';

                inc(x, 2);
              end;

              s := Copy(s, 1, length(s) - 2) + LMSSettings.App.CloseBracket;
            end
            else begin
              for x := 0 to MatrixMain.Matrix.Width - 2 do begin
                s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
              end;

              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[MatrixMain.Matrix.Width - 1] + LMSSettings.App.CloseBracket;
            end;
          end;
      1 : begin
            s := LMSSettings.App.OpenBracket;

            if (aCombineNibbles) then begin
              x := MatrixMain.Matrix.Width - 1;

              while x >= 0 do begin
                s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + Result.ColumnData[x - 1] + ', ';

                dec(x, 2);
              end;

              s := Copy(s, 1, length(s ) - 2) + LMSSettings.App.CloseBracket;
            end
            else begin
              for x := MatrixMain.Matrix.Width - 1 downto 1 do begin
                s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
              end;

              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[0] + LMSSettings.App.CloseBracket;
            end;
          end;
      2 : begin
            s := LMSSettings.App.OpenBracket;

            for x := 7 downto 0 do begin
              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
            end;

            for x := 15 downto 8 do begin
              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
            end;

            for x := 23 downto 17 do begin
              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
            end;

            s := s + LMSSettings.App.HexPrefix + Result.ColumnData[16] + LMSSettings.App.CloseBracket;
          end;
    end;

    Result.Text := s;
  end;
end;


class function TExportMonoBi.SimpleExportBiSequential(aFrame, aSourceLSB, aSource, aSourceDirection : integer; aHexFormat, aSourceDisplayVisible : boolean): TDataOutDisplay;
var
  x, y : integer;
  s, temp : string;

  function BinToInt(s : string): int64;
  var
    i, t : integer;

  begin
    result := 0;

    i := 0;

    for t := length(s) downto 1 do begin
      if s[t] = '1' then
        result := result + powers[i];

      inc(i);
    end;
  end;

begin
  for y := 0 to MatrixMain.Matrix.Height - 1 do begin
    temp := '';

    for x := 0 to MatrixMain.Matrix.Width - 1 do begin
      if (aSourceLSB = 0) then
        temp := temp + BiColoursLSBLeft[MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, y]]
      else
        temp := temp + BiColoursLSBRight[MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[MatrixMain.Matrix.Width - x - 1, y]];
    end;

    if (aHexFormat) then
      s := IntToHex(BinToInt(temp), LMSSettings.App.PadModeHexRow)
    else
      s := IntToStr(BinToInt(temp));

    Result.RowData[y] := s;
  end;

  for x := 0 to MatrixMain.Matrix.Width - 1 do begin
    temp := '';

    for y := 0 to MatrixMain.Matrix.Height - 1 do begin
      if (aSourceLSB = 0) then
        temp := temp + BiColoursLSBLeft[MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, y]]
      else
        temp := temp + BiColoursLSBRight[MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, MatrixMain.Matrix.Height - y - 1]];
    end;

    if (aHexFormat) then
      s := IntToHex(BinToInt(temp), LMSSettings.App.PadModeHexCol)
    else
      s := IntToStr(BinToInt(temp));

    Result.ColumnData[x] := s;
  end;

  // ===========================================================================
  // Need to display anything?
  // ===========================================================================

  if not(aSourceDisplayVisible) then exit;

    s := '';

    if (aSource = 0) then begin

      // ===========================================================================
      // Row data
      // ===========================================================================

      if (aSourceDirection = 0) then begin
        s := LMSSettings.App.OpenBracket;

        for y := 0 to MatrixMain.Matrix.Height - 2 do begin
          s := s + LMSSettings.App.HexPrefix + Result.RowData[y] + ', ';
        end;

        s := s + LMSSettings.App.HexPrefix + Result.RowData[MatrixMain.Matrix.Height - 1] + LMSSettings.App.CloseBracket;
      end
      else begin
        s := LMSSettings.App.OpenBracket;

        for y := MatrixMain.Matrix.Height - 1 downto 1 do begin
          s := s + LMSSettings.App.HexPrefix + Result.RowData[y] + ', ';
        end;

        s := s + LMSSettings.App.HexPrefix + Result.RowData[0] + LMSSettings.App.CloseBracket;
      end;

      Result.Text := s;
    end
    else begin

    // ===========================================================================
    // Column data
    // ===========================================================================

     case (aSourceDirection) of
       0 : begin
             s := LMSSettings.App.OpenBracket;

             for x := 0 to MatrixMain.Matrix.Width - 2 do begin
               s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
             end;

             s := s + LMSSettings.App.HexPrefix + Result.ColumnData[MatrixMain.Matrix.Width - 1] + LMSSettings.App.CloseBracket;
           end;
       1 : begin
             s := LMSSettings.App.OpenBracket;

             for x := MatrixMain.Matrix.Width - 1 downto 1 do begin
               s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
             end;

             s :=s + LMSSettings.App.HexPrefix + Result.ColumnData[0] + LMSSettings.App.CloseBracket;
           end;
       2 : begin
             s := LMSSettings.App.OpenBracket;

             for x := 7 downto 0 do begin
               s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
             end;

             for x := 15 downto 8 do begin
               s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
             end;

             for x := 23 downto 17 do begin
               s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
             end;

             s := s + LMSSettings.App.HexPrefix + Result.ColumnData[16] + LMSSettings.App.CloseBracket;
           end;
     end;

     Result.Text := s;
   end;
end;


class function TExportMonoBi.SimpleExportBiBitplanes(aFrame, aSourceLSB, aSource, aSourceDirection : integer; aHexFormat, aSourceDisplayVisible : boolean): TDataOutDisplay;
var
  x, y, bitplane : integer;
  s  : string;

  function BinToInt(s : string): int64;
  var
    i, t : integer;

  begin
    result := 0;

    i := 0;

    for t := length(s) downto 1 do begin
      if s[t] = '1' then
        result := result + powers[i];

      inc(i);
    end;
  end;

begin
  for y := 0 to MatrixMain.Matrix.Height - 1 do begin
    bitplane := 0;

    for x := 0 to MatrixMain.Matrix.Width - 1 do begin
      if (aSourceLSB = 0) then begin
        case MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, y] of
          0 : {};
          1 : bitplane := bitplane + (powers[x]);
          2 : bitplane := bitplane + (powers[x + (MatrixMain.Matrix.Width - 1)]);
          3 : begin
                bitplane := bitplane + (powers[x]);
                bitplane := bitplane + (powers[x + (MatrixMain.Matrix.Width - 1)]);
              end;
        end;
      end
      else begin
        case MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, y] of
          0 : {};
          1 : bitplane := bitplane + (powers[MatrixMain.Matrix.Width - x - 1]);
          2 : bitplane := bitplane + (powers[MatrixMain.Matrix.Width - x - 1 + (MatrixMain.Matrix.Width - 1)]);
          3 : begin
                bitplane := bitplane + (powers[MatrixMain.Matrix.Width - x - 1]);
                bitplane := bitplane + (powers[MatrixMain.Matrix.Width - x - 1 + (MatrixMain.Matrix.Width - 1)]);
              end;
        end;
      end;
    end;

    if (aHexFormat) then
      s := IntToHex(bitplane, LMSSettings.App.PadModeHexRow)
    else
      s := IntToStr(bitplane);

    Result.RowData[y] := s;
  end;

  for x := 0 to MatrixMain.Matrix.Width - 1 do begin
    bitplane := 0;

    for y := 0 to MatrixMain.Matrix.Height - 1 do begin
      if (aSourceLSB = 0) then begin
        case MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, y] of
          0 : {};
          1 : bitplane := bitplane + (powers[y]);
          2 : bitplane := bitplane + (powers[y + (MatrixMain.Matrix.Height - 1)]);
          3 : begin
                bitplane := bitplane + (powers[y]);
                bitplane := bitplane + (powers[y + (MatrixMain.Matrix.Height - 1)]);
              end;
        end;
      end
      else begin
        case MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, y] of
          0 : {};
          1 : bitplane := bitplane + (powers[MatrixMain.Matrix.Height - y - 1]);
          2 : bitplane := bitplane + (powers[MatrixMain.Matrix.Height - y - 1 + (MatrixMain.Matrix.Height - 1)]);
          3 : begin
                bitplane := bitplane + (powers[MatrixMain.Matrix.Height - y - 1]);
                bitplane := bitplane + (powers[MatrixMain.Matrix.Height - y - 1 + (MatrixMain.Matrix.Height - 1)]);
              end;
        end;
      end;
    end;

    if (aHexFormat) then
      s := IntToHex(bitplane, LMSSettings.App.PadModeHexCol)
    else
      s := IntToStr(bitplane);

    Result.ColumnData[x] := s;
  end;

  // ===========================================================================
  // Need to display anything?
  // ===========================================================================

  if not(aSourceDisplayVisible) then Exit;

  s := '';

  if (aSource = 0) then begin

    // ===========================================================================
    // Row data
    // ===========================================================================

    if (aSourceDirection = 0) then begin
      s := LMSSettings.App.OpenBracket;

      for y := 0 to MatrixMain.Matrix.Height - 2 do begin
        s := s + LMSSettings.App.HexPrefix + Result.RowData[y] + ', ';
      end;

      s := s + LMSSettings.App.HexPrefix + Result.RowData[MatrixMain.Matrix.Height - 1] + LMSSettings.App.CloseBracket;
    end
    else begin
      s := LMSSettings.App.OpenBracket;

      for y := MatrixMain.Matrix.Height - 1 downto 1 do begin
        s := s + LMSSettings.App.HexPrefix + Result.RowData[y] + ', ';
      end;

      s := s + LMSSettings.App.HexPrefix + Result.RowData[0] + LMSSettings.App.CloseBracket;
    end;

    Result.Text := s;
  end
  else begin

    // ===========================================================================
    // Column data
    // ===========================================================================

    case (aSourceDirection) of
      0 : begin
            s := LMSSettings.App.OpenBracket;

            for x := 0 to MatrixMain.Matrix.Width - 2 do begin
              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
            end;

            s := s + LMSSettings.App.HexPrefix + Result.ColumnData[MatrixMain.Matrix.Width - 1] + LMSSettings.App.CloseBracket;
          end;
      1 : begin
            s := LMSSettings.App.OpenBracket;

            for x := MatrixMain.Matrix.Width - 1 downto 1 do begin
              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
            end;

            s := s + LMSSettings.App.HexPrefix + Result.ColumnData[0] + LMSSettings.App.CloseBracket;
          end;
      2 : begin
            s := LMSSettings.App.OpenBracket;

            for x := 7 downto 0 do begin
              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
            end;

            for x := 15 downto 8 do begin
              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
            end;

            for x := 23 downto 17 do begin
              s := s + LMSSettings.App.HexPrefix + Result.ColumnData[x] + ', ';
            end;

            s := s + LMSSettings.App.HexPrefix + Result.ColumnData[16] + LMSSettings.App.CloseBracket;
          end;
    end;

    Result.Text := s;
  end;
end;


end.
