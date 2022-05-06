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

unit exportoutputbinary;

interface


uses classes, System.Contnrs,

     matrix, matrixdead,

     utility, xglobal, exportoptions,

     matrixconstants;


function  BinaryCreateExportAnimation(teo : TExportOptions; var aOutput : TStringList; var entrycount : integer; var aUniqueItems : TStringList): boolean;
function  BinaryCreateExportAnimationRGB(teo : TExportOptions; var aOutput : TStringList; var entrycount : integer; var aUniqueItems : TStringList): boolean;

function  BinaryExportRowData(teo : TExportOptions; aFrame, aRowId : integer; spacingchar : string): TDataOut;
function  BinaryExportRowDataRGB(teo : TExportOptions; aFrame, aRowId : integer; spacingchar : string): TDataOut;

function  BinaryExportColumnData(teo : TExportOptions; aFrame, aColId : integer; spacingchar : string): TDataOut;
function  BinaryExportColumnDataRGB(teo : TExportOptions; aFrame, aColId : integer; spacingchar : string): TDataOut;

function  BinaryGetRowData(hexmode : boolean; direction, aFrame, aRowId : integer): string;
function  BinaryGetColumnData(hexmode : boolean; direction, aFrame, aColId : integer): string;
procedure BinaryAddContentByFrame(teo : TExportOptions; s : string; frame : integer; var aOutput : TStringList);


implementation


uses thematrix, SysUtils;


function BinaryCreateExportAnimation(teo : TExportOptions; var aOutput : TStringList; var entrycount : integer; var aUniqueItems : TStringList): boolean;
var
  spacingstring : string;
  x, y, t, i, z : integer;
  op : string;
  tdo : TDataOut;
  matrixdata : array[0.._MaxHeight] of TStringList;
  zStart, zInc : integer;

  function ProcessUnique(s : string): string;
  var
    t : integer;

   begin
    if aUniqueItems.Count = 0 then
      Result := s
    else begin
      for t := 0 to aUniqueItems.Count - 1 do begin
        s := StringReplace(s, aUniqueItems[t], IntToStr(t), [rfReplaceAll]);
      end;

      Result := s;
    end;
  end;


begin
  Result := True;

  for t := 0 to _MaxHeight do
    matrixdata[t] := TStringList.Create;

  entrycount := 0; // total of all entries added to data variable in output    

  // ===========================================================================

  spacingstring := ' ';

  // =========================================================================

  op := '';

  teo.DataPadding := '';//PadString(' ', length(vartype));

  entrycount      := 0; // total of all entries added to data variable in output

  // =========================================================================
  // =========================================================================

  for t := teo.StartFrame to teo.EndFrame do begin
    for i := 0 to _MaxHeight do
      matrixdata[i].Clear;


    if teo.Source = rsRows then begin
      for y := 0 to MatrixMain.Matrix.Height - 1 do begin
        tdo := BinaryExportRowData(teo, t, y, spacingstring);

        for i:=0 to 7 do begin
          if tdo.data[i] <> '' then begin
            matrixdata[y].Add(ProcessUnique(tdo.data[i]) + spacingstring)
          end;
        end;

        inc(entrycount, tdo.count);
      end;
    end;

    if teo.Source = rsColumns then begin
      for x := 0 to MatrixMain.Matrix.Width - 1 do begin
        tdo := BinaryExportColumnData(teo, t, x, spacingstring);

        for i := 0 to 7 do begin
          if tdo.data[i] <> '' then begin
            matrixdata[x].Add(ProcessUnique(tdo.data[i]) + spacingstring)
          end;
        end;

        inc(entrycount, tdo.count);        
      end;
    end;

    // ===========================================================================
    // ===========================================================================
    // row data
    // ===========================================================================
    // ===========================================================================

    op := '';

    if teo.Source = rsRows then begin
      if teo.orientation = ioTopBottomLeftRight then begin
        zStart := 0;
        zInc   := 1;
      end
      else begin
        zStart := MatrixMain.Matrix.Height - 1;
        zInc   := -1;
      end;

      y := zStart;

      while y <> 99 do begin
        for z := 0 to matrixdata[y].Count - 1 do begin
          op := op + matrixdata[y][z];
        end;

        inc(y, zInc);

        if (y > MatrixMain.Matrix.Height - 1) or (y < 0) then
          y := 99;
      end;

      BinaryAddContentByFrame(teo, op, t, aOutput);
    end;

    // ===========================================================================
    // col data
    // ===========================================================================

    if teo.Source = rsColumns then begin
      case teo.orientation of
        ioTopBottomLeftRight,
        ioBottomTopRightLeft : begin
                if teo.orientation = ioTopBottomLeftRight then begin
                  zStart:= 0;
                  zInc  := 1;
                end
                else begin
                  zStart := MatrixMain.Matrix.Width - 1;
                  zInc   := -1;
                end;

                y := zStart;

                while y <> 99 do begin
                  for z := 0 to matrixdata[y].Count - 1 do begin
                    op := op + matrixdata[y][z];
                  end;

                  inc(y, zInc);

                  if (y > MatrixMain.Matrix.Width - 1) or (y < 0) then
                    y := 99;
                end;
              end;
        ioSure24x16 : begin
                        for y := 7 downto 0 do begin
                          for z := 0 to matrixdata[y].Count - 1 do
                            op := op + matrixdata[y][z] + spacingstring;
                        end;

                        for y := 15 downto 8 do begin
                          for z := 0 to matrixdata[y].Count - 1 do
                            op := op + matrixdata[y][z] + spacingstring;
                        end;

                        for y := 23 downto 16 do begin
                          for z := 0 to matrixdata[y].Count - 1 do
                            op := op + matrixdata[y][z] + spacingstring;
                        end;
                      end;
      end;

      BinaryAddContentByFrame(teo, op, t, aOutput);
    end;
  end;

  for t := 0 to _MaxHeight do
    matrixdata[t].Free;
end;


function BinaryCreateExportAnimationRGB(teo : TExportOptions; var aOutput : TStringList; var entrycount : integer; var aUniqueItems : TStringList): boolean;
var
  s, spacingstring  : string;
  x, y, t, i : integer;
  tdo : TDataOut;
  matrixdata : array[0.._MaxHeight] of string;

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
    matrixdata[t] := '';

  entrycount      := 0;

  spacingstring   := ' ';

  teo.DataPadding := '';

  // =========================================================================
  // =========================================================================

  for t := teo.StartFrame to teo.EndFrame do begin
    for i := 0 to _MaxHeight do
      matrixdata[i] := '';

    if teo.Source = rsRows then begin
      for y := 0 to MatrixMain.Matrix.Height - 1 do begin
        tdo := BinaryExportRowDataRGB(teo, t, y, spacingstring);

        matrixdata[y] := ProcessUnique(tdo.data[0]);

        inc(entrycount, tdo.count);
      end;
    end;

    if teo.Source = rsColumns then begin
      for x := 0 to MatrixMain.Matrix.Width - 1 do begin
        tdo := BinaryExportColumnDataRGB(teo, t, x, spacingstring);

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
          s := s + matrixdata[y];
        end;

        BinaryAddContentByFrame(teo, s, t, aOutput);
      end
      else begin
        s := '';

        for y := MatrixMain.Matrix.Height - 1 downto 0 do begin
          s := s + matrixdata[y];
        end;

        BinaryAddContentByFrame(teo, s, t, aOutput);
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

                  for x := 0 to MatrixMain.Matrix.Width - 1 do begin
                    s := s + matrixdata[x];
                  end;

                  BinaryAddContentByFrame(teo, s, t, aOutput);
                end
                else begin
                  s := '';

                  for x := MatrixMain.Matrix.Width - 1 to 0 do begin
                    s := s + matrixdata[x];
                  end;

                  BinaryAddContentByFrame(teo, s, t, aOutput);
                end;
              end;
        ioSure24x16 : begin
                        // sure 2416 not available in RGB!!
                      end;
      end;
    end;
  end;
end;


function BinaryExportRowData(teo : TExportOptions; aFrame, aRowId : integer; spacingchar : string): TDataOut;
var
  s : string;
  nsbits, nspads : integer;
  bitcounter, dataindex, x, lScanDirection : integer;
  internalnumber : array[0..7] of Int64;
  lMatrixData : TMatrix;

begin
  Result.count := 0;

  for x := 0 to 7 do begin
    internalnumber[x] := -1;
    Result.data[x]    := '';
  end;

  nsbits := NumberSizes[Ord(teo.NumberSize)];
  nspads := NumberPadding[Ord(teo.NumberSize)];

  lScanDirection := teo.ScanDirection;

  // ===========================================================================

  if teo.ExportMode = esAnimation then
    lMatrixData := MatrixMain.MatrixLayers[0].Frames[aFrame]
  else
    lMatrixData := MatrixMain.MatrixUser[aFrame];

  // ===========================================================================

  bitcounter := 0;
  dataindex  := 0;
  internalnumber[dataindex] := 0;

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
            internalnumber[dataindex] := internalnumber[dataindex] + (powers[bitcounter])
          else
            internalnumber[dataindex] := internalnumber[dataindex] + (powers[nsbits - bitcounter]);
        end;

        if bitcounter = nsbits then begin
          bitcounter := 0;
          inc(dataindex);

          if (x <> MatrixMain.Matrix.Width - 1) then
            internalnumber[dataindex] := 0;

          inc(Result.Count);
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
            internalnumber[dataindex] := internalnumber[dataindex] + (powers[bitcounter])
          else
            internalnumber[dataindex] := internalnumber[dataindex] + (powers[nsbits - bitcounter]);
        end;

        if bitcounter=nsbits then begin
          bitcounter := 0;
          inc(dataindex);

          if (x <> 0) then
            internalnumber[dataindex] := 0;

          inc(Result.Count);
        end
        else
          inc(bitcounter);
      end;
    end;
  end;

  // ===========================================================================

  for x := 0 to 7 do begin
    if internalnumber[x] <> -1 then begin
      case teo.NumberSize of
        ns8bitSwap  : begin // swap nybbles
                        s := IntToHex(internalnumber[x], 2);

                        internalnumber[x] := TUtility.HexToInt(s[2] + s[1]);
                      end;
        ns16bitSwap : begin // swap bytes
                        s := IntToHex(internalnumber[x], 4);

                        internalnumber[x] := TUtility.HexToInt(s[3] + s[4] + s[1] + s[2]);
                      end;
      end;

      case teo.NumberFormat of
        nfDecimal : Result.data[x] := IntToStr(internalnumber[x]);
        nfBinary  : Result.data[x] := TUtility.IntegerToBinary(nsbits, internalnumber[x]);
        nfHex     : Result.data[x] := IntToHex(internalnumber[x], nspads);
      end;
    end;
  end;
end;


function BinaryExportRowDataRGB(teo : TExportOptions; aFrame, aRowId : integer; spacingchar : string): TDataOut;
var
  lOutput : string;
  lScanDirection : integer;
  x : integer;
  lMatrixData : TMatrix;

begin
  Result.count   := 0;
  lOutput        := '';
  lScanDirection := teo.ScanDirection;

  // ===========================================================================

  if teo.ExportMode = esAnimation then
    lMatrixData := MatrixMain.MatrixLayers[0].Frames[aFrame]
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
  end;

  // ===========================================================================

  if lScanDirection = scanRowLeftToRight then begin        // left to right
    for x := 0 to MatrixMain.Matrix.Width - 1 do begin
      if MatrixMain.MatrixDead.Grid[x, aRowId] = ptNormal then begin
        if teo.NumberSize = nsRGB8bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[x, aRowId] = MatrixMain.RGBBackground) then
            lOutput := lOutput + TUtility.RGBConvertToSplit(teo.RGBChangeColour, teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, '', spacingchar)
          else
            lOutput := lOutput + TUtility.RGBConvertToSplit(lMatrixData.Grid[x, aRowId], teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, '', spacingchar);

          inc(Result.Count, 3);
        end
        else if teo.NumberSize = nsRGB32bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[x, aRowId] = MatrixMain.RGBBackground) then
            lOutput := lOutput + IntToHex(TUtility.RGBConvertTo(teo.RGBChangeColour, teo.RGBMode, teo.LSB, teo.RGBBrightness), 8)
          else
            lOutput := lOutput + IntToHex(TUtility.RGBConvertTo(lMatrixData.Grid[x, aRowId], teo.RGBMode, teo.LSB, teo.RGBBrightness), 8);

//          if x <> MatrixMain.MatrixWidth - 1 then
            lOutput := lOutput + spacingchar;

          inc(Result.Count);
        end;
      end;
    end;
  end
  else if lScanDirection = scanRowRightToLeft then begin        // right to left
    for x := MatrixMain.Matrix.Width - 1 downto 0 do begin
      if MatrixMain.MatrixDead.Grid[x, aRowId] = ptNormal then begin
        if teo.NumberSize = nsRGB8bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[x, aRowId] = MatrixMain.RGBBackground) then
            lOutput := lOutput + TUtility.RGBConvertToSplit(teo.RGBChangeColour, teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, '', spacingchar)
          else
            lOutput := lOutput + TUtility.RGBConvertToSplit(lMatrixData.Grid[x, aRowId], teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, '', spacingchar);

          inc(Result.Count, 3);
        end
        else if teo.NumberSize = nsRGB32bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[x, aRowId] = MatrixMain.RGBBackground) then
            lOutput := lOutput + IntToHex(TUtility.RGBConvertTo(teo.RGBChangeColour, teo.RGBMode, teo.LSB, teo.RGBBrightness), 8)
          else
            lOutput := lOutput + IntToHex(TUtility.RGBConvertTo(lMatrixData.Grid[x, aRowId], teo.RGBMode, teo.LSB, teo.RGBBrightness), 8);

  //        if x <> 0 then
            lOutput := lOutput + spacingchar;

          inc(Result.Count);
        end;
      end;
    end;
  end;

  // ===========================================================================

  Result.data[0] := lOutput;
end;


function BinaryExportColumnData(teo : TExportOptions; aFrame, aColId : integer; spacingchar : string): TDataOut;
var
  s : string;
  nsbits, nspads : integer;
  bitcounter, dataindex, y, lScanDirection : integer;
  internalnumber : array[0..7] of Int64;
  lMatrixData : TMatrix;

begin
  Result.count := 0;

  for y := 0 to 7 do begin
    internalnumber[y] := -1;
    Result.data[y]    := '';
  end;

  nsbits := NumberSizes[Ord(teo.NumberSize)];
  nspads := NumberPadding[Ord(teo.NumberSize)];

  lScanDirection := teo.ScanDirection;

  // ===========================================================================

  if teo.ExportMode = esAnimation then
    lMatrixData := MatrixMain.MatrixLayers[0].Frames[aFrame]
  else
    lMatrixData := MatrixMain.MatrixUser[aFrame];

  // ===========================================================================

  bitcounter := 0;
  dataindex  := 0;
  internalnumber[dataindex] := 0;

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
    for y := 0 to MatrixMain.Matrix.Height - 1 do begin
      if MatrixMain.MatrixDead.Grid[aColId, y] = ptNormal then begin
        if lMatrixData.Grid[aColId, y] = 1 then begin
          if teo.LSB = llTopLeft then
            internalnumber[dataindex] := internalnumber[dataindex] + (powers[bitcounter])
          else
            internalnumber[dataindex] := internalnumber[dataindex] + (powers[nsbits - bitcounter]);
        end;

        if bitcounter = nsbits then begin
          bitcounter := 0;
          inc(dataindex);

          if (y <> MatrixMain.Matrix.Height - 1) then
            internalnumber[dataindex] := 0;

          inc(Result.Count);
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
            internalnumber[dataindex] := internalnumber[dataindex] + (powers[bitcounter])
          else
            internalnumber[dataindex] := internalnumber[dataindex] + (powers[nsbits - bitcounter]);
        end;

        if bitcounter = nsbits then begin
          bitcounter := 0;
          inc(dataindex);

          if (y <> 0) then
            internalnumber[dataindex] := 0;

          inc(Result.Count);
        end
        else
          inc(bitcounter);
      end;
    end;
  end;

  // ===========================================================================

  for y := 0 to 7 do begin
    if internalnumber[y] <> -1 then begin
      case teo.NumberSize of
        ns8bitSwap  : begin // swap nybbles
                        s := IntToHex(internalnumber[y], 2);

                        internalnumber[y] := TUtility.HexToInt(s[2] + s[1]);
                      end;
        ns16bitSwap : begin // swap bytes
                        s := IntToHex(internalnumber[y], 4);

                        internalnumber[y] := TUtility.HexToInt(s[3] + s[4] + s[1] + s[2]);
                      end;
      end;

      case teo.NumberFormat of
        nfDecimal : Result.data[y] := IntToStr(internalnumber[y]);
        nfBinary  : Result.data[y] := TUtility.IntegerToBinary(nsbits, internalnumber[y]);
        nfHex     : Result.data[y] := IntToHex(internalnumber[y], nspads);
      end;
    end;
  end;
end;


function BinaryExportColumnDataRGB(teo : TExportOptions; aFrame, aColId : integer; spacingchar : string): TDataOut;
var
  output : string;
  lScanDirection : integer;
  y : integer;
  lMatrixData : TMatrix;

begin
  Result.count   := 0;
  lScanDirection := teo.ScanDirection;

  // ===========================================================================

  if teo.ExportMode = esAnimation then
    lMatrixData := MatrixMain.MatrixLayers[0].Frames[aFrame]
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
    for y := 0 to MatrixMain.Matrix.Height - 1 do begin
      if MatrixMain.MatrixDead.Grid[aColId, y] = ptNormal then begin
        if teo.NumberSize = nsRGB8bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[aColId, y] = MatrixMain.RGBBackground) then
            output := output + TUtility.RGBConvertToSplit(teo.RGBChangeColour, teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, '', spacingchar)
          else
            output := output + TUtility.RGBConvertToSplit(lMatrixData.Grid[aColId, y], teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, '', spacingchar);

          inc(Result.Count, 3);
        end
        else if teo.NumberSize = nsRGB32bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[aColId, y] = MatrixMain.RGBBackground) then
            output := output + IntToHex(TUtility.RGBConvertTo(teo.RGBChangeColour, teo.RGBMode, teo.LSB, teo.RGBBrightness), 8)
          else
            output := output + IntToHex(TUtility.RGBConvertTo(lMatrixData.Grid[aColId, y], teo.RGBMode, teo.LSB, teo.RGBBrightness), 8);

          output := output + spacingchar;

          inc(Result.Count);
        end;
      end;
    end;
  end
  else if lScanDirection = scanColBottomToTop then begin        // bottom to top
    for y := MatrixMain.Matrix.Height - 1 downto 0 do begin
      if MatrixMain.MatrixDead.Grid[aColId, y] = ptNormal then begin
        if teo.NumberSize = nsRGB8bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[aColId, y] = MatrixMain.RGBBackground) then
            output := output + TUtility.RGBConvertToSplit(teo.RGBChangeColour, teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, '', spacingchar)
          else
            output := output + TUtility.RGBConvertToSplit(lMatrixData.Grid[aColId, y], teo.RGBMode, teo.RGBBrightness, teo.NumberFormat, '', spacingchar);

          inc(Result.Count, 3);
        end
        else if teo.NumberSize = nsRGB32bit then begin
          if (teo.RGBChangePixels) and (lMatrixData.Grid[aColId, y] = MatrixMain.RGBBackground) then
            output := output + IntToHex(TUtility.RGBConvertTo(teo.RGBChangeColour, teo.RGBMode, teo.LSB, teo.RGBBrightness), 8)
          else
            output := output + IntToHex(TUtility.RGBConvertTo(lMatrixData.Grid[aColId, y], teo.RGBMode, teo.LSB, teo.RGBBrightness), 8);

          output := output + spacingchar;

          inc(Result.Count);
        end;
      end;
    end;
  end;

  // ===========================================================================

  Result.data[0] := output;
end;


function BinaryGetRowData(hexmode : boolean; direction, aFrame, aRowId : integer): string;
var
  x : integer;
  mydata : int64;

begin
  Result := '';
  mydata := 0;

  for x := 0 to MatrixMain.Matrix.Width - 1 do begin
    if MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[x, aRowId] = 1 then begin
      if direction = 0 then
        mydata := mydata + (powers[x])
      else
        mydata := mydata + (powers[MatrixMain.Matrix.Width - x]);
    end;
  end;

  if hexmode then
    Result := IntToHex(mydata, LMSSettings.App.PadModeHexRow)
  else
    Result := IntToStr(mydata);
end;


function BinaryGetColumnData(hexmode : boolean; direction, aFrame, aColId : integer): string;
var
  y : integer;
  mydata : int64;

begin
  Result := '';
  mydata := 0;

  for y := 0 to MatrixMain.Matrix.Height - 1 do begin
    if MatrixMain.MatrixLayers[0].Frames[aFrame].Grid[aColId, y] = 1 then begin
      if direction = 0 then
        mydata := mydata + (powers[y])
      else
        mydata := mydata + (powers[MatrixMain.Matrix.Height - y]);
    end;
  end;

  if hexmode then
    Result := IntToHex(mydata, LMSSettings.App.PadModeHexCol)
  else
    Result := IntToStr(mydata);
end;


procedure BinaryAddContentByFrame(teo : TExportOptions; s : string; frame : integer; var aOutput : TStringList);
var
  m : string;

begin
  m := Copy(s, 1, length(s) - 1);

  aOutput.Add(m + ' ');
  aOutput.Add('');
end;


end.
