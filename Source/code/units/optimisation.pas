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

unit optimisation;


interface


uses System.Classes, System.SysUtils, Dialogs,

     utility, xglobal, languagehandler,

     exportoptions, exportutility,

     exportoptions_monobi, exportoptions_rgb;


function OptimiseData(teo : TExportOptions; var aData : TStringList): boolean;
function OptimiseDataSimple(teo : TExportOptions; var aData : TStringList; var aOutput : TStringList): boolean;
procedure ProcessUnique(aData : TStringList; var aUniqueItems : TStringList);


implementation


function OptimiseData(teo : TExportOptions; var aData : TStringList): boolean;
var
  lUniqueItems : TStringList;
  lOutput : TStringList;
  t, os, i, lIndexDataSize : integer;
  s : string;

begin
  lUniqueItems := TStringList.Create;
  lUniqueItems.Sorted := True;

  lOutput      := TStringList.Create;

  // ===========================================================================

  ProcessUnique(aData, lUniqueItems);

  // == now calculate whether the optimisation is worth it! ====================

  case lUniqueItems.Count of            // lookup table will require...
    0..255     : lIndexDataSize := 1;   // 1 byte per item
    256..65535 : lIndexDataSize := 2;   // 2 bytes per item
  else
    lIndexDataSize := 4;                // 4 bytes per item
  end;

  // ===========================================================================

  // calculate the size of the current animation after optimisation
  os := (MatrixMain.DataSizeBytes * lUniqueItems.Count) +
        ((MatrixMain.Matrix.Width * MatrixMain.Matrix.Height * MatrixMain.FrameCount) * lIndexDataSize);

  if os < MatrixMain.CalculateMemoryUsage then begin

    Result := True;

    aData.Clear;

    TExportUtility.GetPreamble(teo, aData, False);

    TExportUtility.GetSpacerLine(teo.Language, aData);

    aData.Add(TExportUtility.GetCommentCharacter(teo.Language) + 'Unoptimised size: ' + IntToStr(MatrixMain.CalculateMemoryUsage) + ' bytes');
    aData.Add(TExportUtility.GetCommentCharacter(teo.Language) + '  Optimised size: ' + IntToStr(os) + ' bytes');
    aData.Add(TExportUtility.GetCommentCharacter(teo.Language) + '          Saving: ' + IntToStr(MatrixMain.CalculateMemoryUsage - os) + ' bytes (' +
                                     IntToStr(Round(((MatrixMain.CalculateMemoryUsage - os) / MatrixMain.CalculateMemoryUsage) * 100)) + '%)');

    TExportUtility.GetSpacerLine(teo.Language, aData);

    aData.Add(TExportUtility.GetCommentCharacter(teo.Language) + ' ' + GLanguageHandler.Text[kAccessWithLEDDataIndex] + ' ');

    TExportUtility.GetSpacerLine(teo.Language, aData);

    aData.Add('');

    aData.Add(TExportUtility.GetVariableType(teo.Language, teo.NumberSize) + 'leddataindex[] = {');

    s := '';
    for t := 0 to lUniqueItems.Count - 1 do begin
      s := s + lUniqueItems[t];

      if t <> lUniqueItems.Count - 1 then
        s := s + ', ';
    end;

    aData.Add(s + '};');

    aData.Add('');

    // =========================================================================

    teo.CleanMode  := False;
    //teo.NumberSize := lIndexDataSize; // this was causing issues, but no idea what it was meant to do... !

    if teo.RGBEnabled then begin
      if TExportRGB.CreateExportAnimationRGB(teo, lOutput, i, lUniqueItems) then begin
        for t := 0 to lOutput.Count - 1 do
          aData.Add(lOutput[t]);
      end
      else
        aData.Add(GLanguageHandler.Text[kError]);
    end
    else begin
      if TExportMonoBi.CreateExportAnimation(teo, lOutput, i, lUniqueItems) then begin
        for t := 0 to lOutput.Count - 1 do
          aData.Add(lOutput[t]);
      end
      else
        aData.Add(GLanguageHandler.Text[kError]);
    end;
  end
  else
    Result := False;

  // ===========================================================================

  lUniqueItems.Free;
  lOutput.Free;
end;


function OptimiseDataSimple(teo : TExportOptions; var aData : TStringList; var aOutput : TStringList): boolean;
var
  lUniqueItems : TStringList;
  lCount, t, os, uos,i, lIndexDataSize : integer;
  s : string;

begin
  lUniqueItems := TStringList.Create;
  lUniqueItems.Sorted := True;

  // ===========================================================================

  ProcessUnique(aData, lUniqueItems);

  // == now calculate whether the optimisation is worth it! ====================

  case lUniqueItems.Count of
    0..255     : lIndexDataSize := 1;
    256..65535 : lIndexDataSize := 2;
  else
    lIndexDataSize := 4;
  end;

  // ===========================================================================

  uos := aData.Count * Ord(teo.NumberSize);  // is this right? should be numbersizes[ x ]

  os  := (Ord(teo.NumberSize) * lUniqueItems.Count) +     // lookup table size
         (aData.Count * lIndexDataSize);             // size of data

  Result := True;

  aOutput.Clear;

  TExportUtility.GetPreamble(teo, aOutput, True);

  TExportUtility.GetSpacerLine(teo.Language, aOutput);

  aOutput.Add(TExportUtility.GetCommentCharacter(teo.Language) + 'Unoptimised size: ' + IntToStr(uos) + ' bytes');
  aOutput.Add(TExportUtility.GetCommentCharacter(teo.Language) + '  Optimised size: ' + IntToStr(os) + ' bytes');
  aOutput.Add(TExportUtility.GetCommentCharacter(teo.Language) + '          Saving: ' + IntToStr(uos - os) + ' bytes (' +
                                                IntToStr(Round(((uos - os) / uos) * 100)) + '%)');

  TExportUtility.GetSpacerLine(teo.Language, aOutput);

  aOutput.Add(TExportUtility.GetCommentCharacter(teo.Language) + ' Access with leddataindex[ledarray[x]] ');

  TExportUtility.GetSpacerLine(teo.Language, aOutput);

  // ==========================================================================

   aOutput.Add('');

   aOutput.Add(TExportUtility.GetVariableType(teo.Language, teo.NumberSize) + 'leddataindex[] = {');

   s := '';
   for t := 0 to lUniqueItems.Count - 1 do begin
     s := s + lUniqueItems[t];

     if t <> lUniqueItems.Count - 1 then
       s := s + ', ';
   end;

   aOutput.Add(s);
   aOutput.Add('};');

   // ==========================================================================

   aOutput.Add('');

   for t := 0 to aData.Count - 1 do begin

     for i := 0 to lUniqueItems.Count - 1 do begin
       if aData[t] = lUniqueItems[i] then
         aData[t] := IntToStr(i);
     end;
   end;

   aOutput.Add(TExportUtility.GetVariableType(teo.Language, teo.NumberSize) + 'ledarray[] = {');

   s      := '';
   i      := Length(IntToStr(lUniqueItems.Count));
   lCount := 0;

   for t := 0 to aData.Count - 1 do begin

      s := s + TUtility.PadToLength(aData[t], i) + ', ';

      lCount := lCount + 1;

      if lCount = teo.LineCount then begin

        aOutput.Add(s);

        s := '';

        lCount := 0;
      end;

   end;

   if (s <> '') then
     aOutput.Add(s);

   aOutput.Add('};');

  // ===========================================================================

  lUniqueItems.Free;
end;


procedure ProcessUnique(aData : TStringList; var aUniqueItems : TStringList);
var
  u, i : integer;
  s : string;

begin
  for u:=0 to aData.Count - 1 do begin
    s := '';

    for i := 1 to length(aData[u]) do begin
      if aData[u][i] = ' ' then begin
        if aUniqueItems.IndexOf(s) = -1 then begin
          aUniqueItems.Add(s);
        end;

        s := '';
      end
      else
        s := s + aData[u][i];
    end;

    if s <> '' then begin
      if aUniqueItems.IndexOf(s) = -1 then begin
        aUniqueItems.Add(s);
      end;
    end;
  end;
end;


end.
