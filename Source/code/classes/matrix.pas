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


unit matrix;

interface


uses System.Generics.Collections,

     matrixconstants;


type
  TMatrix = class;


  TMatrixHistory = class(TObject)
                     Grid : array of array of integer;                               // dynamic array
                   public
                     constructor Create(AOwner: TMatrix; aWidth, aHeight : integer);
                   end;



  TMatrix = class(TObject)
              Grid          : array of array of integer;                             // dynamic array

              Locked        : boolean;

              HistoryOffset : integer;
              History       : TObjectList<TMatrixHistory>;
            public
              constructor Create(aWidth, aHeight : integer; aType : TMatrixMode; aBackground : integer);
              destructor  Destroy; Override;

              procedure Clear(aMatrixMode : TMatrixMode; aBackground : integer);
              procedure ClearColour(aBackground : integer);
              procedure ChangePixels(aFrom, aTo : integer);

              procedure SafePlot(aX, aY, aColour : integer);

              procedure AddToHistory;                    overload;
              procedure AddToHistory(aMatrix : TMatrix); overload;
              procedure Undo;
              procedure Redo;

              procedure SetFromUndo(aUndo : integer);
            end;


implementation


constructor TMatrix.Create(aWidth, aHeight : integer; aType : TMatrixMode; aBackground : integer);
var
  lColumn, lRow, z : integer;
  tmh : TMatrixHistory;

begin
  SetLength(Grid, aWidth);

  for z := 0 to aWidth - 1 do
    SetLength(Grid[z], aHeight);

  for lColumn := 0 to aWidth - 1 do begin
    for lRow := 0 to aHeight - 1 do begin
      if aType = mtRGB then
        Grid[lColumn, lRow] := aBackground
      else
        Grid[lColumn, lRow] := 0;
    end;
  end;

  Locked        := False;

  HistoryOffset := 0;
  History       := TObjectList<TMatrixHistory>.Create;

  tmh := TMatrixHistory.Create(Self, aWidth, aHeight);
  History.Add(tmh);
end;


destructor TMatrix.Destroy;
 begin
  History.Free;

  inherited Destroy;
end;


procedure TMatrix.Clear(aMatrixMode : TMatrixMode; aBackground : integer);
var
  lColumn, lRow : integer;

begin
  for lColumn := 0 to High(Grid) do begin
    for lRow := 0 to High(Grid[lColumn]) do begin

      if aMatrixMode = mtRGB then
        Grid[lColumn, lRow] := aBackground
      else
        Grid[lColumn, lRow] := 0;
    end;
  end;
end;


procedure TMatrix.ClearColour(aBackground : integer);
var
  lColumn, lRow : integer;

begin
  for lColumn := 0 to High(Grid) do begin
    for lRow := 0 to High(Grid[lColumn]) do begin
      Grid[lColumn, lRow] := aBackground;
    end;
  end;
end;


procedure TMatrix.SafePlot(aX, aY, aColour : integer);
begin
  if (aX >= 0) and (aX <= High(Grid)) and
     (aY >= 0) and (aY <= High(Grid[aX])) then
    Grid[aX, aY] := aColour;
end;


procedure TMatrix.ChangePixels(aFrom, aTo : integer);
var
  lColumn, lRow : integer;

begin
  for lColumn := 0 to High(Grid) do begin
    for lRow := 0 to High(Grid[lColumn]) do begin
      if Grid[lColumn, lRow] = aFrom then
        Grid[lColumn, lRow] := aTo;
    end;
  end;
end;


procedure TMatrix.AddToHistory;
var
  tmh : TMatrixHistory;
  t, lColumn, lRow : integer;

begin
  if HistoryOffset <> History.Count - 1 then begin
    for t:= History.Count - 1 downto HistoryOffSet + 1 do begin
      History.Delete(t);
    end;
  end;

  // to do, maybe put something here to limit memory usage :)
  tmh := TMatrixHistory.Create(Self, High(Grid) + 1, High(Grid[0]) + 1);

  for lColumn := 0 to High(Grid) do begin
    for lRow := 0 to High(Grid[lColumn]) do begin
      tmh.Grid[lColumn, lRow] := Grid[lColumn, lRow];
    end;
  end;

  History.Add(tmh);

  HistoryOffset := History.Count - 1;
end;


procedure TMatrix.AddToHistory(aMatrix : TMatrix);
var
  tmh : TMatrixHistory;
  t, lColumn, lRow : integer;

begin
  if HistoryOffset <> History.Count - 1 then begin
    for t:= History.Count - 1 downto HistoryOffSet + 1 do begin
      History.Delete(t);
    end;
  end;

  // to do, maybe put something here to limit memory usage :)
  tmh := TMatrixHistory.Create(Self, High(Grid) + 1, High(Grid[0]) + 1);

  for lColumn := 0 to High(Grid) do begin
    for lRow := 0 to High(Grid[lColumn]) do begin
      tmh.Grid[lColumn, lRow] := aMatrix.Grid[lColumn, lRow];
    end;
  end;

  History.Add(tmh);

  HistoryOffset := History.Count - 1;
end;


procedure TMatrix.Undo;
var
  lColumn, lRow : integer;

begin
  if HistoryOffset <> 0 then begin
    dec(HistoryOffset);

    for lColumn := 0 to High(Grid) do begin
      for lRow := 0 to High(Grid[lColumn]) do begin
        Grid[lColumn, lRow] := History[HistoryOffset].Grid[lColumn, lRow];
      end;
    end;
  end;
end;


procedure TMatrix.Redo;
var
  tmh : TMatrixHistory;
  lColumn, lRow : integer;

begin
  if HistoryOffset <> History.Count - 1 then begin
    inc(HistoryOffset);

    tmh := History[HistoryOffset];

    for lColumn := 0 to High(Grid) do begin
      for lRow := 0 to High(Grid[lColumn]) do begin
        Grid[lColumn, lRow] := tmh.Grid[lColumn, lRow];
      end;
    end;
  end;
end;


procedure TMatrix.SetFromUndo(aUndo : integer);
var
  lColumn, lRow : integer;

begin
  for lColumn := 0 to High(Grid) do begin
    for lRow := 0 to High(Grid[lColumn]) do begin
      Grid[lColumn, lRow] := History[aUndo].Grid[lColumn, lRow];
    end;
  end;
end;


// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================


constructor TMatrixHistory.Create(AOwner: TMatrix; aWidth, aHeight : integer);
var
  lColumn, lRow, z : integer;

begin
  SetLength(Grid, aWidth);

  for z := 0 to aWidth - 1 do
    SetLength(Grid[z], aHeight);

  for lColumn := 0 to High(Grid) do begin
    for lRow := 0 to High(Grid[lColumn])  do begin
      Grid[lColumn, lRow] := AOwner.Grid[lColumn, lRow];
    end;
  end;
end;



end.
