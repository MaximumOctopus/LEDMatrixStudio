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

unit matrixdead;


interface


uses SysUtils, math, dialogs,

     matrixconstants;


type
  TDataParameter = (dpUnknown, dpDeadPixelBegin, dpDeadPixelEnd, dpRowData);
  TPixelType     = (ptNormal, ptDead);

  TMatrixDead = class(TObject)
    Grid : packed array[0.._MaxWidth, 0.._MaxHeight] of TPixelType;                  // static arry set to max matrix size
  public
    constructor Create;

    function  Load(aFileName : string): boolean;
    function  Save(aFileName : string; aWidth, aHeight : integer): boolean;

    procedure SetFromCustomShape(aWidth, aHeight : integer; aShape : TCustomShape; aParameter : integer);
    procedure SetAllPixels(aNewStatus : TPixelType);
  private
    function  LoadDataParameterType(s : string; aIgnorePixelMode : boolean): TDataParameter;
  end;


implementation


constructor TMatrixDead.Create;
begin
  SetAllPixels(ptNormal);
end;


function TMatrixDead.LoadDataParameterType(s : string; aIgnorePixelMode : boolean): TDataParameter;
begin
  Result := dpUnknown;

  if Pos('deadpixel', s) <> 0 then
    Result := dpDeadPixelBegin
  else if s[1] = '}' then
    Result := dpDeadPixelEnd
  else begin
    if aIgnorePixelMode then begin
      case s[1] of
        'p' : Result := dpRowData;
      end;
    end
  end;
end;


function TMatrixDead.Load(aFileName : string): boolean;
var
  tf : TextFile;
  lIgnorePixelMode : boolean;
  lRow, i, lColumn : integer;
  lInput, v, lPixel : string;

begin
  Result := True;

  lIgnorePixelMode := False;
  lRow             := 0;

  try
    AssignFile(tf, aFileName);
    Reset(tf);

    while not(eof(tf)) do begin
      readln(tf, lInput);

      if lInput <> '' then begin
        v := Copy(lInput, 3, length(lInput) - 2);

        case LoadDataParameterType(LowerCase(lInput), lIgnorePixelMode) of
          dpDeadPixelBegin : begin // dead pixel mode
                               lIgnorePixelMode := True;

                               lRow := 0;
                             end;
          dpDeadPixelEnd   : begin
                               lIgnorePixelMode := False;
                             end;

          // ======================================================================

          dpRowData        : begin
                               lColumn := 0;
                               lPixel   := '';

                               for i := 1 to length(v) do begin
                                 if (v[i] = ' ') or (i = length(v)) then begin
                                   if (lPixel = '0') then
                                     Grid[lColumn, lRow] := ptNormal
                                   else
                                     Grid[lColumn, lRow] := ptDead;

                                   inc(lColumn);

                                   lPixel := '';
                                 end
                                 else
                                   lPixel := lPixel + v[i];
                               end;

                               inc(lRow);
                             end;
        end;
      end;
    end;

    CloseFile(tf);
  except
    on E: Exception do begin
      Result := False;
    end;
  end;
end;


function TMatrixDead.Save(aFileName : string; aWidth, aHeight : integer): boolean;
var
  tf : TextFile;
  s : string;
  lColumn, lRow : integer;

begin
  Result := True;

  try
    AssignFile(tf, aFileName);
    Rewrite(tf);

    writeln(tf, '{deadpixel');

    for lRow := 0 to aHeight - 1 do begin
      s := '';

      for lColumn := 0 to aWidth - 1 do begin
        s := s + IntToStr(Ord(Grid[lColumn, lRow])) + ' ';
      end;

      writeln(tf, 'p:' + s);
    end;

    writeln(tf, '}');

    CloseFile(tf);
  except
    Result := False;
  end;
end;


procedure TMatrixDead.SetAllPixels(aNewStatus : TPixelType);
var
  lColumn, lRow : integer;

begin
  for lColumn := 0 to High(Grid) do begin
    for lRow := 0 to High(Grid[lColumn]) do begin
      Grid[lColumn, lRow] := aNewStatus;
    end;
  end;
end;


procedure TMatrixDead.SetFromCustomShape(aWidth, aHeight : integer; aShape : TCustomShape; aParameter : integer);
var
  x, y, tc       : integer;
  a, b           : integer;  // displacements in x and y
  x1, y1 : integer;

  procedure SimpleLine(x1, y1, x2, y2 : integer);
  var
    lColumn : integer;

  begin
    lColumn := x1;

    while lColumn <= x2 do begin
      Grid[lColumn, y1] := ptNormal;

      inc(lColumn);
    end;
  end;


begin
  case aShape of
    csNone     : {};
    csCircle   : begin
                   SetAllPixels(ptDead);

                   x1 := Floor(aWidth / 2);
                   y1 := Floor(aHeight / 2);

                   // c^2 = a^2 + b^2
                   tc := x1; // radius of circle

                   // midpoint algorithm: http://en.wikipedia.org/wiki/Midpoint_circle_algorithm

                   a := 0;
                   b := 1 - tc;

                   while(tc >= a) do begin
                     SimpleLine(-tc + x1,   a + y1, tc + x1,   a + y1);
                     SimpleLine( -a + x1,  tc + y1,  a + x1,  tc + y1);
                     SimpleLine(-tc + x1,  -a + y1, tc + x1,  -a + y1);
                     SimpleLine( -a + x1, -tc + y1,  a + x1, -tc + y1);

                     inc(a);

                     if (b < 0) then
                       b := b+ 2 * a + 1
                     else begin
                       dec(tc);
                       b := b + 2 * (a - tc + 1);
                     end;
                   end;
                 end;
    csBorders  : begin
                   inc(aParameter, 1);

                   for x := 0 + aParameter to aWidth - (1 + aParameter) do begin
                     for y := 0 + aParameter to aHeight - (1 + aParameter) do begin
                       if (x > 0) and (x < aWidth) and
                          (y > 0) and (y < aHeight) then
                         Grid[x, y] := ptDead;
                     end;
                   end;
                 end;
    csTriangle : begin
                   SetAllPixels(ptDead);

                   b := 1;
                   x := Floor(aWidth / 2) - 1;
                   y := 0;

                   while (y <= aHeight - 1) do begin
                     for a := 1 to b do
                       if ((x + a) > 0) and ((x + a) < aWidth) and
                           (y > 0) and (y < aHeight) then
                         Grid[x + a, y] := ptNormal;

                         dec(x);
                         inc(b, 2);
                         inc(y, 1);
                   end;
                 end;
  end;
end;


end.
