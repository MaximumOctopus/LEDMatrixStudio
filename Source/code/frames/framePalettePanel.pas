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

unit framePalettePanel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, registry,
  Vcl.ComCtrls, Vcl.ExtCtrls, System.UITypes,

  utility;


type
  TColourSelected = procedure(aMouseButton, aColour : integer) of object;
  TColourMove     = procedure(aColour : integer) of object;


  TframePalette = class(TFrame)
    lPaletteColourHex: TLabel;
    sRGBPaletteColour: TShape;
    tbRed: TTrackBar;
    eRed: TEdit;
    tbGreen: TTrackBar;
    eGreen: TEdit;
    eBlue: TEdit;
    tbBlue: TTrackBar;
    sRGBP7: TShape;
    sRGBP6: TShape;
    sRGBP5: TShape;
    sRGBP4: TShape;
    sRGBP3: TShape;
    sRGBP2: TShape;
    sRGBP1: TShape;
    sRGBP8: TShape;
    sRGBP9: TShape;
    sRGBP15: TShape;
    sRGBP16: TShape;
    sRGBP23: TShape;
    sRGBP22: TShape;
    sRGBP24: TShape;
    sRGBP17: TShape;
    sRGBP10: TShape;
    sRGBP11: TShape;
    sRGBP18: TShape;
    sRGBP25: TShape;
    sRGBP26: TShape;
    sRGBP19: TShape;
    sRGBP12: TShape;
    sRGBP13: TShape;
    sRGBP20: TShape;
    sRGBP27: TShape;
    sRGBP28: TShape;
    sRGBP21: TShape;
    sRGBP14: TShape;
    lPaletteColourInteger: TLabel;
    procedure tbRedChange(Sender: TObject);
    procedure eRedKeyPress(Sender: TObject; var Key: Char);
    procedure sRGBP1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sRGBP7MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FColourSelected  : TColourSelected;
    FColourMouseOver : TColourMove;

    procedure SavePaletteHistory;
    procedure LoadPaletteHistory;
  public
    RGBPaletteHistory : array[0..27] of TShape;
    RGBPaletteHistoryIndex : integer;

    procedure Init;
    procedure DeInit;

    procedure AddToHistory(aColour : integer);

    property OnColourClick : TColourSelected read FColourSelected  write FColourSelected;
    property OnColourMove  : TColourMove     read FColourMouseOver write FColourMouseOver;
  end;


implementation


{$R *.dfm}


const
  CRed   = 0;
  CGreen = 1;
  CBlue  = 2;


procedure TframePalette.Init;
begin
  RGBPaletteHistory[0] := sRGBP1; RGBPaletteHistory[1] := sRGBP2; RGBPaletteHistory[2] := sRGBP3; RGBPaletteHistory[3] := sRGBP4;
  RGBPaletteHistory[4] := sRGBP5; RGBPaletteHistory[5] := sRGBP6; RGBPaletteHistory[6] := sRGBP7; RGBPaletteHistory[7] := sRGBP8;
  RGBPaletteHistory[8] := sRGBP9; RGBPaletteHistory[9] := sRGBP10; RGBPaletteHistory[10] := sRGBP11; RGBPaletteHistory[11] := sRGBP12;
  RGBPaletteHistory[12] := sRGBP13; RGBPaletteHistory[13] := sRGBP14; RGBPaletteHistory[14] := sRGBP15; RGBPaletteHistory[15] := sRGBP16;
  RGBPaletteHistory[16] := sRGBP17; RGBPaletteHistory[17] := sRGBP18; RGBPaletteHistory[18] := sRGBP19; RGBPaletteHistory[19] := sRGBP20;
  RGBPaletteHistory[20] := sRGBP21; RGBPaletteHistory[21] := sRGBP22; RGBPaletteHistory[22] := sRGBP23; RGBPaletteHistory[23] := sRGBP24;
  RGBPaletteHistory[24] := sRGBP25; RGBPaletteHistory[25] := sRGBP26; RGBPaletteHistory[26] := sRGBP27; RGBPaletteHistory[27] := sRGBP28;

  RGBPaletteHistoryIndex := 0;

  LoadPaletteHistory;
end;


procedure TframePalette.DeInit;
begin
  SavePaletteHistory;
end;


procedure TframePalette.LoadPaletteHistory;
var
  t, x : integer;
  Reg : TRegistry;

  function ReadRegistryInteger(keyname : string; defaultval : Integer): Integer;
   begin
    if Reg.ValueExists(keyname) then
      Result := Reg.ReadInteger(keyname)
    else
      Result := defaultval;
  end;


begin
  Reg := TRegistry.Create(KEY_READ);

  Reg.RootKey := HKEY_CURRENT_USER;
  Reg.OpenKey('\software\freshney.org\MatrixBuilder', True);

  // ===========================================================================

  for t := 0 to 27 do begin
    x := ReadRegistryInteger('rgbpalettehistory' + IntToStr(t), -1);

    if (x = -1) then
      x := $00000000;

    RGBPaletteHistory[t].Brush.Color := x;
  end;

  // ===========================================================================

  Reg.Free;
end;


procedure TframePalette.SavePaletteHistory;
var
  Reg : TRegistry;
  t : integer;

begin
  // ===========================================================================
  // == Save User Settings =====================================================
  // ===========================================================================

  Reg := TRegistry.Create(KEY_WRITE);

  Reg.RootKey := HKEY_CURRENT_USER;
  Reg.OpenKey('\software\freshney.org\MatrixBuilder', True);

  // ===========================================================================

  for t := 0 to 27 do begin
    Reg.WriteInteger('rgbpalettehistory' + IntToStr(t), RGBPaletteHistory[t].Brush.Color);
  end;

  // ===========================================================================

  Reg.Free;
end;


procedure TframePalette.eRedKeyPress(Sender: TObject; var Key: Char);
var
 s : string;
 lValue : integer;

begin
  if (key = #13) then begin
    s := TEdit(Sender).Text;

    lValue := StrToIntDef(s, 999);

    if (lValue >= 0) and (lValue <= 255) then begin
      case TEdit(Sender).Tag of
        0 : tbRed.Position   := lValue;
        1 : tbGreen.Position := lValue;
        2 : tbBlue.Position  := lValue;
      end;

      tbRedChange(Nil);
    end;
  end;
end;


procedure TframePalette.AddToHistory(aColour : integer);
var
  lCanAdd : boolean;

begin
  // ensures no duplicates adjacent
  if RGBPaletteHistoryIndex = 0 then begin
    lCanAdd := not (aColour = RGBPaletteHistory[27].Brush.Color);
  end
  else
    lCanAdd := not (aColour = RGBPaletteHistory[RGBPaletteHistoryIndex - 1].Brush.Color);

  if lCanAdd then begin
    RGBPaletteHistory[RGBPaletteHistoryIndex].Brush.Color := aColour;

    if RGBPaletteHistoryIndex = 27 then
      RGBPaletteHistoryIndex := 0
    else
      inc(RGBPaletteHistoryIndex);
  end;
end;


procedure TframePalette.sRGBP1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lMouse, lColour : integer;

begin
  lColour := TShape(Sender).Brush.Color;

  if (ssLeft in Shift) then
    lMouse := 0
  else if (ssMiddle in Shift) then
    lMouse := 1
  else if (ssRight in Shift) then
    lMouse := 2
  else
    lMouse := 0;

  if Assigned(FColourSelected) then
    FColourSelected(lMouse, lColour);
end;


procedure TframePalette.sRGBP7MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FColourMouseOver) then
    FColourMouseOver(TShape(Sender).Brush.Color);
end;


procedure TframePalette.tbRedChange(Sender: TObject);
begin
  if (Sender <> Nil) then begin
    case TTrackBar(Sender).Tag of
      CRed   : eRed.Text   := IntToStr(TTrackBar(Sender).Position);
      CGreen : eGreen.Text := IntToStr(TTrackBar(Sender).Position);
      CBlue  : eBlue.Text  := IntToStr(TTrackBar(Sender).Position);
    end;
  end;

  lPaletteColourHex.Caption := LMSSettings.App.HexPrefix +
                               IntToHex(tbRed.Position, 2) +
                               IntToHex(tbGreen.Position, 2) +
                               IntToHex(tbBlue.Position, 2);


  sRGBPaletteColour.Brush.Color := (tbBlue.Position shl 16) +
                                   (tbGreen.Position shl 8) +
                                    tbRed.Position;

  lPaletteColourInteger.Caption := IntToStr(sRGBPaletteColour.Brush.Color);
end;


end.
