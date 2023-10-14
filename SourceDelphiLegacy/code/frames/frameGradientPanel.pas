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


unit frameGradientPanel;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Buttons, System.UITypes,

  fileconstants,

  languagehandler,

  utility;


type
  TLoadGradient = (ldUnknown, ldLoadBegin, ldLoadEnd, ldLoadData);

  TframeGradient = class(TFrame)
    sbOpenGradient: TSpeedButton;
    sbSaveGradient: TSpeedButton;
    sbClearGradient: TSpeedButton;
    clbGradient: TColorListBox;
    sRGBPaletteColour: TShape;
    lPaletteColourText: TLabel;
    sbAddColour: TSpeedButton;
    sbRemoveColour: TSpeedButton;
    tbRed: TTrackBar;
    eRed: TEdit;
    eGreen: TEdit;
    tbGreen: TTrackBar;
    tbBlue: TTrackBar;
    eBlue: TEdit;
    bFromCustom: TSpeedButton;
    bFromShades: TSpeedButton;
    cdGradient: TColorDialog;
    odGradient: TOpenDialog;
    sdGradient: TSaveDialog;
    sbCopyToBrush: TSpeedButton;
    procedure tbRedChange(Sender: TObject);
    procedure sbOpenGradientClick(Sender: TObject);
    procedure sbSaveGradientClick(Sender: TObject);
    procedure sbClearGradientClick(Sender: TObject);
    procedure sbAddColourClick(Sender: TObject);
    procedure sbRemoveColourClick(Sender: TObject);
    procedure eRedKeyPress(Sender: TObject; var Key: Char);
    procedure sRGBPaletteColourMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbCopyToBrushClick(Sender: TObject);
    procedure clbGradientClick(Sender: TObject);
    procedure bFromShadesClick(Sender: TObject);
    procedure bFromCustomClick(Sender: TObject);
  private

    function GetColourCount: integer;

    procedure LoadGradient(aFileName : string);
    procedure SaveGradient(aFileName : string);

    procedure SetSlidersFromColour(aColour : integer);
  public
    FOnCopy        : TNotifyEvent;
    FOnFromShades  : TNotifyEvent;
    FOnFromCustom  : TNotifyEvent;

    procedure SetGUILanguageText;

    function GetColour(aIndex : integer): integer;
    procedure AddColour(aColour : integer);

    property Count  : integer      read GetColourCount;

    property OnCopy       : TNotifyEvent read FOnCopy       write FOnCopy;
    property OnFromCustom : TNotifyEvent read FOnFromCustom write FOnFromCustom;
    property OnFromShades : TNotifyEvent read FOnFromShades write FOnFromShades;
  end;


implementation


{$R *.dfm}


const
  CRed   = 0;
  CGreen = 1;
  CBlue  = 2;


procedure TframeGradient.SetGUILanguageText;
begin
  sbOpenGradient.Caption := GLanguageHandler.Text[kOpen];
  sbSaveGradient.Caption := GLanguageHandler.Text[kSave];
  sbClearGradient.Caption := GLanguageHandler.Text[kClear];

  bFromShades.Caption := GLanguageHandler.Text[kFromShades];
  bFromCustom.Caption := GLanguageHandler.Text[kFromCustom];
  sbCopyToBrush.Caption := GLanguageHandler.Text[kCopyBrush];
end;


procedure TframeGradient.eRedKeyPress(Sender: TObject; var Key: Char);
var
 s : string;
 lValue : integer;

begin
  if (key = #13) then begin
    s := TEdit(Sender).Text;

    lValue := StrToIntDef(s, 999);

    if (lValue >= 0) and (lValue <= 255) then begin
      case TEdit(Sender).Tag of
        CRed   : tbRed.Position   := lValue;
        CGreen : tbGreen.Position := lValue;
        CBlue  : tbBlue.Position  := lValue;
      end;

      tbRedChange(Nil);
    end;
  end;
end;


procedure TframeGradient.clbGradientClick(Sender: TObject);
begin
  if clbGradient.ItemIndex <> -1 then begin
    sRGBPaletteColour.Brush.Color := TColor(clbGradient.Items.Objects[clbGradient.ItemIndex]);

    SetSlidersFromColour(sRGBPaletteColour.Brush.Color);
  end;
end;


function TFrameGradient.GetColourCount: integer;
begin
  Result := clbGradient.Items.Count;
end;


function TFrameGradient.GetColour(aIndex : integer): integer;
begin
  Result := TColor(clbGradient.Items.Objects[aIndex]);
end;


procedure TFrameGradient.AddColour(aColour : integer);
begin
  clbGradient.AddItem(TUtility.RGBPlusInteger(aColour, 100), TObject(aColour));
end;


procedure TframeGradient.sbOpenGradientClick(Sender: TObject);
begin
  if (odGradient.Execute) then
    LoadGradient(odGradient.FileName);
end;


procedure TframeGradient.sbSaveGradientClick(Sender: TObject);
begin
  if (sdGradient.Execute) then
    SaveGradient(sdGradient.FileName);
end;


procedure TframeGradient.sbCopyToBrushClick(Sender: TObject);
begin
  if Assigned(FOnCopy) then
    FOnCopy(Self);
end;


procedure TframeGradient.sbClearGradientClick(Sender: TObject);
begin
  clbGradient.Clear;
end;


procedure TframeGradient.sbAddColourClick(Sender: TObject);
begin
  clbGradient.AddItem(TUtility.RGBPlusInteger(sRGBPaletteColour.Brush.Color, 100), TObject(sRGBPaletteColour.Brush.Color));
end;


procedure TframeGradient.sbRemoveColourClick(Sender: TObject);
begin
  if (clbGradient.ItemIndex <> -1) then
    clbGradient.Items.Delete(clbGradient.ItemIndex);
end;


procedure TframeGradient.sRGBPaletteColourMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if cdGradient.Execute then begin
    sRGBPaletteColour.Brush.Color := cdGradient.Color;

    SetSlidersFromColour(cdGradient.Color);
  end;
end;


procedure TframeGradient.tbRedChange(Sender: TObject);
begin
  if (Sender <> Nil) then begin
    case TTrackBar(Sender).Tag of
      CRed   : eRed.Text   := IntToStr(TTrackBar(Sender).Position);
      CGreen : eGreen.Text := IntToStr(TTrackBar(Sender).Position);
      CBlue  : eBlue.Text  := IntToStr(TTrackBar(Sender).Position);
    end;
  end;

  lPaletteColourText.Caption := LMSSettings.App.HexPrefix +
                                IntToHex(tbRed.Position, 2) +
                                IntToHex(tbGreen.Position, 2) +
                                IntToHex(tbBlue.Position, 2);

  sRGBPaletteColour.Brush.Color := (tbBlue.Position shl 16) +
                                   (tbGreen.Position shl 8) +
                                    tbRed.Position;
end;


procedure TframeGradient.LoadGradient(aFileName : string);
var
  tf : TextFile;
  s, v, lColour : string;
  t : integer;

 function parameterType(s : string): TLoadGradient;
 begin
   if s[1] = kDataBlockStart then
     Result := ldLoadBegin
   else if s[1] = kDataBlockEnd then
     Result := ldLoadEnd
   else if s[1] = kGradientColour then
     Result := ldLoadData
   else
     Result := ldUnknown;
 end;

begin
  // ===========================================================================

  AssignFile(tf, aFileName);
  Reset(tf);

  while not(eof(tf)) do begin
    readln(tf, s);

    if s <> '' then begin
      v := Copy(s, 3, length(s) - 2);

      case parameterType(s) of
        ldLoadData : begin
                       lColour := '';

                       for t := 1 to length(v) do begin
                         if v[t] = ' ' then begin
                           clbGradient.AddItem(TUtility.RGBPlusInteger(StrToInt(lColour), 100), TObject(StrToInt(lColour)));

                           lColour := '';
                         end
                         else
                           lColour := lColour + v[t];
                       end;
                     end;
      end;
    end;
  end;

  CloseFile(tf);
end;


procedure TframeGradient.SaveGradient(aFileName : string);
var
  g : string;
  t : integer;
  tf : TextFile;

begin
  AssignFile(tf, aFileName);
  Rewrite(tf);

  Writeln(tf, '{' + kGradientFileHeader);

  g := '';
  for t := 0 to clbGradient.Items.Count - 1 do
    g := g + IntToStr(TColor(clbGradient.Items.Objects[t])) + ' ';

  Writeln(tf, kGradientColour + ':' + g);
  Writeln(tf, kDataBlockEnd);

  CloseFile(tf);
end;


procedure TframeGradient.SetSlidersFromColour(aColour : integer);
var
  lR, lG, lB : integer;

begin
  lR := (aColour and $0000FF);
  lG := (aColour and $00FF00) shr 8;
  lB := (aColour and $FF0000) shr 16;

  tbRed.Position   := lR;
  tbGreen.Position := lG;
  tbBlue.Position  := lB;

  eRed.Text        := IntToStr(lR);
  eGreen.Text      := IntToStr(lG);
  eBlue.Text       := IntToStr(lB);
end;


procedure TframeGradient.bFromShadesClick(Sender: TObject);
begin
  if Assigned(FOnFromShades) then begin
    clbGradient.Clear;

    FOnFromShades(Self);
  end;
end;


procedure TframeGradient.bFromCustomClick(Sender: TObject);
begin
  if Assigned(FOnFromShades) then begin
    clbGradient.Clear;

    FOnFromCustom(Self);
  end;
end;


end.
