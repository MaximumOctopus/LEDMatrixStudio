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

unit frameFontPanel;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;


type
  TframeFont = class(TFrame)
    sbChangeFont: TSpeedButton;
    lFont: TLabel;
    fdMain: TFontDialog;
    procedure lFontClick(Sender: TObject);
  private
    FFontAscii : integer;
  public
    procedure SetFont(aNewAscii : integer);
  end;


implementation


{$R *.dfm}


procedure TframeFont.lFontClick(Sender: TObject);
begin
  if fdMain.Execute then begin
    lFont.Font := fdMain.Font;

    sbChangeFont.Caption := fdMain.Font.Name;
  end;
end;


procedure TframeFont.SetFont(aNewAscii : integer);
begin
  FFontAscii    := aNewAscii;

  lFont.Caption := Char(aNewAscii);
end;


end.
