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

unit formFontViewer;


interface


uses
  Windows, Messages, System.UITypes, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, thematrix, StdCtrls, ComCtrls, ExtCtrls,

  fonthandler, languagehandler,

  matrixconstants;


type
  TfrmFontViewer = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    tbFont: TTrackBar;
    cbFonts: TComboBox;
    bSelectFont: TLabel;
    lCharacterValue: TLabel;
    Label3: TLabel;
    cbRGBMode: TCheckBox;
    lCharacter: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BuildFontList;
    procedure FormShow(Sender: TObject);
    procedure cbFontsChange(Sender: TObject);
    procedure tbFontChange(Sender: TObject);
    procedure cbRGBModeClick(Sender: TObject);
  private
    procedure SetGUILanguageText;
    procedure SetLabel;
  public
    fLastFrame : integer;
  end;


var
 frmFontViewer: TfrmFontViewer;
 FontMatrix : TTheMatrix;


function ShowFontViewer: word;


implementation


{$R *.dfm}


uses utility, xglobal;


function ShowFontViewer: word;
begin
  with TfrmFontViewer.Create(Application) do begin
    fLastFrame := -1;

    Show;

    Result := ModalResult;
  end;
end;


procedure TfrmFontViewer.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;

  FontMatrix := TTheMatrix.Create(Self, Self);

  FontMatrix.NewMatrix(mtMono, 1, 6, 150, 8, 8, 25, psSquare, True, True, True, $00000000);

  FontMatrix.LEDColours[0]   := $00ffffff;
  FontMatrix.LEDColours[1]   := $00000000;

  FontMatrix.Render.DrawData.Colour := 1;
  FontMatrix.RGBBackground   := $00FFFFFF;

  SetGUILanguageText;
end;


procedure TfrmFontViewer.FormShow(Sender: TObject);
begin
  BuildFontList;

  cbFontsChange(Nil);
end;


procedure TfrmFontViewer.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kFontViewer];

  bSelectFont.Caption := GLanguageHandler.Text[kSelectFont];
  cbRGBMode.Caption := GLanguageHandler.Text[kViewInRGBMode];
  lCharacterValue.Caption := GLanguageHandler.Text[kCharacter];
end;


procedure TfrmFontViewer.tbFontChange(Sender: TObject);
begin
  if (tbFont.Position <> fLastFrame) then begin
    FontMatrix.ClearCurrentFrame;

    FontMatrix.Render.DrawData.Coords[0].X := 0;
    FontMatrix.Render.DrawData.Coords[0].Y := 7;

    FontMatrix.DrawFontCharacter(tbFont.Position - 32, 1);

    fLastFrame := tbFont.Position;

    SetLabel;
  end;
end;


procedure TfrmFontViewer.SetLabel;
begin
  if tbFont.Position = 32 then
    lCharacter.Caption := '''space'''
  else
    lCharacter.Caption := Char(tbFont.Position);
end;


procedure TfrmFontViewer.cbFontsChange(Sender: TObject);
var
  s,temp : string;
  t : integer;

begin
  temp := ExtractFilePath(Application.ExeName) + 'fonts\' + cbFonts.Items[cbFonts.ItemIndex] + '.ledsfont';
  s    := '';

  for t := 1 to length(temp) do
    if temp[t] <> '&' then
      s := s + temp[t];

  if FileExists(s) then begin
    FontMatrix.LoadFont(s);
  end
  else
    MessageDlg(GLanguageHandler.Text[kCannotFindFont] + #13#10 + #13#10 + '"' + s + '"', mtError, [mbOK], 0);

  fLastFrame := -1;

  tbFontChange(Nil);
end;


procedure TfrmFontViewer.cbRGBModeClick(Sender: TObject);
begin
  if cbRGBMode.Checked then
    FontMatrix.NewMatrix(mtRGB, 1, 6, 150, 8, 8, 25, psSquare, True, True, True, $00000000)
  else
    FontMatrix.NewMatrix(mtMono, 1, 6, 150, 8, 8, 25, psSquare, True, True, True, $00000000);

  FontMatrix.Render.DrawData.Colour    := 1;

  cbFontsChange(Nil);
end;


procedure TfrmFontViewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fontmatrix.Free;

  Action := caFree;
end;


procedure TfrmFontViewer.BuildFontList;
begin
   TFontHandler.GetFontList(ExtractFilePath(Application.ExeName) + 'fonts\*.ledsfont', cbFonts);

  cbFonts.ItemIndex := 0;
end;


end.
