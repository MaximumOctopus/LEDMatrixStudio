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


unit formSetIgnoredPixels;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, math,

  languagehandler,

  matrixconstants, Vcl.ExtCtrls, Vcl.Buttons;


type
  TfrmSetIgnoredPixels = class(TForm)
    Image1: TImage;
    GroupBox1: TGroupBox;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    Bevel1: TBevel;
    cbCustomShape: TComboBox;
    Label11: TLabel;
    cbCustomShapeParam: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure cbCustomShapeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMatrixWidth, FMatrixHeight : integer;

    procedure SetGUILanguageText;
  public
  end;


  TSetIgnoredPixelsObject = record
    Process   : boolean;
    Shape     : TCustomShape;
    Parameter : integer;
  end;


function DoIgnoredPixels(aMatrixWidth, aMatrixHeight : integer) : TSetIgnoredPixelsObject;


var
  frmSetIgnoredPixels: TfrmSetIgnoredPixels;


implementation


{$R *.dfm}


function DoIgnoredPixels(aMatrixWidth, aMatrixHeight : integer) : TSetIgnoredPixelsObject;
begin
  with TfrmSetIgnoredPixels.Create(Application) do
    try
      FMatrixWidth  := aMatrixWidth;
      FMatrixHeight := aMatrixHeight;

      Result.Process := False;

      ShowModal;

      if ModalResult = mrOK then begin
        Result.Process   := True;
        Result.Shape     := TCustomShape(cbCustomShape.ItemIndex);
        Result.Parameter := cbCustomShapeParam.ItemIndex;
      end;

    finally
      Free;
    end;
end;


procedure TfrmSetIgnoredPixels.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmSetIgnoredPixels.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kSetIgnoredPixelsFromPattern];

  Label1.Caption := GLanguageHandler.Text[kUseCustomShape];
  Label11.Caption := GLanguageHandler.Text[kBorder];
  Label2.Caption := GLanguageHandler.Text[kPixels];

  cbCustomShape.Items.Add(GLanguageHandler.Text[kNoCustomShape]);
  cbCustomShape.Items.Add(GLanguageHandler.Text[kCircle]);
  cbCustomShape.Items.Add(GLanguageHandler.Text[kFrameBorderNoCentre]);
  cbCustomShape.ItemIndex := 0;

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmSetIgnoredPixels.cbCustomShapeChange(Sender: TObject);
var
  t, c : integer;

begin
  cbCustomShapeParam.Clear;

  case cbCustomShape.ItemIndex of
    customShapeNone        : begin
                               cbCustomShapeParam.Items.Add(GLanguageHandler.Text[kNA]);
                             end;
    customShapeCircle      : begin
                               cbCustomShapeParam.Items.Add(GLanguageHandler.Text[kNA]);
                             end;
    customShapeJustBorders : begin
                               c := Floor(Min(FMatrixWidth, FMatrixHeight) / 2);

                               for t := 1 to c do
                                 cbCustomShapeParam.Items.Add(IntToStr(t));
                             end;
  end;

  cbCustomShapeParam.ItemIndex := 0;
end;


end.
