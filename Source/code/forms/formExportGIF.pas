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

unit formExportGIF;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,

  languagehandler,

  systemsettings;

type
  TfrmExportGIF = class(TForm)
    Image1: TImage;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    bSave: TSpeedButton;
    Label2: TLabel;
    eFileName: TEdit;
    ePixelSize: TEdit;
    sdExportGIF: TSaveDialog;
    Bevel1: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    rbSquare: TRadioButton;
    rbCircle: TRadioButton;
    Label5: TLabel;
    ShapeNorfolkDigital: TShape;
    cdExportGIF: TColorDialog;
    procedure bSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure eFileNameChange(Sender: TObject);
    procedure ShapeNorfolkDigitalMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetGUILanguageText;
  public
    { Public declarations }
  end;


var
  frmExportGIF: TfrmExportGIF;


function DoExportGIF(aExportGIFOld : TExportGIFObject): TExportGIFObject;


implementation


{$R *.dfm}


function DoExportGIF(aExportGIFOld : TExportGIFObject): TExportGIFObject;
begin
  with TfrmExportGIF.Create(Application) do
    try
      Result.Process := False;

      // =======================================================================

      eFileName.Text  := aExportGIFOld.FileName;
      ePixelSize.Text := IntToStr(aExportGIFOld.PixelSize);

      if (aExportGIFOld.PixelShape) = 0 then
        rbSquare.Checked := True
      else
        rbSquare.Checked := False;

      ShapeNorfolkDigital.Brush.Color := aExportGIFOld.Background;

      // =======================================================================

      ShowModal;

      if ModalResult = mrOK then begin
        Result.Process    := True;

        Result.FileName   := eFileName.Text;
        Result.PixelSize  := StrToIntDef(ePixelSize.Text, 1);

        if (rbSquare.Checked) then
          Result.PixelShape := 0
        else
          Result.PixelShape := 1;

        Result.Background := ShapeNorfolkDigital.Brush.Color;
      end;

    finally
      Free;
    end;
end;


procedure TfrmExportGIF.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmExportGIF.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TfrmExportGIF.SetGUILanguageText;
begin
  Caption          := GLanguageHandler.Text[kOK];

  Label1.Caption   := GLanguageHandler.Text[kFileName];
  bSave.Caption    := GLanguageHandler.Text[kSave];
  Label2.Caption   := GLanguageHandler.Text[kPixelSize];
  Label3.Caption   := GLanguageHandler.Text[kSizeCoefficientHelpText];
  Label5.Caption   := GLanguageHandler.Text[kBackground];
  Label4.Caption   := GLanguageHandler.Text[kPixelShape];
  rbSquare.Caption := GLanguageHandler.Text[kSquare];
  rbCircle.Caption := GLanguageHandler.Text[kCircle];

  bOK.Caption      := GLanguageHandler.Text[kOK];
  bCancel.Caption  := GLanguageHandler.Text[kCancel];
end;


procedure TfrmExportGIF.ShapeNorfolkDigitalMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (cdExportGIF.Execute) then
    ShapeNorfolkDigital.Brush.Color := cdExportGIF.Color;
end;


procedure TfrmExportGIF.eFileNameChange(Sender: TObject);
begin
  if (eFileName.Text <> '') then
    bOK.Enabled := True;
end;


procedure TfrmExportGIF.bSaveClick(Sender: TObject);
begin
  if sdExportGIF.Execute then begin
    eFileName.Text := sdExportGIF.FileName;

    bOK.Enabled    := True;
  end;
end;


end.
