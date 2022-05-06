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


unit formAddLayer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,

  languagehandler;

type
  TfrmAddLayer = class(TForm)
    Bevel1: TBevel;
    Image1: TImage;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label6: TLabel;
    eName: TEdit;
    cbSourceLayer: TComboBox;
    cbCopyFrom: TCheckBox;
    Label2: TLabel;
    procedure cbCopyFromClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetGUILanguageText;
  public
    { Public declarations }
  end;


  TAddObject = record
    Process   : boolean;
    Name      : string;
    CopyFrom  : boolean;
    CopyLayer : integer;
  end;


function DoAddLayer(var aLayers : TStringList): TAddObject;


var
  frmAddLayer: TfrmAddLayer;


implementation


{$R *.dfm}


function DoAddLayer(var aLayers : TStringList): TAddObject;
var
  t : integer;

begin
  with TfrmAddLayer.Create(Application) do
    try
      Result.Process := False;

      eName.Text := GLanguageHandler.Text[kLayer] + ' ' + IntToStr(aLayers.Count + 1);

      for t := 0 to aLayers.Count - 1 do
        cbSourceLayer.Items.Add(aLayers[t]);

      cbSourceLayer.ItemIndex := 0;

      // =======================================================================

      ShowModal;

      // =======================================================================

      if ModalResult = mrOK then begin
        Result.Process    := True;

        Result.Name      := eName.Text;
        Result.CopyFrom  := cbCopyFrom.Checked;
        Result.CopyLayer := cbSourceLayer.ItemIndex;
      end;

    finally
      Free;
    end
end;


procedure TfrmAddLayer.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;
end;


procedure TfrmAddLayer.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kAddNewLayer];
  Label1.Caption := GLanguageHandler.Text[kName];
  cbCopyFrom.Caption := GLanguageHandler.Text[kCopyFrom];
  Label6.Caption := GLanguageHandler.Text[kExtraLayersIncreaseApplicationMemoryUsage];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmAddLayer.cbCopyFromClick(Sender: TObject);
begin
  cbSourceLayer.Enabled := cbCopyFrom.Checked;
end;


end.
