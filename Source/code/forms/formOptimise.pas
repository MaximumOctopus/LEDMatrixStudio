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


unit formOptimise;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,

  languagehandler,

  exportoptions,

  optimisation, xglobal;


type
  TfrmOptimise = class(TForm)
    Bevel1: TBevel;
    sbOptimise: TSpeedButton;
    mMemo: TMemo;
    sbCopyOutput: TSpeedButton;
    mData: TMemo;
    Panel1: TPanel;
    gbOutputOptions: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    cbDataSize: TComboBox;
    cbLanguageFormat: TComboBox;
    cbPerRow: TComboBox;
    procedure sbOptimiseClick(Sender: TObject);
    procedure sbCopyOutputClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    MatrixData : TStringList;

    procedure SetGUILanguageText;
  public
    { Public declarations }
    MaxFrames : integer;
    HexFormat : boolean;
    ColumnsLSB, RowsLSB : integer;
    ExportFormat : integer;
    ColumnsDirection : integer;
    XType : integer;
    beforeData, afterData : integer;

    procedure PopulateMatrixData;

    function  GetDataSize: TNumberSize;
  end;

var
  frmOptimise: TfrmOptimise;

function DoOptimise: boolean;


implementation


uses utility;

{$R *.dfm}


function DoOptimise: boolean;
begin
  with TfrmOptimise.Create(Application) do
    try
      ShowModal;

      MatrixData.Free;

      Result := true;
    finally
      Free;
    end;
end;


procedure TfrmOptimise.FormCreate(Sender: TObject);
begin
  MatrixData := TStringList.Create;

  cbDataSize.Items.Add('4 bits (one nybble)');
  cbDataSize.Items.Add('1 byte');
  cbDataSize.Items.Add('2 bytes');
  cbDataSize.Items.Add('4 byte');
  cbDataSize.Items.Add('8 bytes');

  cbDataSize.ItemIndex := 1;

  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportCommaSeparated]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportPICAXEEEPROM]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportCCpp1Dimensional]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportCCpp2Dimensional]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportCCppFastLED]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportPython1Dimensional]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportPython2Dimensional]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportMicrochip]);
  cbLanguageFormat.Items.Add(GLanguageHandler.Text[kExportPascal]);

  cbLanguageFormat.ItemIndex := 0;

  cbPerRow.Items.Add('4');
  cbPerRow.Items.Add('5');
  cbPerRow.Items.Add('7');
  cbPerRow.Items.Add('8');
  cbPerRow.Items.Add('10');
  cbPerRow.Items.Add('16');
  cbPerRow.Items.Add('20');
  cbPerRow.Items.Add('32');
  cbPerRow.Items.Add('64');

  cbPerRow.ItemIndex := 4;

  SetGUILanguageText;
end;


procedure TfrmOptimise.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kOptimise] + ' (beta)';

  sbOptimise.Caption := GLanguageHandler.Text[kOptimise];
  sbCopyOutput.Caption := GLanguageHandler.Text[kCopyOutput];

  gbOutputOptions.Caption := GLanguageHandler.Text[kOutputOptions];
  Label4.Caption := GLanguageHandler.Text[kDataSize];
  Label5.Caption := GLanguageHandler.Text[kLanguage];
  Label6.Caption := GLanguageHandler.Text[kOutput];
  Label7.Caption := GLanguageHandler.Text[kPerRow];
end;


procedure TfrmOptimise.sbCopyOutputClick(Sender: TObject);
 begin
  mMemo.CopyToClipboard;
end;


function TfrmOptimise.GetDataSize: TNumberSize;
begin
  case cbDataSize.ItemIndex of
    0 : Result := ns8Bit;
    1 : Result := ns8Bit;
    2 : Result := ns16bit;
    3 : Result := ns32bit;
    4 : Result := ns64bit;
  else
    Result := ns8Bit;
  end;
end;


procedure TfrmOptimise.sbOptimiseClick(Sender: TObject);
 var
  lOutput : TStringList;
  t : integer;
  teo : TExportOptions;

 begin
  lOutput := TStringList.Create;

  PopulateMatrixData;

  HexFormat  := True;
  ColumnsLSB := 0;
  RowsLSB    := 0;

  // ===========================================================================
  // ===========================================================================
  // ===========================================================================

  teo.NumberSize := GetDataSize;
  teo.Language   := TExportLanguage(cbLanguageFormat.ItemIndex);
  teo.LineCount  := StrToIntDef(cbPerRow.Text, 10);

  if OptimiseDataSimple(teo, MatrixData, lOutput) then begin

    mMemo.Clear;

    for t:= 0 to lOutput.Count - 1 do begin
      mMemo.Lines.Add(lOutput[t]);
    end;

  end;

  // ===========================================================================
  // ===========================================================================
  // ===========================================================================

  lOutput.Free;
end;


procedure TfrmOptimise.PopulateMatrixData;
var
  t, l : integer;
  lData : string;
  lLine : string;
  lInComment : boolean;

begin
  MatrixData.Clear;

  for t:= 0 to mData.Lines.Count - 1 do begin

    lLine := mData.Lines[t];

    lData      := '';
    lInComment := False;

    if (lLine <> '') then begin

      for l := 1 to length(lLine) do begin

        if (not(lInComment)) then begin

          if lLine[l] = ',' then begin
            MatrixData.Add(lData);

            lData := '';
          end
          else begin
            if (lLine[l] = '''') or
               (lLine[l] = '/') or
               (lLine[l] = ';') then
              lInComment := true
            else begin
              if TUtility.IsAlphaNumeric(lLine[l]) then
                lData := lData + lLine[l];
            end;
          end;
        end;

      end;

      if lData <> '' then
        MatrixData.Add(lData);
    end;
  end;
end;


end.
