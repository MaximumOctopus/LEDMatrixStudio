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

unit formExportCode;


interface


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, System.UITypes,

  languagehandler,

  exportoptions, exportutility, matrixconstants, profilehandler,

  exportoptions_monobi, exportoptions_rgb;


type
  TfrmExportCode = class(TForm)
    Panel2: TPanel;
    gbPlatforms: TGroupBox;
    Memo1: TMemo;
    cbPlatforms: TComboBox;
    gbCodeTemplates: TGroupBox;
    cbCode: TComboBox;
    Label1: TLabel;
    lSource: TLabel;
    Label3: TLabel;
    lLSB: TLabel;
    Label5: TLabel;
    lFormat: TLabel;
    lNumbers: TLabel;
    Label8: TLabel;
    lGrouping: TLabel;
    Label10: TLabel;
    lOutput: TLabel;
    Label12: TLabel;
    lRGB: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    lDirection: TLabel;
    lScan: TLabel;
    sbSave: TBitBtn;
    sdExportCode: TSaveDialog;
    sbCopyToClipboard: TBitBtn;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    lMinWidth: TLabel;
    lMaxWidth: TLabel;
    lMinHeight: TLabel;
    lMaxHeight: TLabel;
    bClose: TBitBtn;
    lDescription: TLabel;
    iMiW: TImage;
    iMaW: TImage;
    iMiH: TImage;
    iMaH: TImage;
    gbSettings: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbPlatformsChange(Sender: TObject);
    procedure cbCodeChange(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure sbCopyToClipboardClick(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Integer);
  private
    procedure SetGUILanguageText;

    procedure UpdatePlatformList;
    procedure UpdateCodeList;
    procedure UpdateSettingsDisplay;

    procedure LoadCode;
  public
    { Public declarations }
  end;


var
  frmExportCode: TfrmExportCode;


function DoExportCode(mtype : TMatrixMode): word;


implementation


{$R *.dfm}


uses utility, xglobal;


var
  eeo : TExportOptions;


function DoExportCode(mtype : TMatrixMode): word;
begin
  with TfrmExportCode.Create(Application) do
    try
      ShowModal;

      Result := ModalResult;
    finally
      Free;
    end;
end;


procedure TfrmExportCode.FormCreate(Sender: TObject);
begin
  SetGUILanguageText;

  UpdatePlatformList;
end;


procedure TfrmExportCode.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TfrmExportCode.FormConstrainedResize(Sender: TObject; var MinWidth,
  MinHeight, MaxWidth, MaxHeight: Integer);
begin
  MinHeight := 490;
  MinWidth  := 490;
end;


procedure TfrmExportCode.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kGenerateCode];
  gbPlatforms.Caption := GLanguageHandler.Text[kPlatforms];
  gbCodeTemplates.Caption := GLanguageHandler.Text[kCodeTemplate];

  gbSettings.Caption := GLanguageHandler.Text[kSettings];

  Label1.Caption := GLanguageHandler.Text[kSource];
  Label2.Caption := GLanguageHandler.Text[kDirection];
  Label4.Caption := GLanguageHandler.Text[kScan];
  Label3.Caption := GLanguageHandler.Text[kLSB];
  Label5.Caption := GLanguageHandler.Text[kFormat];
  Label8.Caption := GLanguageHandler.Text[kNumbers];
  Label10.Caption := GLanguageHandler.Text[kGrouping];
  Label12.Caption := GLanguageHandler.Text[kOutput];
  Label14.Caption := GLanguageHandler.Text[kRGB];
  Label6.Caption := GLanguageHandler.Text[kMinWidth];
  Label7.Caption := GLanguageHandler.Text[kMaxWidth];
  Label9.Caption := GLanguageHandler.Text[kMinHeight];
  Label11.Caption := GLanguageHandler.Text[kMaxHeight];

  sbSave.Caption := GLanguageHandler.Text[kSave];
  sbCopyToClipboard.Caption := GLanguageHandler.Text[kCopyToClipboard];
  bClose.Caption := GLanguageHandler.Text[kClose];
end;


procedure TfrmExportCode.sbCopyToClipboardClick(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
end;


procedure TfrmExportCode.sbSaveClick(Sender: TObject);
begin
  if sdExportCode.Execute then
    Memo1.Lines.SaveToFile(sdExportCode.FileName);
end;


procedure TfrmExportCode.cbCodeChange(Sender: TObject);
begin
  LoadCode;
end;


procedure TfrmExportCode.cbPlatformsChange(Sender: TObject);
begin
  UpdateCodeList;
end;


procedure TfrmExportCode.UpdatePlatformList;
var
  searchResult : TSearchRec;

begin
  cbPlatforms.Clear;

  if FindFirst(ExtractFilePath(Application.ExeName) + 'codetemplates\*.*', faDirectory, searchResult) = 0 then begin
    repeat
      if (searchResult.Name <> '.') and (searchResult.Name <> '..') and
         (searchResult.Name[1] <> '_') and
         ((searchResult.Attr and faDirectory) = faDirectory) then
        cbPlatforms.Items.Add(searchResult.Name);
    until FindNext(searchResult) <> 0;

    FindClose(searchResult);
  end;

  if cbPlatforms.Items.Count <> 0 then begin
    cbPlatforms.ItemIndex := 0;
    cbPlatforms.Enabled   := True;

    UpdateCodeList;
  end
  else
    cbPlatforms.Enabled := False;
end;


procedure TfrmExportCode.UpdateCodeList;
var
  searchResult : TSearchRec;

begin
  cbCode.Clear;

  if FindFirst(ExtractFilePath(Application.ExeName) + 'codetemplates\' + cbPlatforms.Text + '\*.*', faAnyFile, searchResult) = 0 then begin
    repeat
      if (searchResult.Name <> '.') and (searchResult.Name <> '..') and
         (Pos('.template', searchResult.Name) = 0) then
        cbCode.Items.Add(searchResult.Name);
    until FindNext(searchResult) <> 0;

    FindClose(searchResult);
  end;

  if cbCode.Items.Count <> 0 then begin
    cbCode.ItemIndex := 0;
    cbCode.Enabled   := True;

    LoadCode;
  end
  else begin
    cbCode.Items.Add(GLanguageHandler.Text[kNoCodeTemplatesFound]);

    cbCode.ItemIndex := 0;
    cbCode.Enabled   := False;
  end;
end;


procedure TfrmExportCode.LoadCode;
var
  totalentrycount : integer;
  lOutput : TStringList;
  lUnique : TStringList;

begin
  if (not(cbCode.Enabled)) then Exit;

  eeo := TProfileHandler.LoadExportProfileFromFile(ExtractFilePath(Application.Exename) + 'codetemplates\' + cbPlatforms.Text + '\' + cbCode.Text + '.template');

  eeo.RGBBrightness := 100;

  if (eeo.Valid) then begin

    lDescription.Caption := eeo.Information;

    totalentrycount := 0;

    eeo.StartFrame     := 1;
    eeo.EndFrame       := MatrixMain.FrameCount;

    if (eeo.Source = rsRows) then begin
      eeo.SelectiveStart := 1;
      eeo.SelectiveEnd   := MatrixMain.Matrix.Height;
    end
    else begin
      eeo.SelectiveStart := 1;
      eeo.SelectiveEnd   := MatrixMain.Matrix.Width;
    end;

    UpdateSettingsDisplay;

    lOutput := TStringList.Create;
    lUnique := TStringList.Create;

    if eeo.RGBEnabled then
      TExportRGB.CreateExportAnimationRGB(eeo, lOutput, totalentrycount, lUnique)
    else
      TExportMonoBi.CreateExportAnimation(eeo, lOutput, totalentrycount, lUnique);

    // =========================================================================

    Memo1.Lines.LoadFromFile(ExtractFilePath(Application.Exename) + 'codetemplates\' + cbPlatforms.Text + '\' + cbCode.Text);

    if lOutput.Count <> 0 then begin

      Memo1.Lines.BeginUpdate;

      // == first lets process the data token ==================================

      if Pos('{$LMS_MATRIX_DATA$}', Memo1.Text) <> 0 then
        Memo1.Text := StringReplace(Memo1.Text, '{$LMS_MATRIX_DATA$}', lOutput.Text, [rfReplaceAll]);

      // == Now the rest of the tokens =========================================

      if Pos('{$LMS_FRAMES$}', Memo1.Text) <> 0 then
        Memo1.Text := StringReplace(Memo1.Text, '{$LMS_FRAMES$}', IntToStr(MatrixMain.FrameCount), [rfReplaceAll]);

      if Pos('{$LMS_FRAMES_MINUS_ONE$}', Memo1.Text) <> 0 then
        Memo1.Text := StringReplace(Memo1.Text, '{$LMS_FRAMES_MINUS_ONE$}', IntToStr(MatrixMain.FrameCount - 1), [rfReplaceAll]);

      if Pos('{$LMS_BYTES$}', Memo1.Text) <> 0 then
        Memo1.Text := StringReplace(Memo1.Text, '{$LMS_BYTES$}', IntToStr(totalentrycount), [rfReplaceAll]);

      if Pos('{$LMS_COUNT$}', Memo1.Text) <> 0 then
        Memo1.Text := StringReplace(Memo1.Text, '{$LMS_COUNT$}', IntToStr(totalentrycount), [rfReplaceAll]);

      if Pos('{$LMS_MATRIX_WIDTH$}', Memo1.Text) <> 0 then
        Memo1.Text := StringReplace(Memo1.Text, '{$LMS_MATRIX_WIDTH$}', IntToStr(MatrixMain.Matrix.Width), [rfReplaceAll]);

      if Pos('{$LMS_MATRIX_HEIGHT$}', Memo1.Text) <> 0 then
        Memo1.Text := StringReplace(Memo1.Text, '{$LMS_MATRIX_HEIGHT$}', IntToStr(MatrixMain.Matrix.Height), [rfReplaceAll]);

      // == Misc tokens ========================================================

      if Pos('{$LMS_DATE$}', Memo1.Text) <> 0 then
        Memo1.Text := StringReplace(Memo1.Text, '{$LMS_DATE$}', TUtility.GetDate, [rfReplaceAll]);

      if Pos('{$LMS_TIME$}', Memo1.Text) <> 0 then
        Memo1.Text := StringReplace(Memo1.Text, '{$LMS_TIME$}', TUtility.GetTime, [rfReplaceAll]);

      // =======================================================================

      Memo1.Lines.EndUpdate;
    end;

    lUnique.Free;
    lOutput.Free;
  end
  else
    MessageDlg(GLanguageHandler.Text[kErrorLoadingTemplate] + #13#10 + #13#10 +  '"' + ExtractFilePath(Application.Exename) + 'codetemplates\' + cbPlatforms.Text + '\' + cbCode.Text + '.template"' , mtError, [mbOK], 0);
end;


procedure TfrmExportCode.UpdateSettingsDisplay;

  function GetDimensionConstraint(aLabel : TLabel; limit, dim, mode : integer; aImage : TImage): string;
   begin
    aLabel.Font.Color := clBlack;

    case mode of
      0 : begin // minimum
            if limit = 0 then
              Result := GLanguageHandler.Text[kUnlimited]
            else begin
              if dim < limit then begin
                Result := IntToStr(dim) + ' < ' + IntToStr(limit);

                aLabel.Font.Color := clMaroon;

                aImage.Visible    := True;
              end
              else begin
                Result := IntToStr(limit);
              end;
            end;
          end;
      1 : begin // maximum
            if limit = 0 then
              Result := GLanguageHandler.Text[kUnlimited]
            else begin
              if dim > limit then begin
                Result := IntToStr(dim) + ' > ' + IntToStr(limit);

                aLabel.Font.Color := clMaroon;

                aImage.Visible    := True;
              end
              else
                Result := IntToStr(limit);
            end;
          end;
    end;
  end;

 begin
  if cbCode.Enabled then begin
    case eeo.Source of
      rsColumns : begin
                    lSource.Caption    := GLanguageHandler.Text[kColumns];
                    lDirection.Caption := TExportUtility.GetOrientation(eeo, False);
                    lScan.Caption      := TExportUtility.GetScanDirection(eeo, False);
                    lOutput.Caption    := TExportUtility.GetLineContent(eeo, False);
                  end;
      rsRows    : begin
                    lSource.Caption    := GLanguageHandler.Text[kRows];
                    lDirection.Caption := TExportUtility.GetOrientation(eeo, False);
                    lScan.Caption      := TExportUtility.GetScanDirection(eeo, False);
                    lOutput.Caption    := TExportUtility.GetLineContent(eeo, False);
                  end;
    end;

    lLSB.Caption    := TExportUtility.GetLSB(eeo, False);
    lFormat.Caption := TExportUtility.GetLanguage(eeo, False);

    case eeo.NumberFormat of
      nfDecimal : lNumbers.Caption := GLanguageHandler.Text[kDecimal];
      nfBinary  : lNumbers.Caption := GLanguageHandler.Text[kBinary];
      nfHex     : lNumbers.Caption := GLanguageHandler.Text[kHex];
    end;

    lGrouping.Caption := TExportUtility.GetNumberSize(eeo.Language, eeo.NumberSize, False);

    if eeo.RGBEnabled then begin
      lRGB.Caption := TExportUtility.GetRGBMode(eeo, False);
    end
    else
      lRGB.Caption := GLanguageHandler.Text[kDisabled];
  end
  else begin
    lSource.Caption    := GLanguageHandler.Text[kNA];
    lDirection.Caption := GLanguageHandler.Text[kNA];
    lScan.Caption      := GLanguageHandler.Text[kNA];
    lOutput.Caption    := GLanguageHandler.Text[kNA];
    lLSB.Caption       := GLanguageHandler.Text[kNA];
    lFormat.Caption    := GLanguageHandler.Text[kNA];
    lNumbers.Caption   := GLanguageHandler.Text[kNA];
    lGrouping.Caption  := GLanguageHandler.Text[kNA];
    lRGB.Caption       := GLanguageHandler.Text[kNA];
  end;

  // ===========================================================================

  lMinWidth.Caption  := GetDimensionConstraint(lMinWidth, eeo.MinWidth, MatrixMain.Matrix.Width, 0, iMiW);
  lMaxWidth.Caption  := GetDimensionConstraint(lMaxWidth, eeo.MaxWidth, MatrixMain.Matrix.Width, 1, iMaW);

  lMinHeight.Caption := GetDimensionConstraint(lMinHeight, eeo.MinHeight, MatrixMain.Matrix.Height, 0, iMiH);
  lMaxHeight.Caption := GetDimensionConstraint(lMaxHeight, eeo.MaxHeight, MatrixMain.Matrix.Height, 1, iMaH);
end;


end.
