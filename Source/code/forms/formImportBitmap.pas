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

unit formImportBitmap;


interface


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, ExtCtrls, StdCtrls, Buttons, Vcl.ComCtrls,

  languagehandler;


type
  TfrmImportBitmap = class(TForm)
    opdMain: TOpenPictureDialog;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    Bevel1: TBevel;
    lHelpText: TLabel;
    gbSettings: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    cbWidth: TComboBox;
    cbHeight: TComboBox;
    eFrames: TEdit;
    lWidth: TLabel;
    lHeight: TLabel;
    lImageHeight: TLabel;
    lImageWidth: TLabel;
    Bevel2: TBevel;
    bAuto: TBitBtn;
    cbRGBImport: TCheckBox;
    cbCreateNew: TCheckBox;
    pcImportMethod: TPageControl;
    tsSingleImage: TTabSheet;
    tsMultipleImages: TTabSheet;
    Shape1: TShape;
    iImport: TImage;
    lFileName: TLabel;
    bSelect: TBitBtn;
    iMultipleImages: TImage;
    eMIFirstImage: TEdit;
    eMIPattern: TEdit;
    Label5: TLabel;
    Label7: TLabel;
    sbMISelectFirstImage: TSpeedButton;
    Label8: TLabel;
    eMIFirstFrame: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    eMIPadLength: TEdit;
    lImageLengthExample: TLabel;
    procedure bSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bAutoClick(Sender: TObject);
    procedure cbRGBImportClick(Sender: TObject);
    procedure sbMISelectFirstImageClick(Sender: TObject);
  private
    procedure SetGUILanguageText;
    procedure SetMultipleImageDetails;
  public
    ImportMode    : integer;

    RGBImport     : boolean;
    CreateNew     : boolean;
    ImageFilename : string;
    FrameCount    : integer;
    FrameWidth    : integer;
    FrameHeight   : integer;

    FirstFrame    : integer;
    PadLength     : integer;
    Pattern       : string;
  end;


const
  ImportModeInvalid        = 0;
  ImportModeSingleImage    = 1;
  ImportModeMultipleImages = 2;


var
  frmImportBitmap: TfrmImportBitmap;


implementation


{$R *.dfm}


procedure TfrmImportBitmap.FormCreate(Sender: TObject);
var
  x : integer;

begin
  ImageFilename := '';
  FrameCount    := -1;

  for x := 1 to 64 do begin
    cbWidth.Items.Add(IntToStr(x));
    cbHeight.Items.Add(IntToStr(x));
  end;

  cbWidth.ItemIndex  := 15;
  cbHeight.ItemIndex := 15;

  SetGUILanguageText;
end;


procedure TfrmImportBitmap.SetGUILanguageText;
begin
  Caption := GLanguageHandler.Text[kImportFromBitmap];

  tsSingleImage.Caption := GLanguageHandler.Text[kSingleImage];
  bSelect.Caption := GLanguageHandler.Text[kSelect];
  lFileName.Caption := GLanguageHandler.Text[kNoImagesSelected];

  tsMultipleImages.Caption := GLanguageHandler.Text[kMultipleImages];
  Label5.Caption := GLanguageHandler.Text[kFirstImage];
  Label8.Caption := GLanguageHandler.Text[kFirstFrame];
  Label10.Caption := GLanguageHandler.Text[kIndexLength];
  lImageLengthExample.Caption := GLanguageHandler.Text[kIndexLengthExample];
  Label7.Caption := GLanguageHandler.Text[kPattern];

  gbSettings.Caption := GLanguageHandler.Text[kImportSettings];
  cbRGBImport.Caption := GLanguageHandler.Text[kRGBImport];
  cbCreateNew.Caption := GLanguageHandler.Text[kCreateNewMatrixClearsAllData];
  Label2.Caption := GLanguageHandler.Text[kFramesToImport];
  Label3.Caption := GLanguageHandler.Text[kFrameWidth];
  Label4.Caption := GLanguageHandler.Text[kFrameHeight];
  bAuto.Caption := GLanguageHandler.Text[kAuto];

  lWidth.Caption := GLanguageHandler.Text[kWidth];
  lHeight.Caption := GLanguageHandler.Text[kHeight];
  lImageWidth.Caption := GLanguageHandler.Text[kNA];
  lImageHeight.Caption := GLanguageHandler.Text[kNA];

  lHelpText.Caption := GLanguageHandler.Text[kForNonRGBImport];

  bOK.Caption := GLanguageHandler.Text[kOK];
  bCancel.Caption := GLanguageHandler.Text[kCancel];
end;


procedure TfrmImportBitmap.sbMISelectFirstImageClick(Sender: TObject);
begin
  if opdMain.Execute then begin
    iMultipleImages.Picture.LoadFromFile(opdMain.FileName);

    cbWidth.Text       := IntToStr(iMultipleImages.Width);
    cbHeight.Text      := IntToStr(iMultipleImages.Height);

    eMIFirstImage.Text := opdMain.FileName;

    SetMultipleImageDetails;

    bOK.Enabled := True;
  end;
end;

procedure TfrmImportBitmap.bOKClick(Sender: TObject);
begin
  ImportMode := ImportModeInvalid;

  case pcImportMethod.ActivePageIndex of
    0 : begin
          if ImageFilename <> '' then begin
            ImportMode  := ImportModeSingleImage;

            FrameCount  := StrToInt(eFrames.Text);
            FrameWidth  := StrToInt(cbWidth.Text);
            FrameHeight := StrToInt(cbHeight.Text);

            RGBImport   := cbRGBImport.Checked;
            CreateNew   := cbCreateNew.Checked;
          end;
        end;
    1 : begin
          if eMIFirstImage.Text <> '' then begin
            ImportMode  := ImportModeMultipleImages;

            FrameCount  := StrToInt(eFrames.Text);
            FrameWidth  := StrToInt(cbWidth.Text);
            FrameHeight := StrToInt(cbHeight.Text);

            FirstFrame  := StrToInt(eMIFirstFrame.Text);
            PadLength   := StrToInt(eMIPadLength.Text);
            Pattern     := eMIPattern.Text;

            RGBImport   := cbRGBImport.Checked;
            CreateNew   := cbCreateNew.Checked;
          end;
        end;
  end;

end;


procedure TfrmImportBitmap.cbRGBImportClick(Sender: TObject);
begin
  lHelpText.Visible := not cbRGBImport.Checked;
end;


procedure TfrmImportBitmap.bCancelClick(Sender: TObject);
begin
  FrameCount := -1;
end;


procedure TfrmImportBitmap.bSelectClick(Sender: TObject);
begin
  if opdMain.Execute then begin
    if FileExists(opdMain.Filename) then begin
      iImport.Picture.LoadFromFile(opdMain.Filename);

      ImageFilename        := opdMain.Filename;

      lFileName.Caption    := opdMain.Filename;

      lImageWidth.Caption  := IntToStr(iImport.Picture.Width);
      lImageHeight.Caption := IntToStr(iImport.Picture.Height);

      bOK.Enabled := True;
    end;
  end;
end;


procedure TfrmImportBitmap.bAutoClick(Sender: TObject);
var
  lFrameCount : integer;

begin
  if (pcImportMethod.ActivePageIndex = 0) then begin

    lFrameCount   := StrToIntDef(eFrames.Text, 1);

    cbWidth.Text  := IntToStr(Round(iImport.Picture.Width / lFrameCount));
    cbHeight.Text := IntToStr(iImport.Picture.Height);
  end;
end;


procedure TfrmImportBitmap.SetMultipleImageDetails;
var
  t : integer;
  lFileName : string;
  lInNumber : boolean;
  lFirstFrame : string;
  lPatternEnd : integer;
  lPatternStart : integer;

begin
  lInNumber := False;
  lFileName := ExtractFileName(eMIFirstImage.Text);

  lPatternStart := -1;
  lPatternEnd   := -1;

  for t := length(lFileName) downto 1 do begin
    if (lFileName[t] >= '0') and (lFileName[t] <= '9') then begin
      if lInNumber then
        lFirstFrame := lFileName[t] + lFirstFrame
      else begin
        lInNumber := True;

        lFirstFrame := lFileName[t];

        lPatternEnd := t;
      end;
    end
    else
      if lInNumber then begin
        lPatternStart := t;

        Break;
      end;
  end;

  if (lPatternStart <> -1) and (lPatternEnd <> -1) then begin
    eMIFirstFrame.Text := lFirstFrame;

    eMIPattern.Text    := ExtractFilePath(eMIFirstImage.Text) + Copy(lFileName, 1, lPatternStart) + '$$' + Copy(lFileName, lPatternEnd + 1);

    eMIPadLength.Text  := IntToStr(lPatternEnd - lPatternStart);
  end;
end;


end.
