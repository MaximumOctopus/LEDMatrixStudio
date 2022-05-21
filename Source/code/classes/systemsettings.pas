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

unit systemsettings;


interface


uses SysUtils, Classes, Registry, Windows, Graphics, Forms, System.TypInfo,

     AppSettings, Toolbars, DataDisplay, PreviewSettings, ProjectSettings,

     matrixconstants;


type
  TAutoSaveInterval = (asTwoMinutes, asFiveMinutes, asTenMinutes);
  TRowColumnData    = (rdOff, rdData, rdIndex);
  TPixelSize        = (psSizeAuto, psSize10, psSize15, psSize20, psSize25, psSize30, psSize40, psSize50);


  TExportGIFObject = record
    Process    : boolean;
    FileName   : string;
    PixelSize  : integer;
    PixelShape : integer;
    Background : integer;
  end;


  TSystemSettingsObject = class
  public
    App               : TAppSettings;
    Project           : TProjectSettings;

    LEDColoursSingle  : array[0..5] of integer;
    LEDColoursBi      : array[0..5] of integer;
    RGBBackground     : integer;
    LEDRGBColours     : array[0..5] of integer;
    SelectionColours  : array[1..3] of integer;
    Toolbars          : TToolbars;
    RowColumnData     : TRowColumnData;
    PixelSize         : integer;
    PixelShape        : TPixelShape;
    AnimSpeed         : integer;
    UseFormatData     : boolean;
    AutoSaveEnabled   : boolean;
    AutoSaveInterval  : TAutoSaveInterval;
    PreviewOptions    : TPreviewOptions;
    RGBPalette        : array[0..15] of integer;

    ExportGIFSettings : TExportGIFObject;

    FileHistory       : TStringList;

    constructor Create;
    destructor  Destroy; Override;

    procedure LoadSettings;
    procedure SaveSettings;
  private
    FReg : TRegistry;

    function ReadRegistryInteger(keyname : string; defaultval : Integer): Integer;
    function ReadRegistryBool(keyname : string; defaultval : boolean): boolean;
    function ReadRegistryString(keyname : string; defaultval : string): string;
  end;


const
  CPixelSizeAuto = 0;
  CPixelSize10   = 10;
  CPixelSize15   = 15;
  CPixelSize20   = 20;
  CPixelSize25   = 25;
  CPixelSize30   = 30;
  CPixelSize40   = 40;
  CPixelSize50   = 50;


implementation


const
  defaultRGBPalatte : array[0..15] of integer = ($000000, $FFFFFF,
                                                 $0000FF, $0088FF, $0044FF,
                                                 $00FFFF, $88FFFF, $44FFFF,
                                                 $00FF00, $88FF88, $44FF44,
                                                 $FF0000, $FF8800,
                                                 $FF00FF, $FF44FF, $FF88FF);


constructor TSystemSettingsObject.Create;
begin
  FileHistory := TStringList.Create;

  App.DataFilename          := '';
  App.HexPrefix             := '';
  App.BinaryPrefix          := '';
  App.OpenBracket           := '(';
  App.CloseBracket          := ')';
  App.ASCIIIndex            := 32;   // start ascii code (space)
  App.ExportUpdateMaxPixels := 100000;

  ExportGIFSettings.FileName   := '';
  ExportGIFSettings.PixelSize  := 1;
  ExportGIFSettings.PixelShape := 0;
  ExportGIFSettings.Background := $000000;

  LoadSettings;
end;


destructor TSystemSettingsObject.Destroy;
begin
  FileHistory.Free;
end;


procedure TSystemSettingsObject.LoadSettings;
var
  lColour, t : integer;
  s : string;

begin
  FReg := TRegistry.Create(KEY_READ);

  FReg.RootKey := HKEY_CURRENT_USER;
  FReg.OpenKey('\software\freshney.org\MatrixBuilder', True);

  // ===========================================================================

  Project.MatrixMode  := TMatrixMode(ReadRegistryInteger('matrixtype', Ord(mtMono)));
  Project.Width       := ReadRegistryInteger('gridwidth', 7);
  Project.Height      := ReadRegistryInteger('gridheight', 7);
  Project.Pixel       := TPixelShape(ReadRegistryInteger('pixelshape', Ord(psSquare)));

  // ===========================================================================

  LEDColoursSingle[0]         := ReadRegistryInteger('offcolour', clBlack);
  LEDColoursSingle[1]         := ReadRegistryInteger('oncolour', clWhite);
  LEDColoursSingle[2]         := ReadRegistryInteger('oncolour2', clBlack);
  LEDColoursSingle[3]         := ReadRegistryInteger('oncolour3', clBlack);
  LEDColoursSingle[4]         := ReadRegistryInteger('selectcolour', clBlue);
  LEDColoursSingle[5]         := ReadRegistryInteger('lightboxcolour', $00DDDDDD);

  LEDColoursBi[0]             := ReadRegistryInteger('offcolourbi', clBlack);
  LEDColoursBi[1]             := ReadRegistryInteger('oncolourbi', clRed);
  LEDColoursBi[2]             := ReadRegistryInteger('oncolour2bi', clGreen);
  LEDColoursBi[3]             := ReadRegistryInteger('oncolour3bi', clYellow);
  LEDColoursBi[4]             := ReadRegistryInteger('selectcolourbi', clBlue);
  LEDColoursBi[5]             := ReadRegistryInteger('lightboxcolourbi', $00DDDDDD);

  RGBBackground               := ReadRegistryInteger('rgbbackground', clBlack);
  LEDRGBColours[1]            := ReadRegistryInteger('LEDRGBColoursLMB', clRed);
  LEDRGBColours[2]            := ReadRegistryInteger('LEDRGBColoursMMB', clBlue);
  LEDRGBColours[3]            := ReadRegistryInteger('LEDRGBColoursRMB', clYellow);

  // ===========================================================================

  SelectionColours[1] := ReadRegistryInteger('sSelectionLMB', 3);
  SelectionColours[2] := ReadRegistryInteger('sSelectionMMB', 2);
  SelectionColours[3] := ReadRegistryInteger('sSelectionRMB', 0);

  // ===========================================================================

  App.Language             := ReadRegistryString('language', 'English');

  if (App.Language[1] = '&') then
    App.Language := Copy(App.Language, 2)
  else
    App.Language := App.Language;

  // ===========================================================================

  App.BackgroundColour     := ReadRegistryInteger('displaybackground', clBtnFace);

  // ===========================================================================

  App.LastSaveLocation     := ReadRegistryString('savelocation', '');
  App.LastLoadLocation     := ReadRegistryString('loadlocation', '');

  if App.LastSaveLocation = '' then
    App.LastSaveLocation := ExtractFilePath(Application.ExeName) + 'saves\';

  if App.LastLoadLocation = '' then
    App.LastLoadLocation := ExtractFilePath(Application.ExeName) + 'saves\';

  App.CustomSpeed          := ReadRegistryInteger('customspeed', 1000);

  if (App.CustomSpeed <= 0) then
    App.CustomSpeed := 1000;

  // ===========================================================================

  App.ExportUpdateMaxPixels := ReadRegistryInteger('exportupdatemaxpixels', 100000);
  App.ExportPreviewSize     := ReadRegistryInteger('exportpreviewsize', 512);

  // ===========================================================================

  App.HexPrefix             := ReadRegistryString('hexprefix2', '0x');

  // ===========================================================================

  PixelSize         := ReadRegistryInteger('pixelsize', CPixelSize20);

  PixelShape        := TPixelShape(ReadRegistryInteger('pixelshape', Ord(psSquare)));

  // ===========================================================================

  Toolbars.Animation  := ReadRegistryBool('showanimtoolbar', True);

  AnimSpeed           := ReadRegistryInteger('animspeed', 1000);

  // ===========================================================================

  Toolbars.ColumnRow  := ReadRegistryBool('columnrowtoolbar', True);

  Toolbars.RGBPalette := ReadRegistryBool('rgbpalettetoolbar', True);

  Toolbars.Pattern    := ReadRegistryBool('patterntoolbar', True);

  // ===========================================================================

  UseFormatData            := ReadRegistryBool('useformatdata', True);

  // ===========================================================================

  AutoSaveEnabled          := ReadRegistryBool('autosave', False);
  AutoSaveInterval         := TAutoSaveInterval(ReadRegistryInteger('autosaveinterval', Ord(asTwoMinutes)));

  // ===========================================================================

  PreviewOptions.Enabled   := ReadRegistryBool('previewactive', False);

  PreviewOptions.Size      := ReadRegistryInteger('previewsize', 1);
  PreviewOptions.View      := TViewShape(ReadRegistryInteger('previewview', Ord(vsSquare)));
  PreviewOptions.Void      := ReadRegistryInteger('previewvoid', 15);
  PreviewOptions.Offset    := ReadRegistryInteger('previewoffset', 0);
  PreviewOptions.Direction := ReadRegistryBool('previewoffsetdirection', False);

  // ===========================================================================

  for t := 0 to 15 do begin
    lColour := ReadRegistryInteger('rgbpalette' + IntToStr(t), -1);

    if (lColour = -1) then
      lColour := defaultRGBPalatte[t];

    RGBPalette[t] := lColour;
  end;

  // ===========================================================================

  RowColumnData            := TRowColumnData(ReadRegistryInteger('rowcolumndata', Ord(rdOff)));

  // ===========================================================================

  t := 0;

  s := ReadRegistryString('reopen_0', '');

  while (s <> '') and (t < 20) do begin
    FileHistory.Add(s);

    inc(t);

    s := ReadRegistryString('reopen_' + IntToStr(t), '');
  end;

  // ===========================================================================

  ExportGIFSettings.FileName   := ReadRegistryString('exportgiffilename', '');
  ExportGIFSettings.PixelSize  := ReadRegistryInteger('exportgifpixelsize', 1);
  ExportGIFSettings.PixelShape := ReadRegistryInteger('exportgifpixelshape', 0);
  ExportGIFSettings.Background := ReadRegistryInteger('exportgifbackground', $000000);

  // ===========================================================================

  FReg.Free;
end;


procedure TSystemSettingsObject.SaveSettings;
var
  t : integer;

begin
  FReg := TRegistry.Create(KEY_WRITE);

  FReg.RootKey := HKEY_CURRENT_USER;
  FReg.OpenKey('\software\freshney.org\MatrixBuilder', True);

  FReg.WriteInteger('matrixtype',       Ord(Project.MatrixMode));
  FReg.WriteInteger('gridwidth',        Project.Width);
  FReg.WriteInteger('gridheight',       Project.Height);
  FReg.WriteInteger('pixelshape',       Ord(Project.Pixel));

  FReg.WriteInteger('oncolour',         LEDColoursSingle[1]);
  FReg.WriteInteger('oncolour2',        LEDColoursSingle[2]);
  FReg.WriteInteger('oncolour3',        LEDColoursSingle[3]);
  FReg.WriteInteger('offcolour',        LEDColoursSingle[0]);
  FReg.WriteInteger('selectcolour',     LEDColoursSingle[4]);
  FReg.WriteInteger('lightboxcolour',   LEDColoursSingle[5]);

  FReg.WriteInteger('oncolourbi',       LEDColoursBi[1]);
  FReg.WriteInteger('oncolour2bi',      LEDColoursBi[2]);
  FReg.WriteInteger('oncolour3bi',      LEDColoursBi[3]);
  FReg.WriteInteger('offcolourbi',      LEDColoursBi[0]);
  FReg.WriteInteger('selectcolourbi',   LEDColoursBi[4]);
  FReg.WriteInteger('lightboxcolourbi', LEDColoursBi[5]);

  FReg.WriteInteger('rgbbackground',    RGBBackground);
  FReg.WriteInteger('LEDRGBColoursLMB', LEDRGBColours[1]);
  FReg.WriteInteger('LEDRGBColoursMMB', LEDRGBColours[2]);
  FReg.WriteInteger('LEDRGBColoursRMB', LEDRGBColours[3]);

  FReg.WriteString('language',          App.Language);

  FReg.WriteString('savelocation',      App.LastSaveLocation);
  FReg.WriteString('loadlocation',      App.LastLoadLocation);

  FReg.WriteInteger('customspeed',      App.CustomSpeed);

  FReg.WriteInteger('displaybackground',App.BackgroundColour);

  FReg.WriteInteger('pixelsize',        Ord(PixelSize));
  FReg.WriteInteger('pixelshape',       Ord(PixelShape));

  // ===========================================================================

  FReg.WriteInteger('sSelectionLMB',    SelectionColours[1]);
  FReg.WriteInteger('sSelectionMMB',    SelectionColours[2]);
  FReg.WriteInteger('sSelectionRMB',    SelectionColours[3]);

  // ===========================================================================

  FReg.WriteBool('showanimtoolbar',     Toolbars.Animation);
  FReg.WriteBool('columnrowtoolbar',    Toolbars.ColumnRow);
  FReg.WriteBool('patterntoolbar',      Toolbars.Pattern);
  FReg.WriteBool('rgbpalettetoolbar',   Toolbars.RGBPalette);

  FReg.WriteInteger('animspeed',        AnimSpeed);

  // ===========================================================================

  FReg.WriteBool('autosave',            AutoSaveEnabled);
  FReg.WriteInteger('autosaveinterval', Ord(AutoSaveInterval));

  // ===========================================================================

  FReg.WriteBool('useformatdata', UseFormatData);

  // ===========================================================================

  FReg.WriteBool('previewactive',          PreviewOptions.Enabled);

  FReg.WriteInteger('previewsize',         PreviewOptions.Size);
  FReg.WriteInteger('previewview',         Ord(PreviewOptions.View));
  FReg.WriteInteger('previewvoid',         PreviewOptions.Void);

  FReg.WriteInteger('previewoffset',       PreviewOptions.Offset);
  FReg.WriteBool('previewoffsetdirection', PreviewOptions.Direction);

  // ===========================================================================

  for t := 0 to 15 do
    FReg.WriteInteger('rgbpalette' + IntToStr(t), RGBPalette[t]);

  // ===========================================================================

  FReg.WriteInteger('rowcolumndata',       Ord(RowColumnData));

  // ===========================================================================

  FReg.WriteInteger('exportupdatemaxpixels', App.ExportUpdateMaxPixels);
  FReg.WriteInteger('exportpreviewsize',     App.ExportPreviewSize);

  // ===========================================================================

  FReg.WriteString('hexprefix2',              App.HexPrefix);

  // ===========================================================================

  if FileHistory.Count <> 0 then begin
    for t := 0 to FileHistory.Count - 1 do begin
      if FileHistory[t] <> '' then
        FReg.WriteString('reopen_' + IntToStr(t), FileHistory[t]);
    end;
  end
  else
    for t := 0 to 19 do
      FReg.WriteString('reopen_' + IntToStr(t), '');

  // ===========================================================================

  FReg.WriteString('exportgiffilename',    ExportGIFSettings.FileName);
  FReg.WriteInteger('exportgifpixelsize',  ExportGIFSettings.PixelSize);
  FReg.WriteInteger('exportgifpixelshape', ExportGIFSettings.PixelShape);
  FReg.WriteInteger('exportgifbackground', ExportGIFSettings.Background);

  // ===========================================================================

  FReg.Free;
end;


function TSystemSettingsObject.ReadRegistryInteger(keyname : string; defaultval : Integer): Integer;
begin
  if FReg.ValueExists(keyname) then begin
    try
      Result := FReg.ReadInteger(keyname)
    except
      Result := defaultval;
    end;
  end
  else
    Result := defaultval;
end;


function TSystemSettingsObject.ReadRegistryBool(keyname : string; defaultval : boolean): boolean;
begin
  if FReg.ValueExists(keyname) then begin
    try
      Result := FReg.ReadBool(keyname)
    except
      Result := defaultval;
    end;
  end
  else
    Result := defaultval;
end;


function TSystemSettingsObject.ReadRegistryString(keyname : string; defaultval : string): string;
begin
  if FReg.ValueExists(keyname) then begin
    if FReg.ReadString(keyname) = '' then
      Result := defaultval
    else begin
      try
        Result := FReg.ReadString(keyname);
      except
        Result := defaultval;
      end;
    end;
  end
  else
    Result := defaultval;
end;


end.
