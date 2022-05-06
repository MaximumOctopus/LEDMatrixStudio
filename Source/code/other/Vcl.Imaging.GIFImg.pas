{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2014 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Vcl.Imaging.GIFImg;

(*

This source code is adapted from the original TGIFImage written by Anders Melander.

CodeGear is grateful for his donation of his source code to the VCL.

*)

{$IFDEF CPUX64}
  {$DEFINE PUREPASCAL}
{$ENDIF CPUX64}

interface
////////////////////////////////////////////////////////////////////////////////
//
//		Conditional Compiler Symbols
//
////////////////////////////////////////////////////////////////////////////////
(*
  PIXELFORMAT_SLOW		When this symbol is defined, the internal
  				PixelFormat routines are used in some places
                                instead of TBitmap.PixelFormat.
                                The symbol is defined by default.

  CREATEDIBSECTION_SLOW		If this symbol is defined, TDIBWriter will
  				use global memory as scanline storage, instead
                                of a DIB section.
                                Benchmarks have shown that a DIB section is
                                twice as slow as global memory.
                                The symbol is defined by default.
                                The symbol requires that PIXELFORMAT_SLOW
                                is defined.

  PIXELFORMAT_BROKEN		Define this symbol to use TGIFImage's internal
  				DIB reader instead of TBitmap's PixelFormat and
                                Scanline when importing bitmaps.
                                Some versions of TBitmap has
                                serious problems converting DDBs to DIBs using
                                the PixelFormat property.
*)

{$define PIXELFORMAT_SLOW}
{$define CREATEDIBSECTION_SLOW}
{.$define PIXELFORMAT_BROKEN}


////////////////////////////////////////////////////////////////////////////////
//
//		Compiler Options required to compile this library
//
////////////////////////////////////////////////////////////////////////////////
{$ALIGN ON}             // Record alignment
{$BOOLEVAL OFF}		// Short circuit boolean evaluation.
{$EXTENDEDSYNTAX ON}	// Enable Delphi Pascal extensions.
{$LONGSTRINGS ON}       // String = AnsiString.
{$WARN SYMBOL_PLATFORM OFF} // Disable platform warnings. This library is only supported on Windows.
{$TYPEDADDRESS ON}	// @ operator returns typed pointer.
{$WRITEABLECONST OFF}   // Typed consts are R/O.
{$ifdef BCB}
  {$ObjExportAll ON}    // Required for C++ Builder
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//			External dependecies
//
////////////////////////////////////////////////////////////////////////////////
uses
  System.SysUtils,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  System.Classes;

{$HPPEMIT ''}
{$HPPEMIT '#ifdef USEPACKAGES'}
{$HPPEMIT ' #pragma link "vclimg.bpi"'}
{$HPPEMIT '#else'}
{$HPPEMIT ' #ifndef _WIN64'}
{$HPPEMIT '  #pragma link "vclimg.lib"'}
{$HPPEMIT ' #else'}
{$HPPEMIT '  #pragma link "vclimg.a"'}
{$HPPEMIT ' #endif'}
{$HPPEMIT '#endif'}
{$HPPEMIT '/* automatically link to gifimg.obj so that the property editors are registered */'}
{$HPPEMIT LINKUNIT}
{$HPPEMIT ''}

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFImage library version
//
////////////////////////////////////////////////////////////////////////////////
const
  GIFVersion		= $0300;
  GIFVersionMajor	= 3;
  GIFVersionMinor	= 0;
  GIFVersionRelease	= 7;

const
  // Max number of colors supported by GIF
  GIFMaxColors	= 256;

type
  TGIFVersion = (gvUnknown, gv87a, gv89a);
  TGIFVersionRec = array[0..2] of AnsiChar;

const
  GIFVersions : array[gv87a..gv89a] of TGIFVersionRec = (AnsiString('87a'), AnsiString('89a'));

type
  // TGIFImage mostly throws exceptions of type GIFException
  GIFException = class(EInvalidGraphic);

  // Severity level as indicated in the Warning methods and the OnWarning event
  TGIFSeverity = (gsInfo, gsWarning, gsError);

////////////////////////////////////////////////////////////////////////////////
//
//			Forward declarations
//
////////////////////////////////////////////////////////////////////////////////
type
  TGIFImage = class;
  TGIFFrame = class;


////////////////////////////////////////////////////////////////////////////////
//
//			TGIFItem
//
////////////////////////////////////////////////////////////////////////////////
  TGIFItem = class(TPersistent)
  strict private
    FImage: TGIFImage;
  strict protected
    function GetVersion: TGIFVersion; virtual;
  protected
    procedure Warning(Severity: TGIFSeverity; const Msg: string);
  public
    constructor Create(AImage: TGIFImage); virtual;

    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToFile(const Filename: string);
    procedure LoadFromFile(const Filename: string);
    property Version: TGIFVersion read GetVersion;
    property Image: TGIFImage read FImage;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//			TGIFList
//
////////////////////////////////////////////////////////////////////////////////
  TGIFList = class(TPersistent)
  strict private
    FItems: TList;
  strict protected
    function GetItem(Index: Integer): TGIFItem;
    procedure SetItem(Index: Integer; Item: TGIFItem);
    function GetCount: Integer;
    procedure Warning(Severity: TGIFSeverity; Message: string); virtual;
    function GetImage: TGIFImage; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Item: TGIFItem): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: TGIFItem;
    function IndexOf(Item: TGIFItem): Integer;
    procedure Insert(Index: Integer; Item: TGIFItem);
    function Last: TGIFItem;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TGIFItem): Integer;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;

    property Items[Index: Integer]: TGIFItem read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property List: TList read FItems;
    property Image: TGIFImage read GetImage;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFColorMap
//
////////////////////////////////////////////////////////////////////////////////
  // One way to do it:
  //  TBaseColor = (bcRed, bcGreen, bcBlue);
  //  TGIFColor = array[bcRed..bcBlue] of BYTE;
  // Another way:
  TGIFColor = record
    Red: byte;
    Green: byte;
    Blue: byte;
  end;

  TColorMap = array of TGIFColor;

  TUsageCount = record
    Count: integer; // # of pixels using color index
    Index: integer; // Color index
  end;
  TColormapHistogram = array[0..255] of TUsageCount;
  TColormapReverse = array[0..255] of byte;

  TGIFColorMap = class(TPersistent)
  strict private
    FColorMap: TColorMap;
    FCount: integer;
    FOptimized: boolean;
  strict protected
    function GetColor(Index: integer): TColor;
    procedure SetColor(Index: integer; Value: TColor);
    function GetBitsPerPixel: integer;
    function DoOptimize: boolean;
    procedure SetCapacity(Size: integer);
    procedure Warning(Severity: TGIFSeverity; const Msg: string); virtual; abstract;
    procedure BuildHistogram(var Histogram: TColormapHistogram); virtual; abstract;
    procedure MapImages(var Map: TColormapReverse); virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;
    class function Color2RGB(Color: TColor): TGIFColor;
    class function RGB2Color(Color: TGIFColor): TColor;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream; ACount: integer);
    procedure Assign(Source: TPersistent); override;
    function IndexOf(Color: TColor): integer;
    function Add(Color: TColor): integer;
    function AddUnique(Color: TColor): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    function Optimize: boolean; virtual; abstract;
    procedure Changed; virtual; abstract;
    procedure ImportPalette(Palette: HPalette);
    procedure ImportColorTable(Pal: pointer; ACount: integer);
    procedure ImportDIBColors(Handle: HDC);
    procedure ImportColorMap(const Map: TColorMap; ACount: integer);
    function ExportPalette: HPalette;
    property Colors[Index: integer]: TColor read GetColor write SetColor; default;
    property Data: TColorMap read FColorMap;
    property Count: integer read FCount;
    property Optimized: boolean read FOptimized write FOptimized;
    property BitsPerPixel: integer read GetBitsPerPixel;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFHeader
//
////////////////////////////////////////////////////////////////////////////////
{$ALIGN 1}
  TLogicalScreenDescriptor = record
    ScreenWidth: word;              { logical screen width }
    ScreenHeight: word;             { logical screen height }
    PackedFields: byte;             { packed fields }
    BackgroundColorIndex: byte;     { index to global color table }
    AspectRatio: byte;              { actual ratio = (AspectRatio + 15) / 64 }
  end;
{$ALIGN ON}

  TGIFHeader = class(TGIFItem)
  strict private
    FLogicalScreenDescriptor: TLogicalScreenDescriptor;
    FColorMap: TGIFColorMap;
  strict protected
    function GetVersion: TGIFVersion; override;
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(Color: TColor);
    procedure SetBackgroundColorIndex(Index: BYTE);
    function GetBitsPerPixel: integer;
    function GetColorResolution: integer;
  public
    constructor Create(GIFImage: TGIFImage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure Clear;
    procedure Prepare;

    property Version: TGIFVersion read GetVersion;
    property Width: WORD read FLogicalScreenDescriptor.ScreenWidth
                         write FLogicalScreenDescriptor.ScreenWidth;
    property Height: WORD read FLogicalScreenDescriptor.ScreenHeight
                          write FLogicalScreenDescriptor.Screenheight;
    property BackgroundColorIndex: BYTE read FLogicalScreenDescriptor.BackgroundColorIndex
                                        write SetBackgroundColorIndex;
    property BackgroundColor: TColor read GetBackgroundColor
                                     write SetBackgroundColor;
    property AspectRatio: BYTE read FLogicalScreenDescriptor.AspectRatio
                               write FLogicalScreenDescriptor.AspectRatio;
    property ColorMap: TGIFColorMap read FColorMap;
    property BitsPerPixel: integer read GetBitsPerPixel;
    property ColorResolution: integer read GetColorResolution;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFExtensionType = BYTE;
  TGIFExtension = class;
  TGIFExtensionClass = class of TGIFExtension;

  TGIFGraphicControlExtension = class;

  TGIFExtension = class(TGIFItem)
  strict private
    FFrame: TGIFFrame;
  strict protected
    function GetExtensionType: TGIFExtensionType; virtual; abstract;
    function GetVersion: TGIFVersion; override;
    function DoReadFromStream(Stream: TStream): TGIFExtensionType;
  public
     // Ignore compiler warning about hiding base class constructor
    constructor Create(AFrame: TGIFFrame); reintroduce; virtual;
    destructor Destroy; override;
    class procedure RegisterExtension(elabel: BYTE; eClass: TGIFExtensionClass);
    class function FindExtension(Stream: TStream): TGIFExtensionClass;
    class function FindSubExtension(Stream: TStream): TGIFExtensionClass; virtual;

    procedure Assign(Source: TPersistent); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property ExtensionType: TGIFExtensionType read GetExtensionType;
    property Frame: TGIFFrame read FFrame;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFFrame
//
////////////////////////////////////////////////////////////////////////////////
  TGIFExtensionList = class(TGIFList)
  strict private
    FFrame: TGIFFrame;
  strict protected
    function GetExtension(Index: Integer): TGIFExtension;
    procedure SetExtension(Index: Integer; Extension: TGIFExtension);
    function GetImage: TGIFImage; override;
  public
    constructor Create(AFrame: TGIFFrame);
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Extensions[Index: Integer]: TGIFExtension read GetExtension write SetExtension; default;
    property Frame: TGIFFrame read FFrame;
  end;

//{$ALIGN 1}
  TImageDescriptor = packed record
    Separator: byte;	{ fixed value of ImageSeparator }
    Left: word;		{ Column in pixels in respect to left edge of logical screen }
    Top: word;		{ row in pixels in respect to top of logical screen }
    Width: word;	{ width of image in pixels }
    Height: word;	{ height of image in pixels }
    PackedFields: byte;	{ Bit fields }
  end;
//{$ALIGN ON}

  TGIFFrame = class(TGIFItem)
  strict private
    FBitmap: TBitmap;
    FMask: HBitmap;
    FNeedMask: boolean;
    FLocalPalette: HPalette;
    FData: PByte;
    FDataSize: integer;
    FColorMap: TGIFColorMap;
    FImageDescriptor: TImageDescriptor;
    FExtensions: TGIFExtensionList;
    FGCE: TGIFGraphicControlExtension;
  private
    function GetTransparent: boolean;
  strict protected
    procedure Prepare;
    procedure Compress(Stream: TStream);
    procedure Decompress(Stream: TStream);
    function GetVersion: TGIFVersion; override;
    function GetInterlaced: boolean;
    procedure SetInterlaced(Value: boolean);
    function GetColorResolution: integer;
    function GetBitsPerPixel: integer;
    procedure AssignTo(Dest: TPersistent); override;
    function DoGetBitmap: TBitmap;
    function DoGetDitherBitmap: TBitmap;
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    procedure FreeMask;
    procedure FreePalette;
    function GetEmpty: Boolean;
    function GetPalette: HPALETTE;
    procedure SetPalette(Value: HPalette);
    function GetActiveColorMap: TGIFColorMap;
    function GetBoundsRect: TRect;
    procedure SetBoundsRect(const Value: TRect);
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
    function GetClientRect: TRect;
    function GetPixel(x, y: integer): BYTE;
    procedure SetPixel(x, y: integer; const Value: BYTE);
    function GetScanline(y: integer): pointer;
    procedure SetHeight(const Value: WORD);
    procedure SetLeft(const Value: WORD);
    procedure SetTop(const Value: WORD);
    procedure SetWidth(const Value: WORD);
    procedure NewBitmap;
    procedure FreeBitmap;
    procedure NewImage;
    procedure ClearImage;
    procedure FreeImage;
    procedure NeedImage;
    function HasMask: boolean;
    function GetHasBitmap: boolean;
    procedure SetHasBitmap(Value: boolean);
  protected
    property GCE: TGIFGraphicControlExtension read FGCE write FGCE;
  public
    constructor Create(GIFImage: TGIFImage); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Dormant;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect;
      DoTransparent, DoTile: boolean);
    procedure StretchDraw(ACanvas: TCanvas; const Rect: TRect;
      DoTransparent, DoTile: boolean);
    procedure Crop;
    procedure Merge(Previous: TGIFFrame);
    function ScaleRect(DestRect: TRect): TRect;

    property HasBitmap: boolean read GetHasBitmap write SetHasBitmap;
    property Left: WORD read FImageDescriptor.Left write SetLeft;
    property Top: WORD read FImageDescriptor.Top write SetTop;
    property Width: WORD read FImageDescriptor.Width write SetWidth;
    property Height: WORD read FImageDescriptor.Height write SetHeight;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property ClientRect: TRect read GetClientRect;
    property Interlaced: boolean read GetInterlaced write SetInterlaced;
    property ColorMap: TGIFColorMap read FColorMap;
    property ActiveColorMap: TGIFColorMap read GetActiveColorMap;
    property Data: PByte read FData;
    property DataSize: integer read FDataSize;
    property Extensions: TGIFExtensionList read FExtensions;
    property Version: TGIFVersion read GetVersion;
    property ColorResolution: integer read GetColorResolution;
    property BitsPerPixel: integer read GetBitsPerPixel;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Mask: HBitmap read FMask;
    property Palette: HPALETTE read GetPalette write SetPalette;
    property Empty: boolean read GetEmpty;
    property Transparent: boolean read GetTransparent;
    property GraphicControlExtension: TGIFGraphicControlExtension read FGCE;
    property Pixels[x, y: integer]: BYTE read GetPixel write SetPixel;
    property Scanline[y: integer]: pointer read GetScanline;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFTrailer
//
////////////////////////////////////////////////////////////////////////////////
  TGIFTrailer = class(TGIFItem)
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFGraphicControlExtension
//
////////////////////////////////////////////////////////////////////////////////
  // Graphic Control Extension block a.k.a GCE
  TGIFGCERec = record
    BlockSize: byte;         { should be 4 }
    PackedFields: Byte;
    DelayTime: Word;         { in centiseconds }
    TransparentColorIndex: Byte;
    Terminator: Byte;
  end;

  TDisposalMethod = (dmNone, dmNoDisposal, dmBackground, dmPrevious);

  TGIFGraphicControlExtension = class(TGIFExtension)
  strict private
    FGCExtension: TGIFGCERec;
  strict protected
    function GetExtensionType: TGIFExtensionType; override;
    function GetTransparent: boolean;
    procedure SetTransparent(Value: boolean);
    function GetTransparentColor: TColor;
    procedure SetTransparentColor(Color: TColor);
    function GetTransparentColorIndex: BYTE;
    procedure SetTransparentColorIndex(Value: BYTE);
    function GetDelay: WORD;
    procedure SetDelay(Value: WORD);
    function GetUserInput: boolean;
    procedure SetUserInput(Value: boolean);
    function GetDisposal: TDisposalMethod;
    procedure SetDisposal(Value: TDisposalMethod);

  public
    constructor Create(AFrame: TGIFFrame); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Delay: WORD read GetDelay write SetDelay;
    property Transparent: boolean read GetTransparent write SetTransparent;
    property TransparentColorIndex: BYTE read GetTransparentColorIndex
                                         write SetTransparentColorIndex;
    property TransparentColor: TColor read GetTransparentColor
                                      write SetTransparentColor;
    property UserInput: boolean read GetUserInput
                                write SetUserInput;
    property Disposal: TDisposalMethod read GetDisposal
                                       write SetDisposal;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFTextExtension
//
////////////////////////////////////////////////////////////////////////////////
//{$ALIGN 1}
  TGIFPlainTextExtensionRec = packed record
    BlockSize: byte;         { should be 12 }
    Left, Top, Width, Height: Word;
    CellWidth, CellHeight: Byte;
    TextFGColorIndex,
    TextBGColorIndex: Byte;
  end;
//{$ALIGN ON}

  TGIFTextExtension = class(TGIFExtension)
  strict private
    FText: TStrings;
    FPlainTextExtension: TGIFPlainTextExtensionRec;
  strict protected
    function GetExtensionType: TGIFExtensionType; override;
    function GetForegroundColor: TColor;
    procedure SetForegroundColor(Color: TColor);
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(Color: TColor);
    procedure SetText(const Value: TStrings);
  public
    constructor Create(ASubImage: TGIFFrame); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Left: WORD read FPlainTextExtension.Left write FPlainTextExtension.Left;
    property Top: WORD read FPlainTextExtension.Top write FPlainTextExtension.Top;
    property GridWidth: WORD read FPlainTextExtension.Width write FPlainTextExtension.Width;
    property GridHeight: WORD read FPlainTextExtension.Height write FPlainTextExtension.Height;
    property CharWidth: BYTE read FPlainTextExtension.CellWidth write FPlainTextExtension.CellWidth;
    property CharHeight: BYTE read FPlainTextExtension.CellHeight write FPlainTextExtension.CellHeight;
    property ForegroundColorIndex: BYTE read FPlainTextExtension.TextFGColorIndex write FPlainTextExtension.TextFGColorIndex;
    property ForegroundColor: TColor read GetForegroundColor;
    property BackgroundColorIndex: BYTE  read FPlainTextExtension.TextBGColorIndex write FPlainTextExtension.TextBGColorIndex;
    property BackgroundColor: TColor read GetBackgroundColor;
    property Text: TStrings read FText write SetText;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFCommentExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFCommentExtension = class(TGIFExtension)
  strict private
    FText: TStrings;
  strict protected
    function GetExtensionType: TGIFExtensionType; override;
    procedure SetText(const Value: TStrings);
  public
    constructor Create(AFrame: TGIFFrame); override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property Text: TStrings read FText write SetText;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFApplicationExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFIdentifierCode = array[0..7] of AnsiChar;
  TGIFAuthenticationCode = array[0..2] of AnsiChar;
  TGIFApplicationRec = record
    Identifier: TGIFIdentifierCode;
    Authentication: TGIFAuthenticationCode;
  end;

  TGIFApplicationExtension = class;
  TGIFAppExtensionClass = class of TGIFApplicationExtension;

  TGIFApplicationExtension = class(TGIFExtension)
  strict private
    FIdent: TGIFApplicationRec;
    function GetAuthentication: string;
    function GetIdentifier: string;
  strict protected
    function GetExtensionType: TGIFExtensionType; override;
    procedure SetAuthentication(const Value: string);
    procedure SetIdentifier(const Value: string);
    procedure SaveData(Stream: TStream); virtual; abstract;
    procedure LoadData(Stream: TStream); virtual; abstract;
  public
    constructor Create(ASubImage: TGIFFrame); overload; override;
    constructor Create(ASubImage: TGIFFrame; const Ident: TGIFApplicationRec); reintroduce; overload;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    class procedure RegisterExtension(eIdent: TGIFApplicationRec; eClass: TGIFAppExtensionClass);
    class function FindSubExtension(Stream: TStream): TGIFExtensionClass; override;
    property Identifier: string read GetIdentifier write SetIdentifier;
    property Authentication: string read GetAuthentication write SetAuthentication;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFUnknownAppExtension
//
////////////////////////////////////////////////////////////////////////////////
  TGIFBlock = class(TObject)
  strict private
    FSize: BYTE;
    FData: pointer;
  public
    constructor Create(ASize: integer);
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property Size: BYTE read FSize;
    property Data: pointer read FData;
  end;

  TGIFUnknownAppExtension = class(TGIFApplicationExtension)
  strict private
    FBlocks: TList;
  strict protected
    procedure SaveData(Stream: TStream); override;
    procedure LoadData(Stream: TStream); override;
  public
    constructor Create(ASubImage: TGIFFrame); override;
    destructor Destroy; override;
    property Blocks: TList read FBlocks;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFAppExtNSLoop
//
////////////////////////////////////////////////////////////////////////////////
  TGIFAppExtNSLoop = class(TGIFApplicationExtension)
  strict private
    FLoops: WORD;
    FBufferSize: DWORD;
  strict protected
    procedure SaveData(Stream: TStream); override;
    procedure LoadData(Stream: TStream); override;
  public
    constructor Create(AFrame: TGIFFrame); override;
    property Loops: WORD read FLoops write FLoops;
    property BufferSize: DWORD read FBufferSize write FBufferSize;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFImage
//
////////////////////////////////////////////////////////////////////////////////
  TGIFImageList = class(TGIFList)
  strict private
    FImage: TGIFImage;
  strict protected
    function GetFrame(Index: Integer): TGIFFrame;
    procedure SetFrame(Index: Integer; AFrame: TGIFFrame);
    function GetImage: TGIFImage; override;
  public
    constructor Create(AImage: TGIFImage);
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property Frames[Index: Integer]: TGIFFrame read GetFrame write SetFrame; default;
  end;

  // Color reduction methods
  TColorReduction = (
    rmNone,			// Do not perform color reduction
    rmWindows20,		// Reduce to the Windows 20 color system palette
    rmWindows256,		// Reduce to the Windows 256 color halftone palette (Only works in 256 color display mode)
    rmWindowsGray,		// Reduce to the Windows 4 grayscale colors
    rmMonochrome,		// Reduce to a black/white monochrome palette
    rmGrayScale,		// Reduce to a uniform 256 shade grayscale palette
    rmNetscape,		        // Reduce to the Netscape 216 color palette
    rmQuantize,		        // Reduce to optimal 2^n color palette
    rmQuantizeWindows,		// Reduce to optimal 256 color windows palette
    rmPalette                   // Reduce to custom palette
    );
  TDitherMode = (
    dmNearest,			// Nearest color matching w/o error correction
    dmFloydSteinberg,		// Floyd Steinberg Error Diffusion dithering
    dmStucki,			// Stucki Error Diffusion dithering
    dmSierra,			// Sierra Error Diffusion dithering
    dmJaJuNI,			// Jarvis, Judice & Ninke Error Diffusion dithering
    dmSteveArche,		// Stevenson & Arche Error Diffusion dithering
    dmBurkes			// Burkes Error Diffusion dithering
    );

  // Optimization options
  TGIFOptimizeOption = (
    ooCrop,			// Crop animated GIF frames
    ooMerge,			// Merge pixels of same color
    ooCleanup,			// Remove comments and application extensions
    ooColorMap                  // Sort color map by usage and remove unused entries
    );
  TGIFOptimizeOptions = set of TGIFOptimizeOption;

  // Animation loop behaviour
  TGIFAnimationLoop = (
    glDisabled,                 // Never loop
    glEnabled,                  // Loop is specified in GIF
    glContinously               // Loop continously regardless of GIF
    );

  // Auto dithering of GIF output to Netscape 216 color palette
  TGIFDithering = (
    gdDisabled,                 // Never dither
    gdEnabled,                  // Always dither
    gdAuto                      // Dither if Desktop DC supports <= 256 colors.
    );

  TCustomGIFRenderer = class;

  TGIFWarning = procedure(Sender: TObject; Severity: TGIFSeverity; const Msg: string) of object;

  TGIFImage = class(TGraphic)
  strict private
    FAnimate: boolean;
    FAnimateLoop: TGIFAnimationLoop;
    FDithering: TGIFDithering;

    IsDrawing: Boolean;
    IsInsideGetPalette: boolean;
    FImages: TGIFImageList;
    FHeader: TGIFHeader;
    FGlobalPalette: HPalette;
    FColorReduction: TColorReduction;
    FReductionBits: integer;
    FOnWarning: TGIFWarning;
    FBitmap: TBitmap;
    FAnimationSpeed: integer;
    FDrawBackgroundColor: TColor;
    FOnStartPaint: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FOnAfterPaint: TNotifyEvent;
    FOnLoop: TNotifyEvent;
    FOnEndPaint: TNotifyEvent;
    FRenderer: TCustomGIFRenderer;
    FDitherMode: TDitherMode;
    FDrawSuspendCount: Integer;
    FSuspendedAnimation: boolean;
  strict protected
    function GetHeight: Integer; override;
    procedure SetHeight(Value: Integer); override;
    function GetWidth: Integer; override;
    procedure SetWidth(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function Equals(Graphic: TGraphic): Boolean; override;
    function GetPalette: HPALETTE; override;
    procedure SetPalette(Value: HPalette); override;
    function GetEmpty: Boolean; override;
    procedure WriteData(Stream: TStream); override;
    function GetIsTransparent: Boolean;
    function GetVersion: TGIFVersion;
    function GetColorResolution: integer;
    function GetBitsPerPixel: integer;
    function GetBackgroundColorIndex: BYTE;
    procedure SetBackgroundColorIndex(const Value: BYTE);
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(const Value: TColor);
    function GetAspectRatio: BYTE;
    procedure SetAspectRatio(const Value: BYTE);
    procedure SetAnimationSpeed(Value: integer);
    procedure SetReductionBits(Value: integer);
    procedure NewImage;
    function GetBitmap: TBitmap;
    function NewBitmap: TBitmap;
    procedure FreePalette;
    procedure FreeBitmap;
    function GetColorMap: TGIFColorMap;
    function GetDoDither: boolean;
    procedure InternalClear;
    procedure SetAnimate(const Value: boolean);
    procedure SetAnimateLoop(const Value: TGIFAnimationLoop);
    procedure SetDithering(const Value: TGIFDithering);
    procedure SetTransparent(Value: boolean); override;
    function CreateRenderer: TCustomGIFRenderer; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Dormant;
    procedure SaveToStream(Stream: TStream); override;
    class function CanLoadFromStream(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function Add(Source: TPersistent): TGIFFrame;
    procedure Pack;
    procedure OptimizeColorMap;
    procedure Optimize(Options: TGIFOptimizeOptions;
      ColorReduction: TColorReduction = rmNone; DitherMode: TDitherMode = dmNearest;
      ReductionBits: integer = 8);
    procedure Clear;
    procedure Warning(Sender: TObject; Severity: TGIFSeverity; const Msg: string); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    function EffectiveBackgroundColor: TColor;
    procedure StopDraw;
    procedure SuspendDraw;
    procedure ResumeDraw;

    property GlobalColorMap: TGIFColorMap read GetColorMap;
    property Version: TGIFVersion read GetVersion;
    property Images: TGIFImageList read FImages;
    property ColorResolution: integer read GetColorResolution;
    property BitsPerPixel: integer read GetBitsPerPixel;
    property BackgroundColorIndex: BYTE read GetBackgroundColorIndex write SetBackgroundColorIndex;
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor;
    property AspectRatio: BYTE read GetAspectRatio write SetAspectRatio;
    property IsTransparent: boolean read GetIsTransparent;
    property DrawBackgroundColor: TColor read FDrawBackgroundColor write FDrawBackgroundColor;
    property ColorReduction: TColorReduction read FColorReduction write FColorReduction;
    property ReductionBits: integer read FReductionBits write SetReductionBits;
    property DitherMode: TDitherMode read FDitherMode write FDitherMode;
    property AnimationSpeed: integer read FAnimationSpeed write SetAnimationSpeed;
    property Bitmap: TBitmap read GetBitmap; // Volatile - beware!
    property OnWarning: TGIFWarning read FOnWarning write FOnWarning;
    property OnStartPaint: TNotifyEvent read FOnStartPaint write FOnStartPaint;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
    property OnEndPaint	: TNotifyEvent read FOnEndPaint	 write FOnEndPaint	;

    // Transparent property is declared in TGraphic.
    // property Transparent: boolean read GetTransparent write SetTransparent;
    property Animate: boolean read FAnimate write SetAnimate;
    property AnimateLoop: TGIFAnimationLoop read FAnimateLoop write SetAnimateLoop;
    property Dithering: TGIFDithering read FDithering write SetDithering;
    property ShouldDither: boolean read GetDoDither;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			Rendering
//
////////////////////////////////////////////////////////////////////////////////
  TCustomGIFRenderer = class
  strict private
    FTransparent: boolean;
    FAnimate: boolean;
    FImage: TGIFImage;
    FCanvas: TCanvas;
    FDestRect: TREct;
    FBackgroundColor: TColor;
    FSpeed: integer;
    FFrame: TGIFFrame;
    FFrameIndex: integer;
    FChanged: boolean;
    FLoopMax: integer;
    FLoopCount: integer;
    FAnimating: boolean;
  strict protected
    procedure SetAnimate(const Value: boolean); virtual;
    procedure SetBackgroundColor(const Value: TColor); virtual;
    procedure SetSpeed(const Value: integer); virtual;
    procedure SetTransparent(const Value: boolean); virtual;
    procedure InternalSetFrameIndex(Value: integer); virtual;
    procedure SetFrameIndex(Value: integer); virtual;
    function GetBitmap: TBitmap; virtual; abstract;
    procedure Changed; virtual;
    procedure CheckChange; virtual;
    procedure Reset; virtual;
    procedure Loop; virtual;
    procedure DoNextFrame; virtual;
    procedure SetLoopMax(Value: integer);
    procedure Initialize; virtual;
    property HasChanged: boolean read FChanged;
  public
    constructor Create(AImage: TGIFImage); virtual;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; // Draw current state
    procedure NextFrame; virtual; // Move to next frame
    procedure StartAnimation; virtual;
    procedure StopAnimation; virtual;
    procedure HaltAnimation; virtual;

    property Speed: integer read FSpeed write SetSpeed;
    property Transparent: boolean read FTransparent write SetTransparent;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Animate: boolean read FAnimate write SetAnimate;
    property FrameIndex: integer read FFrameIndex write SetFrameIndex;

    property Image: TGIFImage read FImage;
    property TargetCanvas: TCanvas read FCanvas;
    property TargetRect: TRect read FDestRect;
    property Frame: TGIFFrame read FFrame;
    property LoopMax: integer read FLoopMax;
    property LoopCount: integer read FLoopCount;
    property Animating: boolean read FAnimating;
    property Bitmap: TBitmap read GetBitmap;
  end;

  TGIFRenderer = class(TCustomGIFRenderer)
  strict private
    FBackground: TBitmap;
    FDisposal: TDisposalMethod;
    FPrevDisposal: TDisposalMethod;
    FPrevFrame: TGIFFrame;
    FPreviousBuffer: TBitmap;
    FBuffer: TBitmap;
    FFrameDelay: integer;
    FNeedRender: boolean;
    FAnimationTimer: TTimer;
    FNeedTimer: boolean;
  strict protected
    procedure UndoPreviousFrame;
    procedure RenderFrame;
    procedure Clear; // Clear context
    procedure Reset; override; // Reset animation state
    procedure DoNextFrame; override;
    procedure Loop; override;
    procedure Changed; override;
    function GetBitmap: TBitmap; override;
    procedure Initialize;  override;
    procedure StopAnimationTimer;
    procedure StartAnimationTimer(Delay: integer);
    procedure AnimationTimerEvent(Sender: TObject);
    property PreviousBuffer: TBitmap read FPreviousBuffer;
    property Buffer: TBitmap read FBuffer;
  public
    constructor Create(AImage: TGIFImage); override;
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override; // Draw current state
    procedure StartAnimation; override;
    procedure StopAnimation; override;
    procedure HaltAnimation; override;

    property Background: TBitmap read FBackground;
    property FrameDelay: integer read FFrameDelay;
    property Disposal: TDisposalMethod read FDisposal;
    property PrevDisposal: TDisposalMethod read FPrevDisposal;
    property PrevFrame: TGIFFrame read FPrevFrame;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			Color Mapping
//
////////////////////////////////////////////////////////////////////////////////
type
  TColorLookup = class(TObject)
  strict private
    FColors: integer;
  strict protected
    procedure SetColors(Value: integer);
  public
    constructor Create(Palette: hPalette); virtual;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; virtual; abstract;
    property Colors: integer read FColors;
  end;

  TColorLookupClass = class of TColorLookup;

  PRGBQuadArray = ^TRGBQuadArray;		// From graphics.pas
  TRGBQuadArray = array[Byte] of TRGBQuad;	// From graphics.pas

  BGRArray = array[0..0] of TRGBTriple;
  PBGRArray = ^BGRArray;

  PalArray =  array[byte] of TPaletteEntry;
  PPalArray = ^PalArray;

  // TFastColorLookup implements a simple but reasonably fast generic color
  // mapper. It trades precision for speed by reducing the size of the color
  // space.
  // Using a class instead of inline code results in a speed penalty of
  // approx. 15% but reduces the complexity of the color reduction routines that
  // uses it. If bitmap to GIF conversion speed is really important to you, the
  // implementation can easily be inlined again.
  TInverseLookup = array[0..1 SHL 16-1] of SmallInt;
  PInverseLookup = ^TInverseLookup;

  TFastColorLookup = class(TColorLookup)
  strict private
    FPaletteEntries: PPalArray;
    FInverseLookup: PInverseLookup;
  public
    constructor Create(Palette: hPalette); override;
    destructor Destroy; override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
  end;

  // TSlowColorLookup implements a precise but very slow generic color mapper.
  // It uses the GetNearestPaletteIndex GDI function.
  // Note: Tests has shown TFastColorLookup to be more precise than
  // TSlowColorLookup in many cases. I can't explain why...
  TSlowColorLookup = class(TColorLookup)
  strict private
    FPaletteEntries: PPalArray;
    FPalette: hPalette;
  public
    constructor Create(Palette: hPalette); override;
    destructor Destroy; override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
  end;

var
  // TGenericColorMapper
  // Change this value to use an alternate color mapper. E.g. the fast color
  // mapper instead of the slow, but more precise, color mapper.
  // The color mappers are used by the dithering routines. E.g. when high color
  // images are converted to GIF format.
  // If your images contain many skin tones or the colors are very unevenly
  // distributed, you should the slow, but precise, color mapper. If the colors
  // are evenly distributed across the color spectrum, the fast color mapper
  // might actually work better than the slow one.
  TGenericColorMapper: TColorLookupClass = TSlowColorLookup;

type
  // TNetscapeColorLookup maps colors to the netscape 6*6*6 color cube.
  TNetscapeColorLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
  end;

  // TGrayWindowsLookup maps colors to a 4 shade palette.
  TGrayWindowsLookup = class(TSlowColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
  end;

  // TGrayScaleLookup maps colors to a uniform 256 shade palette.
  TGrayScaleLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
  end;

  // TMonochromeLookup maps colors to a black/white palette.
  TMonochromeLookup = class(TColorLookup)
  public
    constructor Create(Palette: hPalette); override;
    function Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//                      Utility routines
//
////////////////////////////////////////////////////////////////////////////////
  // WebPalette creates a 216 color uniform palette a.k.a. the Netscape Palette
  function WebPalette: HPalette;

  // ReduceColors
  // Map colors in a bitmap to their nearest representation in a palette using
  // the methods specified by the ColorReduction and DitherMode parameters.
  // The ReductionBits parameter specifies the desired number of colors (bits
  // per pixel) when the reduction method is rmQuantize. The CustomPalette
  // specifies the palette when the rmPalette reduction method is used.
  function ReduceColors(Bitmap: TBitmap; ColorReduction: TColorReduction;
    DitherMode: TDitherMode; ReductionBits: integer; CustomPalette: hPalette): TBitmap;

  // CreateOptimizedPaletteFromManyBitmaps
  //: Performs Color Quantization on multiple bitmaps.
  // The Bitmaps parameter is a list of bitmaps. Returns an optimized palette.
  function CreateOptimizedPaletteFromManyBitmaps(Bitmaps: TList; Colors, ColorBits: integer;
    Windows: boolean): hPalette;

  procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer;
    var ImageSize: longInt; PixelFormat: TPixelFormat);
  function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
   var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;

////////////////////////////////////////////////////////////////////////////////
//
//                      Global variables
//
////////////////////////////////////////////////////////////////////////////////
// GIF Clipboard format identifier for use by LoadFromClipboardFormat and
// SaveToClipboardFormat.
// Set in Initialization section.
var
  CF_GIF: WORD = 0;


////////////////////////////////////////////////////////////////////////////////
//
//                      Library defaults
//
////////////////////////////////////////////////////////////////////////////////
var
  //: Default options for TGIFImage.Draw
  GIFImageDefaultTransparent: boolean = True;
  GIFImageDefaultAnimate: boolean = False;
  GIFImageDefaultAnimationLoop: TGIFAnimationLoop = glEnabled;
  GIFImageDefaultDithering: TGIFDithering = gdDisabled;

  //: Default color reduction methods for bitmap import.
  // These are the slowest settings, but also the ones that gives the
  // best result (in most cases).
  GIFImageDefaultColorReduction: TColorReduction = rmQuantize;
  GIFImageDefaultColorReductionBits: integer = 8; // Range 3 - 8
  GIFImageDefaultDitherMode: TDitherMode = dmFloydSteinberg;

  //: Default animation speed in % of normal speed (range 0 - 1000)
  GIFImageDefaultAnimationSpeed: integer = 100;

  // DoAutoDither is set to True in the initializaion section if the desktop DC
  // supports 256 colors or less.
  // It can be modified in your application to disable/enable Auto Dithering
  DoAutoDither: boolean = False;

  // Palette is set to True in the initialization section if the desktop DC
  // supports 256 colors or less.
  // You should NOT modify it.
  PaletteDevice: boolean = False;

  // Set GIFImageRenderOnLoad to True to render (convert to bitmap) the
  // GIF frames as they are loaded instead of rendering them on-demand.
  // This might increase resource consumption and will increase load time,
  // but will cause animated GIFs to display more smoothly.
  GIFImageRenderOnLoad: boolean = False;

  // If GIFImageOptimizeOnStream is true, the GIF will be optimized
  // before it is streamed to the DFM file.
  // This will not affect TGIFImage.SaveToStream or SaveToFile.
  GIFImageOptimizeOnStream: boolean = False;

  // BitmapAllocationThreshold:
  // Bitmap pixel count limit at which a newly allocated bitmap will be
  // converted to 1 bit format before being resized and converted to 8 bit.
  BitmapAllocationThreshold: integer = 500000;

  // GIFDelayExp:
  // The following delay values should all be multiplied by this value to
  // calculate the effective time (in mS).
  // According to the GIF specs, this value should be 10.
  // Since our paint routines are much faster than Mozilla's, you might need
  // to increase this value if your animations loops too fast. The
  // optimal value is impossible to determine since it depends on the
  // speed of the CPU, the video card, memory bandwidth and many other factors.
  GIFDelayExp: integer = 12; // Delay multiplier in mS.

  // GIFDefaultDelay:
  // Default animation delay.
  // This value is used if no GCE is defined.
  // (10 = 100 mS)
  GIFDefaultDelay: integer = 10;

  // GIFMinimumDelay:
  // Minimum delay.
  // The minumum delay used in the Mozilla source is 10mS (MINIMUM_DELAY_TIME).
  // This corresponds to a value of 1. However, since our paint routines are
  // much faster than Mozilla's, a value of 3 or 4 gives better results.
  // (1 = 10 mS)
  GIFMinimumDelay: integer = 3;

  // GIFMaximumDelay:
  // Maximum delay when painter is running in main thread (goAsync is not set).
  // This value guarantees that a very long and slow GIF does not hang the
  // system.
  // (1000 = 10000 mS = 10 Seconds)
  GIFMaximumDelay: integer = 1000;

  // GIFFileBufferSize:
  // Size of read ahead buffer for GIF decompression.
  GIFFileBufferSize: integer = 4096;

  // GIFDefaultTransparentColor:
  // The color used for transparency e.g. when importing transparent bitmaps
  // and optimizing the color maps.
  // There are no restrictions on the color specified, but the value must
  // be a real color (such as clBlack or clWhite), not a logic color (such as
  // clWindow or clBtnFace).
  GIFDefaultTransparentColor: TColor = clBlack;

  // GIFClearOnLoop:
  // GIFClearOnLoop specifies if the canvas should be cleared when an animation
  // loops.
  // The original Mozilla GIF implementation (i.e. NetScape 2-5) did not clear
  // the canvas when an animation looped.
  // Newer implementations of both Mozilla (i.e. Firefox) and Internet Explorer
  // does clear the canvas when an animation loops.
  // and optimizing the color maps.
  GIFClearOnLoop: boolean = True;


////////////////////////////////////////////////////////////////////////////////
//
//                      Design Time support
//
////////////////////////////////////////////////////////////////////////////////
// Dummy component registration for design time support of GIFs in TImage
procedure Register;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//			Implementation
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
  System.Types,
  Vcl.Clipbrd,
  Winapi.MMSystem, // timeGetTime()
  Vcl.Consts,
  Vcl.Imaging.GIFConsts;


////////////////////////////////////////////////////////////////////////////////
//
//			Misc consts
//
////////////////////////////////////////////////////////////////////////////////
const
  { Extension/block label values }
  bsPlainTextExtension		= $01;
  bsGraphicControlExtension	= $F9;
  bsCommentExtension		= $FE;
  bsApplicationExtension	= $FF;

  bsImageDescriptor		= ord(','); // $2C = 44
  bsExtensionIntroducer		= ord('!'); // $21 = 33
  bsTrailer			= ord(';'); // $3B = 59


////////////////////////////////////////////////////////////////////////////////
//
//                      Design Time support
//
////////////////////////////////////////////////////////////////////////////////
//: Dummy component registration to add design-time support of GIFs to TImage.
// Since TGIFImage isn't a component there's nothing to register here, but
// since Register is only called at design time we can set the design time
// GIF paint options here (modify as you please):
procedure Register;
begin
  // Don't loop animations at design-time. Animated GIFs will animate once and
  // then stop thus not using CPU resources and distracting the developer.
  GIFImageDefaultAnimationLoop := glDisabled;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Utilities
//
////////////////////////////////////////////////////////////////////////////////
//: Creates a 216 color uniform non-dithering Netscape palette.
function WebPalette: HPalette;
type
  TLogWebPalette= record
    palVersion: word;
    palNumEntries: word;
    PalEntries: array[0..5, 0..5, 0..5] of TPaletteEntry;
  end;
var
  r, g, b: byte;
  LogWebPalette: TLogWebPalette;
begin
  LogWebPalette.palVersion:= $0300;
  LogWebPalette.palNumEntries:= 6*6*6; // 216;
  for r := 0 to 5 do
    for g := 0 to 5 do
      for b := 0 to 5 do
        with LogWebPalette.PalEntries[r, g, b] do
        begin
          peRed := 51 * r;
          peGreen := 51 * g;
          peBlue := 51 * b;
          peFlags := 0;
        end;
  Result := CreatePalette(PLogpalette(@LogWebPalette)^);
end;

(*
**  GDI Error handling
**  Adapted from graphics.pas
*)
{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}

procedure GDIError;
const
  BufSize = 256;
var
  ErrorCode: Integer;
  Buf: array [Byte] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, BufSize, nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
    raise EOutOfResources.Create(SOutOfResources);
end;

function GDICheck(Value: THandle): THandle;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

(*
**  Raise error condition
*)
procedure Error(const msg: string);
begin
  raise GIFException.Create(msg);
end;

(*
**  Return number bytes required to
**  hold a given number of bits.
*)
function ByteAlignBit(Bits: Cardinal): Cardinal; inline;
begin
  Result := (Bits+7) SHR 3;
end;
// Rounded up to nearest 2
function WordAlignBit(Bits: Cardinal): Cardinal; inline;
begin
  Result := ((Bits+15) SHR 4) SHL 1;
end;
// Rounded up to nearest 4
function DWordAlignBit(Bits: Cardinal): Cardinal; inline;
begin
  Result := ((Bits+31) SHR 5) SHL 2;
end;
// Round to arbitrary number of bits
function AlignBit(Bits, BitsPerPixel, Alignment: Cardinal): Cardinal; inline;
begin
  Dec(Alignment);
  Result := ((Bits * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result SHR 3;
end;

(*
**  Compute Bits per Pixel from Number of Colors
**  (Return the ceiling log of n)
*)
function Colors2bpp(Colors: integer): integer;
var
  MaxColor		: integer;
begin
  (*
  ** This might be faster computed by multiple if then else statements.

  As an alternative, Gary Williams has suggested the following assembler
  version:

  function ColorDepthToBPP(ColorDepth: Cardinal): Cardinal;
  asm
    CMP EAX, 0
    JE @@1

    DEC EAX
    BSR EAX, EAX
    INC EAX
  @@1:
  end;

  *)

  if (Colors = 0) then
    Result := 0
  else
  begin
    Result := 1;
    MaxColor := 2;
    while (Colors > MaxColor) do
    begin
      inc(Result);
      MaxColor := MaxColor SHL 1;
    end;
  end;
end;

(*
**  Write an ordinal byte value to a stream
*)
procedure WriteByte(Stream: TStream; b: BYTE);
begin
  Stream.Write(b, 1);
end;

(*
**  Read an ordinal byte value from a stream
*)
function ReadByte(Stream: TStream): BYTE;
begin
  Stream.Read(Result, 1);
end;

(*
**  Read data from stream and raise exception of EOF
*)
procedure ReadCheck(Stream: TStream; var Buffer; Size: LongInt);
var
  ReadSize: integer;
begin
  ReadSize := Stream.Read(Buffer, Size);
  if (ReadSize <> Size) then
    Error(sOutOfData);
end;

(*
**  Write a string list to a stream as multiple blocks
**  of max 255 characters in each.
*)
procedure WriteStrings(Stream: TStream; Strings: TStrings);
var
  b: BYTE;
  Size: integer;
  s: AnsiString;
  p: PAnsiChar;
begin
  s := AnsiString(Strings.Text);
  Size := length(s);
  p := PAnsiChar(s);
  while (Size > 0) do
  begin
    if (Size > 255) then
      b := 255
    else
      b := size;
    WriteByte(Stream, b);
    Stream.Write(p^, b);
    inc(p, b);
    dec(Size, b);
  end;
  // Terminating zero (length = 0)
  WriteByte(Stream, 0);
end;


(*
**  Read a string list from a stream as multiple blocks
**  of max 255 characters in each.
*)
                                                                    
procedure ReadStrings(Stream: TStream; Strings: TStrings);
var
  size: BYTE;
  buf: array[0..255] of AnsiChar;
  s: AnsiString;
begin
  Strings.Clear;
  if (Stream.Read(size, 1) <> 1) then
    exit;
  while (size > 0) do
  begin
    ReadCheck(Stream, buf, size);
    buf[size] := #0;
    s := s + buf;
    if (Stream.Read(size, 1) <> 1) then
      break;
  end;
  Strings.Text := string(s);
end;


// --------------------------
// InitializeBitmapInfoHeader
// --------------------------
// Fills a TBitmapInfoHeader with the values of a bitmap when converted to a
// DIB of a specified PixelFormat.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// Info		The TBitmapInfoHeader buffer that will receive the values.
// PixelFormat	The pixel format of the destination DIB.
//
procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var Info: TBitmapInfoHeader;
  PixelFormat: TPixelFormat);
// From graphics.pas, "optimized" for our use
var
  DIB: TDIBSection;
  Bytes: Integer;
begin
  DIB.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DIB), @DIB);
  if (Bytes = 0) then
    Error(sInvalidBitmap);

  if (Bytes >= (SizeOf(DIB.dsbm) + SizeOf(DIB.dsbmih))) and
    (DIB.dsbmih.biSize >= SizeOf(DIB.dsbmih)) then
    Info := DIB.dsbmih
  else
  begin
    FillChar(Info, SizeOf(Info), 0);
    with Info, DIB.dsbm do
    begin
      biSize := SizeOf(Info);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case PixelFormat of
    pf1bit: Info.biBitCount := 1;
    pf4bit: Info.biBitCount := 4;
    pf8bit: Info.biBitCount := 8;
    pf24bit: Info.biBitCount := 24;
  else
    Error(sInvalidPixelFormat);
    // Info.biBitCount := DIB.dsbm.bmBitsPixel * DIB.dsbm.bmPlanes;
  end;
  Info.biPlanes := 1;
  Info.biCompression := BI_RGB; // Always return data in RGB format
  Info.biSizeImage := AlignBit(Info.biWidth, Info.biBitCount, 32) * Cardinal(abs(Info.biHeight));
end;

// -------------------
// InternalGetDIBSizes
// -------------------
// Calculates the buffer sizes nescessary for convertion of a bitmap to a DIB
// of a specified PixelFormat.
// See the GetDIBSizes API function for more info.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// InfoHeaderSize
//		The returned size of a buffer that will receive the DIB's
//		TBitmapInfo structure.
// ImageSize	The returned size of a buffer that will receive the DIB's
//		pixel data.
// PixelFormat	The pixel format of the destination DIB.
//
procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: Integer;
  var ImageSize: longInt; PixelFormat: TPixelFormat);
// From graphics.pas, "optimized" for our use
var
  Info: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, Info, PixelFormat);
  // Check for palette device format
  if (Info.biBitCount > 8) then
  begin
    // Header but no palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if ((Info.biCompression and BI_BITFIELDS) <> 0) then
      Inc(InfoHeaderSize, 12);
  end else
    // Header and palette
    InfoHeaderSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl Info.biBitCount);
  ImageSize := Info.biSizeImage;
end;

// --------------
// InternalGetDIB
// --------------
// Converts a bitmap to a DIB of a specified PixelFormat.
//
// Parameters:
// Bitmap	The handle of the source bitmap.
// Pal		The handle of the source palette.
// BitmapInfo	The buffer that will receive the DIB's TBitmapInfo structure.
//		A buffer of sufficient size must have been allocated prior to
//		calling this function.
// Bits		The buffer that will receive the DIB's pixel data.
//		A buffer of sufficient size must have been allocated prior to
//		calling this function.
// PixelFormat	The pixel format of the destination DIB.
//
// Returns:
// True on success, False on failure.
//
// Note: The InternalGetDIBSizes function can be used to calculate the
// nescessary sizes of the BitmapInfo and Bits buffers.
//
function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; PixelFormat: TPixelFormat): Boolean;
// From graphics.pas, "optimized" for our use
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), PixelFormat);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if (Palette <> 0) then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := (GetDIBits(DC, Bitmap, 0, abs(TBitmapInfoHeader(BitmapInfo).biHeight),
      @Bits, TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0);
  finally
    if (OldPal <> 0) then
      SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

// ----------
// DIBFromBit
// ----------
// Converts a bitmap to a DIB of a specified PixelFormat.
// The DIB is returned in a TMemoryStream ready for streaming to a BMP file.
//
// Note: As opposed to D2's DIBFromBit function, the returned stream also
// contains a TBitmapFileHeader at offset 0.
//
// Parameters:
// Stream	The TMemoryStream used to store the bitmap data.
//		The stream must be allocated and freed by the caller prior to
//		calling this function.
// Src		The handle of the source bitmap.
// Pal		The handle of the source palette.
// PixelFormat	The pixel format of the destination DIB.
// DIBHeader	A pointer to the DIB's TBitmapInfo (or TBitmapInfoHeader)
//		structure in the memory stream.
//		The size of the structure can either be deduced from the
//		pixel format (i.e. number of colors) or calculated by
//		subtracting the DIBHeader pointer from the DIBBits pointer.
// DIBBits	A pointer to the DIB's pixel data in the memory stream.
//
procedure DIBFromBit(Stream: TMemoryStream; Src: HBITMAP;
  Pal: HPALETTE; PixelFormat: TPixelFormat; var DIBHeader, DIBBits: Pointer); deprecated;
// (From D2 graphics.pas, "optimized" for our use)
var
  HeaderSize: integer;
  FileSize: longInt;
  ImageSize: longInt;
  BitmapFileHeader: PBitmapFileHeader;
begin
  if (Src = 0) then
    Error(sInvalidBitmap);
  // Get header- and pixel data size for new pixel format
  InternalGetDIBSizes(Src, HeaderSize, ImageSize, PixelFormat);
  // Make room in stream for a TBitmapInfo and pixel data
  FileSize := SizeOf(TBitmapFileHeader) + HeaderSize + ImageSize;
  Stream.SetSize(FileSize);
  // Get pointer to TBitmapFileHeader
  BitmapFileHeader := Stream.Memory;
  // Get pointer to TBitmapInfo
  DIBHeader := PByte(BitmapFileHeader) + SizeOf(TBitmapFileHeader);
  // Get pointer to pixel data
  DIBBits := PByte(DIBHeader) + HeaderSize;
  // Initialize file header
  FillChar(BitmapFileHeader^, SizeOf(TBitmapFileHeader), 0);
  with BitmapFileHeader^ do
  begin
    bfType := $4D42; // 'BM' = Windows BMP signature
    bfSize := FileSize; // File size (not needed)
    bfOffBits := SizeOf(TBitmapFileHeader) + HeaderSize; // Offset of pixel data
  end;
  // Get pixel data in new pixel format
  InternalGetDIB(Src, Pal, DIBHeader^, DIBBits^, PixelFormat);
end;

function GetPixelFormat(Bitmap: TBitmap): TPixelFormat; deprecated;
begin
  Result := Bitmap.PixelFormat;
end;

procedure SetPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat); // deprecated;
begin
  Bitmap.PixelFormat := PixelFormat;
end;

// ------------------
// SafeSetPixelFormat
// ------------------
// Changes the pixel format of a TBitmap but doesn't preserve the contents.
//
// Replacement for Delphi 3 TBitmap.PixelFormat setter.
// The returned TBitmap will always be an empty DIB of the same size as the
// original bitmap.
//
// Parameters:
// Bitmap	The bitmap to modify.
// PixelFormat	The pixel format to convert to.
//
procedure SafeSetPixelFormat(Bitmap: TBitmap; PixelFormat: TPixelFormat);// deprecated;
var
  Palette: hPalette;
begin
  Bitmap.PixelFormat := PixelFormat;

  // Work around a bug in TBitmap:
  // When converting to pf8bit format, the palette assigned to TBitmap.Palette
  // will be a half tone palette (which only contains the 20 system colors).
  // Unfortunately this is not the palette used to render the bitmap and it
  // is also not the palette saved with the bitmap.
  if (PixelFormat = pf8bit) then
  begin
    // Disassociate the wrong palette from the bitmap (without affecting
    // the DIB color table)
    Palette := Bitmap.ReleasePalette;
    if (Palette <> 0) then
      DeleteObject(Palette);
    // Recreate the palette from the DIB color table
    Bitmap.Palette;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//			TGIFItem
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFItem.Create(AImage: TGIFImage);
begin
  inherited Create;

  FImage := AImage;
end;

procedure TGIFItem.Warning(Severity: TGIFSeverity; const Msg: string);
begin
  Image.Warning(Self, Severity, Msg);
end;

function TGIFItem.GetVersion: TGIFVersion;
begin
  Result := gv87a;
end;

procedure TGIFItem.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead OR fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGIFItem.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFList
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFList.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TGIFList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TGIFList.GetItem(Index: Integer): TGIFItem;
begin
  Result := TGIFItem(FItems[Index]);
end;

procedure TGIFList.SetItem(Index: Integer; Item: TGIFItem);
begin
  FItems[Index] := Item;
end;

function TGIFList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TGIFList.Add(Item: TGIFItem): Integer;
begin
  Result := IndexOf(Item);
  if (Result = -1) then
    Result := FItems.Add(Item);
end;

procedure TGIFList.Clear;
begin
  while (FItems.Count > 0) do
    Delete(0);
end;

procedure TGIFList.Delete(Index: Integer);
var
  Item: TGIFItem;
begin
  Item := TGIFItem(FItems[Index]);
  // Delete before item is destroyed to avoid recursion
  FItems.Delete(Index);
  Item.Free;
end;

procedure TGIFList.Exchange(Index1, Index2: Integer);
begin
  FItems.Exchange(Index1, Index2);
end;

function TGIFList.First: TGIFItem;
begin
  Result := TGIFItem(FItems.First);
end;

function TGIFList.IndexOf(Item: TGIFItem): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

procedure TGIFList.Insert(Index: Integer; Item: TGIFItem);
begin
  FItems.Insert(Index, Item);
end;

function TGIFList.Last: TGIFItem;
begin
  Result := TGIFItem(FItems.Last);
end;

procedure TGIFList.Move(CurIndex, NewIndex: Integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

function TGIFList.Remove(Item: TGIFItem): Integer;
begin
  // Note: TGIFList.Remove must not destroy item
  Result := FItems.Remove(Item);
end;

procedure TGIFList.SaveToStream(Stream: TStream);
var
  i: integer;
begin
  for i := 0 to FItems.Count-1 do
    TGIFItem(FItems[i]).SaveToStream(Stream);
end;

procedure TGIFList.Warning(Severity: TGIFSeverity; Message: string);
begin
  Image.Warning(Self, Severity, Message);
end;


////////////////////////////////////////////////////////////////////////////////
//
//			TDIB Classes
//
//  These classes gives read and write access to TBitmap's pixel data
//  independently of the Delphi version used.
//
////////////////////////////////////////////////////////////////////////////////
type
  TDIB = class(TObject)
  strict private
    FBitmap: TBitmap;
    FPixelFormat: TPixelFormat;
  strict protected
    function GetScanline(Row: integer): pointer; virtual; abstract;
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
  public
    property Scanline[Row: integer]: pointer read GetScanline;
    property Bitmap: TBitmap read FBitmap;
    property PixelFormat: TPixelFormat read FPixelFormat;
  end;

  TDIBReader = class(TDIB)
  strict private
{$ifdef PIXELFORMAT_BROKEN}
    FDC: HDC;
    FScanLine: pointer;
    FLastRow: integer;
    FInfo: PBitmapInfo;
{$endif}
  strict protected
    function GetScanline(Row: integer): pointer; override;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
    destructor Destroy; override;
  end;

  TDIBWriter = class(TDIB)
  strict private
{$ifdef PIXELFORMAT_SLOW}
    FDIBInfo: PBitmapInfo;
    FDIBBits: pointer;
    FDIBInfoSize: integer;
    FDIBBitsSize: longInt;
{$ifndef CREATEDIBSECTION_SLOW}
    FDIB: HBITMAP;
{$endif}
{$endif}
    FPalette: HPalette;
    FHeight: integer;
    FWidth: integer;
  strict protected
    procedure CreateDIB;
    procedure FreeDIB;
    procedure NeedDIB;
    function HasDIB: boolean;
    function GetScanline(Row: integer): pointer; override;
  public
    constructor Create(ABitmap: TBitmap; APixelFormat: TPixelFormat;
      AWidth, AHeight: integer; APalette: HPalette);
    destructor Destroy; override;
    procedure UpdateBitmap;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Palette: HPalette read FPalette;
  end;

////////////////////////////////////////////////////////////////////////////////
constructor TDIB.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
begin
  inherited Create;
  FBitmap := ABitmap;
  FPixelFormat := APixelFormat;
end;

////////////////////////////////////////////////////////////////////////////////
constructor TDIBReader.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat);
{$ifdef PIXELFORMAT_BROKEN}
var
  InfoHeaderSize: integer;
  ImageSize: longInt;
{$endif}
begin
  inherited Create(ABitmap, APixelFormat);
{$ifdef PIXELFORMAT_BROKEN}
  FDC := CreateCompatibleDC(0);
  SelectPalette(FDC, FBitmap.Palette, False);

  // Allocate DIB info structure
  InternalGetDIBSizes(ABitmap.Handle, InfoHeaderSize, ImageSize, APixelFormat);
  GetMem(FInfo, InfoHeaderSize);
  // Get DIB info
  InitializeBitmapInfoHeader(ABitmap.Handle, FInfo^.bmiHeader, APixelFormat);

  // Allocate scan line buffer
  GetMem(FScanLine, ImageSize DIV abs(FInfo^.bmiHeader.biHeight));

  FLastRow := -1;
{$else}
  SetPixelFormat(Bitmap, PixelFormat);
{$endif}
end;

destructor TDIBReader.Destroy;
begin
{$ifdef PIXELFORMAT_BROKEN}
  DeleteDC(FDC);
  FreeMem(FScanLine);
  FreeMem(FInfo);
{$endif}
  inherited Destroy;
end;

function TDIBReader.GetScanline(Row: integer): pointer;
begin
{$ifdef PIXELFORMAT_BROKEN}
  if (Row < 0) or (Row >= FBitmap.Height) then
    raise EInvalidGraphicOperation.Create(SScanLine);
  GDIFlush;

  Result := FScanLine;
  if (Row = FLastRow) then
    exit;
  FLastRow := Row;

  if (FInfo^.bmiHeader.biHeight > 0) then  // bottom-up DIB
    Row := FInfo^.bmiHeader.biHeight - Row - 1;
  GetDIBits(FDC, FBitmap.Handle, Row, 1, FScanLine, FInfo^, DIB_RGB_COLORS);

{$else}
  Result := Bitmap.ScanLine[Row];
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
constructor TDIBWriter.Create(ABitmap: TBitmap; APixelFormat: TPixelFormat;
  AWidth, AHeight: integer; APalette: HPalette);
begin
  inherited Create(ABitmap, APixelFormat);

  // DIB writer only supports 8 or 24 bit bitmaps
  if not(APixelFormat in [pf8bit, pf24bit]) then
    Error(sInvalidPixelFormat);
  if (AWidth = 0) or (AHeight = 0) then
    Error(sBadDimension);

  FHeight := AHeight;
  FWidth := AWidth;
{$ifdef PIXELFORMAT_SLOW}
  FPalette := APalette;
  FDIBInfo := nil;
  FDIBBits := nil;
{$ifndef CREATEDIBSECTION_SLOW}
  FDIB := 0;
{$endif}
{$else}
  FBitmap.Palette := 0;
  FBitmap.Height := FHeight;
  FBitmap.Width := FWidth;
  SafeSetPixelFormat(FBitmap, FPixelFormat);
  FPalette := CopyPalette(APalette);
  FBitmap.Palette := FPalette;
{$endif}
end;

destructor TDIBWriter.Destroy;
begin
  UpdateBitmap;
  FreeDIB;
  inherited Destroy;
end;

function TDIBWriter.GetScanline(Row: Integer): Pointer;
begin
{$ifdef PIXELFORMAT_SLOW}
  NeedDIB;

  if (FDIBBits = nil) then
    Error(sNoDIB);
  with FDIBInfo^.bmiHeader do
  begin
    if (Row < 0) or (Row >= Height) then
      raise EInvalidGraphicOperation.Create(SScanLine);
    GDIFlush;

    if biHeight > 0 then  // bottom-up DIB
      Row := biHeight - Row - 1;
    Result := PByte(NativeUInt(FDIBBits) + Cardinal(Row) * AlignBit(biWidth, biBitCount, 32));
  end;
{$else}
  Result := FBitmap.ScanLine[Row];
{$endif}
end;

procedure TDIBWriter.CreateDIB;
{$ifdef PIXELFORMAT_SLOW}
var
  SrcColors: WORD;

  procedure ByteSwapColors(var Colors; Count: Integer);
  {$IFDEF PUREPASCAL}
  var   // convert RGB to BGR and vice-versa.  TRGBQuad <-> TPaletteEntry
    C: PDWORD;
    Color: DWORD;
    I: Integer;
  begin
    C := @Colors;
    I := 0;
    while I < Count do
    begin
      Color := C^;
      C^ := (Byte(Color shr 16) or (Word(Color) shr 8) shl 8) or
        (Byte(Word(Color)) shl 16);
      Inc(I);
      Inc(C);
    end;
  end;
  {$ELSE !PUREPASCAL}
  {$IFDEF CPUX86}
  var   // convert RGB to BGR and vice-versa.  TRGBQuad <-> TPaletteEntry
    SysInfo: TSystemInfo;
  begin
    GetSystemInfo(SysInfo);
    asm
          MOV   EDX, Colors
          MOV   ECX, Count
          DEC   ECX
          JS    @@END
          LEA   EAX, SysInfo
          CMP   [EAX].TSystemInfo.wProcessorLevel, 3
          JE    @@386
    @@1:  MOV   EAX, [EDX+ECX*4]
          BSWAP EAX
          SHR   EAX,8
          MOV   [EDX+ECX*4],EAX
          DEC   ECX
          JNS   @@1
          JMP   @@END
    @@386:
          PUSH  EBX
    @@2:  XOR   EBX,EBX
          MOV   EAX, [EDX+ECX*4]
          MOV   BH, AL
          MOV   BL, AH
          SHR   EAX,16
          SHL   EBX,8
          MOV   BL, AL
          MOV   [EDX+ECX*4],EBX
          DEC   ECX
          JNS   @@2
          POP   EBX
      @@END:
    end;
  end;
  {$ENDIF CPUX86}
  {$ENDIF !PUREPASCAL}
{$endif PIXELFORMAT_SLOW}
begin
{$ifdef PIXELFORMAT_SLOW}
  FreeDIB;

  if (PixelFormat = pf8bit) then
    // 8 bit: Header and palette
    FDIBInfoSize := SizeOf(TBitmapInfoHeader) + SizeOf(TRGBQuad) * (1 shl 8)
  else
    // 24 bit: Header but no palette
    FDIBInfoSize := SizeOf(TBitmapInfoHeader);

  // Allocate TBitmapInfo structure
  GetMem(FDIBInfo, FDIBInfoSize);
  try
    FDIBInfo^.bmiHeader.biSize := SizeOf(FDIBInfo^.bmiHeader);
    FDIBInfo^.bmiHeader.biWidth := Width;
    FDIBInfo^.bmiHeader.biHeight := Height;
    FDIBInfo^.bmiHeader.biPlanes := 1;
    FDIBInfo^.bmiHeader.biSizeImage := 0;
    FDIBInfo^.bmiHeader.biCompression := BI_RGB;

    if (PixelFormat = pf8bit) then
    begin
      FDIBInfo^.bmiHeader.biBitCount := 8;
      // Find number of colors defined by palette
      if (Palette <> 0) and
        (GetObject(Palette, SizeOf(SrcColors), @SrcColors) <> 0) and
        (SrcColors <> 0) then
      begin
        // Copy all colors...
        GetPaletteEntries(Palette, 0, SrcColors, FDIBInfo^.bmiColors[0]);
        // ...and convert BGR to RGB
        ByteSwapColors(FDIBInfo^.bmiColors[0], SrcColors);
      end else
        SrcColors := 0;

      // Finally zero any unused entried
      if (SrcColors < 256) then
        FillChar(pointer(PByte(@FDIBInfo^.bmiColors)+SizeOf(TRGBQuad)*SrcColors)^,
          256 - SrcColors, 0);
      FDIBInfo^.bmiHeader.biClrUsed := 256;
      FDIBInfo^.bmiHeader.biClrImportant := SrcColors;
    end else
    begin
      FDIBInfo^.bmiHeader.biBitCount := 24;
      FDIBInfo^.bmiHeader.biClrUsed := 0;
      FDIBInfo^.bmiHeader.biClrImportant := 0;
    end;
    FDIBBitsSize := AlignBit(Width, FDIBInfo^.bmiHeader.biBitCount, 32) * Cardinal(abs(Height));

{$ifdef CREATEDIBSECTION_SLOW}
    FDIBBits := GlobalAllocPtr(GMEM_MOVEABLE, FDIBBitsSize);
    if (FDIBBits = nil) then
      raise EOutOfMemory.Create(sOutOfMemDIB);
{$else}
    // Allocate DIB section
    // Note: You can ignore warnings about the HDC parameter being 0. The
    // parameter is not used for 24 bit bitmaps
    FDIB := GDICheck(CreateDIBSection(0, FDIBInfo^, DIB_RGB_COLORS,
      FDIBBits, 0, 0));
{$endif}

  except
    FreeDIB;
    raise;
  end;
{$endif}
end;

procedure TDIBWriter.FreeDIB;
begin
{$ifdef PIXELFORMAT_SLOW}
  if (FDIBInfo <> nil) then
    FreeMem(FDIBInfo);
{$ifdef CREATEDIBSECTION_SLOW}
  if (FDIBBits <> nil) then
    GlobalFreePtr(FDIBBits);
{$else}
  if (FDIB <> 0) then
    DeleteObject(FDIB);
  FDIB := 0;
{$endif}
  FDIBInfo := nil;
  FDIBBits := nil;
{$endif}
end;

function TDIBWriter.HasDIB: boolean;
begin
{$ifdef PIXELFORMAT_SLOW}
{$ifdef CREATEDIBSECTION_SLOW}
  Result := (FDIBBits <> nil);
{$else}
  Result := (FDIB <> 0);
{$endif}
{$else}
  Result := False;
{$endif}
end;

procedure TDIBWriter.NeedDIB;
begin
{$ifdef PIXELFORMAT_SLOW}
  if (not HasDIB) then
    CreateDIB;
{$endif}
end;

// Convert the DIB created by CreateDIB back to a TBitmap
procedure TDIBWriter.UpdateBitmap;
{$ifdef PIXELFORMAT_SLOW}
var
  Stream: TMemoryStream;
  FileSize: longInt;
  BitmapFileHeader: TBitmapFileHeader;
{$endif}
begin
{$ifdef PIXELFORMAT_SLOW}

  if (not HasDIB) then
    exit;

  // Win9x and NT differs in what solution performs best
{$ifndef CREATEDIBSECTION_SLOW}
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    // Assign DIB to bitmap
    FBitmap.Handle := FDIB;
    FDIB := 0;
    FBitmap.Palette := CopyPalette(Palette);
  end else
{$endif}
  begin
    // Write DIB to a stream in the BMP file format
    Stream := TMemoryStream.Create;
    try
      // Make room in stream for a TBitmapInfo and pixel data
      FileSize := SizeOf(TBitmapFileHeader) + FDIBInfoSize + FDIBBitsSize;
      Stream.SetSize(FileSize);
      // Initialize file header
      FillChar(BitmapFileHeader, SizeOf(TBitmapFileHeader), 0);
      with BitmapFileHeader do
      begin
        bfType := $4D42; // 'BM' = Windows BMP signature
        bfSize := FileSize; // File size (not needed)
        bfOffBits := SizeOf(TBitmapFileHeader) + FDIBInfoSize; // Offset of pixel data
      end;
      // Save file header
      Stream.Write(BitmapFileHeader, SizeOf(TBitmapFileHeader));
      // Save TBitmapInfo structure
      Stream.Write(FDIBInfo^, FDIBInfoSize);
      // Save pixel data
      Stream.Write(FDIBBits^, FDIBBitsSize);

      // Rewind and load bitmap from stream
      Stream.Position := 0;
      Bitmap.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Color Mapping
//
////////////////////////////////////////////////////////////////////////////////
constructor TColorLookup.Create(Palette: hPalette);
begin
  inherited Create;
end;

procedure TColorLookup.SetColors(Value: integer);
begin
  FColors := Value;
end;
constructor TFastColorLookup.Create(Palette: hPalette);
var
  i: integer;
  InverseIndex: integer;
begin
  inherited Create(Palette);

  GetMem(FPaletteEntries, SizeOf(TPaletteEntry) * 256);
  SetColors(GetPaletteEntries(Palette, 0, 256, FPaletteEntries^));

  New(FInverseLookup);
  for i := low(TInverseLookup) to high(TInverseLookup) do
    FInverseLookup^[i] := -1;

  // Premap palette colors
  if (Colors > 0) then
    for i := 0 to Colors-1 do
      with FPaletteEntries^[i] do
      begin
        InverseIndex := (peRed SHR 3) OR
                        ((peGreen AND $FC) SHL 3) OR
                        ((peBlue AND $F8) SHL 8);
        if (FInverseLookup^[InverseIndex] = -1) then
          FInverseLookup^[InverseIndex] := i;
      end;
end;

destructor TFastColorLookup.Destroy;
begin
  if (FPaletteEntries <> nil) then
    FreeMem(FPaletteEntries);
  if (FInverseLookup <> nil) then
    Dispose(FInverseLookup);

  inherited Destroy;
end;

// Map color to arbitrary palette
function TFastColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;

  function IntSqr(x: integer): integer; inline;
  begin
    Result := x*x;
  end;

var
  i: integer;
  InverseIndex: integer;
  Delta, MinDelta, MinColor: integer;
begin
  // Reduce color space with 3 bits in each dimension
                                                                                              
  // be expanded from 5 to 6 bits. We favor green because the human eye is more
  // sensitive to green.
  InverseIndex := (Red SHR 3) OR ((Green AND $FC) SHL 3) OR ((Blue AND $F8) SHL 8);

  if (FInverseLookup^[InverseIndex] <> -1) then
    Result := Byte(FInverseLookup^[InverseIndex])
  else
  begin
    // Sequential scan for nearest color to minimize euclidian distance
    MinDelta := 3 * (256 * 256);
    MinColor := 0;
    for i := 0 to Colors-1 do
      with FPaletteEntries[i] do
      begin
        Delta := IntSqr(peRed - Red) + IntSqr(peGreen - Green) + IntSqr(peBlue - Blue);
        if (Delta < MinDelta) then
        begin
          MinDelta := Delta;
          MinColor := i;
        end;
      end;
    Result := Byte(MinColor);
    FInverseLookup^[InverseIndex] := MinColor;
  end;

  with FPaletteEntries^[ord(Result)] do
  begin
    R := peRed;
    G := peGreen;
    B := peBlue;
  end;
end;

constructor TSlowColorLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  FPalette := Palette;
  SetColors(GetPaletteEntries(Palette, 0, 256, nil^));
  if (Colors > 0) then
  begin
    GetMem(FPaletteEntries, SizeOf(TPaletteEntry) * Colors);
    SetColors(GetPaletteEntries(Palette, 0, 256, FPaletteEntries^));
  end;
end;

destructor TSlowColorLookup.Destroy;
begin
  if (FPaletteEntries <> nil) then
    FreeMem(FPaletteEntries);

  inherited Destroy;
end;

// Map color to arbitrary palette
function TSlowColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
begin
  Result := Byte(GetNearestPaletteIndex(FPalette, Red OR (Green SHL 8) OR (Blue SHL 16)));
  if (FPaletteEntries <> nil) then
    with FPaletteEntries^[ord(Result)] do
    begin
      R := peRed;
      G := peGreen;
      B := peBlue;
    end;
end;

constructor TNetscapeColorLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  SetColors(6*6*6);
end;

// Map color to netscape 6*6*6 color cube
function TNetscapeColorLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
begin
  R := (Red+3) DIV 51;
  G := (Green+3) DIV 51;
  B := (Blue+3) DIV 51;
  Result := Byte(B + 6*G + 36*R);
  R := R * 51;
  G := G * 51;
  B := B * 51;
end;

constructor TGrayWindowsLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  SetColors(4);
end;

// Convert color to windows grays
function TGrayWindowsLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
begin
  Result := inherited Lookup(MulDiv(Red, 77, 256),
    MulDiv(Green, 150, 256), MulDiv(Blue, 29, 256), R, G, B);
end;

constructor TGrayScaleLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  SetColors(256);
end;

// Convert color to grayscale
function TGrayScaleLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
begin
  Result := Byte((Blue*29 + Green*150 + Red*77) DIV 256);
  R := ord(Result);
  G := ord(Result);
  B := ord(Result);
end;

constructor TMonochromeLookup.Create(Palette: hPalette);
begin
  inherited Create(Palette);
  SetColors(2);
end;

// Convert color to black/white
function TMonochromeLookup.Lookup(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
begin
  if ((Blue*29 + Green*150 + Red*77) > 32512) then
  begin
    Result := 1;
    R := 255;
    G := 255;
    B := 255;
  end else
  begin
    Result := 0;
    R := 0;
    G := 0;
    B := 0;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Dithering engine
//
////////////////////////////////////////////////////////////////////////////////
type
  // Note: TErrorTerm does only *need* to be 16 bits wide, but since
  // it is *much* faster to use native machine words (32 bit), we sacrifice
  // some bytes (a lot actually) to improve performance.
  TErrorTerm = Integer;
  TErrors = array[0..0] of TErrorTerm;
  PErrors = ^TErrors;

  TErrorLimit = array[-255..255] of TErrorTerm;

  TDitherEngine = class
  strict private
  strict protected
    FDirection: integer;
    FColumn: integer;
    FLookup: TColorLookup;
    FWidth: integer;
    ErrorLimit: TErrorLimit;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); virtual;
    procedure Reset; virtual;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; virtual;
    procedure NextLine; virtual;
    procedure NextColumn;

    property Direction: integer read FDirection;
    property Column: integer read FColumn;
    property Width: integer read FWidth write FWidth;
  end;

  TFloydSteinbergDitherer = class(TDitherEngine)
  strict private
    ErrorsR, ErrorsG, ErrorsB: PErrors;
    ErrorR, ErrorG, ErrorB: PErrors;
    CurrentErrorR, CurrentErrorG, CurrentErrorB: TErrorTerm; // Current error or pixel value
    BelowErrorR, BelowErrorG, BelowErrorB: TErrorTerm; // Error for pixel below current
    BelowPrevErrorR, BelowPrevErrorG, BelowPrevErrorB: TErrorTerm;// Error for pixel below previous pixel
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    procedure Reset; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
    procedure NextLine; override;
  end;

  T5by3Ditherer = class(TDitherEngine)
  strict private
    ErrorsR0, ErrorsG0, ErrorsB0,
    ErrorsR1, ErrorsG1, ErrorsB1,
    ErrorsR2, ErrorsG2, ErrorsB2: PErrors;
    ErrorR0, ErrorG0, ErrorB0,
    ErrorR1, ErrorG1, ErrorB1,
    ErrorR2, ErrorG2, ErrorB2: PErrors;
    FDirection2: integer;
  strict protected
    FDivisor: integer;
    procedure Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer); virtual; abstract;
    property Direction2: integer read FDirection2;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    procedure Reset; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
    procedure NextLine; override;
  end;

  TStuckiDitherer = class(T5by3Ditherer)
  strict protected
    procedure Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer); override;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
  end;

  TSierraDitherer = class(T5by3Ditherer)
  strict protected
    procedure Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer); override;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
  end;

  TJaJuNiDitherer = class(T5by3Ditherer)
  strict protected
    procedure Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer); override;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
  end;

  TSteveArcheDitherer = class(TDitherEngine)
  strict private
    ErrorsR0, ErrorsG0, ErrorsB0,
    ErrorsR1, ErrorsG1, ErrorsB1,
    ErrorsR2, ErrorsG2, ErrorsB2,
    ErrorsR3, ErrorsG3, ErrorsB3: PErrors;
    ErrorR0, ErrorG0, ErrorB0,
    ErrorR1, ErrorG1, ErrorB1,
    ErrorR2, ErrorG2, ErrorB2,
    ErrorR3, ErrorG3, ErrorB3: PErrors;
    FDirection2, FDirection3: integer;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    procedure Reset; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
    procedure NextLine; override;
  end;

  TBurkesDitherer = class(TDitherEngine)
  strict private
    ErrorsR0, ErrorsG0, ErrorsB0,
    ErrorsR1, ErrorsG1, ErrorsB1: PErrors;
    ErrorR0, ErrorG0, ErrorB0,
    ErrorR1, ErrorG1, ErrorB1: PErrors;
    FDirection2: integer;
  public
    constructor Create(AWidth: integer; Lookup: TColorLookup); override;
    destructor Destroy; override;
    procedure Reset; override;
    function Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte; override;
    procedure NextLine; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//	TDitherEngine
constructor TDitherEngine.Create(AWidth: integer; Lookup: TColorLookup);
var
  i: integer;
begin
  inherited Create;

  FLookup := Lookup;
  Width := AWidth;

  for i := -255 to 255 do
    ErrorLimit[i] := i;
end;

function TDitherEngine.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
begin
  // Map color to palette
  Result := FLookup.Lookup(Red, Green, Blue, R, G, B);
  NextColumn;
end;

procedure TDitherEngine.NextLine;
begin
  FDirection := -FDirection;
  if (FDirection = 1) then
    FColumn := 0
  else
    FColumn := Width-1;
end;

procedure TDitherEngine.NextColumn;
begin
  inc(FColumn, FDirection);
end;

procedure TDitherEngine.Reset;
begin
  FDirection := 1;
  FColumn := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//	TFloydSteinbergDitherer
constructor TFloydSteinbergDitherer.Create(AWidth: integer; Lookup: TColorLookup);
var
  i: integer;
  Limit: byte;
begin
  inherited Create(AWidth, Lookup);

  i := 0;
  Limit := 0;
  while (i < 16) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(Limit);
    inc(i);
  end;
  while (i < 48) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(Limit, i AND 1);
    inc(i);
  end;
  while (i < 256) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(i);
  end;

  // The Error arrays has (columns + 2) entries; the extra entry at
  // each end saves us from special-casing the first and last pixels.
  // We can get away with a single array (holding one row's worth of errors)
  // by using it to store the current row's errors at pixel columns not yet
  // processed, but the next row's errors at columns already processed.  We
  // need only a few extra variables to hold the errors immediately around the
  // current column.  (If we are lucky, those variables are in registers, but
  // even if not, they're probably cheaper to access than array elements are.)
  GetMem(ErrorsR, SizeOf(TErrorTerm)*(Width+2));
  GetMem(ErrorsG, SizeOf(TErrorTerm)*(Width+2));
  GetMem(ErrorsB, SizeOf(TErrorTerm)*(Width+2));
end;

destructor TFloydSteinbergDitherer.Destroy;
begin
  FreeMem(ErrorsR);
  FreeMem(ErrorsG);
  FreeMem(ErrorsB);
  inherited Destroy;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TFloydSteinbergDitherer.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
var
  BelowNextError: TErrorTerm;
  Delta: TErrorTerm;
begin
  CurrentErrorR := (CurrentErrorR + ErrorR[0] + 8) DIV 16;
  CurrentErrorR := Red + ErrorLimit[CurrentErrorR];
//  CurrentErrorR := Red + (CurrentErrorR + ErrorR[Direction] + 8) DIV 16;
  if (CurrentErrorR < 0) then
    CurrentErrorR := 0
  else if (CurrentErrorR > 255) then
    CurrentErrorR := 255;

  CurrentErrorG := (CurrentErrorG + ErrorG[0] + 8) DIV 16;
  CurrentErrorG := Green + ErrorLimit[CurrentErrorG];
//  CurrentErrorG := Green + (CurrentErrorG + ErrorG[Direction] + 8) DIV 16;
  if (CurrentErrorG < 0) then
    CurrentErrorG := 0
  else if (CurrentErrorG > 255) then
    CurrentErrorG := 255;

  CurrentErrorB := (CurrentErrorB + ErrorB[0] + 8) DIV 16;
  CurrentErrorB := Blue + ErrorLimit[CurrentErrorB];
//  CurrentErrorB := Blue + (CurrentErrorB + ErrorB[Direction] + 8) DIV 16;
  if (CurrentErrorB < 0) then
    CurrentErrorB := 0
  else if (CurrentErrorB > 255) then
    CurrentErrorB := 255;

  // Map color to palette
  Result := inherited Dither(CurrentErrorR, CurrentErrorG, CurrentErrorB, R, G, B);

  // Propagate Floyd-Steinberg error terms.
  // Errors are accumulated into the error arrays, at a resolution of
  // 1/16th of a pixel count.  The error at a given pixel is propagated
  // to its not-yet-processed neighbors using the standard F-S fractions,
  //		...	(here)	7/16
  //		3/16	5/16	1/16
  // We work left-to-right on even rows, right-to-left on odd rows.

  // Red component
  CurrentErrorR := CurrentErrorR - R;
  if (CurrentErrorR <> 0) then
  begin
    BelowNextError := CurrentErrorR;			// Error * 1

    Delta := CurrentErrorR * 2;
    inc(CurrentErrorR, Delta);
    ErrorR[0] := BelowPrevErrorR + CurrentErrorR;	// Error * 3

    inc(CurrentErrorR, Delta);
    BelowPrevErrorR := BelowErrorR + CurrentErrorR;	// Error * 5

    BelowErrorR := BelowNextError;			// Error * 1

    inc(CurrentErrorR, Delta);				// Error * 7
  end;

  // Green component
  CurrentErrorG := CurrentErrorG - G;
  if (CurrentErrorG <> 0) then
  begin
    BelowNextError := CurrentErrorG;			// Error * 1

    Delta := CurrentErrorG * 2;
    inc(CurrentErrorG, Delta);
    ErrorG[0] := BelowPrevErrorG + CurrentErrorG;	// Error * 3

    inc(CurrentErrorG, Delta);
    BelowPrevErrorG := BelowErrorG + CurrentErrorG;	// Error * 5

    BelowErrorG := BelowNextError;			// Error * 1

    inc(CurrentErrorG, Delta);				// Error * 7
  end;

  // Blue component
  CurrentErrorB := CurrentErrorB - B;
  if (CurrentErrorB <> 0) then
  begin
    BelowNextError := CurrentErrorB;			// Error * 1

    Delta := CurrentErrorB * 2;
    inc(CurrentErrorB, Delta);
    ErrorB[0] := BelowPrevErrorB + CurrentErrorB;	// Error * 3

    inc(CurrentErrorB, Delta);
    BelowPrevErrorB := BelowErrorB + CurrentErrorB;	// Error * 5

    BelowErrorB := BelowNextError;			// Error * 1

    inc(CurrentErrorB, Delta);				// Error * 7
  end;

  // Move on to next column
  if (Direction = 1) then
  begin
    inc(PByte(ErrorR), SizeOf(TErrorTerm));
    inc(PByte(ErrorG), SizeOf(TErrorTerm));
    inc(PByte(ErrorB), SizeOf(TErrorTerm));
  end else
  begin
    dec(PByte(ErrorR), SizeOf(TErrorTerm));
    dec(PByte(ErrorG), SizeOf(TErrorTerm));
    dec(PByte(ErrorB), SizeOf(TErrorTerm));
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TFloydSteinbergDitherer.NextLine;
begin
  ErrorR[0] := BelowPrevErrorR;
  ErrorG[0] := BelowPrevErrorG;
  ErrorB[0] := BelowPrevErrorB;

  // Note: The optimizer produces better code for this construct:
  //   a := 0; b := a; c := a;
  // compared to this construct:
  //   a := 0; b := 0; c := 0;
  CurrentErrorR := 0;
  CurrentErrorG := CurrentErrorR;
  CurrentErrorB := CurrentErrorG;
  BelowErrorR := CurrentErrorG;
  BelowErrorG := CurrentErrorG;
  BelowErrorB := CurrentErrorG;
  BelowPrevErrorR := CurrentErrorG;
  BelowPrevErrorG := CurrentErrorG;
  BelowPrevErrorB := CurrentErrorG;

  inherited NextLine;

  if (Direction = 1) then
  begin
    ErrorR := ErrorsR;
    ErrorG := ErrorsG;
    ErrorB := ErrorsB;
  end else
  begin
    ErrorR := PErrors(@ErrorsR[Width+1]);
    ErrorG := PErrors(@ErrorsG[Width+1]);
    ErrorB := PErrors(@ErrorsB[Width+1]);
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

procedure TFloydSteinbergDitherer.Reset;
begin
  inherited;

  FillChar(ErrorsR^, SizeOf(TErrorTerm)*(Width+2), 0);
  FillChar(ErrorsG^, SizeOf(TErrorTerm)*(Width+2), 0);
  FillChar(ErrorsB^, SizeOf(TErrorTerm)*(Width+2), 0);
  ErrorR := ErrorsR;
  ErrorG := ErrorsG;
  ErrorB := ErrorsB;
  CurrentErrorR := 0;
  CurrentErrorG := CurrentErrorR;
  CurrentErrorB := CurrentErrorR;
  BelowErrorR := CurrentErrorR;
  BelowErrorG := CurrentErrorR;
  BelowErrorB := CurrentErrorR;
  BelowPrevErrorR := CurrentErrorR;
  BelowPrevErrorG := CurrentErrorR;
  BelowPrevErrorB := CurrentErrorR;
end;

////////////////////////////////////////////////////////////////////////////////
//	T5by3Ditherer
constructor T5by3Ditherer.Create(AWidth: integer; Lookup: TColorLookup);
var
  i: integer;
  Limit: byte;
begin
  inherited Create(AWidth, Lookup);

  i := 0;
  Limit := 0;
  while (i < 8) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(Limit);
    inc(i);
  end;
  while (i < 32) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(Limit, i AND 1);
    inc(i);
  end;
  while (i < 256) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(i);
  end;

  GetMem(ErrorsR0, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsG0, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsB0, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsR1, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsG1, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsB1, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsR2, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsG2, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsB2, SizeOf(TErrorTerm)*(Width+4));
  FDivisor := 1;
end;

destructor T5by3Ditherer.Destroy;
begin
  FreeMem(ErrorsR0);
  FreeMem(ErrorsG0);
  FreeMem(ErrorsB0);
  FreeMem(ErrorsR1);
  FreeMem(ErrorsG1);
  FreeMem(ErrorsB1);
  FreeMem(ErrorsR2);
  FreeMem(ErrorsG2);
  FreeMem(ErrorsB2);
  inherited Destroy;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function T5by3Ditherer.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
var
  ColorR, ColorG, ColorB: integer; // Error for current pixel
begin
  // Apply red component error correction
  ColorR := (ErrorR0[0] + FDivisor DIV 2) DIV FDivisor;
  ColorR := Red + ErrorLimit[ColorR];
  if (ColorR < 0) then
    ColorR := 0
  else if (ColorR > 255) then
    ColorR := 255;

  // Apply green component error correction
  ColorG := (ErrorG0[0] + FDivisor DIV 2) DIV FDivisor;
  ColorG := Green + ErrorLimit[ColorG];
  if (ColorG < 0) then
    ColorG := 0
  else if (ColorG > 255) then
    ColorG := 255;

  // Apply blue component error correction
  ColorB := (ErrorB0[0] + FDivisor DIV 2) DIV FDivisor;
  ColorB := Blue + ErrorLimit[ColorB];
  if (ColorB < 0) then
    ColorB := 0
  else if (ColorB > 255) then
    ColorB := 255;

  // Map color to palette
  Result := inherited Dither(ColorR, ColorG, ColorB, R, G, B);

  // Propagate red component error
  Propagate(ErrorR0, ErrorR1, ErrorR2, ColorR - R);
  // Propagate green component error
  Propagate(ErrorG0, ErrorG1, ErrorG2, ColorG - G);
  // Propagate blue component error
  Propagate(ErrorB0, ErrorB1, ErrorB2, ColorB - B);

  // Move on to next column
  if (Direction = 1) then
  begin
    inc(PByte(ErrorR0), SizeOf(TErrorTerm));
    inc(PByte(ErrorG0), SizeOf(TErrorTerm));
    inc(PByte(ErrorB0), SizeOf(TErrorTerm));
    inc(PByte(ErrorR1), SizeOf(TErrorTerm));
    inc(PByte(ErrorG1), SizeOf(TErrorTerm));
    inc(PByte(ErrorB1), SizeOf(TErrorTerm));
    inc(PByte(ErrorR2), SizeOf(TErrorTerm));
    inc(PByte(ErrorG2), SizeOf(TErrorTerm));
    inc(PByte(ErrorB2), SizeOf(TErrorTerm));
  end else
  begin
    dec(PByte(ErrorR0), SizeOf(TErrorTerm));
    dec(PByte(ErrorG0), SizeOf(TErrorTerm));
    dec(PByte(ErrorB0), SizeOf(TErrorTerm));
    dec(PByte(ErrorR1), SizeOf(TErrorTerm));
    dec(PByte(ErrorG1), SizeOf(TErrorTerm));
    dec(PByte(ErrorB1), SizeOf(TErrorTerm));
    dec(PByte(ErrorR2), SizeOf(TErrorTerm));
    dec(PByte(ErrorG2), SizeOf(TErrorTerm));
    dec(PByte(ErrorB2), SizeOf(TErrorTerm));
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure T5by3Ditherer.NextLine;
var
  TempErrors: PErrors;
begin
  FillChar(ErrorsR0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB0^, SizeOf(TErrorTerm)*(Width+4), 0);

  // Swap lines
  TempErrors := ErrorsR0;
  ErrorsR0 := ErrorsR1;
  ErrorsR1 := ErrorsR2;
  ErrorsR2 := TempErrors;

  TempErrors := ErrorsG0;
  ErrorsG0 := ErrorsG1;
  ErrorsG1 := ErrorsG2;
  ErrorsG2 := TempErrors;

  TempErrors := ErrorsB0;
  ErrorsB0 := ErrorsB1;
  ErrorsB1 := ErrorsB2;
  ErrorsB2 := TempErrors;

  inherited NextLine;

  FDirection2 := 2 * Direction;
  if (Direction = 1) then
  begin
    // ErrorsR0[1] gives compiler error, so we
    // use PErrors(longInt(ErrorsR0)+SizeOf(TErrorTerm)) instead...
    ErrorR0 := PErrors(PByte(ErrorsR0)+2*SizeOf(TErrorTerm));
    ErrorG0 := PErrors(PByte(ErrorsG0)+2*SizeOf(TErrorTerm));
    ErrorB0 := PErrors(PByte(ErrorsB0)+2*SizeOf(TErrorTerm));
    ErrorR1 := PErrors(PByte(ErrorsR1)+2*SizeOf(TErrorTerm));
    ErrorG1 := PErrors(PByte(ErrorsG1)+2*SizeOf(TErrorTerm));
    ErrorB1 := PErrors(PByte(ErrorsB1)+2*SizeOf(TErrorTerm));
    ErrorR2 := PErrors(PByte(ErrorsR2)+2*SizeOf(TErrorTerm));
    ErrorG2 := PErrors(PByte(ErrorsG2)+2*SizeOf(TErrorTerm));
    ErrorB2 := PErrors(PByte(ErrorsB2)+2*SizeOf(TErrorTerm));
  end else
  begin
    ErrorR0 := PErrors(@ErrorsR0[Width+1]);
    ErrorG0 := PErrors(@ErrorsG0[Width+1]);
    ErrorB0 := PErrors(@ErrorsB0[Width+1]);
    ErrorR1 := PErrors(@ErrorsR1[Width+1]);
    ErrorG1 := PErrors(@ErrorsG1[Width+1]);
    ErrorB1 := PErrors(@ErrorsB1[Width+1]);
    ErrorR2 := PErrors(@ErrorsR2[Width+1]);
    ErrorG2 := PErrors(@ErrorsG2[Width+1]);
    ErrorB2 := PErrors(@ErrorsB2[Width+1]);
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

procedure T5by3Ditherer.Reset;
begin
  inherited;

  FillChar(ErrorsR0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsR1^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG1^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB1^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsR2^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG2^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB2^, SizeOf(TErrorTerm)*(Width+4), 0);

  FDirection2 := 2 * Direction;
  ErrorR0 := PErrors(PByte(ErrorsR0)+2*SizeOf(TErrorTerm));
  ErrorG0 := PErrors(PByte(ErrorsG0)+2*SizeOf(TErrorTerm));
  ErrorB0 := PErrors(PByte(ErrorsB0)+2*SizeOf(TErrorTerm));
  ErrorR1 := PErrors(PByte(ErrorsR1)+2*SizeOf(TErrorTerm));
  ErrorG1 := PErrors(PByte(ErrorsG1)+2*SizeOf(TErrorTerm));
  ErrorB1 := PErrors(PByte(ErrorsB1)+2*SizeOf(TErrorTerm));
  ErrorR2 := PErrors(PByte(ErrorsR2)+2*SizeOf(TErrorTerm));
  ErrorG2 := PErrors(PByte(ErrorsG2)+2*SizeOf(TErrorTerm));
  ErrorB2 := PErrors(PByte(ErrorsB2)+2*SizeOf(TErrorTerm));
end;

////////////////////////////////////////////////////////////////////////////////
//	TStuckiDitherer
constructor TStuckiDitherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);
  FDivisor := 42;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TStuckiDitherer.Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer);
begin
  if (Error = 0) then
    exit;
  // Propagate Stucki error terms:
  //	...	...	(here)	8/42	4/42
  //	2/42	4/42	8/42	4/42	2/42
  //	1/42	2/42	4/42	2/42	1/42
  inc(Errors2[Direction2], Error);	// Error * 1
  inc(Errors2[-Direction2], Error);	// Error * 1

  Error := Error + Error;
  inc(Errors1[Direction2], Error);	// Error * 2
  inc(Errors1[-Direction2], Error);	// Error * 2
  inc(Errors2[Direction], Error);	// Error * 2
  inc(Errors2[-Direction], Error);	// Error * 2

  Error := Error + Error;
  inc(Errors0[Direction2], Error);	// Error * 4
  inc(Errors1[-Direction], Error);	// Error * 4
  inc(Errors1[Direction], Error);	// Error * 4
  inc(Errors2[0], Error);		// Error * 4

  Error := Error + Error;
  inc(Errors0[Direction], Error);	// Error * 8
  inc(Errors1[0], Error);		// Error * 8
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//	TSierraDitherer
constructor TSierraDitherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);
  FDivisor := 32;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TSierraDitherer.Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer);
var
  TempError: integer;
begin
  if (Error = 0) then
    exit;
  // Propagate Sierra error terms:
  //	...	...	(here)	5/32	3/32
  //	2/32	4/32	5/32	4/32	2/32
  //	...	2/32	3/32	2/32	...
  TempError := Error + Error;
  inc(Errors1[Direction2], TempError);	// Error * 2
  inc(Errors1[-Direction2], TempError);// Error * 2
  inc(Errors2[Direction], TempError);	// Error * 2
  inc(Errors2[-Direction], TempError);	// Error * 2

  inc(TempError, Error);
  inc(Errors0[Direction2], TempError);	// Error * 3
  inc(Errors2[0], TempError);		// Error * 3

  inc(TempError, Error);
  inc(Errors1[-Direction], TempError);	// Error * 4
  inc(Errors1[Direction], TempError);	// Error * 4

  inc(TempError, Error);
  inc(Errors0[Direction], TempError);	// Error * 5
  inc(Errors1[0], TempError);		// Error * 5
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//	TJaJuNiDitherer
constructor TJaJuNiDitherer.Create(AWidth: integer; Lookup: TColorLookup);
begin
  inherited Create(AWidth, Lookup);
  FDivisor := 38;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TJaJuNiDitherer.Propagate(Errors0, Errors1, Errors2: PErrors; Error: integer);
var
  TempError: integer;
begin
  if (Error = 0) then
    exit;
  // Propagate Jarvis, Judice and Ninke error terms:
  //	...	...	(here)	8/38	4/38
  //	2/38	4/38	8/38	4/38	2/38
  //	1/38	2/38	4/38	2/38	1/38
  inc(Errors2[Direction2], Error);	// Error * 1
  inc(Errors2[-Direction2], Error);	// Error * 1

  TempError := Error + Error;
  inc(Error, TempError);
  inc(Errors1[Direction2], Error);	// Error * 3
  inc(Errors1[-Direction2], Error);	// Error * 3
  inc(Errors2[Direction], Error);	// Error * 3
  inc(Errors2[-Direction], Error);	// Error * 3

  inc(Error, TempError);
  inc(Errors0[Direction2], Error);	// Error * 5
  inc(Errors1[-Direction], Error);	// Error * 5
  inc(Errors1[Direction], Error);	// Error * 5
  inc(Errors2[0], Error);		// Error * 5

  inc(Error, TempError);
  inc(Errors0[Direction], Error);	// Error * 7
  inc(Errors1[0], Error);		// Error * 7
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//	TSteveArcheDitherer
constructor TSteveArcheDitherer.Create(AWidth: integer; Lookup: TColorLookup);
var
  i: integer;
  Limit: byte;
begin
  inherited Create(AWidth, Lookup);

  i := 0;
  Limit := 0;
  while (i < 8) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(Limit);
    inc(i);
  end;
  while (i < 16) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(Limit, i AND 1);
    inc(i);
  end;
  while (i < 256) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(i);
  end;

  GetMem(ErrorsR0, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsG0, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsB0, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsR1, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsG1, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsB1, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsR2, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsG2, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsB2, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsR3, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsG3, SizeOf(TErrorTerm)*(Width+6));
  GetMem(ErrorsB3, SizeOf(TErrorTerm)*(Width+6));
end;

destructor TSteveArcheDitherer.Destroy;
begin
  FreeMem(ErrorsR0);
  FreeMem(ErrorsG0);
  FreeMem(ErrorsB0);
  FreeMem(ErrorsR1);
  FreeMem(ErrorsG1);
  FreeMem(ErrorsB1);
  FreeMem(ErrorsR2);
  FreeMem(ErrorsG2);
  FreeMem(ErrorsB2);
  FreeMem(ErrorsR3);
  FreeMem(ErrorsG3);
  FreeMem(ErrorsB3);
  inherited Destroy;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TSteveArcheDitherer.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
var
  ColorR, ColorG, ColorB: integer; // Error for current pixel

  // Propagate Stevenson & Arche error terms:
  //	...	...	...	(here)	...	32/200	...
  //    12/200	...	26/200	...	30/200	...	16/200
  //	...	12/200	...	26/200	...	12/200	...
  //	5/200	...	12/200	...	12/200	...	5/200
  procedure Propagate(Errors0, Errors1, Errors2, Errors3: PErrors; Error: integer);
  var
    TempError: integer;
  begin
    if (Error = 0) then
      exit;
    TempError := 5 * Error;
    inc(Errors3[FDirection3], TempError);	// Error * 5
    inc(Errors3[-FDirection3], TempError);	// Error * 5

    TempError := 12 * Error;
    inc(Errors1[-FDirection3], TempError);	// Error * 12
    inc(Errors2[-FDirection2], TempError);	// Error * 12
    inc(Errors2[FDirection2], TempError);	// Error * 12
    inc(Errors3[-Direction], TempError);	// Error * 12
    inc(Errors3[Direction], TempError);		// Error * 12

    inc(Errors1[FDirection3], 16 * TempError);	// Error * 16

    TempError := 26 * Error;
    inc(Errors1[-Direction], TempError);	// Error * 26
    inc(Errors2[0], TempError);			// Error * 26

    inc(Errors1[Direction], 30 * Error);	// Error * 30

    inc(Errors0[FDirection2], 32 * Error);	// Error * 32
  end;

begin
  // Apply red component error correction
  ColorR := (ErrorR0[0] + 100) DIV 200;
  ColorR := Red + ErrorLimit[ColorR];
  if (ColorR < 0) then
    ColorR := 0
  else if (ColorR > 255) then
    ColorR := 255;

  // Apply green component error correction
  ColorG := (ErrorG0[0] + 100) DIV 200;
  ColorG := Green + ErrorLimit[ColorG];
  if (ColorG < 0) then
    ColorG := 0
  else if (ColorG > 255) then
    ColorG := 255;

  // Apply blue component error correction
  ColorB := (ErrorB0[0] + 100) DIV 200;
  ColorB := Blue + ErrorLimit[ColorB];
  if (ColorB < 0) then
    ColorB := 0
  else if (ColorB > 255) then
    ColorB := 255;

  // Map color to palette
  Result := inherited Dither(ColorR, ColorG, ColorB, R, G, B);

  // Propagate red component error
  Propagate(ErrorR0, ErrorR1, ErrorR2, ErrorR3, ColorR - R);
  // Propagate green component error
  Propagate(ErrorG0, ErrorG1, ErrorG2, ErrorG3, ColorG - G);
  // Propagate blue component error
  Propagate(ErrorB0, ErrorB1, ErrorB2, ErrorB3, ColorB - B);

  // Move on to next column
  if (Direction = 1) then
  begin
    inc(PByte(ErrorR0), SizeOf(TErrorTerm));
    inc(PByte(ErrorG0), SizeOf(TErrorTerm));
    inc(PByte(ErrorB0), SizeOf(TErrorTerm));
    inc(PByte(ErrorR1), SizeOf(TErrorTerm));
    inc(PByte(ErrorG1), SizeOf(TErrorTerm));
    inc(PByte(ErrorB1), SizeOf(TErrorTerm));
    inc(PByte(ErrorR2), SizeOf(TErrorTerm));
    inc(PByte(ErrorG2), SizeOf(TErrorTerm));
    inc(PByte(ErrorB2), SizeOf(TErrorTerm));
    inc(PByte(ErrorR3), SizeOf(TErrorTerm));
    inc(PByte(ErrorG3), SizeOf(TErrorTerm));
    inc(PByte(ErrorB3), SizeOf(TErrorTerm));
  end else
  begin
    dec(PByte(ErrorR0), SizeOf(TErrorTerm));
    dec(PByte(ErrorG0), SizeOf(TErrorTerm));
    dec(PByte(ErrorB0), SizeOf(TErrorTerm));
    dec(PByte(ErrorR1), SizeOf(TErrorTerm));
    dec(PByte(ErrorG1), SizeOf(TErrorTerm));
    dec(PByte(ErrorB1), SizeOf(TErrorTerm));
    dec(PByte(ErrorR2), SizeOf(TErrorTerm));
    dec(PByte(ErrorG2), SizeOf(TErrorTerm));
    dec(PByte(ErrorB2), SizeOf(TErrorTerm));
    dec(PByte(ErrorR3), SizeOf(TErrorTerm));
    dec(PByte(ErrorG3), SizeOf(TErrorTerm));
    dec(PByte(ErrorB3), SizeOf(TErrorTerm));
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TSteveArcheDitherer.NextLine;
var
  TempErrors: PErrors;
begin
  FillChar(ErrorsR0^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG0^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB0^, SizeOf(TErrorTerm)*(Width+6), 0);

  // Swap lines
  TempErrors := ErrorsR0;
  ErrorsR0 := ErrorsR1;
  ErrorsR1 := ErrorsR2;
  ErrorsR2 := ErrorsR3;
  ErrorsR3 := TempErrors;

  TempErrors := ErrorsG0;
  ErrorsG0 := ErrorsG1;
  ErrorsG1 := ErrorsG2;
  ErrorsG2 := ErrorsG3;
  ErrorsG3 := TempErrors;

  TempErrors := ErrorsB0;
  ErrorsB0 := ErrorsB1;
  ErrorsB1 := ErrorsB2;
  ErrorsB2 := ErrorsB3;
  ErrorsB3 := TempErrors;

  inherited NextLine;

  FDirection2 := 2 * Direction;
  FDirection3 := 3 * Direction;

  if (Direction = 1) then
  begin
    // ErrorsR0[1] gives compiler error, so we
    // use PErrors(longInt(ErrorsR0)+SizeOf(TErrorTerm)) instead...
    ErrorR0 := PErrors(PByte(ErrorsR0)+3*SizeOf(TErrorTerm));
    ErrorG0 := PErrors(PByte(ErrorsG0)+3*SizeOf(TErrorTerm));
    ErrorB0 := PErrors(PByte(ErrorsB0)+3*SizeOf(TErrorTerm));
    ErrorR1 := PErrors(PByte(ErrorsR1)+3*SizeOf(TErrorTerm));
    ErrorG1 := PErrors(PByte(ErrorsG1)+3*SizeOf(TErrorTerm));
    ErrorB1 := PErrors(PByte(ErrorsB1)+3*SizeOf(TErrorTerm));
    ErrorR2 := PErrors(PByte(ErrorsR2)+3*SizeOf(TErrorTerm));
    ErrorG2 := PErrors(PByte(ErrorsG2)+3*SizeOf(TErrorTerm));
    ErrorB2 := PErrors(PByte(ErrorsB2)+3*SizeOf(TErrorTerm));
    ErrorR3 := PErrors(PByte(ErrorsR3)+3*SizeOf(TErrorTerm));
    ErrorG3 := PErrors(PByte(ErrorsG3)+3*SizeOf(TErrorTerm));
    ErrorB3 := PErrors(PByte(ErrorsB3)+3*SizeOf(TErrorTerm));
  end else
  begin
    ErrorR0 := PErrors(@ErrorsR0[Width+2]);
    ErrorG0 := PErrors(@ErrorsG0[Width+2]);
    ErrorB0 := PErrors(@ErrorsB0[Width+2]);
    ErrorR1 := PErrors(@ErrorsR1[Width+2]);
    ErrorG1 := PErrors(@ErrorsG1[Width+2]);
    ErrorB1 := PErrors(@ErrorsB1[Width+2]);
    ErrorR2 := PErrors(@ErrorsR2[Width+2]);
    ErrorG2 := PErrors(@ErrorsG2[Width+2]);
    ErrorB2 := PErrors(@ErrorsB2[Width+2]);
    ErrorR3 := PErrors(@ErrorsR2[Width+2]);
    ErrorG3 := PErrors(@ErrorsG2[Width+2]);
    ErrorB3 := PErrors(@ErrorsB2[Width+2]);
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

procedure TSteveArcheDitherer.Reset;
begin
  inherited;

  FillChar(ErrorsR0^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG0^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB0^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsR1^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG1^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB1^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsR2^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG2^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB2^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsR3^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsG3^, SizeOf(TErrorTerm)*(Width+6), 0);
  FillChar(ErrorsB3^, SizeOf(TErrorTerm)*(Width+6), 0);

  FDirection2 := 2 * Direction;
  FDirection3 := 3 * Direction;

  ErrorR0 := PErrors(PByte(ErrorsR0)+3*SizeOf(TErrorTerm));
  ErrorG0 := PErrors(PByte(ErrorsG0)+3*SizeOf(TErrorTerm));
  ErrorB0 := PErrors(PByte(ErrorsB0)+3*SizeOf(TErrorTerm));
  ErrorR1 := PErrors(PByte(ErrorsR1)+3*SizeOf(TErrorTerm));
  ErrorG1 := PErrors(PByte(ErrorsG1)+3*SizeOf(TErrorTerm));
  ErrorB1 := PErrors(PByte(ErrorsB1)+3*SizeOf(TErrorTerm));
  ErrorR2 := PErrors(PByte(ErrorsR2)+3*SizeOf(TErrorTerm));
  ErrorG2 := PErrors(PByte(ErrorsG2)+3*SizeOf(TErrorTerm));
  ErrorB2 := PErrors(PByte(ErrorsB2)+3*SizeOf(TErrorTerm));
  ErrorR3 := PErrors(PByte(ErrorsR3)+3*SizeOf(TErrorTerm));
  ErrorG3 := PErrors(PByte(ErrorsG3)+3*SizeOf(TErrorTerm));
  ErrorB3 := PErrors(PByte(ErrorsB3)+3*SizeOf(TErrorTerm));
end;

////////////////////////////////////////////////////////////////////////////////
//	TBurkesDitherer
constructor TBurkesDitherer.Create(AWidth: integer; Lookup: TColorLookup);
var
  i: integer;
  Limit: byte;
begin
  inherited Create(AWidth, Lookup);

  i := 0;
  Limit := 0;
  while (i < 16) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(Limit);
    inc(i);
  end;
  while (i < 48) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(Limit, i AND 1);
    inc(i);
  end;
  while (i < 256) do
  begin
    ErrorLimit[i] := Limit;
    ErrorLimit[-i] := -Limit;
    inc(i);
  end;

  GetMem(ErrorsR0, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsG0, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsB0, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsR1, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsG1, SizeOf(TErrorTerm)*(Width+4));
  GetMem(ErrorsB1, SizeOf(TErrorTerm)*(Width+4));
end;

destructor TBurkesDitherer.Destroy;
begin
  FreeMem(ErrorsR0);
  FreeMem(ErrorsG0);
  FreeMem(ErrorsB0);
  FreeMem(ErrorsR1);
  FreeMem(ErrorsG1);
  FreeMem(ErrorsB1);
  inherited Destroy;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TBurkesDitherer.Dither(Red, Green, Blue: BYTE; var R, G, B: BYTE): Byte;
var
  ErrorR, ErrorG, ErrorB: integer; // Error for current pixel

  // Propagate Burkes error terms:
  //	...	...	(here)	8/32	4/32
  //	2/32	4/32	8/32	4/32	2/32
  procedure Propagate(Errors0, Errors1: PErrors; Error: integer);
  begin
    if (Error = 0) then
      exit;
    inc(Error, Error);
    inc(Errors1[FDirection2], Error);	// Error * 2
    inc(Errors1[-FDirection2], Error);	// Error * 2

    inc(Error, Error);
    inc(Errors0[FDirection2], Error);	// Error * 4
    inc(Errors1[-Direction], Error);	// Error * 4
    inc(Errors1[Direction], Error);	// Error * 4

    inc(Error, Error);
    inc(Errors0[Direction], Error);	// Error * 8
    inc(Errors1[0], Error);		// Error * 8
  end;

begin
  // Apply red component error correction
  ErrorR := (ErrorR0[0] + 16) DIV 32;
  ErrorR := Red + ErrorLimit[ErrorR];
  if (ErrorR < 0) then
    ErrorR := 0
  else if (ErrorR > 255) then
    ErrorR := 255;

  // Apply green component error correction
  ErrorG := (ErrorG0[0] + 16) DIV 32;
  ErrorG := Green + ErrorLimit[ErrorG];
  if (ErrorG < 0) then
    ErrorG := 0
  else if (ErrorG > 255) then
    ErrorG := 255;

  // Apply blue component error correction
  ErrorB := (ErrorB0[0] + 16) DIV 32;
  ErrorB := Blue + ErrorLimit[ErrorB];
  if (ErrorB < 0) then
    ErrorB := 0
  else if (ErrorB > 255) then
    ErrorB := 255;

  // Map color to palette
  Result := inherited Dither(ErrorR, ErrorG, ErrorB, R, G, B);

  // Propagate red component error
  Propagate(ErrorR0, ErrorR1, ErrorR - R);
  // Propagate green component error
  Propagate(ErrorG0, ErrorG1, ErrorG - G);
  // Propagate blue component error
  Propagate(ErrorB0, ErrorB1, ErrorB - B);

  // Move on to next column
  if (Direction = 1) then
  begin
    inc(PByte(ErrorR0), SizeOf(TErrorTerm));
    inc(PByte(ErrorG0), SizeOf(TErrorTerm));
    inc(PByte(ErrorB0), SizeOf(TErrorTerm));
    inc(PByte(ErrorR1), SizeOf(TErrorTerm));
    inc(PByte(ErrorG1), SizeOf(TErrorTerm));
    inc(PByte(ErrorB1), SizeOf(TErrorTerm));
  end else
  begin
    dec(PByte(ErrorR0), SizeOf(TErrorTerm));
    dec(PByte(ErrorG0), SizeOf(TErrorTerm));
    dec(PByte(ErrorB0), SizeOf(TErrorTerm));
    dec(PByte(ErrorR1), SizeOf(TErrorTerm));
    dec(PByte(ErrorG1), SizeOf(TErrorTerm));
    dec(PByte(ErrorB1), SizeOf(TErrorTerm));
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TBurkesDitherer.NextLine;
var
  TempErrors: PErrors;
begin
  FillChar(ErrorsR0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB0^, SizeOf(TErrorTerm)*(Width+4), 0);

  // Swap lines
  TempErrors := ErrorsR0;
  ErrorsR0 := ErrorsR1;
  ErrorsR1 := TempErrors;

  TempErrors := ErrorsG0;
  ErrorsG0 := ErrorsG1;
  ErrorsG1 := TempErrors;

  TempErrors := ErrorsB0;
  ErrorsB0 := ErrorsB1;
  ErrorsB1 := TempErrors;

  inherited NextLine;

  FDirection2 := 2 * Direction;
  if (Direction = 1) then
  begin
    // ErrorsR0[1] gives compiler error, so we
    // use PErrors(longInt(ErrorsR0)+SizeOf(TErrorTerm)) instead...
    ErrorR0 := PErrors(PByte(ErrorsR0)+2*SizeOf(TErrorTerm));
    ErrorG0 := PErrors(PByte(ErrorsG0)+2*SizeOf(TErrorTerm));
    ErrorB0 := PErrors(PByte(ErrorsB0)+2*SizeOf(TErrorTerm));
    ErrorR1 := PErrors(PByte(ErrorsR1)+2*SizeOf(TErrorTerm));
    ErrorG1 := PErrors(PByte(ErrorsG1)+2*SizeOf(TErrorTerm));
    ErrorB1 := PErrors(PByte(ErrorsB1)+2*SizeOf(TErrorTerm));
  end else
  begin
    ErrorR0 := PErrors(@ErrorsR0[Width+1]);
    ErrorG0 := PErrors(@ErrorsG0[Width+1]);
    ErrorB0 := PErrors(@ErrorsB0[Width+1]);
    ErrorR1 := PErrors(@ErrorsR1[Width+1]);
    ErrorG1 := PErrors(@ErrorsG1[Width+1]);
    ErrorB1 := PErrors(@ErrorsB1[Width+1]);
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

procedure TBurkesDitherer.Reset;
begin
  inherited;

  FillChar(ErrorsR0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB0^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsR1^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsG1^, SizeOf(TErrorTerm)*(Width+4), 0);
  FillChar(ErrorsB1^, SizeOf(TErrorTerm)*(Width+4), 0);

  FDirection2 := 2 * Direction;
  ErrorR0 := PErrors(PByte(ErrorsR0)+2*SizeOf(TErrorTerm));
  ErrorG0 := PErrors(PByte(ErrorsG0)+2*SizeOf(TErrorTerm));
  ErrorB0 := PErrors(PByte(ErrorsB0)+2*SizeOf(TErrorTerm));
  ErrorR1 := PErrors(PByte(ErrorsR1)+2*SizeOf(TErrorTerm));
  ErrorG1 := PErrors(PByte(ErrorsG1)+2*SizeOf(TErrorTerm));
  ErrorB1 := PErrors(PByte(ErrorsB1)+2*SizeOf(TErrorTerm));
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Octree Color Quantization Engine
//
////////////////////////////////////////////////////////////////////////////////
type
  TOctreeNode = class;	// Forward definition so TReducibleNodes can be declared

  TReducibleNodes = array[0..7] of TOctreeNode;

  TOctreeNode = Class(TObject)
  public
    IsLeaf: Boolean;
    PixelCount: integer;
    RedSum: integer;
    GreenSum: integer;
    BlueSum: integer;
    Next: TOctreeNode;
    Child: TReducibleNodes;

    constructor Create(Level: integer; ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);
    destructor Destroy; override;
  end;

  // Generic Octree Quantizer
  TColorQuantizer = class(TObject)
  strict private
    FTree: TOctreeNode;
    FLeafCount: integer;
    FReducibleNodes: TReducibleNodes;
    FMaxColors: integer;
    FColorBits: integer;
  strict protected
    procedure AddColor(var Node: TOctreeNode; r, g, b: byte; ColorBits: integer;
      Level: integer; var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
    procedure DeleteTree(var Node: TOctreeNode);
    procedure GetPaletteColors(const Node: TOctreeNode;
      var RGBQuadArray: TRGBQuadArray; var Index: integer);
    procedure ReduceTree(ColorBits: integer; var LeafCount: integer;
      var ReducibleNodes: TReducibleNodes);
    procedure ProcessPixel(r, g, b: byte);
  public
    constructor Create(MaxColors: integer; ColorBits: integer);
    destructor Destroy; override;
    procedure GetColorTable(var RGBQuadArray: TRGBQuadArray);
    property ColorCount: integer read FLeafCount;
  end;

constructor TOctreeNode.Create(Level: integer; ColorBits: integer;
  var LeafCount: integer; var ReducibleNodes: TReducibleNodes);
var
  i: integer;
begin
  PixelCount := 0;
  RedSum := 0;
  GreenSum := 0;
  BlueSum := 0;
  for i := Low(Child) to High(Child) do
    Child[i] := nil;

  IsLeaf := (Level = ColorBits);
  if (IsLeaf) then
  begin
    Next := nil;
    inc(LeafCount);
  end else
  begin
    Next := ReducibleNodes[Level];
    ReducibleNodes[Level] := Self;
  end;
end;

destructor TOctreeNode.Destroy;
var
  i: integer;
begin
  for i := High(Child) downto Low(Child) do
    Child[i].Free;
end;

constructor TColorQuantizer.Create(MaxColors: integer; ColorBits: integer);
var
  i: integer;
begin
  ASSERT(ColorBits <= 8, 'ColorBits must be 8 or less');

  FTree := nil;
  FLeafCount := 0;

  // Initialize all nodes even though only ColorBits+1 of them are needed
  for i := Low(FReducibleNodes) to High(FReducibleNodes) do
    FReducibleNodes[i] := nil;

  FMaxColors := MaxColors;
  FColorBits := ColorBits;
end;

destructor TColorQuantizer.Destroy;
begin
  if (FTree <> nil) then
    DeleteTree(FTree);
end;

procedure TColorQuantizer.GetColorTable(var RGBQuadArray: TRGBQuadArray);
var
  Index: integer;
begin
  Index := 0;
  GetPaletteColors(FTree, RGBQuadArray, Index);
end;

procedure TColorQuantizer.AddColor(var Node: TOctreeNode; r,g,b: byte;
  ColorBits: integer; Level: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
const
  Mask: array[0..7] of BYTE = ($80, $40, $20, $10, $08, $04, $02, $01);
var
  Index: integer;
  Shift: integer;
begin
  // If the node doesn't exist, create it.
  if (Node = nil) then
    Node := TOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

  if (Node.IsLeaf) then
  begin
    inc(Node.PixelCount);
    inc(Node.RedSum, r);
    inc(Node.GreenSum, g);
    inc(Node.BlueSum, b);
  end else
  begin
    // Recurse a level deeper if the node is not a leaf.
    Shift := 7 - Level;

    Index := (((r and mask[Level]) SHR Shift) SHL 2)  or
             (((g and mask[Level]) SHR Shift) SHL 1)  or
              ((b and mask[Level]) SHR Shift);
    AddColor(Node.Child[Index], r, g, b, ColorBits, Level+1, LeafCount, ReducibleNodes);
  end;
end;

procedure TColorQuantizer.DeleteTree(var Node: TOctreeNode);
var
  i: integer;
begin
  for i := High(TReducibleNodes) downto Low(TReducibleNodes) do
    if (Node.Child[i] <> nil) then
      DeleteTree(Node.Child[i]);

  Node.Free;
  Node := nil;
end;

procedure TColorQuantizer.GetPaletteColors(const Node: TOctreeNode;
  var RGBQuadArray: TRGBQuadArray; var Index: integer);
var
  i: integer;
begin
  if (Node.IsLeaf) then
  begin
    with RGBQuadArray[Index] do
    begin
      if (Node.PixelCount <> 0) then
      begin
        rgbRed   := BYTE(Node.RedSum   DIV Node.PixelCount);
        rgbGreen := BYTE(Node.GreenSum DIV Node.PixelCount);
        rgbBlue  := BYTE(Node.BlueSum  DIV Node.PixelCount);
      end else
      begin
        rgbRed := 0;
        rgbGreen := 0;
        rgbBlue := 0;
      end;
      rgbReserved := 0;
    end;
    inc(Index);
  end else
  begin
    for i := Low(Node.Child) to High(Node.Child) do
      if (Node.Child[i] <> nil) then
        GetPaletteColors(Node.Child[i], RGBQuadArray, Index);
  end;
end;

procedure TColorQuantizer.ReduceTree(ColorBits: integer; var LeafCount: integer;
  var ReducibleNodes: TReducibleNodes);
var
  RedSum, GreenSum, BlueSum: integer;
  Children: integer;
  i: integer;
  Node: TOctreeNode;
begin
  // Find the deepest level containing at least one reducible node
  i := Colorbits - 1;
  while (i > 0) and (ReducibleNodes[i] = nil) do
    dec(i);

  // Reduce the node most recently added to the list at level i.
  Node := ReducibleNodes[i];
  ReducibleNodes[i] := Node.Next;

  RedSum   := 0;
  GreenSum := 0;
  BlueSum  := 0;
  Children := 0;

  for i := Low(ReducibleNodes) to High(ReducibleNodes) do
    if (Node.Child[i] <> nil) then
    begin
      inc(RedSum, Node.Child[i].RedSum);
      inc(GreenSum, Node.Child[i].GreenSum);
      inc(BlueSum, Node.Child[i].BlueSum);
      inc(Node.PixelCount, Node.Child[i].PixelCount);
      Node.Child[i].Free;
      Node.Child[i] := nil;
      inc(Children);
    end;

  Node.IsLeaf := TRUE;
  Node.RedSum := RedSum;
  Node.GreenSum := GreenSum;
  Node.BlueSum := BlueSum;
  dec(LeafCount, Children-1);
end;

procedure TColorQuantizer.ProcessPixel(r, g, b: byte);
begin
  AddColor(FTree, r, g, b, FColorBits, 0, FLeafCount, FReducibleNodes);

  while (FLeafCount > FMaxColors) do
    ReduceTree(FColorbits, FLeafCount, FReducibleNodes);
end;

type
  // DIB Quantizer
  TDIBQuantizer = class(TColorQuantizer)
  public
    function ProcessImage(const DIB: TDIBReader): boolean;
  end;

function TDIBQuantizer.ProcessImage(const DIB: TDIBReader): boolean;
var
  i, j: integer;
  ScanLine: pointer;
  Pixel: PRGBTriple;
begin
  Result := True;

  for j := 0 to DIB.Bitmap.Height-1 do
  begin
    Scanline := DIB.Scanline[j];
    Pixel := ScanLine;
    for i := 0 to DIB.Bitmap.Width-1 do
    begin
      with Pixel^ do
        ProcessPixel(rgbtRed, rgbtGreen, rgbtBlue);
      inc(Pixel);
    end;
  end;
end;

type
  // GIF Quantizer
  TGIFQuantizer = class(TColorQuantizer)
  public
    function ProcessImage(const GIF: TGIFImage): boolean;
  end;

function TGIFQuantizer.ProcessImage(const GIF: TGIFImage): boolean;

  procedure ProcessPixels(Pixel: PByte; Size: integer;
    const ColorMap: TColorMap; Transparent: integer);
  begin
    while (Size > 0) do
    begin
      if (ord(Pixel^) <> Transparent) then
        with ColorMap[ord(Pixel^)] do
          ProcessPixel(Red, Green, Blue)
      else
        Result := True;
      inc(Pixel);
      Dec(Size);
    end;
  end;

var
  i: integer;
  Transparent: integer;
  Frame: TGIFFrame;
begin
  Result := False;

  if (GIF.Empty) then
    exit;

  for i := 0 to GIF.Images.Count-1 do
  begin
    Frame := GIF.Images[i];
    if (Frame.Empty) then
      continue;
    if (Frame.Transparent) then
      Transparent := Frame.GraphicControlExtension.TransparentColorIndex
    else
      Transparent := -1;

    ProcessPixels(Frame.Data, Frame.DataSize, Frame.ActiveColorMap.Data, Transparent);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			Octree Color Quantization Wrapper
//
////////////////////////////////////////////////////////////////////////////////

function BuildPalette(const RGBQuadArray: TRGBQuadArray; Colors: integer;
  Windows: boolean): hPalette;
var
  SystemPalette: HPalette;
  LogicalPalette: TMaxLogPalette;
  PaletteEntry: PPaletteEntry;
  RGBQuad: PRGBQuad;
begin
  if (Windows) then
  begin
    if (Colors > 236) then
      Colors := 236;
    // Get the windows 20 color system palette
    SystemPalette := GetStockObject(DEFAULT_PALETTE);
    GetPaletteEntries(SystemPalette, 0, 20, LogicalPalette.palPalEntry[0]);
    PaletteEntry := @LogicalPalette.palPalEntry[20];
    LogicalPalette.palNumEntries := Colors+20;
  end else
  begin
    PaletteEntry := @LogicalPalette.palPalEntry[0];
    LogicalPalette.palNumEntries := Colors;
  end;
  LogicalPalette.palVersion := $0300;

  RGBQuad := @RGBQuadArray[0];

  while Colors > 0 do
  begin
    with PaletteEntry^, RGBQuad^ do
    begin
      peRed   := rgbRed;
      peGreen := rgbGreen;
      peBlue  := rgbBlue;
      peFlags := rgbReserved;
    end;
    inc(PaletteEntry);
    inc(RGBQuad);
    dec(Colors);
  end;

  Result := CreatePalette(pLogPalette(@LogicalPalette)^);
end;

// Wrapper for internal use - uses TDIBReader for bitmap access
function doCreateOptimizedPaletteFromSingleBitmap(const DIB: TDIBReader;
  Colors, ColorBits: integer; Windows: boolean): hPalette;
var
  Quantizer: TDIBQuantizer;
  RGBQuadArray: TRGBQuadArray;
begin
  if (Windows) and (Colors > 236) then
    Colors := 236;
  // Normally for 24-bit images, use ColorBits of 5 or 6.  For 8-bit images
  // use ColorBits = 8.
  Quantizer := TDIBQuantizer.Create(Colors, ColorBits);
  try
    Quantizer.ProcessImage(DIB);
    Quantizer.GetColorTable(RGBQuadArray);
  finally
    Quantizer.Free;
  end;
  Result := BuildPalette(RGBQuadArray, Colors, Windows);
end;

function doCreateOptimizedPaletteFromGIF(const GIF: TGIFImage; Colors: integer;
  Windows: boolean; var TransparentIndex: integer): hPalette;
var
  Quantizer: TGIFQuantizer;
  RGBQuadArray: TRGBQuadArray;
  Transparent: boolean;
  TransparentColor: TColor;
  i: integer;
begin
  // Make sure that there's room for the Windows system colors.
  if (Windows) and (Colors > 236) then
    Colors := 236;

  // Determine if GIF uses transparency.
  Transparent := False;
  i := 0;
  if ((Windows) and (Colors = 236)) or ((not Windows) and (Colors = 256)) then
  begin
    while (not Transparent) and (i < GIF.Images.Count) do
    begin
      Transparent := GIF.Images[i].Transparent;
      if (Transparent) then
        break;
      inc(i);
    end;
  end;

  if (Transparent) then
  begin
    // If the GIF uses transparency, we need to reserve one color for the
    // transparency.
    dec(Colors);
    TransparentIndex := Colors;
    // Copy the current transparency color into the color table.
    TransparentColor := GIF.Images[i].GraphicControlExtension.TransparentColor;
    with RGBQuadArray[TransparentIndex] do
    begin
      rgbBlue := (TransparentColor shr 16) and $FF;
      rgbGreen := (TransparentColor shr 8) and $FF;
      rgbRed  := TransparentColor and $FF;
      rgbReserved := 0;
    end;
  end else
    TransparentIndex := -1;

  Quantizer := TGIFQuantizer.Create(Colors, 8);
  try
    Quantizer.ProcessImage(GIF);
    Quantizer.GetColorTable(RGBQuadArray);
  finally
    Quantizer.Free;
  end;

  if (Transparent) then
    inc(Colors);

  Result := BuildPalette(RGBQuadArray, Colors, Windows);
end;

function CreateOptimizedPaletteFromSingleBitmap(const Bitmap: TBitmap;
  Colors, ColorBits: integer; Windows: boolean): hPalette;
var
  DIB: TDIBReader;
begin
  DIB := TDIBReader.Create(Bitmap, pf24bit);
  try
    Result := doCreateOptimizedPaletteFromSingleBitmap(DIB, Colors, ColorBits, Windows);
  finally
    DIB.Free;
  end;
end;

function CreateOptimizedPaletteFromManyBitmaps(Bitmaps: TList; Colors, ColorBits: integer;
  Windows: boolean): hPalette;
var
  i: integer;
  Quantizer: TDIBQuantizer;
  RGBQuadArray: TRGBQuadArray;
  DIB: TDIBReader;
begin
  if (Bitmaps = nil) or (Bitmaps.Count = 0) then
    Error(sInvalidBitmapList);

  if (Windows) and (Colors > 236) then
    Colors := 236;
  // Normally for 24-bit images, use ColorBits of 5 or 6.  For 8-bit images
  // use ColorBits = 8.
  Quantizer := TDIBQuantizer.Create(Colors, ColorBits);
  try
    for i := 0 to Bitmaps.Count-1 do
    begin
      DIB := TDIBReader.Create(TBitmap(Bitmaps[i]), pf24bit);
      try
        Quantizer.ProcessImage(DIB);
      finally
        DIB.Free;
      end;
    end;
    Quantizer.GetColorTable(RGBQuadArray);
  finally
    Quantizer.Free;
  end;

  Result := BuildPalette(RGBQuadArray, Colors, Windows);
end;


////////////////////////////////////////////////////////////////////////////////
//
//			Color reduction
//
////////////////////////////////////////////////////////////////////////////////

function CreateDitherPalette(ColorReduction: TColorReduction;
  CustomPalette: hPalette): hPalette;

  function GrayScalePalette: hPalette;
  var
    i: integer;
    Pal: TMaxLogPalette;
  begin
    Pal.palVersion := $0300;
    Pal.palNumEntries := 256;
    for i := 0 to 255 do
      with Pal.palPalEntry[i] do
      begin
        peRed := i;
        peGreen := i;
        peBlue  := i;
        peFlags := PC_NOCOLLAPSE;
      end;
    Result := CreatePalette(pLogPalette(@Pal));
  end;

  function MonochromePalette: hPalette;
  var
    Pal: TMaxLogPalette;
  const
    Values: array[0..1] of byte = (0, 255);
  begin
    Pal.palVersion := $0300;
    Pal.palNumEntries := 2;
    with Pal.palPalEntry[0] do
    begin
      peRed := 0;
      peGreen := 0;
      peBlue  := 0;
      peFlags := PC_NOCOLLAPSE;
    end;
    with Pal.palPalEntry[1] do
    begin
      peRed := 255;
      peGreen := 255;
      peBlue  := 255;
      peFlags := PC_NOCOLLAPSE;
    end;
    Result := CreatePalette(pLogPalette(@Pal));
  end;

  function WindowsGrayScalePalette: hPalette;
  var
    i: integer;
    Pal: TMaxLogPalette;
  const
    Values: array[0..3] of byte = (0, 128, 192, 255);
  begin
    Pal.palVersion := $0300;
    Pal.palNumEntries := 4;
    for i := 0 to 3 do
    begin
      with Pal.palPalEntry[i] do
      begin
        peRed := Values[i];
        peGreen := Values[i];
        peBlue  := Values[i];
        peFlags := PC_NOCOLLAPSE;
      end;
    end;
    Result := CreatePalette(pLogPalette(@Pal));
  end;

  function WindowsHalftonePalette: hPalette;
  var
    DC: HDC;
  begin
    DC := GDICheck(GetDC(0));
    try
      Result := CreateHalfTonePalette(DC);
    finally
      ReleaseDC(0, DC);
    end;
  end;

begin
  // Create a palette based on current options
  case (ColorReduction) of
    rmQuantize, rmQuantizeWindows:
      Result := 0; // Not handled here!
    rmNetscape:
      Result := WebPalette;
    rmGrayScale:
      Result := GrayScalePalette;
    rmMonochrome:
      Result := MonochromePalette;
    rmWindowsGray:
      Result := WindowsGrayScalePalette;
    rmWindows20:
      Result := GetStockObject(DEFAULT_PALETTE);
    rmWindows256:
      Result := WindowsHalftonePalette;
    rmPalette:
      begin
        if (CustomPalette = 0) then
          Error(sNoPalette);
        Result := CopyPalette(CustomPalette);
        if (Result = 0) then
          Error(sNoPalette);
      end;
  else
    Result := 0;
  end;
end;

function CreateColorMapper(Palette: hPalette;
  ColorReduction: TColorReduction): TColorLookup;
begin
  // Create an optimal color mapper based on current options.
  case (ColorReduction) of
    // rmWindowsGray:
    //  ColorLookup := TGrayWindowsLookup.Create(Palette);
    rmQuantize:
      Result := TGenericColorMapper.Create(Palette);
    rmNetscape:
      Result := TNetscapeColorLookup.Create(Palette);
    rmGrayScale:
      Result := TGrayScaleLookup.Create(Palette);
    rmMonochrome:
      Result := TMonochromeLookup.Create(Palette);
  else
    // For some strange reason my fast and dirty color lookup
    // is more precise that Windows GetNearestPaletteIndex...
    // ...but there are cases where it fails to match the correct color.
    // Therefore we cannot allow that it is used unless the developer has
    // explicitly requested it.
    Result := TGenericColorMapper.Create(Palette);
  end;
end;

function CreateDitherer(DitherMode: TDitherMode; Width: integer;
  ColorLookup: TColorLookup): TDitherEngine;
begin
  // Create a ditherer based on current options
  case (DitherMode) of
    dmNearest:
      Result := TDitherEngine.Create(Width, ColorLookup);
    dmFloydSteinberg:
      Result := TFloydSteinbergDitherer.Create(Width, ColorLookup);
    dmStucki:
      Result := TStuckiDitherer.Create(Width, ColorLookup);
    dmSierra:
      Result := TSierraDitherer.Create(Width, ColorLookup);
    dmJaJuNI:
      Result := TJaJuNIDitherer.Create(Width, ColorLookup);
    dmSteveArche:
      Result := TSteveArcheDitherer.Create(Width, ColorLookup);
    dmBurkes:
      Result := TBurkesDitherer.Create(Width, ColorLookup);
  else
    Result := nil;
  end;
end;


{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function ReduceColors(Bitmap: TBitmap; ColorReduction: TColorReduction;
  DitherMode: TDitherMode; ReductionBits: integer; CustomPalette: hPalette): TBitmap;
var
  Palette: hPalette;
  ColorLookup: TColorLookup;
  Ditherer: TDitherEngine;
  Row: Integer;
  DIBResult: TDIBWriter;
  DIBSource: TDIBReader;
  SrcScanLine, Src: PRGBTriple;
  DstScanLine, Dst: PByte;
  BGR: TRGBTriple;
begin
  Result := TBitmap.Create;
  try

    if (ColorReduction = rmNone) then
    begin
      Result.Assign(Bitmap);
      SetPixelFormat(Result, pf24bit);
      exit;
    end;

    if (Bitmap.Width*Bitmap.Height > BitmapAllocationThreshold) then
      SetPixelFormat(Result, pf1bit); // To reduce resource consumption of resize

    ColorLookup := nil;
    Ditherer := nil;
    DIBResult := nil;
    DIBSource := nil;
    Palette := 0;
    try

      // Dithering and color mapper only supports 24 bit bitmaps,
      // so we have to convert the source bitmap to the appropiate format.
      DIBSource := TDIBReader.Create(Bitmap, pf24bit);

      // Create a palette based on current options
      if (ColorReduction in [rmQuantize, rmQuantizeWindows]) then
        Palette := doCreateOptimizedPaletteFromSingleBitmap(DIBSource,
          1 SHL ReductionBits, 6, (ColorReduction = rmQuantizeWindows))
      else
        Palette := CreateDitherPalette(ColorReduction, CustomPalette);

                                                                                                                                                                   

      // Create a color mapper based on current options
      ColorLookup := CreateColorMapper(Palette, ColorReduction);

      // Nothing to do if palette doesn't contain any colors
      if (ColorLookup.Colors = 0) then
        exit;

      // Create a ditherer based on current options
      Ditherer := CreateDitherer(DitherMode, Bitmap.Width, ColorLookup);
      if (Ditherer = nil) then
        exit;

      // The processed bitmap is returned in pf8bit format
      DIBResult := TDIBWriter.Create(Result, pf8bit, Bitmap.Width, Bitmap.Height,
        Palette);

      // Process the image
      Row := 0;
      Ditherer.Reset;
      while (Row < Bitmap.Height) do
      begin
        SrcScanline := DIBSource.ScanLine[Row];
        DstScanline := DIBResult.ScanLine[Row];
        Src := Pointer(PByte(SrcScanLine) + Ditherer.Column*SizeOf(TRGBTriple));
        Dst := PByte(DstScanLine) + Ditherer.Column;

        while (Ditherer.Column < Ditherer.Width) and (Ditherer.Column >= 0) do
        begin
          BGR := Src^;
          // Dither and map a single pixel
          Dst^ := Ditherer.Dither(BGR.rgbtRed, BGR.rgbtGreen, BGR.rgbtBlue,
            BGR.rgbtRed, BGR.rgbtGreen, BGR.rgbtBlue);

          inc(Src, Ditherer.Direction);
          inc(Dst, Ditherer.Direction);
        end;

        Inc(Row);
        Ditherer.NextLine;
      end;
    finally
      if (ColorLookup <> nil) then
        ColorLookup.Free;
      if (Ditherer <> nil) then
        Ditherer.Free;
      if (DIBResult <> nil) then
        DIBResult.Free;
      if (DIBSource <> nil) then
        DIBSource.Free;
      // Must delete palette *after* TDIBWriter, since TDIBWriter uses palette.
      if (Palette <> 0) then
        DeleteObject(Palette);
    end;
  except
    Result.Free;
    raise;
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

                                         
(*
function ReduceGIFColors(GIF: TGIFImage; ColorReduction: TColorReduction;
  DitherMode: TDitherMode; ReductionBits: integer; CustomPalette: hPalette): boolean;

  procedure Dither(Ditherer: TDitherEngine; Frame: TGIFFrame; const ColorMap: TColorMap);
  var
    Row: Integer;
    Pixel: PChar;
    TransparentIndex: integer;
  begin
    if (Frame.Transparent) then
      TransparentIndex := Frame.GraphicControlExtension.TransparentColorIndex
    else
      TransparentIndex := -1;
    Row := 0;
    Ditherer.Reset;
    while (Row < Frame.Height) do
    begin
      Pixel := PChar(integer(Frame.ScanLine[Row]) + Ditherer.Column);

      while (Ditherer.Column < Frame.Width) and (Ditherer.Column >= 0) do
      begin
        if (integer(Pixel^) <> TransparentIndex) then
          // Dither and map a single pixel
          with ColorMap[ord(Pixel^)] do
            Pixel^ := Ditherer.Dither(Red, Green, Blue,
              Dummy.Red, Dummy.Green, Dummy.Blue)
        else
          // Skip transparent pixels.
          Ditherer.NextColumn;

        inc(Pixel, Ditherer.Direction);
      end;

      Inc(Row);
      Ditherer.NextLine;
    end;
  end;

var
  Palette: hPalette;
  ColorLookup: TColorLookup;
  Ditherer: TDitherEngine;
  ColorMap: PColorMap;
  NewTransparentIndex: integer;
  Dummy: TGIFColor;
  i: integer;
  Frame: TGIFFrame;
begin
  Result := False;
  if (ColorReduction = rmNone) or (GIF.Empty) then
    exit;

  ColorLookup := nil;
  Ditherer := nil;
  Palette := 0;
  try
    // Create a palette based on current options
    if (ColorReduction in [rmQuantize, rmQuantizeWindows]) then
      Palette := doCreateOptimizedPaletteFromGIF(GIF,
        1 SHL ReductionBits, (ColorReduction = rmQuantizeWindows),
        NewTransparentIndex)
    else
      Palette := CreateDitherPalette(ColorReduction, CustomPalette);

    // Create a color mapper based on current options
    ColorLookup := CreateColorMapper(Palette, ColorReduction);

    // Nothing to do if palette doesn't contain any colors
    if (ColorLookup.Colors = 0) then
      exit;

    // Create a ditherer based on current options
    Ditherer := CreateDitherer(DitherMode, GIF.Width, ColorLookup);
    if (Ditherer = nil) then
      exit;

    // Dither the image to the new palette.
    // TODO : Lots of things need to be considered here:
    // - Unless we have quantized, the new palette probably doesn't have any
    //   transparency color reserved.
    // - We need to remap the transparent pixels to new values during dither.
    // - Unless we have quantized, palette should be optimized after dither.
    // - Once palette has been optimized, we can make room for transparent
    //   color. Then we should start all over again. Aaargh!
    for i := 0 to GIF.Images.Count-1 do
    begin
      Frame := GIF.Images[i];
      if (Frame.Empty) then
        continue;

      Dither(Ditherer, Frame, Frame.ActiveColorMap.Data);
    end;

    // Now we have processed all frames. Delete old color maps and create a new
    // global one.
    for i := 0 to GIF.Images.Count-1 do
      if (GIF.Images[i].ColorMap <> nil) then
        GIF.Images[i].ColorMap.Clear;
    // Store optimized global color map
    GIF.GlobalColorMap.ImportPalette(Palette);
    GIF.GlobalColorMap.Optimized := True;

    Result := True;
  finally
    if (ColorLookup <> nil) then
      ColorLookup.Free;
    if (Ditherer <> nil) then
      Ditherer.Free;
    if (Palette <> 0) then
      DeleteObject(Palette);
  end;
end;
*)

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFColorMap
//
////////////////////////////////////////////////////////////////////////////////
const
  InitColorMapSize = 16;
  DeltaColorMapSize = 32;

//: Creates an instance of a TGIFColorMap object.
constructor TGIFColorMap.Create;
begin
  inherited Create;
  FCount := 0;
  FOptimized := False;
end;

//: Destroys an instance of a TGIFColorMap object.
destructor TGIFColorMap.Destroy;
begin
  Clear;
  Changed;
  inherited Destroy;
end;

//: Empties the color map.
procedure TGIFColorMap.Clear;
begin
  SetLength(FColorMap, 0);
  FCount := 0;
  FOptimized := False;
end;

//: Converts a Windows color value to a RGB value.
class function TGIFColorMap.Color2RGB(Color: TColor): TGIFColor;
begin
  Result.Blue := (Color shr 16) and $FF;
  Result.Green := (Color shr 8) and $FF;
  Result.Red  := Color and $FF;
end;

//: Converts a RGB value to a Windows color value.
class function TGIFColorMap.RGB2Color(Color: TGIFColor): TColor;
begin
  Result := (Color.Blue SHL 16) OR (Color.Green SHL 8) OR Color.Red;
end;

//: Saves the color map to a stream.
procedure TGIFColorMap.SaveToStream(Stream: TStream);
var
  Dummies: integer;
  Dummy: TGIFColor;
begin
  if (FCount = 0) then
    exit;
  Stream.WriteBuffer(FColorMap[0], Count*SizeOf(TGIFColor));
  Dummies := (1 SHL BitsPerPixel)-Count;
  Dummy.Red := 0;
  Dummy.Green := 0;
  Dummy.Blue := 0;
  while (Dummies > 0) do
  begin
    Stream.WriteBuffer(Dummy, SizeOf(TGIFColor));
    dec(Dummies);
  end;
end;

//: Loads the color map from a stream.
procedure TGIFColorMap.LoadFromStream(Stream: TStream; ACount: integer);
begin
  Clear;
  SetCapacity(ACount);
  ReadCheck(Stream, FColorMap[0], ACount*SizeOf(TGIFColor));
  FCount := ACount;
end;

//: Returns the position of a color in the color map.
function TGIFColorMap.IndexOf(Color: TColor): integer;
var
  RGB: TGIFColor;
begin
  RGB := Color2RGB(Color);
  if (Optimized) then
  begin
    // Optimized palette has most frequently occuring entries first
    Result := 0;
    while (Result < Count) do
      with (FColorMap[Result]) do
      begin
        if (RGB.Red = Red) and (RGB.Green = Green) and (RGB.Blue = Blue) then
          exit;
        Inc(Result);
      end;
    Result := -1;
  end else
  begin
    Result := Count-1;
    // Reverse search to (hopefully) check latest colors first
    while (Result >= 0) do
      with (FColorMap[Result]) do
      begin
        if (RGB.Red = Red) and (RGB.Green = Green) and (RGB.Blue = Blue) then
          exit;
        Dec(Result);
      end;
  end;
end;

procedure TGIFColorMap.SetCapacity(Size: integer);
begin
  if (Size > Length(FColorMap)) then
  begin
    if (Size <= InitColorMapSize) then
      Size := InitColorMapSize
    else
      Size := MulDiv(Size+DeltaColorMapSize-1, DeltaColorMapSize, DeltaColorMapSize);
    if (Size > GIFMaxColors) then
      Size := GIFMaxColors;
    SetLength(FColorMap, Size);
  end;
end;

//: Imports a Windows palette into the color map.
procedure TGIFColorMap.ImportPalette(Palette: HPalette);
type
  PalArray =  array[byte] of TPaletteEntry;
var
  Pal: PalArray;
  NewCount: integer;
  i: integer;
begin
  Clear;
  NewCount := GetPaletteEntries(Palette, 0, 256, pal);
  if (NewCount = 0) then
    exit;
  SetCapacity(NewCount);
  for i := 0 to NewCount-1 do
    with FColorMap[i], Pal[i] do
    begin
      Red := peRed;
      Green := peGreen;
      Blue := peBlue;
    end;
  FCount := NewCount;
  Changed;
end;

//: Imports a color map structure into the color map.
procedure TGIFColorMap.ImportColorMap(const Map: TColorMap; ACount: integer);
begin
  Clear;
  if (ACount = 0) then
    exit;
  SetCapacity(ACount);
  FCount := ACount;

  System.Move(Map[0], FColorMap[0], FCount * SizeOf(TGIFColor));

  Changed;
end;

//: Imports a Windows palette structure into the color map.
procedure TGIFColorMap.ImportColorTable(Pal: pointer; ACount: integer);
var
  i: integer;
begin
  Clear;
  if (ACount = 0) then
    exit;
  SetCapacity(ACount);
  for i := 0 to ACount-1 do
    with FColorMap[i], PRGBQuadArray(Pal)[i] do
    begin
      Red := rgbRed;
      Green := rgbGreen;
      Blue := rgbBlue;
    end;
  FCount := ACount;
  Changed;
end;

//: Imports the color table of a DIB into the color map.
procedure TGIFColorMap.ImportDIBColors(Handle: HDC);
var
  Pal: Pointer;
  NewCount: integer;
begin
  Clear;
  GetMem(Pal, SizeOf(TRGBQuad) * 256);
  try
    NewCount := GetDIBColorTable(Handle, 0, 256, Pal^);
    ImportColorTable(Pal, NewCount);
  finally
    FreeMem(Pal);
  end;
  Changed;
end;

//: Creates a Windows palette from the color map.
function TGIFColorMap.ExportPalette: HPalette;
var
  Pal: TMaxLogPalette;
  i: Integer;
begin
  if (Count = 0) then
  begin
    Result := 0;
    exit;
  end;
  Pal.palVersion := $300;
  Pal.palNumEntries := Count;
  for i := 0 to Count-1 do
    with FColorMap[i], Pal.palPalEntry[i] do
    begin
      peRed := Red;
      peGreen := Green;
      peBlue := Blue;
      peFlags := PC_NOCOLLAPSE;
    end;
    Result := CreatePalette(PLogPalette(@Pal));
                                                            
  //Result := CreatePalette(PLogPalette(@Pal)^);
end;

//: Adds a color to the color map.
function TGIFColorMap.Add(Color: TColor): integer;
begin
  if (Count >= GIFMaxColors) then
    // Color map full
    Error(sTooManyColors);

  Result := Count;
  if (Result >= Length(FColorMap)) then
    SetCapacity(Count+1);
  FColorMap[FCount] := Color2RGB(Color);
  inc(FCount);
  FOptimized := False;
  Changed;
end;

function TGIFColorMap.AddUnique(Color: TColor): integer;
begin
  // Look up color before add (same as IndexOf)
  Result := IndexOf(Color);
  if (Result >= 0) then
    // Color already in map
    exit;

  Result := Add(Color);
end;

//: Removes a color from the color map.
procedure TGIFColorMap.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= Count) then
    // Color index out of range
    Error(sBadColorIndex);
  dec(FCount);
  if (Index < Count) then
    System.Move(FColorMap[Index + 1], FColorMap[Index], (FCount - Index)* SizeOf(TGIFColor));
  FOptimized := False;
  Changed;
end;

function TGIFColorMap.GetColor(Index: integer): TColor;
begin
  if (Index < 0) or (Index >= Count) then
  begin
    // Color index out of range
    Warning(gsWarning, sBadColorIndex);
    // Raise an exception if the color map is empty
    if (Count = 0) then
      Error(sEmptyColorMap);
    // Default to color index 0
    Index := 0;
  end;
  Result := RGB2Color(FColorMap[Index]);
end;

procedure TGIFColorMap.SetColor(Index: integer; Value: TColor);
begin
  if (Index < 0) or (Index >= Count) then
    // Color index out of range
    Error(sBadColorIndex);
  FColorMap[Index] := Color2RGB(Value);
  Changed;
end;

function TGIFColorMap.DoOptimize: boolean;
var
  Usage: TColormapHistogram;

  procedure Swap(var A, B: TUsageCount);
  var
    T: TUsageCount;
  begin
    T := A;
    A := B;
    B := T;
  end;

  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi: Integer;
    Pivot: integer;
  begin
    repeat
      Lo := iLo;
      Hi := iHi;
      Pivot := Usage[(iLo + iHi) SHR 1].Count;
      repeat
        while (Usage[Lo].Count - Pivot > 0) do inc(Lo);
        while (Usage[Hi].Count - Pivot < 0) do dec(Hi);
        if (Lo <= Hi) then
        begin
          Swap(Usage[Lo], Usage[Hi]);
          inc(Lo);
          dec(Hi);
        end;
      until (Lo > Hi);
      if (iLo < Hi) then
        QuickSort(iLo, Hi);
      iLo := Lo;
    until (Lo >= iHi);
  end;

var
  TempMap: TColorMap;
  ReverseMap: TColormapReverse;
  i: integer;
  LastFound: boolean;
  NewCount: integer;
begin
  if (Count <= 1) then
  begin
    Result := False;
    exit;
  end;

  FOptimized := True;
  Result := True;

  BuildHistogram(Usage);

  (*
  **  Sort according to usage count
  *)
  QuickSort(0, Count-1);

  (*
  ** Test for table already sorted
  *)
  for i := 0 to Count-1 do
    if (Usage[i].Index <> i) then
      break;
  if (i = Count) then
    exit;

  (*
  ** Build old to new map
  *)
  for i := 0 to Count-1 do
    ReverseMap[Usage[i].Index] := i;


  MapImages(ReverseMap);

  (*
  **  Reorder colormap
  *)
  LastFound := False;
  NewCount := Count;
  TempMap := Copy(FColorMap);
  for i := 0 to Count-1 do
  begin
    FColorMap[ReverseMap[i]] := TempMap[i];
    // Find last used color index
    if (Usage[i].Count = 0) and not(LastFound) then
    begin
      LastFound := True;
      NewCount := i;
    end;
  end;

  FCount := NewCount;

  Changed;
end;

function TGIFColorMap.GetBitsPerPixel: integer;
begin
  Result := Colors2bpp(FCount);
end;

//: Copies one color map to another.
procedure TGIFColorMap.Assign(Source: TPersistent);
begin
  if (Source is TGIFColorMap) then
  begin
    ImportColorMap(TGIFColorMap(Source).Data, TGIFColorMap(Source).Count);
    FOptimized := TGIFColorMap(Source).Optimized;
    Changed;
  end else
    inherited Assign(Source);
end;


////////////////////////////////////////////////////////////////////////////////
//
//			TGIFGlobalColorMap
//
////////////////////////////////////////////////////////////////////////////////
type
  TGIFGlobalColorMap = class(TGIFColorMap)
  strict private
    FHeader	: TGIFHeader;
  strict protected
    procedure Warning(Severity: TGIFSeverity; const Msg: string); override;
    procedure BuildHistogram(var Histogram: TColormapHistogram); override;
    procedure MapImages(var Map: TColormapReverse); override;
  public
    constructor Create(HeaderItem: TGIFHeader);
    function Optimize: boolean; override;
    procedure Changed; override;
  end;

constructor TGIFGlobalColorMap.Create(HeaderItem: TGIFHeader);
begin
  Inherited Create;
  FHeader := HeaderItem;
end;

procedure TGIFGlobalColorMap.Warning(Severity: TGIFSeverity; const Msg: string);
begin
  FHeader.Image.Warning(Self, Severity, Msg);
end;

procedure TGIFGlobalColorMap.BuildHistogram(var Histogram: TColormapHistogram);
var
  Pixel, LastPixel: PByte;
  i: integer;
begin
  (*
  ** Init histogram
  *)
  for i := 0 to Count-1 do
  begin
    Histogram[i].Index := i;
    Histogram[i].Count := 0;
  end;

  for i := 0 to FHeader.Image.Images.Count-1 do
    if (FHeader.Image.Images[i].ActiveColorMap = Self) then
    begin
      Pixel := FHeader.Image.Images[i].Data;
      LastPixel := Pixel + FHeader.Image.Images[i].Width * FHeader.Image.Images[i].Height;

      (*
      ** Sum up usage count for each color
      *)
      while (Pixel < LastPixel) do
      begin
        inc(Histogram[ord(Pixel^)].Count);
        inc(Pixel);
      end;
    end;
end;

procedure TGIFGlobalColorMap.MapImages(var Map: TColormapReverse);
var
  Pixel, LastPixel: PByte;
  i: integer;
begin
  for i := 0 to FHeader.Image.Images.Count-1 do
    if (FHeader.Image.Images[i].ActiveColorMap = Self) then
    begin
      Pixel := FHeader.Image.Images[i].Data;
      LastPixel := Pixel + FHeader.Image.Images[i].Width * FHeader.Image.Images[i].Height;

      (*
      **  Reorder all pixel to new map
      *)
      while (Pixel < LastPixel) do
      begin
        Pixel^ := Map[ord(Pixel^)];
        inc(Pixel);
      end;

      (*
      **  Reorder transparent colors
      *)
      if (FHeader.Image.Images[i].Transparent) then
        FHeader.Image.Images[i].GraphicControlExtension.TransparentColorIndex :=
          Map[FHeader.Image.Images[i].GraphicControlExtension.TransparentColorIndex];
    end;
end;

function TGIFGlobalColorMap.Optimize: boolean;
begin
  { Optimize with first image, Remove unused colors if only one image }
  if (FHeader.Image.Images.Count > 0) then
    Result := DoOptimize
  else
    Result := False;
end;

procedure TGIFGlobalColorMap.Changed;
begin
  FHeader.Image.Palette := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFHeader
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFHeader.Create(GIFImage: TGIFImage);
begin
  inherited Create(GIFImage);
  FColorMap := TGIFGlobalColorMap.Create(Self);
  Clear;
end;

destructor TGIFHeader.Destroy;
begin
  FColorMap.Free;
  inherited Destroy;
end;

procedure TGIFHeader.Clear;
begin
  FColorMap.Clear;
  FLogicalScreenDescriptor.ScreenWidth := 0;
  FLogicalScreenDescriptor.ScreenHeight := 0;
  FLogicalScreenDescriptor.PackedFields := 0;
  FLogicalScreenDescriptor.BackgroundColorIndex := 0;
  FLogicalScreenDescriptor.AspectRatio := 0;
end;

procedure TGIFHeader.Assign(Source: TPersistent);
begin
  if (Source is TGIFHeader) then
  begin
    ColorMap.Assign(TGIFHeader(Source).ColorMap);
    FLogicalScreenDescriptor := TGIFHeader(Source).FLogicalScreenDescriptor;
  end else
  if (Source is TGIFColorMap) then
  begin
    Clear;
    ColorMap.Assign(TGIFColorMap(Source));
  end else
    inherited Assign(Source);
end;

type
  TGIFHeaderRec = record
    Signature: array[0..2] of AnsiChar; { contains 'GIF' }
    Version: TGIFVersionRec;   { '87a' or '89a' }
  end;

const
  { logical screen descriptor packed field masks }
  lsdGlobalColorTable	= $80;		{ set if global color table follows L.S.D. }
  lsdColorResolution	= $70;		{ Color resolution - 3 bits }
  lsdSort		= $08;		{ set if global color table is sorted - 1 bit }
  lsdColorTableSize	= $07;		{ size of global color table - 3 bits }
  					{ Actual size = 2^value+1    - value is 3 bits }
procedure TGIFHeader.Prepare;
var
  pack: BYTE;
begin
  Pack := $00;
  if (ColorMap.Count > 0) then
  begin
    Pack := lsdGlobalColorTable;
    if (ColorMap.Optimized) then
      Pack := Pack OR lsdSort;
  end;
  // Note: The SHL below was SHL 5 in the mozilla source, but that doesn't make sense
  Pack := Pack OR ((ColorResolution SHL 4) AND lsdColorResolution);
  Pack := Pack OR ((BitsPerPixel-1) AND lsdColorTableSize);
  FLogicalScreenDescriptor.PackedFields := Pack;
end;

procedure TGIFHeader.SaveToStream(Stream: TStream);
var
  GifHeader: TGIFHeaderRec;
  v: TGIFVersion;
begin
  v := Image.Version;
  if (v = gvUnknown) then
    Error(sGIFErrorSaveEmpty);

  GifHeader.Signature := AnsiString('GIF');  // Do not localize
  GifHeader.Version := GIFVersions[v];

  Prepare;
  Stream.Write(GifHeader, SizeOf(GifHeader));
  Stream.Write(FLogicalScreenDescriptor, SizeOf(FLogicalScreenDescriptor));
  if (FLogicalScreenDescriptor.PackedFields AND lsdGlobalColorTable = lsdGlobalColorTable) then
    ColorMap.SaveToStream(Stream);
end;

procedure TGIFHeader.LoadFromStream(Stream: TStream);
var
  GifHeader: TGIFHeaderRec;
  ColorCount: integer;
  Position: integer;
begin
  Position := Stream.Position;

  ReadCheck(Stream, GifHeader, SizeOf(GifHeader));
  if (uppercase(string(GifHeader.Signature)) <> 'GIF') then // Do not localize
  begin
    // Attempt recovery in case we are reading a GIF stored in a DFM by rxLib
    Stream.Position := Position;
    // Seek past size stored in stream
    Stream.Seek(SizeOf(longInt), soFromCurrent);
    // Attempt to read signature again
    ReadCheck(Stream, GifHeader, SizeOf(GifHeader));
    if (uppercase(string(GifHeader.Signature)) <> 'GIF') then  // Do not localize
      Error(sBadSignature);
  end;

  ReadCheck(Stream, FLogicalScreenDescriptor, SizeOf(FLogicalScreenDescriptor));

  if (FLogicalScreenDescriptor.PackedFields AND lsdGlobalColorTable = lsdGlobalColorTable) then
  begin
    ColorCount := 2 SHL (FLogicalScreenDescriptor.PackedFields AND lsdColorTableSize);
    if (ColorCount < 2) or (ColorCount > 256) then
      Error(sScreenBadColorSize);
    ColorMap.LoadFromStream(Stream, ColorCount)
  end else
    ColorMap.Clear;
end;

function TGIFHeader.GetVersion: TGIFVersion;
begin
  if (FColorMap.Optimized) or (AspectRatio <> 0) then
    Result := gv89a
  else
    Result := inherited GetVersion;
end;

function TGIFHeader.GetBackgroundColor: TColor;
begin
  Result := FColorMap[BackgroundColorIndex];
end;

procedure TGIFHeader.SetBackgroundColor(Color: TColor);
begin
  BackgroundColorIndex := FColorMap.AddUnique(Color);
end;

procedure TGIFHeader.SetBackgroundColorIndex(Index: BYTE);
begin
  if ((Index >= FColorMap.Count) and (FColorMap.Count > 0)) then
  begin
    Warning(gsWarning, sBadColorIndex);
    Index := 0;
  end;
  FLogicalScreenDescriptor.BackgroundColorIndex := Index;
  Image.Changed(Self);
end;

function TGIFHeader.GetBitsPerPixel: integer;
begin
  Result := FColorMap.BitsPerPixel;
end;

function TGIFHeader.GetColorResolution: integer;
begin
  Result := FColorMap.BitsPerPixel-1;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFLocalColorMap
//
////////////////////////////////////////////////////////////////////////////////
type
  TGIFLocalColorMap = class(TGIFColorMap)
  strict private
    FFrame: TGIFFrame;
  strict protected
    procedure Warning(Severity: TGIFSeverity; const Msg: string); override;
    procedure BuildHistogram(var Histogram: TColormapHistogram); override;
    procedure MapImages(var Map: TColormapReverse); override;
  public
    constructor Create(AFrame: TGIFFrame);
    function Optimize: boolean; override;
    procedure Changed; override;
    property Frame: TGIFFrame read FFrame;
  end;

constructor TGIFLocalColorMap.Create(AFrame: TGIFFrame);
begin
  Inherited Create;
  FFrame := AFrame;
end;

procedure TGIFLocalColorMap.Warning(Severity: TGIFSeverity; const Msg: string);
begin
  Frame.Image.Warning(Self, Severity, Msg);
end;

procedure TGIFLocalColorMap.BuildHistogram(var Histogram: TColormapHistogram);
var
  Pixel, LastPixel: PByte;
  i: integer;
begin
  Pixel := Frame.Data;
  LastPixel := Pixel + Frame.Width * Frame.Height;

  (*
  ** Init histogram
  *)
  for i := 0 to Count-1 do
  begin
    Histogram[i].Index := i;
    Histogram[i].Count := 0;
  end;

  (*
  ** Sum up usage count for each color
  *)
  while (Pixel < LastPixel) do
  begin
    inc(Histogram[ord(Pixel^)].Count);
    inc(Pixel);
  end;
end;

procedure TGIFLocalColorMap.MapImages(var Map: TColormapReverse);
var
  Pixel, LastPixel: PByte;
begin
  Pixel := Frame.Data;
  LastPixel := Pixel + Frame.Width * Frame.Height;

  (*
  **  Reorder all pixel to new map
  *)
  while (Pixel < LastPixel) do
  begin
    Pixel^ := Map[ord(Pixel^)];
    inc(Pixel);
  end;

  (*
  **  Reorder transparent colors
  *)
  if (Frame.Transparent) then
    Frame.GraphicControlExtension.TransparentColorIndex :=
      Map[Frame.GraphicControlExtension.TransparentColorIndex];
end;

function TGIFLocalColorMap.Optimize: boolean;
begin
  Result := DoOptimize;
end;

procedure TGIFLocalColorMap.Changed;
begin
  Frame.Palette := 0;
end;


////////////////////////////////////////////////////////////////////////////////
//		TGIFStream - Abstract GIF block stream
//
// Descendants of TGIFStream either reads or writes data in blocks
// of up to 255 bytes. These blocks are organized as a leading byte
// containing the number of bytes in the block (exclusing the count
// byte itself), followed by the data (up to 254 bytes of data).
////////////////////////////////////////////////////////////////////////////////
type
  TGIFDelegateWarning = procedure(Severity: TGIFSeverity; const Msg: string) of object;

  TGIFStream = class(TStream)
  strict private
    FOnWarning: TGIFWarning;
    FStream: TStream;
    FOnProgress: TNotifyEvent;

  strict protected
    constructor Create(Stream: TStream);

    // TStream implementation
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure Warning(Sender: TObject; Severity: TGIFSeverity; const Msg: string); virtual;
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;

  public
    property Stream: TStream read FStream;
    property OnWarning: TGIFWarning read FOnWarning write FOnWarning;
  end;

constructor TGIFStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
end;

procedure TGIFStream.Warning(Sender: TObject; Severity: TGIFSeverity; const Msg: string);
begin
  if Assigned(FOnWarning) then
    FOnWarning(Sender, Severity, Msg);
end;

procedure TGIFStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;

function TGIFStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise GIFException.Create(sInvalidStream);
end;

function TGIFStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise GIFException.Create(sInvalidStream);
end;

function TGIFStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise GIFException.Create(sInvalidStream);
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFReader - GIF block reader
////////////////////////////////////////////////////////////////////////////////
type
  TGIFReader = class(TGIFStream)
  strict private
    FBuffer: PByte;
    FData: PByte;
    FBufferSize: integer;
    FBufferCount: integer;
    FEOF: boolean;
    function GetEOF: boolean;
  strict protected
    procedure GetData;
  public
    constructor Create(Stream: TStream; ABufferSize: integer);
    destructor Destroy; override;
    procedure SkipData;

    // TStream implementation
    function Read(var Buffer; Count: Longint): Longint; override;

    property EOF: boolean read GetEOF;
  end;

constructor TGIFReader.Create(Stream: TStream; ABufferSize: integer);
begin
  inherited Create(Stream);
  FBufferSize := ABufferSize;
  FBufferCount := 0;
  GetMem(FBuffer, FBufferSize);
  FData := FBuffer;
  FEOF := False;
end;

destructor TGIFReader.Destroy;
begin
  if (FBuffer <> nil) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
  inherited Destroy;
end;

function TGIFReader.GetEOF: boolean;
begin
  Result := (FEOF) and (FBufferCount = 0);
end;


procedure TGIFReader.SkipData;
var
  BlockSize	: byte;
begin
  FBufferCount := 0;
  FData := FBuffer;

  // Skip past all data blocks
  if (not FEOF) then
    while (True) do
    begin
      // Check for end of file
      if (Stream.Read(BlockSize, 1) <> 1) then
        break;

      // Check for end-of-data block
      if (BlockSize = 0) then
        break;

      // Seek past data block
      Stream.Seek(BlockSize, soFromCurrent);
    end;

  FEOF := True;
end;


procedure TGIFReader.GetData;
var
  BlockSize: byte;
  RealBlockSize: byte;
  p: PByte;
begin
  ASSERT(FData-FBuffer <= FBufferSize, 'TGIFReader buffer overflow');
  ASSERT(FBuffer <= FData, 'TGIFReader buffer underflow');
  ASSERT(FBufferCount <= FBufferSize, 'TGIFReader buffer size overflow');
  ASSERT(FBufferCount >= 0, 'TGIFReader buffer size underflow');

  // Move old data to start of b
  if (FBufferCount > 0) then
    Move(FData^, FBuffer^, FBufferCount);
  FData := FBuffer;

  p := FData + FBufferCount;

  // Fill buffer from stream
  while (not FEOF) and (FBufferCount < FBufferSize) do
  begin
    // Check for end of file
    if (Stream.Read(BlockSize, 1) <> 1) then
    begin
      FEOF := True;
      break;
    end;

    // Check for end-of-data block
    if (BlockSize = 0) then
    begin
      FEOF := True;
      break;
    end;

    // Check for premature end of file
    if (Stream.Size - Stream.Position < BlockSize) then
    begin
      Warning(Self, gsWarning, sOutOfData);
      // Not enough data left - Just read as much as we can get
      BlockSize := Stream.Size - Stream.Position;
    end;

    // Check for block too large for buffer
    if (BlockSize + FBufferCount > FBufferSize) then
    begin
      // Unread block size byte
      Stream.Seek(-1, soFromCurrent);
      break;
    end;

    // Read block into buffer
    RealBlockSize := Stream.Read(p^, BlockSize);

    // Check for premature end of file
    if (BlockSize <> RealBlockSize) then
    begin
      BlockSize := RealBlockSize;
      Warning(Self, gsWarning, sOutOfData);
    end;

    inc(p, BlockSize);
    inc(FBufferCount, BlockSize);
  end;
end;

function TGIFReader.Read(var Buffer; Count: Longint): Longint;
var
  n: integer;
  Dst: PByte;
begin
  Dst := @Buffer;
  Result := 0;

  while (Count > 0) do
  begin
    // Get data from buffer
    if (FBufferCount > 0) then
    begin
      if (FBufferCount > Count) then
        n := Count
      else
        n := FBufferCount;
      Move(FData^, Dst^, n);

      inc(FData, n);
      dec(FBufferCount, n);
      inc(Dst, n);
      dec(Count, n);
      inc(Result, n);
    end else
    begin
      // Refill buffer when it becomes empty
      if (FEOF) then
        break
      else
        GetData;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFWriter - GIF block writer
////////////////////////////////////////////////////////////////////////////////
type
  TGIFWriter = class(TGIFStream)
  strict private
    FBuffer: array[BYTE] of Byte;
    FBufferCount: integer;
    FNeedsFlush: boolean;
  strict protected
    procedure FlushBuffer;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function Write(const Buffer; Count: Longint): Longint; override;
    function WriteByte(Value: BYTE): Longint;
  end;

constructor TGIFWriter.Create(Stream: TStream);
begin
  inherited Create(Stream);
  FBufferCount := 1; // Reserve first byte of buffer for length
  FNeedsFlush := False;
end;

destructor TGIFWriter.Destroy;
begin
  FlushBuffer;
  inherited Destroy;
end;

procedure TGIFWriter.FlushBuffer;
begin
  if (FNeedsFlush) then
  begin
    FBuffer[0] := Byte(FBufferCount-1); // Block size excluding the count
    Stream.WriteBuffer(FBuffer, FBufferCount);
    FBufferCount := 1; // Reserve first byte of buffer for length
  end;
end;

function TGIFWriter.Write(const Buffer; Count: Longint): Longint;
var
  n: integer;
  Src: PByte;
begin
  FNeedsFlush := True;
  Result := Count;
  Src := @Buffer;
  while (Count > 0) do
  begin
    // Move data to the internal buffer in 255 byte chunks
    while (FBufferCount < SizeOf(FBuffer)) and (Count > 0) do
    begin
      n := SizeOf(FBuffer) - FBufferCount;
      if (n > Count) then
        n := Count;
      Move(Src^, FBuffer[FBufferCount], n);
      inc(Src, n);
      inc(FBufferCount, n);
      dec(Count, n);
    end;

    // Flush the buffer when it is full
    if (FBufferCount >= SizeOf(FBuffer)) then
      FlushBuffer;
  end;
end;

function TGIFWriter.WriteByte(Value: BYTE): Longint;
begin
  Result := Write(Value, 1);
end;


////////////////////////////////////////////////////////////////////////////////
//		TGIFBitReader - GIF LZW stream bit reader
////////////////////////////////////////////////////////////////////////////////
type
  TGIFBitReader = class(TObject)
  strict private
    FOnWarning: TGIFWarning;
    FStream: TGIFReader;
    FBuffer: array[byte] of byte;
    FStartBit,			// Index of bit buffer start
    FLastBit,			// Index of last bit in buffer
    FLastByte: integer;		// Index of last byte in buffer
    FEOF: boolean;
  strict protected
    procedure Warning(Sender: TObject; Severity: TGIFSeverity; const Msg: string);
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure SkipData;

    function ReadBits(Bits: integer): integer;

    property EOF: boolean read FEOF;
    property OnWarning: TGIFWarning read FOnWarning write FOnWarning;
  end;

constructor TGIFBitReader.Create(AStream: TStream);
begin
  inherited Create;
  FStream := TGIFReader.Create(AStream, GIFFileBufferSize);
  FStream.OnWarning := Warning;
  FStartBit := 0;
  FLastBit := 0;
  FLastByte := 2;
  FEOF := False;
end;

destructor TGIFBitReader.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TGIFBitReader.Warning(Sender: TObject; Severity: TGIFSeverity; const Msg: string);
begin
  if Assigned(FOnWarning) then
    FOnWarning(Sender, Severity, Msg);
end;

function TGIFBitReader.ReadBits(Bits: integer): integer;
const
  Masks: array[0..15] of integer =
    ($0000, $0001, $0003, $0007,
     $000f, $001f, $003f, $007f,
     $00ff, $01ff, $03ff, $07ff,
     $0fff, $1fff, $3fff, $7fff);
var
  StartIndex, EndIndex: integer;
  Count: integer;
  ret: integer;
begin
  // If bit buffer is empty, we need to read more data from stream
  if (FLastBit-FStartBit <= Bits) then
  begin
    // Check for premature end of data
    if (FEOF) then
    begin
      Warning(Self, gsWarning, sDecodeTooFewBits);
      Result := -1;
      exit;
    end;

    // Move data left overs to start of buffer
    FBuffer[0] := FBuffer[FLastByte-2];
    FBuffer[1] := FBuffer[FLastByte-1];

    // Read LZW data from stream
    Count := FStream.Read(FBuffer[2], SizeOf(FBuffer)-2);

    // Calculate byte index of last byte in buffer
    FLastByte := 2 + Count;
    // Calculate bit index of first bit of current code
    FStartBit := (FStartBit - FLastBit) + 16; // 16 = 2 * 8 bits
    // Calculate bit index of last bit in buffer
    FLastBit := FLastByte * 8;

    // Check for premature end of data
    if (FLastBit-FStartBit <= Bits) then
    begin
      Warning(Self, gsWarning, sDecodeTooFewBits);
      Result := -1;
      exit;
    end;
    FEOF := FStream.EOF;
  end;

  // Calculate byte index of first and last part of current code
  EndIndex := (FStartBit + Bits) DIV 8;
  StartIndex := FStartBit DIV 8;
  ASSERT(StartIndex <= high(FBuffer), 'LZW bit buffer index too large');

  // Extract bits from buffer
  if (StartIndex = EndIndex) then
    // All bits are in a single byte
    Ret := FBuffer[StartIndex]
  else
    if (StartIndex + 1 = EndIndex) then
      // Bits span two bytes
      Ret := FBuffer[StartIndex] OR (FBuffer[StartIndex+1] SHL 8)
    else
      // Bits span three bytes
      Ret := FBuffer[StartIndex] OR (FBuffer[StartIndex+1] SHL 8) OR (FBuffer[StartIndex+2] SHL 16);

  Result := (Ret SHR (FStartBit AND $0007)) AND Masks[Bits];

  Inc(FStartBit, Bits);
end;

procedure TGIFBitReader.SkipData;
begin
  FStream.SkipData;
  FEOF := True;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			LZW Decoder
//
////////////////////////////////////////////////////////////////////////////////
                                                                                    
                                                                                     
const
  GIFCodeBits = 12;                     // Max number of bits per GIF token code
  GIFCodeMax = (1 SHL GIFCodeBits)-1;   // Max GIF token code
                                        // 12 bits = 4095
                                        // Note: Mozilla source uses 4097,
                                        // but GIF spects uses 4095.
  StackSize = (2 SHL GIFCodeBits);	// Size of decompression Stack
  TableSize = (1 SHL GIFCodeBits);	// Size of decompression table

type
  TDictionary = array[0..TableSize-1] of integer;
  TStack = array[0..StackSize-1] of integer;

procedure TGIFFrame.Decompress(Stream: TStream);
var
  Prefix: TDictionary;
  Suffix: TDictionary;
  FirstCode, OldCode: integer;

  Dest: PByte;
  v, xpos, ypos, pass: integer;

  Stack: TStack;
  Source: Pinteger;
  BitsPerCode: integer;		// number of CodeTableBits/code
  InitialBitsPerCode: BYTE;

  MaxCode: integer;		// maximum code, given BitsPerCode
  MaxCodeSize: integer;
  ClearCode: integer;		// Special code to signal "Clear table"
  EOFCode: integer;		// Special code to signal EOF
  step: integer;
  i: integer;

  DoClear: boolean;		// Set to true to pretend that a clear code is read
  BitStream: TGIFBitReader;
  PixelCount: integer;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}

  // Inline this without error checking for better performance
  procedure Push(Value: integer);
  begin
    if (UIntPtr(Source) > UIntPtr(@Stack[high(Stack)])) then
      Error(sInvalidData);
    Source^ := Value;
    Inc(Source);
  end;

  function Lookup(Code, ClearCode: integer): integer;
  begin
    Result := Code;
    // Traverse dictionary
    while (Result >= ClearCode) do
    begin
      if (UIntPtr(Source) > UIntPtr(@Stack[high(Stack)])) then
        Error(sInvalidData);
      // Push decoded data
      Source^ := Suffix[Result];
      Inc(Source);

      // Check for recursion
      if (Result = Prefix[Result]) then
        Error(sDecodeCircular);
      Result := Prefix[Result];
      ASSERT(Result < TableSize, 'LZW Code too large');
    end;
  end;

  function NextLZW: integer;
  var
    Code, InCode: integer;
    i: integer;
  begin
    if (DoClear) then
    begin
      DoClear := False;
      Code := ClearCode;
    end else
      Code := BitStream.ReadBits(BitsPerCode);

    while (Code >= 0) do
    begin
      // Reset dictionary to initial state if requested
      if (Code = ClearCode) then
      begin
        ASSERT(ClearCode < TableSize, 'LZW ClearCode too large');

        FillChar(Prefix, SizeOf(Prefix), 0);
        for i := 0 to ClearCode-1 do
          Suffix[i] := i;
        FillChar(Suffix[ClearCode], TableSize-ClearCode-1, 0);

        BitsPerCode := InitialBitsPerCode+1;
        MaxCodeSize := 2 * ClearCode;
        MaxCode := ClearCode + 2;
        Source := PInteger(@Stack);

        // Ignore multiple clear codes
        repeat
          FirstCode := BitStream.ReadBits(BitsPerCode);
        until (FirstCode <> ClearCode);

        OldCode := FirstCode;
        Code := FirstCode;
        break;
      end else
      // Check for explicit end-of-data code
      if (Code = EOFCode) then
      begin
        Code := -1;
        break;
      end else
      begin
        InCode := Code;

        if (Code >= MaxCode) then
        begin
          Push(FirstCode);
          Code := OldCode;
        end;

        ASSERT(Code < TableSize, 'LZW Code too large');

        // Traverse dictionary
        Code := Lookup(Code, ClearCode);

        FirstCode := Suffix[Code];
        Push(FirstCode);

        Code := MaxCode;
        if (Code <= GIFCodeMax) then
        begin
          Prefix[Code] := OldCode;
          Suffix[Code] := FirstCode;
          Inc(MaxCode);
          if ((MaxCode >= MaxCodeSize) and (MaxCodeSize <= GIFCodeMax)) then
          begin
            MaxCodeSize := MaxCodeSize * 2;
            Inc(BitsPerCode);
          end;
        end;

        OldCode := InCode;

        if (UIntPtr(Source) > UIntPtr(@Stack)) then
        begin
          Dec(Source);
          Code := Source^;
          break;
        end
      end;

    end;
    Result := Code;

  end;

  function readLZW: integer;
  begin
    // Pop data off the stack if there are any...
    if (UIntPtr(Source) > UIntPtr(@Stack)) then
    begin
      Dec(Source);
      Result := Source^;
    end else
      // ...otherwise get some more data from the LZW stream.
      Result := NextLZW;
  end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

begin
  (*
  ** Read initial code size in bits from stream.
  *)
  if (Stream.Read(InitialBitsPerCode, 1) <> 1) then
  begin
    Warning(gsWarning, sOutOfData);
    exit;
  end;

  (*
  ** Validate Code Size.
  *)
  if (InitialBitsPerCode > GIFCodeBits) then
  begin
    // Data stream is corrupt - impossible to recover
    Warning(gsWarning, sInvalidData);
    exit;
  end;

  // Now that we know the LZW stream is at least partially valid, we allocate
  // the image memory.
  NewImage;
  // Clear image data in case decompress exits prematurely
  ClearImage;

  (*
  **  Initialize the Compression routines
  *)
  BitsPerCode := InitialBitsPerCode + 1;
  ClearCode := 1 SHL InitialBitsPerCode;
  EOFCode := ClearCode + 1;
  MaxCodeSize := 2 * ClearCode;
  MaxCode := ClearCode + 2;

  DoClear := True;

  Source := PInteger(@Stack);

  BitStream := TGIFBitReader.Create(Stream);
  try
    BitStream.OnWarning := Image.Warning;
    PixelCount := 10000;
    if (Interlaced) then
    begin
      ypos := 0;
      pass := 0;
      step := 8;

      for i := 0 to Height-1 do
      begin
        Dest := FData + Width * ypos;
        for xpos := 0 to width-1 do
        begin
          v := readLZW;
          if (v < 0) then
            exit;
          Dest^ := Byte(v);
          Inc(Dest);
        end;
        Inc(ypos, step);
        if (ypos >= height) then
          repeat
            if (pass > 0) then
              step := step DIV 2;
            Inc(pass);
            ypos := step DIV 2;
          until (ypos < height);

        dec(PixelCount, Width);
        // Update progress every 10,000 pixels...
        // Note: Due to buffering, the stream position might not have changed
        // since the last OnProgress event, but we generate an OnProgress event
        // nevertheless so the application doesn't apprear to have frozen and
        // the user has a chance to abort.
        if (PixelCount <= 0) then
        begin
          Image.Progress(Self, psRunning, MulDiv(Stream.Position, 100, Stream.Size),
            False, Rect(0,0,0,0), sProgressLoading);
          PixelCount := 10000;
        end;
      end;
    end else
    begin
      Dest := FData;
      for ypos := 0 to (height * width)-1 do
      begin
        v := readLZW;
        if (v < 0) then
          break;
        Dest^ := Byte(v);
        Inc(Dest);

        dec(PixelCount);
        // Update progress every 10,000 pixels...
        // See note above.
        if (PixelCount < 0) then
        begin
          Image.Progress(Self, psRunning, MulDiv(Stream.Position, 100, Stream.Size),
            False, Rect(0,0,0,0), sProgressLoading);
          PixelCount := 10000;
        end;
      end;
    end;
    // Ignore rest of data stream for this frame
    BitStream.SkipData;
  finally
    BitStream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			LZW Encoder stuff
//
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
//			LZW Encoder THashTable
////////////////////////////////////////////////////////////////////////////////
const
  HashKeyBits		= 13;			// Max number of bits per Hash Key

  HashSize		= 8009;			// Size of hash table
  						// Must be prime
                                                // Must be > than HashMaxCode
                                                // Must be < than HashMaxKey

  HashKeyMax		= (1 SHL HashKeyBits)-1;// Max hash key value
  						// 13 bits = 8191

  HashKeyMask		= HashKeyMax;		// $1FFF
  GIFCodeMask		= GIFCodeMax;		// $0FFF

  HashEmpty		= $000FFFFF;		// 20 bits

type
  // A Hash Key is 20 bits wide.
  // - The lower 8 bits are the postfix character (the new pixel).
  // - The upper 12 bits are the prefix code (the GIF token).
  // A THashKey must be able to represent the integer values -1..(2^20)-1
  THashKey = longInt;	// 32 bits
  THashCode = SmallInt;	// 16 bits

  THashArray = array[0..HashSize-1] of THashKey;
  PHashArray = ^THashArray;

  THashTable = class
  strict private
    HashTable: PHashArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Insert(Key: THashKey; Code: THashCode);
    function Lookup(Key: THashKey): THashCode;
  end;

function HashKey(Key: THashKey): THashCode;
begin
  Result := ((Key shr (GIFCodeBits-8)) xor Key) mod HashSize;
end;

function NextHashKey(HKey: THashCode): THashCode;
var
  disp: THashCode;
begin
  (*
  ** secondary hash (after G. Knott)
  *)
  disp := HashSize - HKey;
  if (HKey = 0) then
    disp := 1;
//  disp := 13;		// disp should be prime relative to HashSize, but
			// it doesn't seem to matter here...
  dec(HKey, disp);
  if (HKey < 0) then
    inc(HKey, HashSize);
  Result := HKey;
end;


constructor THashTable.Create;
begin
  ASSERT(longInt($FFFFFFFF) = -1, 'TGIFImage implementation assumes $FFFFFFFF = -1');

  inherited Create;
  GetMem(HashTable, SizeOf(THashArray));
  Clear;
end;

destructor THashTable.Destroy;
begin
  FreeMem(HashTable);
  inherited Destroy;
end;

// Clear hash table and fill with empty slots (doh!)
procedure THashTable.Clear;
begin
  FillChar(HashTable^, SizeOf(THashArray), $FF);
end;

// Insert new key/value pair into hash table
procedure THashTable.Insert(Key: THashKey; Code: THashCode);
var
  HKey: THashCode;
begin
  // Create hash key from prefix string
  HKey := HashKey(Key);

  // Scan for empty slot
  // while (HashTable[HKey] SHR GIFCodeBits <> HashEmpty) do { Unoptimized }
  while (HashTable[HKey] AND (HashEmpty SHL GIFCodeBits) <> (HashEmpty shl GIFCodeBits)) do { Optimized }
    HKey := NextHashKey(HKey);
  // Fill slot with key/value pair
  HashTable[HKey] := (Key shl GIFCodeBits) or (Code and GIFCodeMask);
end;

// Search for key in hash table.
// Returns value if found or -1 if not
function THashTable.Lookup(Key: THashKey): THashCode;
var
  HKey: THashCode;
  HTKey: THashKey;
begin
  // Create hash key from prefix string
  HKey := HashKey(Key);

  // Scan table for key
  // HTKey := HashTable[HKey] SHR GIFCodeBits; { Unoptimized }
  Key := Key shl GIFCodeBits; { Optimized }
  HTKey := HashTable[HKey] and (HashEmpty shl GIFCodeBits); { Optimized }
  // while (HTKey <> HashEmpty) do { Unoptimized }
  while (HTKey <> HashEmpty shl GIFCodeBits) do { Optimized }
  begin
    if (Key = HTKey) then
    begin
      // Extract and return value
      Result := HashTable[HKey] and GIFCodeMask;
      exit;
    end;
    // Try next slot
    HKey := NextHashKey(HKey);
    // HTKey := HashTable[HKey] SHR GIFCodeBits; { Unoptimized }
    HTKey := HashTable[HKey] and (HashEmpty shl GIFCodeBits); { Optimized }
  end;
  // Found empty slot - key doesn't exist
  Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////
//		TGIFEncoder - Abstract encoder
////////////////////////////////////////////////////////////////////////////////
type
  TGIFEncoder = class(TObject)
  strict protected
    FOnWarning: TGIFWarning;
    MaxColor: integer;
    BitsPerPixel: BYTE;		// Bits per pixel of image
    Stream: TStream;	// Output stream
    Width,		// Width of image in pixels
    Height: integer;	// height of image in pixels
    Interlace: boolean;	// Interlace flag (True = interlaced image)
    Data: PByte;	// Pointer to pixel data
    GIFStream: TGIFWriter;	// Output buffer

    OutputBucket: longInt;	// Output bit bucket
    OutputBits: integer;	// Current # of bits in bucket

    ClearFlag: Boolean;	// True if dictionary has just been cleared
    BitsPerCode,		// Current # of bits per code
    InitialBitsPerCode: integer;	// Initial # of bits per code after
  					// dictionary has been cleared
    MaxCode: THashCode;	// maximum code, given BitsPerCode
    ClearCode: THashCode;	// Special output code to signal "Clear table"
    EOFCode: THashCode;	// Special output code to signal EOF
    BaseCode: THashCode;	// ...

    Pixel: PByte;	// Pointer to current pixel

    cX,		// Current X counter (Width - X)
    Y: integer;	// Current Y
    Pass: integer;	// Interlace pass

    function MaxCodesFromBits(Bits: integer): THashCode;
    procedure Output(Value: integer); virtual;
    procedure Clear; virtual;
    function BumpPixel: boolean;
    procedure DoCompress; virtual; abstract;
  public
    procedure Compress(AStream: TStream; ABitsPerPixel: integer;
      AWidth, AHeight: integer; AInterlace: boolean; AData: PByte; AMaxColor: integer);
    property Warning: TGIFWarning read FOnWarning write FOnWarning;
  end;

// Calculate the maximum number of codes that a given number of bits can represent
// MaxCodes := (1^bits)-1
function TGIFEncoder.MaxCodesFromBits(Bits: integer): THashCode;
begin
  Result := (THashCode(1) shl Bits) - 1;
end;

// Stuff bits (variable sized codes) into a buffer and output them
// a byte at a time
                                                                
procedure TGIFEncoder.Output(Value: integer);
const
  BitBucketMask: array[0..16] of longInt =
    ($0000,
     $0001, $0003, $0007, $000F,
     $001F, $003F, $007F, $00FF,
     $01FF, $03FF, $07FF, $0FFF,
     $1FFF, $3FFF, $7FFF, $FFFF);
begin
  if (OutputBits > 0) then
    OutputBucket :=
      (OutputBucket AND BitBucketMask[OutputBits]) OR (longInt(Value) SHL OutputBits)
  else
    OutputBucket := Value;

  inc(OutputBits, BitsPerCode);

  while (OutputBits >= 8) do
  begin
    GIFStream.WriteByte(OutputBucket AND $FF);
    OutputBucket := OutputBucket SHR 8;
    dec(OutputBits, 8);
  end;

  if (Value = EOFCode) then
  begin
    // At EOF, write the rest of the buffer.
    while (OutputBits > 0) do
    begin
      GIFStream.WriteByte(OutputBucket AND $FF);
      OutputBucket := OutputBucket SHR 8;
      dec(OutputBits, 8);
    end;
  end;
end;

procedure TGIFEncoder.Clear;
begin
  // just_cleared = 1;
  ClearFlag := TRUE;
  Output(ClearCode);
end;

// Bump (X,Y) and data pointer to point to the next pixel
function TGIFEncoder.BumpPixel: boolean;
begin
  // Bump the current X position
  dec(cX);

  // If we are at the end of a scan line, set cX back to the beginning
  // If we are interlaced, bump Y to the appropriate spot, otherwise,
  // just increment it.
  if (cX <= 0) then
  begin

    if not(Interlace) then
    begin
                            
      Result := False;
      exit;
    end;

    cX := Width;
    case (Pass) of
      0:
        begin
          inc(Y, 8);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 4;
          end;
        end;
      1:
        begin
          inc(Y, 8);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 2;
          end;
        end;
      2:
        begin
          inc(Y, 4);
          if (Y >= Height) then
          begin
            inc(Pass);
            Y := 1;
          end;
        end;
      3:
        inc(Y, 2);
    end;

    if (Y >= height) then
    begin
                            
      Result := False;
      exit;
    end;
    Pixel := Data + (Y * Width);
  end;
  Result := True;
end;


procedure TGIFEncoder.Compress(AStream: TStream; ABitsPerPixel: integer;
  AWidth, AHeight: integer; AInterlace: boolean; AData: PByte; AMaxColor: integer);
const
  EndBlockByte= $00;			// End of block marker
begin
  MaxColor := AMaxColor;
  Stream := AStream;
  BitsPerPixel := ABitsPerPixel;
  Width := AWidth;
  Height := AHeight;
  Interlace := AInterlace;
  Data := AData;

  if (BitsPerPixel <= 1) then
    BitsPerPixel := 2;

  InitialBitsPerCode := BitsPerPixel + 1;
  Stream.Write(BitsPerPixel, 1);

  // out_bits_init = init_bits;
  BitsPerCode := InitialBitsPerCode;
  MaxCode := MaxCodesFromBits(BitsPerCode);

  ClearCode := (1 SHL (InitialBitsPerCode - 1));
  EOFCode := ClearCode + 1;
  BaseCode := EOFCode + 1;

  // Clear bit bucket
  OutputBucket := 0;
  OutputBits  := 0;

  // Reset pixel counter
  if (Interlace) then
    cX := Width
  else
    cX := Width*Height;
  // Reset row counter
  Y := 0;
  Pass := 0;

  GIFStream := TGIFWriter.Create(AStream);
  try
    GIFStream.OnWarning := Warning;
    if (Data <> nil) and (Height > 0) and (Width > 0) then
    begin
      // Call compress implementation
      DoCompress;

      // Output the final code.
      Output(EOFCode);
    end else
      // Output the final code (and nothing else).
      Output(EOFCode);
  finally
    GIFStream.Free;
  end;

  WriteByte(Stream, EndBlockByte);
end;

////////////////////////////////////////////////////////////////////////////////
//		TLZWEncoder - LZW encoder
////////////////////////////////////////////////////////////////////////////////
const
  TableMaxMaxCode	= (1 SHL GIFCodeBits);	//
  TableMaxFill		= TableMaxMaxCode-1;	// Clear table when it fills to
  						// this point.
  						// Note: Must be <= GIFCodeMax
type
  TLZWEncoder = class(TGIFEncoder)
  strict private
    Prefix: THashCode;	// Current run color
    FreeEntry: THashCode;	// next unused code in table
    HashTable: THashTable;
  strict protected
    procedure Output(Value: integer); override;
    procedure Clear; override;
    procedure DoCompress; override;
  end;


procedure TLZWEncoder.Output(Value: integer);
begin
  inherited Output(Value);

  // If the next entry is going to be too big for the code size,
  // then increase it, if possible.
  if (FreeEntry > MaxCode) or (ClearFlag) then
  begin
    if (ClearFlag) then
    begin
      BitsPerCode := InitialBitsPerCode;
      MaxCode := MaxCodesFromBits(BitsPerCode);
      ClearFlag := False;
    end else
    begin
      inc(BitsPerCode);
      if (BitsPerCode = GIFCodeBits) then
        MaxCode := TableMaxMaxCode
      else
        MaxCode := MaxCodesFromBits(BitsPerCode);
    end;
  end;
end;

procedure TLZWEncoder.Clear;
begin
  inherited Clear;
  HashTable.Clear;
  FreeEntry := ClearCode + 2;
end;


procedure TLZWEncoder.DoCompress;
var
  Color: Byte;
  NewKey: THashKey;
  NewCode: THashCode;

begin
  HashTable := THashTable.Create;
  try
    // clear hash table and sync decoder
    Clear;

    Pixel := Data;
    Prefix := THashCode(Pixel^);
    inc(Pixel);
    if (Prefix >= MaxColor) then
      Error(sInvalidColor);
    while (BumpPixel) do
    begin
      // Fetch the next pixel
      Color := Pixel^;
      inc(Pixel);
      if (ord(Color) >= MaxColor) then
        Error(sInvalidColor);

      // Append Postfix to Prefix and lookup in table...
      NewKey := (THashKey(Prefix) shl 8) OR ord(Color);
      NewCode := HashTable.Lookup(NewKey);
      if (NewCode >= 0) then
      begin
        // ...if found, get next pixel
        Prefix := NewCode;
        continue;
      end;

      // ...if not found, output and start over
      Output(Prefix);
      Prefix := THashCode(Color);

      if (FreeEntry < TableMaxFill) then
      begin
        HashTable.Insert(NewKey, FreeEntry);
        inc(FreeEntry);
      end else
        Clear;
    end;
    Output(Prefix);
  finally
    HashTable.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFFrame
//
////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////
//		TGIFFrame.Compress
/////////////////////////////////////////////////////////////////////////
procedure TGIFFrame.Compress(Stream: TStream);
var
  Encoder: TGIFEncoder;
  BitsPerPixel: BYTE;
  MaxColors: integer;
begin
  if (ColorMap.Count > 0) then
  begin
    MaxColors := ColorMap.Count;
    BitsPerPixel := ColorMap.BitsPerPixel
  end else
  begin
    BitsPerPixel := Image.BitsPerPixel;
    MaxColors := 1 shl BitsPerPixel;
  end;

  // Compress image data
  Encoder := TLZWEncoder.Create;
  try
    Encoder.Warning := Image.Warning;
    Encoder.Compress(Stream, BitsPerPixel, Width, Height, Interlaced, FData, MaxColors);
  finally
    Encoder.Free;
  end;
end;

constructor TGIFExtensionList.Create(AFrame: TGIFFrame);
begin
  inherited Create;
  FFrame := AFrame;
end;

function TGIFExtensionList.GetImage: TGIFImage;
begin
  Result := Frame.Image;
end;

function TGIFExtensionList.GetExtension(Index: Integer): TGIFExtension;
begin
  Result := TGIFExtension(Items[Index]);
end;

procedure TGIFExtensionList.SetExtension(Index: Integer; Extension: TGIFExtension);
begin
  Items[Index] := Extension;
end;

procedure TGIFExtensionList.LoadFromStream(Stream: TStream);
var
  b: BYTE;
  Extension: TGIFExtension;
  ExtensionClass: TGIFExtensionClass;
begin
  // Peek ahead to determine block type
  if (Stream.Read(b, 1) <> 1) then
    exit;
  while not(b in [bsTrailer, bsImageDescriptor]) do
  begin
    if (b = bsExtensionIntroducer) then
    begin
      ExtensionClass := TGIFExtension.FindExtension(Stream);
      if (ExtensionClass = nil) then
        Error(sUnknownExtension);
      Stream.Seek(-1, soFromCurrent);
      Extension := ExtensionClass.Create(Frame);
      try
        Extension.LoadFromStream(Stream);
        Add(Extension);
      except
        Extension.Free;
        raise;
      end;
    end else
    begin
      Warning(gsWarning, sBadExtensionLabel);
      break;
    end;
    if (Stream.Read(b, 1) <> 1) then
      exit;
  end;
  Stream.Seek(-1, soFromCurrent);
end;

procedure TGIFExtensionList.Assign(Source: TPersistent);
var
  MemoryStream: TMemoryStream;
begin
  if (Source is TGIFExtensionList) then
  begin
    // Copy all extensions from source extension list.
    // Note: No validation is performed on the copied extensions (e.g. color
    // index values against color maps).
    MemoryStream := TMemoryStream.Create;
    try
      TGIFExtensionList(Source).SaveToStream(MemoryStream);
      MemoryStream.Seek(0, soFromBeginning);
      LoadFromStream(MemoryStream);
    finally
      MemoryStream.Free;
    end;
  end else
    inherited Assign(Source);
end;

const
  { image descriptor bit masks }
  idLocalColorTable	= $80;    { set if a local color table follows }
  idInterlaced		= $40;    { set if image is interlaced }
  idSort		= $20;    { set if color table is sorted }
  idReserved		= $0C;    { reserved - must be set to $00 }
  idColorTableSize	= $07;    { size of color table as above }

constructor TGIFFrame.Create(GIFImage: TGIFImage);
begin
  inherited Create(GIFImage);
  FExtensions := TGIFExtensionList.Create(Self);
  FColorMap := TGIFLocalColorMap.Create(Self);
  FImageDescriptor.Separator := bsImageDescriptor;
  FImageDescriptor.Left := 0;
  FImageDescriptor.Top := 0;
  FImageDescriptor.Width := 0;
  FImageDescriptor.Height := 0;
  FImageDescriptor.PackedFields := 0;
  FBitmap := nil;
  FMask := 0;
  FNeedMask := True;
  FData := nil;
  FDataSize := 0;
  FGCE := nil;
  Image.Images.Add(Self);
  // Remember to synchronize with TGIFFrame.Clear
end;

destructor TGIFFrame.Destroy;
begin
  if (Image <> nil) then
    Image.Images.Remove(Self);
  Clear;
  FExtensions.Free;
  FColorMap.Free;
  if (FLocalPalette <> 0) then
    DeleteObject(FLocalPalette);
  inherited Destroy;
end;

procedure TGIFFrame.Clear;
begin
  FExtensions.Clear;
  FColorMap.Clear;
  FreeImage;
  DoSetBounds(0,0,0,0);
  FGCE := nil;
  Dormant;
  // Remember to synchronize with TGIFFrame.Create
end;

function TGIFFrame.GetEmpty: Boolean;
begin
  Result := (FData = nil) or (FDataSize = 0) or (Height = 0) or (Width = 0);
end;

function TGIFFrame.GetPalette: HPALETTE;
begin
  if (FBitmap <> nil) and (FBitmap.Palette <> 0) then
    // Use bitmaps own palette if possible
    Result := FBitmap.Palette
  else if (FLocalPalette <> 0) then
    // Or a previously exported local palette
    Result := FLocalPalette
  else if (Image.ShouldDither) then
  begin
    // or create a new dither palette
    FLocalPalette := WebPalette;
    Result := FLocalPalette;
  end
  else if (ColorMap.Count > 0) then
  begin
    // or create a new if first time
    FLocalPalette := ColorMap.ExportPalette;
    Result := FLocalPalette;
  end else
    // Use global palette if everything else fails
    Result := Image.Palette;
end;

procedure TGIFFrame.SetPalette(Value: HPalette);
var
  NeedNewBitmap: boolean;
begin
  if (Value <> FLocalPalette) then
  begin
    // Zap old palette
    if (FLocalPalette <> 0) then
      DeleteObject(FLocalPalette);
    // Zap bitmap unless new palette is same as bitmaps own
    NeedNewBitmap := (FBitmap <> nil) and (Value <> FBitmap.Palette);

    // Use new palette
    FLocalPalette := Value;
    if (NeedNewBitmap) then
    begin
      // Need to create new bitmap and repaint
      FreeBitmap;
      Image.PaletteModified := True;
      Image.Changed(Self);
    end;
  end;
end;

procedure TGIFFrame.NeedImage;
begin
  if (FData = nil) then
  begin
    NewImage;
    ClearImage;
  end;
  if (FDataSize = 0) then
    Error(sEmptyImage);
end;

procedure TGIFFrame.ClearImage;
var
  ClearValue: byte;
begin
  if (FData <> nil) and (FDataSize <> 0) then
  begin
    if (Transparent) then
      // Clear to transparent color
      ClearValue := GraphicControlExtension.TransparentColorIndex
    else
      // Clear to first color
      ClearValue := 0;

    FillChar(FData^, FDataSize, ClearValue);
  end;
end;

procedure TGIFFrame.NewImage;
var
  NewSize: integer;
begin
  FreeImage;
  NewSize := Height * Width;
  if (NewSize <> 0) then
    GetMem(FData, NewSize)
  else
    FData := nil;
  FDataSize := NewSize;
end;

procedure TGIFFrame.FreeImage;
begin
  if (FData <> nil) then
    FreeMem(FData);
  FDataSize := 0;
  FData := nil;
end;

function TGIFFrame.GetHasBitmap: boolean;
begin
  Result := (FBitmap <> nil);
end;

procedure TGIFFrame.SetHasBitmap(Value: boolean);
begin
  if (Value <> (FBitmap <> nil)) then
  begin
    if (Value) then
      Bitmap // Referencing Bitmap will automatically create it
    else
      FreeBitmap;
  end;
end;

procedure TGIFFrame.NewBitmap;
begin
  FreeBitmap;
  FBitmap := TBitmap.Create;
                                                                                                    
                                                        
end;

procedure TGIFFrame.Dormant;
begin
  FreeBitmap;
  FreeMask;
  FreePalette;
end;

procedure TGIFFrame.FreeBitmap;
begin
  if (FBitmap <> nil) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TGIFFrame.FreeMask;
begin
  if (FMask <> 0) then
  begin
    DeleteObject(FMask);
    FMask := 0;
  end;
  FNeedMask := True;
end;

procedure TGifFrame.FreePalette;
begin
  if (FLocalPalette <> 0) then
  begin
    DeleteObject(FLocalPalette);
    FLocalPalette := 0;
  end;
end;

function TGIFFrame.HasMask: boolean;
begin
  if (FNeedMask) and (Transparent) then
  begin
    // Zap old bitmap
    FreeBitmap;
    // Create new bitmap and mask
    GetBitmap;
  end;
  Result := (FMask <> 0);
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TGIFFrame.DoGetDitherBitmap: TBitmap;
var
  ColorLookup: TColorLookup;
  Ditherer: TDitherEngine;
  DIBResult: TDIB;
  Src: PByte;
  Dst: PByte;

  Row: integer;
  Color: TGIFColor;
  ColMap: TColorMap;
  Index: byte;
  TransparentIndex: byte;
  IsTransparent: boolean;
  WasTransparent: boolean;
  MappedTransparentIndex: Byte;

  MaskBits: PByte;
  MaskDest: PByte;
  MaskRow: PByte;
  MaskRowWidth, MaskRowBitWidth: integer;
  Bit, RightBit: BYTE;

begin
  Result := TBitmap.Create;
  try

    if (Width*Height > BitmapAllocationThreshold) then
      SetPixelFormat(Result, pf1bit); // To reduce resource consumption of resize

    if (Empty) then
    begin
      // Set bitmap width and height
      Result.Width := Width;
      Result.Height := Height;

      // Build and copy palette to bitmap
      Result.Palette := CopyPalette(Palette);

                                                                    
      exit;
    end;

    ColorLookup := nil;
    Ditherer := nil;
    DIBResult := nil;
    try // Protect above resources
      ColorLookup := TNetscapeColorLookup.Create(Palette);
      Ditherer := TFloydSteinbergDitherer.Create(Width, ColorLookup);
      // Get DIB buffer for scanline operations
      // It is assumed that the source palette is the 216 color Netscape palette
      DIBResult := TDIBWriter.Create(Result, pf8bit, Width, Height, Palette);

      // Determine if this image is transparent
      ColMap := ActiveColorMap.Data;
      IsTransparent := FNeedMask and Transparent;
      WasTransparent := False;
      FNeedMask := False;
      TransparentIndex := 0;
      MappedTransparentIndex := 0;
      if (FMask = 0) and (IsTransparent) then
      begin
        IsTransparent := True;
        TransparentIndex := GraphicControlExtension.TransparentColorIndex;
        Color := ColMap[ord(TransparentIndex)];
        MappedTransparentIndex := Byte(Color.Blue DIV 51 +
          MulDiv(6, Color.Green, 51) + MulDiv(36, Color.Red, 51)+1);
      end;

      // Allocate bit buffer for transparency mask
      MaskDest := nil;
      Bit := $00;
      if (IsTransparent) then
      begin
        MaskRowWidth := ((Width+15) SHR 4) SHL 1;
        MaskRowBitWidth := (Width+7) SHR 3;
        RightBit := $01 SHL ((8 - (Width AND $0007)) AND $0007);
        GetMem(MaskBits, MaskRowWidth * Height);
        FillChar(MaskBits^, MaskRowWidth * Height, 0);
      end else
      begin
        MaskBits := nil;
        MaskRowWidth := 0;
        MaskRowBitWidth := 0;
        RightBit := $00;
      end;

      try
        // Process the image
        Row := 0;
        Ditherer.Reset;
        MaskRow := MaskBits;
        Src := FData;
        while (Row < Height) do
        begin
          if ((Row AND $1F) = 0) then
            Image.Progress(Self, psRunning, MulDiv(Row, 100, Height),
              False, Rect(0,0,0,0), sProgressRendering);

          Dst := DIBResult.ScanLine[Row];
          if (IsTransparent) then
          begin
            // Preset all pixels to transparent
            FillChar(Dst^, Width, ord(MappedTransparentIndex));
            if (Ditherer.Direction = 1) then
            begin
              MaskDest := MaskRow;
              Bit := $80;
            end else
            begin
              MaskDest := MaskRow + MaskRowBitWidth-1;
              Bit := RightBit;
            end;
          end;
          inc(Dst, Ditherer.Column);

          while (Ditherer.Column < Ditherer.Width) and (Ditherer.Column >= 0) do
          begin
            Index := ord(Src^);
            Color := ColMap[ord(Index)];

            if (IsTransparent) and (Index = TransparentIndex) then
            begin
              MaskDest^ := Byte(byte(MaskDest^) OR Bit);
              WasTransparent := True;
              Ditherer.NextColumn;
            end else
            begin
              // Dither and map a single pixel
              Dst^ := Ditherer.Dither(Color.Red, Color.Green, Color.Blue,
                Color.Red, Color.Green, Color.Blue);
            end;

            if (IsTransparent) then
            begin
              if (Ditherer.Direction = 1) then
              begin
                Bit := Bit SHR 1;
                if (Bit = $00) then
                begin
                  Bit := $80;
                  inc(MaskDest, 1);
                end;
              end else
              begin
                Bit := Bit SHL 1;
                if (Bit = $00) then
                begin
                  Bit := $01;
                  dec(MaskDest, 1);
                end;
              end;
            end;

            inc(Src, Ditherer.Direction);
            inc(Dst, Ditherer.Direction);
          end;

          if (IsTransparent) then
            Inc(MaskRow, MaskRowWidth);
          Inc(Row);
          inc(Src, Width-Ditherer.Direction);
          Ditherer.NextLine;
        end;

        // Transparent paint needs a mask bitmap
        if (IsTransparent) and (WasTransparent) then
          FMask := CreateBitmap(Width, Height, 1, 1, MaskBits);
      finally
        if (MaskBits <> nil) then
          FreeMem(MaskBits);
      end;
    finally
      if (ColorLookup <> nil) then
        ColorLookup.Free;
      if (Ditherer <> nil) then
        Ditherer.Free;
      if (DIBResult <> nil) then
        DIBResult.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

function TGIFFrame.DoGetBitmap: TBitmap;
var
  ScanLineRow: Integer;
  DIBResult: TDIB;
  DestScanLine, Src: PByte;
  TransparentIndex: byte;
  IsTransparent: boolean;
  WasTransparent: boolean;

  MaskBits: PByte;
  MaskDest: PByte;
  MaskRow: PByte;
  MaskRowWidth: integer;
  Col: integer;
  MaskByte: byte;
  Bit: byte;
begin
  Result := TBitmap.Create;
  try

    if (Width*Height > BitmapAllocationThreshold) then
      SetPixelFormat(Result, pf1bit); // To reduce resource consumption of resize

    if (Empty) then
    begin
      // Set bitmap width and height
      Result.Width := Width;
      Result.Height := Height;

      // Build and copy palette to bitmap
      Result.Palette := CopyPalette(Palette);

                                                                    
      exit;
    end;

    // Get DIB buffer for scanline operations
    DIBResult := TDIBWriter.Create(Result, pf8bit, Width, Height, Palette);
    try

      // Determine if this image is transparent
      IsTransparent := FNeedMask and Transparent;
      WasTransparent := False;
      FNeedMask := False;
      TransparentIndex := 0;
      if (FMask = 0) and (IsTransparent) then
        TransparentIndex := GraphicControlExtension.TransparentColorIndex;
      // Allocate bit buffer for transparency mask
      if (IsTransparent) then
      begin
        MaskRowWidth := ((Width+15) SHR 4) SHL 1;
        GetMem(MaskBits, MaskRowWidth * Height);
        FillChar(MaskBits^, MaskRowWidth * Height, 0);
        IsTransparent := (MaskBits <> nil);
      end else
      begin
        MaskBits := nil;
        MaskRowWidth := 0;
      end;

      try
        ScanLineRow := 0;
        Src := FData;
        MaskRow := MaskBits;
        while (ScanLineRow < Height) do
        begin
          DestScanline := DIBResult.ScanLine[ScanLineRow];

          if ((ScanLineRow AND $1F) = 0) then
            Image.Progress(Self, psRunning, MulDiv(ScanLineRow, 100, Height),
              False, Rect(0,0,0,0), sProgressRendering);

          Move(Src^, DestScanline^, Width);
          Inc(ScanLineRow);

          if (IsTransparent) then
          begin
            Bit := $80;
            MaskDest := MaskRow;
            MaskByte := 0;
            for Col := 0 to Width-1 do
            begin
              // Set a bit in the mask if the pixel is transparent
              if (Src^ = Byte(TransparentIndex)) then
                MaskByte := MaskByte OR Bit;

              Bit := Bit SHR 1;
              if (Bit = $00) then
              begin
                // Store a mask byte for each 8 pixels
                Bit := $80;
                WasTransparent := WasTransparent or (MaskByte <> 0);
                MaskDest^ := Byte(MaskByte);
                inc(MaskDest);
                MaskByte := 0;
              end;
              Inc(Src);
            end;
            // Save the last mask byte in case the width isn't divisable by 8
            if (MaskByte <> 0) then
            begin
              WasTransparent := True;
              MaskDest^ := Byte(MaskByte);
            end;
            Inc(MaskRow, MaskRowWidth);
          end else
            Inc(Src, Width);
        end;

        // Transparent paint needs a mask bitmap
        if (IsTransparent) and (WasTransparent) then
          FMask := CreateBitmap(Width, Height, 1, 1, MaskBits);
      finally
        if (MaskBits <> nil) then
          FreeMem(MaskBits);
      end;
    finally
      // Free DIB buffer used for scanline operations
      DIBResult.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TGIFFrame.GetBitmap: TBitmap;
var
  Progress: integer;
begin
  Result := FBitmap;
  if (Result <> nil) or (Empty) then
    Exit;

  try
    Progress := 0;
    Image.Progress(Self, psStarting, Progress, False, Rect(0,0,0,0), sProgressRendering);
    try
      if (Image.ShouldDither) then
        // Create dithered bitmap
        FBitmap := DoGetDitherBitmap
      else
        // Create "regular" bitmap
        FBitmap := DoGetBitmap;

      Progress := 100;
      Result := FBitmap;

                                                                                                        
                                                            

    finally
      Image.Progress(Self, psEnding, Progress, Image.PaletteModified, Rect(0,0,0,0),
        sProgressRendering);
      // Make sure new palette gets realized, in case OnProgress event didn't.
      if Image.PaletteModified then
        Image.Changed(Self);
    end;
  except
    on EAbort do
      Result := nil;   // OnProgress can raise EAbort to cancel image load
  end;
end;

procedure TGIFFrame.SetBitmap(Value: TBitmap);
begin
  FreeBitmap;
  if (Value <> nil) then
    Assign(Value);
end;

function TGIFFrame.GetActiveColorMap: TGIFColorMap;
begin
  if (ColorMap.Count > 0) or (Image.GlobalColorMap.Count = 0) then
    Result := ColorMap
  else
    Result := Image.GlobalColorMap;
end;

function TGIFFrame.GetInterlaced: boolean;
begin
  Result := (FImageDescriptor.PackedFields and idInterlaced) <> 0;
end;

procedure TGIFFrame.SetInterlaced(Value: boolean);
begin
  if (Value) then
    FImageDescriptor.PackedFields := FImageDescriptor.PackedFields or idInterlaced
  else
    FImageDescriptor.PackedFields := FImageDescriptor.PackedFields and not(idInterlaced);
end;

function TGIFFrame.GetVersion: TGIFVersion;
var
  v: TGIFVersion;
  i: integer;
begin
  if (ColorMap.Optimized) then
    Result := gv89a
  else
    Result := inherited GetVersion;
  i := 0;
  while (Result < high(TGIFVersion)) and (i < FExtensions.Count) do
  begin
    v := FExtensions[i].Version;
    if (v > Result) then
      Result := v;
  end;
end;

function TGIFFrame.GetColorResolution: integer;
begin
  Result := ColorMap.BitsPerPixel-1;
end;

function TGIFFrame.GetBitsPerPixel: integer;
begin
  Result := ColorMap.BitsPerPixel;
end;

function TGIFFrame.GetBoundsRect: TRect;
begin
  Result := Rect(FImageDescriptor.Left,
    FImageDescriptor.Top,
    FImageDescriptor.Left+FImageDescriptor.Width,
    FImageDescriptor.Top+FImageDescriptor.Height);
end;

procedure TGIFFrame.SetHeight(const Value: WORD);
begin
  DoSetBounds(Left, Top, Width, Value);
end;

procedure TGIFFrame.SetLeft(const Value: WORD);
begin
  DoSetBounds(Value, Top, Width, Height);
end;

procedure TGIFFrame.SetTop(const Value: WORD);
begin
  DoSetBounds(Left, Value, Width, Height);
end;

procedure TGIFFrame.SetWidth(const Value: WORD);
begin
  DoSetBounds(Left, Top, Value, Height);
end;

procedure TGIFFrame.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
var
  TooLarge: boolean;
  Zap: boolean;

  procedure CheckSize(Value: integer);
  begin
    // Check for size out of range
    if (Value < low(Word)) or (Value > High(Word))  then
      // This is a range check error condition - Impossible to recover
      Error(sBadSize);
  end;

begin
  CheckSize(ALeft+AWidth);
  CheckSize(ATop+AHeight);

  Zap := (FImageDescriptor.Width <> AWidth) or (FImageDescriptor.Height <> AHeight);
  FImageDescriptor.Left := ALeft;
  FImageDescriptor.Top := ATop;
  FImageDescriptor.Width := AWidth;
  FImageDescriptor.Height := AHeight;

  // Delete existing image and bitmaps if size has changed
  if (Zap) then
  begin
    FreeBitmap;
    FreeMask;
    // ...and allocate a new image
    NewImage;
    ClearImage;
  end;

  TooLarge := False;
  // Set width & height if added image is larger than logical screen size
  if (Image.Width < ALeft+AWidth) then
  begin
    if (Image.Width > 0) then
      TooLarge := True;
    Image.Width := ALeft+AWidth;
  end;
  if (Image.Height < ATop+AHeight) then
  begin
    if (Image.Height > 0) then
      TooLarge := True;
    Image.Height := ATop+AHeight;
  end;

  if (TooLarge) then
    Warning(gsWarning, sScreenSizeExceeded);
end;

procedure TGIFFrame.SetBoundsRect(const Value: TRect);
begin
  DoSetBounds(Value.Left, Value.Top, Value.Right-Value.Left+1, Value.Bottom-Value.Top+1);
end;

function TGIFFrame.GetClientRect: TRect;
begin
  Result := Rect(0, 0, FImageDescriptor.Width, FImageDescriptor.Height);
end;

function TGIFFrame.GetPixel(x, y: integer): BYTE;
begin
  if (x < 0) or (x > Width-1) then
    Error(sBadPixelCoordinates);
  Result := PByte(PByte(Scanline[y]) + x)^;
end;

procedure TGIFFrame.SetPixel(x, y: integer; const Value: BYTE);
var
  p: PBYTE;
begin
  if (x < 0) or (x > Width-1) then
    Error(sBadPixelCoordinates);
  p := PByte(PByte(Scanline[y]) + x);
  if (p^ <> Value) then
  begin
    // Validate value against color map.
    if (Value >= ActiveColorMap.Count) then
      Error(sInvalidColor);
    // Invalidate frame bitmap.
    FreeBitmap;
    FreeMask;
    p^ := Value;
  end;
end;

function TGIFFrame.GetScanline(y: integer): pointer;
begin
  if (y < 0) or (y > Height-1) then
    Error(sBadPixelCoordinates);
  NeedImage;
  Result := PByte(FData) + y * Width;
end;

function TGIFFrame.GetTransparent: boolean;
begin
  Result := (GCE <> nil) and (GCE.Transparent);
end;

procedure TGIFFrame.Prepare;
begin
  FImageDescriptor.PackedFields := FImageDescriptor.PackedFields and
    not(idLocalColorTable or idSort or idReserved or idColorTableSize);

  if (ColorMap.Count > 0) then
  begin
    FImageDescriptor.PackedFields := FImageDescriptor.PackedFields or
      idLocalColorTable;
    if (ColorMap.Optimized) then
      FImageDescriptor.PackedFields := FImageDescriptor.PackedFields or idSort;
    FImageDescriptor.PackedFields := (FImageDescriptor.PackedFields and
      (not idColorTableSize)) or (ColorResolution and idColorTableSize);
  end;
end;

procedure TGIFFrame.SaveToStream(Stream: TStream);
begin
  FExtensions.SaveToStream(Stream);
  if (Empty) then
    exit;
  Prepare;
  Stream.Write(FImageDescriptor, SizeOf(TImageDescriptor));
  ColorMap.SaveToStream(Stream);
  Compress(Stream);
end;

procedure TGIFFrame.LoadFromStream(Stream: TStream);
var
  ColorCount: integer;
  b: BYTE;
  BadSize: boolean;
begin
  Clear;
  FExtensions.LoadFromStream(Stream);
  // Check for extension without image
  if (Stream.Read(b, 1) <> 1) then
    exit;
  Stream.Seek(-1, soFromCurrent);
  if (b = bsTrailer) or (b = 0) then
    exit;

  ReadCheck(Stream, FImageDescriptor, SizeOf(TImageDescriptor));
  if (FImageDescriptor.Separator <> bsImageDescriptor) then
    Error(sInvalidData);

  // Work around broken Animagic GIFs with invalid frame sizes.
  // Note: This isn't guaranteed to calculate the correct size of the frame,
  // but tests have shown it to work most of the time.
  BadSize := False;
  if (FImageDescriptor.Left+FImageDescriptor.Width > high(Word)) then
  begin
    BadSize := True;
    FImageDescriptor.Width := (FImageDescriptor.Left+FImageDescriptor.Width) and $FFFF;
  end;
  if (FImageDescriptor.Top+FImageDescriptor.Height > high(Word)) then
  begin
    BadSize := True;
    FImageDescriptor.Height := (FImageDescriptor.Top+FImageDescriptor.Height) and $FFFF;
  end;
  if (BadSize) then
    Warning(gsWarning, sBadSize);

  // From Mozilla source:
  // Work around more broken GIF files that have zero image
  // width or height
  if (FImageDescriptor.Height = 0) or (FImageDescriptor.Width = 0) then
  begin
    FImageDescriptor.Height := Image.Height;
    FImageDescriptor.Width := Image.Width;
    Warning(gsWarning, sScreenSizeExceeded);
  end;

  if (FImageDescriptor.PackedFields and idLocalColorTable = idLocalColorTable) then
  begin
    ColorCount := 2 shl (FImageDescriptor.PackedFields and idColorTableSize);
    if (ColorCount < 2) or (ColorCount > 256) then
      Error(sImageBadColorSize);
    ColorMap.LoadFromStream(Stream, ColorCount);
  end;

  // Decompress LZW data stream.
  // We defer allocation of image data until we are sure that the LZW data
  // is at least partially valid.
  Decompress(Stream);

  // Note: We have deferred this check of the size in order not to modify size
  // of parent Image until we are sure that the GIF actually contains valid
  // data.
  if (not Empty) then
  begin
    // Set bounds to current values to validate
    DoSetBounds(FImageDescriptor.Left, FImageDescriptor.Top,
      FImageDescriptor.Width, FImageDescriptor.Height);

    // On-load rendering
    if (GIFImageRenderOnLoad) then
      // Touch bitmap to force frame to be rendered
      Bitmap;
  end;
end;

procedure TGIFFrame.AssignTo(Dest: TPersistent);
begin
  if (Dest is TBitmap) then
    Dest.Assign(Bitmap)
  else
    inherited AssignTo(Dest);
end;

procedure TGIFFrame.Assign(Source: TPersistent);
var
  i: integer;
  PixelFormat: TPixelFormat;
  DIBSource: TDIB;
  ABitmap: TBitmap;
  APalette: HPalette;
  DIB: TDIBSection;

  procedure Import8Bit;
  var
    Dest: PByte;
    y: integer;
    MaxColor: Byte;
    BadBitmap: boolean;
  begin
    Dest := FData;
    // Copy colormap
    if (FBitmap.HandleType = bmDIB) then
      FColorMap.ImportDIBColors(FBitmap.Canvas.Handle)
    else
      FColorMap.ImportPalette(FBitmap.Palette);
    // Copy pixels
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      Move(DIBSource.Scanline[y]^, Dest^, Width);
      inc(Dest, Width);
    end;
    // Validate all pixels against color map
    if (FColorMap.Count < 256) then
    begin
      MaxColor := FColorMap.Count;
      Dest := FData;
      y := FDataSize;
      BadBitmap := False;
      while (y > 0) do
      begin
        // Zero pixels with out-of-bounds values
        if (Dest^ >= MaxColor) then
        begin
          Dest^ := 0;
          BadBitmap := True;
        end;
        inc(Dest);
        dec(y);
      end;
      if (BadBitmap) then
        Warning(gsWarning, sInvalidBitmap);
    end;
  end;

  procedure Import4Bit;
  var
    Dest: PByte;
    x, y: integer;
    Scanline: PByte;
  begin
    Dest := FData;
    // Copy colormap
    FColorMap.ImportPalette(FBitmap.Palette);
    // Copy pixels
    for y := 0 to Height-1 do
    begin
      if ((y and $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      ScanLine := DIBSource.Scanline[y];
      for x := 0 to Width-1 do
      begin
        if (x and $01 = 0) then
          Dest^ := ord(ScanLine^) shr 4
        else
        begin
          Dest^ := ord(ScanLine^) and $0F;
          inc(ScanLine);
        end;
        inc(Dest);
      end;
    end;
  end;

  procedure Import1Bit;
  var
    Dest: PByte;
    x, y: integer;
    Scanline: PByte;
    Bit: integer;
    Byte: integer;
  begin
    Dest := FData;
    // Copy colormap
    FColorMap.ImportPalette(FBitmap.Palette);
    // Copy pixels
    for y := 0 to Height-1 do
    begin
      if ((y and $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      ScanLine := DIBSource.Scanline[y];
      x := Width;
      Bit := 0;
      Byte := 0; // To avoid compiler warning
      while (x > 0) do
      begin
        if (Bit = 0) then
        begin
          Bit := 8;
          Byte := ord(ScanLine^);
          inc(Scanline);
        end;
        Dest^ := (Byte and $80) shr 7;
        Byte := Byte shl 1;
        inc(Dest);
        dec(Bit);
        dec(x);
      end;
    end;
  end;

  procedure Import24Bit;
  type
    TCacheEntry = record
      Color: TColor;
      Index: integer;
    end;
  const
    // Size of palette cache. Must be 2^n.
    // The cache holds the palette index of the last "CacheSize" colors
    // processed. Hopefully the cache can speed things up a bit... Initial
    // testing shows that this is indeed the case at least for non-dithered
    // bitmaps.
    // All the same, a small hash table would probably be much better.
    CacheSize = 8;
  var
    Dest: PByte;
    i: integer;
    Cache: array[0..CacheSize-1] of TCacheEntry;
    LastEntry: integer;
    Scanline: PRGBTriple;
    Pixel: TColor;
    RGBTriple: TRGBTriple absolute Pixel;
    x, y: integer;
    ColorMap: TColorMap;
    t: byte;
  label
    NextPixel;
  begin
    Dest := FData;
    for i := 0 to CacheSize-1 do
      Cache[i].Index := -1;
    LastEntry := 0;

    // Copy all pixels and build colormap
    for y := 0 to Height-1 do
    begin
      if ((y AND $1F) = 0) then
        Image.Progress(Self, psRunning, MulDiv(y, 100, Height), False, Rect(0,0,0,0), sProgressConverting);
      ScanLine := DIBSource.Scanline[y];
      for x := 0 to Width-1 do
      begin
        Pixel := 0;
        RGBTriple := Scanline^;
        // Scan cache for color from most recently processed color to last
        // recently processed. This is done because TColorMap.AddUnique is very slow.
        i := LastEntry;
        repeat
          if (Cache[i].Index = -1) then
            break;
          if (Cache[i].Color = Pixel) then
          begin
            Dest^ := Cache[i].Index;
            LastEntry := i;
            goto NextPixel;
          end;
          if (i = 0) then
            i := CacheSize-1
          else
            dec(i);
        until (i = LastEntry);
        // Color not found in cache, do it the slow way instead
        Dest^ := FColorMap.AddUnique(Pixel);
        // Add color and index to cache
        LastEntry := (LastEntry + 1) AND (CacheSize-1);
        Cache[LastEntry].Color := Pixel;
        Cache[LastEntry].Index := ord(Dest^);

        NextPixel:
        Inc(Dest);
        Inc(Scanline);
      end;
    end;
    // Convert colors in colormap from BGR to RGB
                                                                                        
    ColorMap := FColorMap.Data;
    i := FColorMap.Count-1;
    while (i >= 0) do
    begin
      t := ColorMap[i].Red;
      ColorMap[i].Red := ColorMap[i].Blue;
      ColorMap[i].Blue := t;
      inc(PByte(ColorMap), SizeOf(TGIFColor));
      dec(i);
    end;
  end;

  procedure ImportViaDraw(ABitmap: TBitmap; Graphic: TGraphic);
  begin
    ABitmap.Height := Graphic.Height;
    ABitmap.Width := Graphic.Width;

    // Note: Disable the call to SafeSetPixelFormat below to import
    // in max number of colors with the risk of having to use
    // TCanvas.Pixels to do it (very slow).

    // Make things a little easier for TGIFFrame.Assign by converting
    // pfDevice to a more import friendly format
    SetPixelFormat(ABitmap, pf24bit);
    ABitmap.Canvas.Draw(0, 0, Graphic);
  end;

  function MakeTransparent(TransparentIndex: integer): TGIFGraphicControlExtension;
  begin
    Result := TGIFGraphicControlExtension.Create(self);
    Result.Transparent := True;
    Result.TransparentColorIndex := TransparentIndex;
  end;

  procedure AddMask(Mask: TBitmap);
  var
    DIBReader: TDIBReader;
    TransparentIndex: integer;
    i, j: integer;
    GIFPixel, MaskPixel: PByte;
    WasTransparent: boolean;
  begin
    // Optimize colormap to make room for transparent color
    ColorMap.Optimize;
    // Can't make transparent if no color or colormap full
    if (ColorMap.Count = 0) or (ColorMap.Count = 256) then
      exit;

    // Add the transparent color to the color map
    TransparentIndex := ColorMap.Add(GIFDefaultTransparentColor);
    WasTransparent := False;

    DIBReader := TDIBReader.Create(Mask, pf8bit);
    try
      for i := 0 to Height-1 do
      begin
        MaskPixel := DIBReader.Scanline[i];
        GIFPixel := Scanline[i];
        for j := 0 to Width-1 do
        begin
          // Change all unmasked pixels to transparent
          if (MaskPixel^ <> 0) then
          begin
            GIFPixel^ := TransparentIndex;
            WasTransparent := True;
          end;
          inc(MaskPixel);
          inc(GIFPixel);
        end;
      end;
    finally
      DIBReader.Free;
    end;

    // Add a Graphic Control Extension if any part of the mask was transparent
    if (WasTransparent) then
      MakeTransparent(TransparentIndex)
    else
      // Otherwise removed the transparency color since it wasn't used
      ColorMap.Delete(TransparentIndex);
  end;

  procedure AddMaskOnly(hMask: hBitmap);
  var
    Mask: TBitmap;
    MaskCopy: TBitmap;
  begin
    if (hMask = 0) then
      exit;

    // Encapsulate the mask
    Mask := TBitmap.Create;
    MaskCopy := TBitmap.Create;
    try
      Mask.Handle := hMask;
      // Copy bitmap.
      // Note: This is required since AddMask (via TDIBReader) might cause the
      // original bitmap handle to be destroyed. We can't allow that since we
      // don't own the handle.
      MaskCopy.Assign(Mask);
      AddMask(MaskCopy);
    finally
      Mask.ReleaseHandle;
      Mask.Free;
      MaskCopy.Free;
    end;
  end;

  procedure AddIconMask(Icon: TIcon);
  var
    IconInfo: TIconInfo;
  begin
    if (not GetIconInfo(Icon.Handle, IconInfo)) then
      exit;
    try
      // Extract the icon mask
      AddMaskOnly(IconInfo.hbmMask);
    finally
      DeleteObject(IconInfo.hbmMask);
      if IconInfo.hbmColor <> 0 then
        DeleteObject(IconInfo.hbmColor);
    end;
  end;

  procedure AddMetafileMask(Metafile: TMetaFile);
  var
    Mask1, Mask2: TBitmap;

    procedure DrawMetafile(ABitmap: TBitmap; Background: TColor);
    begin
      ABitmap.Width := Metafile.Width;
      ABitmap.Height := Metafile.Height;
      SetPixelFormat(ABitmap, pf24bit);
      ABitmap.Canvas.Brush.Color := Background;
      ABitmap.Canvas.Brush.Style := bsSolid;
      ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);
      ABitmap.Canvas.Draw(0,0, Metafile);
    end;

  begin
    // Create the metafile mask
    Mask1 := TBitmap.Create;
    try
      Mask2 := TBitmap.Create;
      try
        DrawMetafile(Mask1, clWhite);
        DrawMetafile(Mask2, clBlack);
        Mask1.Canvas.CopyMode := cmSrcInvert;
        Mask1.Canvas.Draw(0,0, Mask2);
        AddMask(Mask1);
      finally
        Mask2.Free;
      end;
    finally
      Mask1.Free;
    end;
  end;

var
  Progress: integer;
begin
  if (Source = Self) then
    exit;
  if (Source = nil) then
  begin
    Clear;
  end else
  //
  // TGIFFrame import
  //
  if (Source is TGIFFrame) then
  begin
    // Zap existing colormap, extensions and bitmap
    Clear;
    if (TGIFFrame(Source).Empty) then
      exit;
    // Copy source data
    FImageDescriptor := TGIFFrame(Source).FImageDescriptor;
    // Copy image data
    NewImage;
    if (FData <> nil) and (TGIFFrame(Source).Data <> nil) then
      Move(TGIFFrame(Source).Data^, FData^, FDataSize);
    // Copy palette
    // Note: We copy the source frame's "ActiveColorMap" since the source frame
    // might not have a local color map!
    FColorMap.Assign(TGIFFrame(Source).ActiveColorMap);
    // Copy extensions
    Extensions.Assign(TGIFFrame(Source).Extensions);

    // Copy bitmap representation
    // (Not really nescessary but improves performance if the bitmap is needed
    // later on)
    if (TGIFFrame(Source).HasBitmap) then
    begin
      NewBitmap;
      FBitmap.Assign(TGIFFrame(Source).Bitmap);
    end;
  end else
  //
  // Bitmap import
  //
  if (Source is TBitmap) then
  begin
    // Zap existing colormap, extensions and bitmap
    Clear;
    if (TBitmap(Source).Empty) then
      exit;

    Width := TBitmap(Source).Width;
    Height := TBitmap(Source).Height;

    PixelFormat := TBitmap(Source).PixelFormat;
    // Determine if we have a DDB with 8 or less bits/pixel
    if (PixelFormat = pfDevice) then
    begin
      FillChar(DIB, SizeOf(DIB), 0);
      GetObject(TBitmap(Source).Handle, SizeOf(DIB), @DIB);
      if (DIB.dsbm.bmBits = nil) then
        DIB.dsbmih.biSize := 0;
      if (DIB.dsbmih.biSize < DWORD(SizeOf(DIB.dsbmih))) then
        // If source is a DDB in 1, 4, or 8 bit format, there is no need to
        // perform color reduction on it (hopefully).
        case (DIB.dsBm.bmBitsPixel) of
          8: PixelFormat := pf8bit;
          4: PixelFormat := pf4bit;
          1: PixelFormat := pf1bit;
        end;
    end;
    if (PixelFormat > pf8bit) or (PixelFormat = pfDevice) then
    begin
      // Use source palette if requested (rmPalette reduction mode)
      if (Image.ColorReduction = rmPalette) then
        APalette := TBitmap(Source).Palette
      else
        APalette := 0;
      // Convert image to 8 bits/pixel or less
      FBitmap := ReduceColors(TBitmap(Source), Image.ColorReduction,
        Image.DitherMode, Image.ReductionBits, APalette);
      PixelFormat := FBitmap.PixelFormat;
    end else
    begin
      // Create new bitmap and copy
      NewBitmap;
      FBitmap.Assign(TBitmap(Source));
    end;

    // Allocate new buffer
    NewImage;

    Progress := 0;
    Image.Progress(Self, psStarting, Progress, False, Rect(0,0,0,0), sProgressConverting);
    try
      if (not(PixelFormat in [pf1bit, pf4bit, pf8bit, pf24bit])) then
        PixelFormat := pf24bit;
      DIBSource := TDIBReader.Create(FBitmap, PixelFormat);
      try
        // Copy pixels
        case (PixelFormat) of
          pf8bit: Import8Bit;
          pf4bit: Import4Bit;
          pf1bit: Import1Bit;
        else
          Import24Bit;
        end;

      finally
        DIBSource.Free;
      end;

      // Add mask for transparent bitmaps
                                                                      
                                                                              
                                                   
      if (TBitmap(Source).Transparent) then
      begin
        if (PixelFormat > pf8bit) or (PixelFormat = pfDevice) then
        begin
          // Transparent color might not be in color map
          AddMaskOnly(TBitmap(Source).MaskHandle);
          // Change transparency color to bitmaps original transparency color
          if (Transparent) then
            ActiveColorMap[GraphicControlExtension.TransparentColorIndex] := TBitmap(Source).TransparentColor;
        end else
        begin
          // Transparent color must be present in color map - use it directly.
          i := ColorMap.IndexOf(TBitmap(Source).TransparentColor);
          if (i <> -1) then
            MakeTransparent(i)
        end;
      end;

      Progress := 100;

    finally
      Image.Progress(Self, psEnding, Progress, Image.PaletteModified, Rect(0,0,0,0), sProgressConverting);
    end;
  end else
  //
  // TGraphic import
  //
  if (Source is TGraphic) then
  begin
    // Zap existing colormap, extensions and bitmap
    Clear;
    if (TGraphic(Source).Empty) then
      exit;

    ABitmap := TBitmap.Create;
    try
      // Import TIcon and TMetafile by drawing them onto a bitmap...
      // ...and then importing the bitmap recursively
      if (Source is TIcon) or (Source is TMetafile) then
      begin
        try
          ImportViaDraw(ABitmap, TGraphic(Source))
        except
          // If import via TCanvas.Draw fails (which it shouldn't), we try the
          // Assign mechanism instead
          ABitmap.Assign(Source);
        end;
      end else
        try
          ABitmap.Assign(Source);
        except
          // If automatic conversion to bitmap fails, we try and draw the
          // graphic on the bitmap instead
          ImportViaDraw(ABitmap, TGraphic(Source));
        end;
      // Convert the bitmap to a GIF frame recursively
      Assign(ABitmap);
    finally
      ABitmap.Free;
    end;

    // Import transparency mask
    if (Source is TIcon) then
      AddIconMask(TIcon(Source));
    if (Source is TMetaFile) then
      AddMetafileMask(TMetaFile(Source));

  end else
  //
  // TPicture import
  //
  if (Source is TPicture) then
  begin
    // Recursively import TGraphic
    Assign(TPicture(Source).Graphic);
  end else
    // Unsupported format - fall back to Source.AssignTo
    inherited Assign(Source);
end;

                                                                                        
                                                                                          
                                                                                  
// Copied from D3 graphics.pas
// Fixed by Brian Lowe of Acro Technology Inc. 30Jan98
function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy = $00AA0029;
var
  MemDC, OrMaskDC: HDC;
  MemBmp, OrMaskBmp: HBITMAP;
  Save, OrMaskSave: THandle;
  crText, crBack: TColorRef;
  SavePal: HPALETTE;

begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, 1, 1));
    MemBmp := SelectObject(MaskDC, MemBmp);
    try
      MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY, MemBmp, MaskX,
        MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
    end;
    Exit;
  end;

  SavePal := 0;
  MemDC := GDICheck(CreateCompatibleDC(DstDC));
  try
    { Color bitmap for combining OR mask with source bitmap }
    MemBmp := GDICheck(CreateCompatibleBitmap(DstDC, SrcW, SrcH));
    try
      Save := SelectObject(MemDC, MemBmp);
      try
        { This bitmap needs the size of the source but DC of the dest }
        OrMaskDC := GDICheck(CreateCompatibleDC(DstDC));
        try
          { Need a monochrome bitmap for OR mask!! }
          OrMaskBmp := GDICheck(CreateBitmap(SrcW, SrcH, 1, 1, nil));
          try
            OrMaskSave := SelectObject(OrMaskDC, OrMaskBmp);
            try

              // OrMask := 1
              // Original: BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, OrMaskDC, SrcX, SrcY, WHITENESS);
              // Replacement, but not needed: PatBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, WHITENESS);
              // OrMask := OrMask XOR Mask
              // Not needed: BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, MaskDC, SrcX, SrcY, SrcInvert);
              // OrMask := NOT Mask
              BitBlt(OrMaskDC, SrcX, SrcY, SrcW, SrcH, MaskDC, SrcX, SrcY, NotSrcCopy);

              // Retrieve source palette (with dummy select)
              SavePal := SelectPalette(SrcDC, SystemPalette16, False);
              // Restore source palette
              SelectPalette(SrcDC, SavePal, False);
              // Select source palette into memory buffer
              if SavePal <> 0 then
                SavePal := SelectPalette(MemDC, SavePal, True)
              else
                SavePal := SelectPalette(MemDC, SystemPalette16, True);
              RealizePalette(MemDC);

              // Mem := OrMask
              BitBlt(MemDC, SrcX, SrcY, SrcW, SrcH, OrMaskDC, SrcX, SrcY, SrcCopy);
              // Mem := Mem AND Src
{$IFNDEF GIF_TESTMASK} // Define GIF_TESTMASK if you want to know what it does...
              BitBlt(MemDC, SrcX, SrcY, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcAnd);
{$ELSE}
              StretchBlt(DstDC, DstX, DstY, DstW DIV 2, DstH, MemDC, SrcX, SrcY, SrcW, SrcH, SrcCopy);
              StretchBlt(DstDC, DstX+DstW DIV 2, DstY, DstW DIV 2, DstH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcCopy);
              exit;
{$ENDIF}
            finally
              if (OrMaskSave <> 0) then
                SelectObject(OrMaskDC, OrMaskSave);
            end;
          finally
            DeleteObject(OrMaskBmp);
          end;
        finally
          DeleteDC(OrMaskDC);
        end;

        crText := SetTextColor(DstDC, $00000000);
        crBack := SetBkColor(DstDC, $00FFFFFF);

        { All color rendering is done at 1X (no stretching),
          then final 2 masks are stretched to dest DC }
        // Dst := Dst AND Mask
        StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, SrcX, SrcY, SrcW, SrcH, SrcAnd);
        // Dst := Dst OR Mem
        StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, SrcX, SrcY, SrcW, SrcH, SrcPaint);

        SetTextColor(DstDC, crText);
        SetTextColor(DstDC, crBack);

      finally
        if (Save <> 0) then
          SelectObject(MemDC, Save);
      end;
    finally
      DeleteObject(MemBmp);
    end;
  finally
    if (SavePal <> 0) then
      SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end;

procedure TGIFFrame.Draw(ACanvas: TCanvas; const Rect: TRect;
  DoTransparent, DoTile: boolean);
begin
  if (DoTile) then
    StretchDraw(ACanvas, Rect, DoTransparent, DoTile)
  else
    StretchDraw(ACanvas, ScaleRect(Rect), DoTransparent, DoTile);
end;

type
  TCanvasCracker = class(TCanvas);

procedure TGIFFrame.StretchDraw(ACanvas: TCanvas; const Rect: TRect;
  DoTransparent, DoTile: boolean);
var
  MaskDC: HDC;
  Save: THandle;
  Tile: TRect;
begin
  if (DoTransparent) and (Transparent) and (HasMask) then
  begin
    // Draw transparent using mask
    Save := 0;
    MaskDC := 0;
    try
      MaskDC := GDICheck(CreateCompatibleDC(0));
      Save := SelectObject(MaskDC, FMask);

      if (DoTile) then
      begin
        Tile.Left := Rect.Left+Left;
        Tile.Right := Tile.Left + Width;
        while (Tile.Left < Rect.Right) do
        begin
          Tile.Top := Rect.Top+Top;
          Tile.Bottom := Tile.Top + Height;
          while (Tile.Top < Rect.Bottom) do
          begin
            TransparentStretchBlt(ACanvas.Handle, Tile.Left, Tile.Top, Width, Height,
              Bitmap.Canvas.Handle, 0, 0, Width, Height, MaskDC, 0, 0);
            Tile.Top := Tile.Top + Image.Height;
            Tile.Bottom := Tile.Bottom + Image.Height;
          end;
          Tile.Left := Tile.Left + Image.Width;
          Tile.Right := Tile.Right + Image.Width;
        end;
      end else
        TransparentStretchBlt(ACanvas.Handle, Rect.Left, Rect.Top,
          Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
          Bitmap.Canvas.Handle, 0, 0, Width, Height, MaskDC, 0, 0);

      // Since we are not using any of the TCanvas functions (only handle)
      // we need to fire the TCanvas.Changed method "manually".
      TCanvasCracker(ACanvas).Changed;

    finally
      if (Save <> 0) then
        SelectObject(MaskDC, Save);
      if (MaskDC <> 0) then
        DeleteDC(MaskDC);
    end;
  end else
  begin
    if (DoTile) then
    begin
      Tile.Left := Rect.Left+Left;
      Tile.Right := Tile.Left + Width;
      while (Tile.Left < Rect.Right) do
      begin
        Tile.Top := Rect.Top+Top;
        Tile.Bottom := Tile.Top + Height;
        while (Tile.Top < Rect.Bottom) do
        begin
          ACanvas.StretchDraw(Tile, Bitmap);
          Tile.Top := Tile.Top + Image.Height;
          Tile.Bottom := Tile.Bottom + Image.Height;
        end;
        Tile.Left := Tile.Left + Image.Width;
        Tile.Right := Tile.Right + Image.Width;
      end;
    end else
      ACanvas.StretchDraw(Rect, Bitmap);
  end;
end;

// Given a destination rect (DestRect) calculates the
// area covered by this frame
function TGIFFrame.ScaleRect(DestRect: TRect): TRect;
var
  HeightMul, HeightDiv: integer;
  WidthMul, WidthDiv: integer;
begin
  HeightDiv := Image.Height;
  HeightMul := DestRect.Bottom-DestRect.Top;
  WidthDiv := Image.Width;
  WidthMul := DestRect.Right-DestRect.Left;

  Result.Left := DestRect.Left + MulDiv(Left, WidthMul, WidthDiv);
  Result.Top := DestRect.Top + MulDiv(Top, HeightMul, HeightDiv);
  Result.Right := DestRect.Left + MulDiv(Left+Width, WidthMul, WidthDiv);
  Result.Bottom := DestRect.Top + MulDiv(Top+Height, HeightMul, HeightDiv);
end;

procedure TGIFFrame.Crop;
var
  TransparentColorIndex: byte;
  CropLeft, CropTop, CropRight, CropBottom: integer;
  WasTransparent: boolean;
  i: integer;
  NewSize: integer;
  NewData: PByte;
  NewWidth, NewHeight: integer;
  pSource, pDest: PByte;
begin
  if (Empty) or (not Transparent) then
    exit;
  TransparentColorIndex := GraphicControlExtension.TransparentColorIndex;
  CropLeft := 0;
  CropRight := Width - 1;
  CropTop := 0;
  CropBottom := Height - 1;
  // Find left edge
  WasTransparent := True;
  while (CropLeft <= CropRight) and (WasTransparent) do
  begin
    for i := CropTop to CropBottom do
      if (Pixels[CropLeft, i] <> TransparentColorIndex) then
      begin
        WasTransparent := False;
        break;
      end;
    if (WasTransparent) then
      inc(CropLeft);
  end;
  // Find right edge
  WasTransparent := True;
  while (CropLeft <= CropRight) and (WasTransparent) do
  begin
    for i := CropTop to CropBottom do
      if (pixels[CropRight, i] <> TransparentColorIndex) then
      begin
        WasTransparent := False;
        break;
      end;
    if (WasTransparent) then
      dec(CropRight);
  end;
  if (CropLeft <= CropRight) then
  begin
    // Find top edge
    WasTransparent := True;
    while (CropTop <= CropBottom) and (WasTransparent) do
    begin
      for i := CropLeft to CropRight do
        if (pixels[i, CropTop] <> TransparentColorIndex) then
        begin
          WasTransparent := False;
          break;
        end;
      if (WasTransparent) then
        inc(CropTop);
    end;
    // Find bottom edge
    WasTransparent := True;
    while (CropTop <= CropBottom) and (WasTransparent) do
    begin
      for i := CropLeft to CropRight do
        if (pixels[i, CropBottom] <> TransparentColorIndex) then
        begin
          WasTransparent := False;
          break;
        end;
      if (WasTransparent) then
        dec(CropBottom);
    end;
  end;

  if (CropLeft > CropRight) or (CropTop > CropBottom) then
  begin
    // Cropped to nothing - frame is invisible

    // Detect if frame is used for animation delay or transparent spacer
    if ((Image.Images.Count > 1) and (GraphicControlExtension.Delay > 0)) or
      (Image.Images.Count = 1) then
    begin
      // Crop to single pixel
      FreeImage;
      FreeBitmap;
      FreeMask;
      FImageDescriptor.Left := 0;;
      FImageDescriptor.Top := 0;
      FImageDescriptor.Width := 1;
      FImageDescriptor.Height := 1;
      NewImage;
      // Copy single pixel value
      PByte(Scanline[0])^ := TransparentColorIndex;
    end else
      Clear;
  end else
  begin
    // Crop frame - move data
    NewWidth := CropRight - CropLeft + 1;
    Newheight := CropBottom - CropTop + 1;
    NewSize := NewWidth * NewHeight;
    GetMem(NewData, NewSize);
    pSource := PByte(FData) + CropTop * Width + CropLeft;
    pDest := NewData;
    for i := 0 to NewHeight-1 do
    begin
      Move(pSource^, pDest^, NewWidth);
      inc(pSource, Width);
      inc(pDest, NewWidth);
    end;
    FreeImage;
    FData := NewData;
    FDataSize := NewSize;
    inc(FImageDescriptor.Left, CropLeft);
    inc(FImageDescriptor.Top, CropTop);
    FImageDescriptor.Width := NewWidth;
    FImageDescriptor.Height := NewHeight;
    FreeBitmap;
    FreeMask
  end;
end;

procedure TGIFFrame.Merge(Previous: TGIFFrame);
var
  SourceIndex, DestIndex: byte;
  SourceTransparent: boolean;
  NeedTransparentColorIndex: boolean;
  PreviousRect, ThisRect, MergeRect: TRect;
  PreviousY, X, Y: integer;
  pSource, pDest: PByte;
  SourceMap, DestMap: TColorMap;
  GCE: TGIFGraphicControlExtension;

  function CanMakeTransparent: boolean;
  begin
    // Is there a local color map...
    if (ColorMap.Count > 0) then
      // ...and is there room in it?
      Result := (ColorMap.Count < 256)
    // Is there a global color map...
    else if (Image.GlobalColorMap.Count > 0) then
      // ...and is there room in it?
      Result := (Image.GlobalColorMap.Count < 256)
    else
      Result := False;
  end;

  function GetTransparentColorIndex: byte;
  var
    i: integer;
  begin
    if (ColorMap.Count > 0) then
    begin
      // Get the transparent color from the local color map
      Result := ColorMap.Add(GIFDefaultTransparentColor);
    end else
    begin
      // Are any other frames using the global color map for transparency
      for i := 0 to Image.Images.Count-1 do
        if (Image.Images[i] <> Self) and (Image.Images[i].Transparent) and
          (Image.Images[i].ColorMap.Count = 0) then
        begin
          // Use the same transparency color as the other frame
          Result := Image.Images[i].GraphicControlExtension.TransparentColorIndex;
          exit;
        end;
      // Get the transparent color from the global color map
      Result := Image.GlobalColorMap.Add(GIFDefaultTransparentColor);
    end;
  end;

begin
  // Determine if it is possible to merge this frame
  if (Empty) or (Previous = nil) or (Previous.Empty) or
    ((Previous.GraphicControlExtension <> nil) and
     (Previous.GraphicControlExtension.Disposal in [dmBackground, dmPrevious])) then
    exit;

  PreviousRect := Previous.BoundsRect;
  ThisRect := BoundsRect;

  // Cannot merge unless the frames intersect
  if (not IntersectRect(MergeRect, PreviousRect, ThisRect)) then
    exit;

  // If the frame isn't already transparent, determine
  // if it is possible to make it so
  if (Transparent) then
  begin
    DestIndex := GraphicControlExtension.TransparentColorIndex;
    NeedTransparentColorIndex := False;
  end else
  begin
    if (not CanMakeTransparent) then
      exit;
    DestIndex := 0; // To avoid compiler warning
    NeedTransparentColorIndex := True;
  end;

  SourceTransparent := Previous.Transparent;
  if (SourceTransparent) then
    SourceIndex := Previous.GraphicControlExtension.TransparentColorIndex
  else
    SourceIndex := 0; // To avoid compiler warning

  PreviousY := MergeRect.Top - Previous.Top;

  SourceMap := Previous.ActiveColorMap.Data;
  DestMap := ActiveColorMap.Data;

  for Y := MergeRect.Top - Top to MergeRect.Bottom - Top-1 do
  begin
    pSource := PByte(Previous.Scanline[PreviousY]) + MergeRect.Left - Previous.Left;
    pDest := PByte(Scanline[Y]) + MergeRect.Left - Left;

    for X := MergeRect.Left to MergeRect.Right-1 do
    begin
      // Ignore pixels if either this frame's or the previous frame's pixel is transparent
      if (
            not(
              ((not NeedTransparentColorIndex) and (pDest^ = Byte(DestIndex))) or
              ((SourceTransparent) and (pSource^ = Byte(SourceIndex)))
            )
          ) and (
            // Replace same colored pixels with transparency
            ((DestMap = SourceMap) and (pDest^ = pSource^)) or
            (CompareMem(@(DestMap[ord(pDest^)]), @(SourceMap[ord(pSource^)]), SizeOf(TGIFColor)))
          ) then
      begin
        if (NeedTransparentColorIndex) then
        begin
          NeedTransparentColorIndex := False;
          DestIndex := GetTransparentColorIndex;
        end;
        pDest^ := Byte(DestIndex);
      end;
      inc(pDest);
      inc(pSource);
    end;
    inc(PreviousY);
  end;

  (*
  ** Create a GCE if the frame wasn't already transparent and any
  ** pixels were made transparent
  *)
  if (not Transparent) and (not NeedTransparentColorIndex) then
  begin
    if (GraphicControlExtension = nil) then
      GCE := TGIFGraphicControlExtension.Create(Self)
    else
      GCE := GraphicControlExtension;
    GCE.Transparent := True;
    GCE.TransparentColorIndex := DestIndex;
  end;

  FreeBitmap;
  FreeMask
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFTrailer
//
////////////////////////////////////////////////////////////////////////////////
procedure TGIFTrailer.SaveToStream(Stream: TStream);
begin
  WriteByte(Stream, bsTrailer);
end;

procedure TGIFTrailer.LoadFromStream(Stream: TStream);
var
  b: BYTE;
begin
  if (Stream.Read(b, 1) <> 1) then
    exit;
  if (b <> bsTrailer) then
    Warning(gsWarning, sBadTrailer);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		TGIFExtension registration database
//
////////////////////////////////////////////////////////////////////////////////
type
  TExtensionLeadIn = record
    Introducer: byte;      { always $21 }
    ExtensionLabel: byte;
  end;

  PExtRec = ^TExtRec;
  TExtRec = record
    ExtClass: TGIFExtensionClass;
    ExtLabel: BYTE;
  end;

  TExtensionList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(eLabel: BYTE; eClass: TGIFExtensionClass);
    function FindExt(eLabel: BYTE): TGIFExtensionClass;
    procedure Remove(eClass: TGIFExtensionClass);
  end;

constructor TExtensionList.Create;
begin
  inherited Create;
  Add(bsPlainTextExtension, TGIFTextExtension);
  Add(bsGraphicControlExtension, TGIFGraphicControlExtension);
  Add(bsCommentExtension, TGIFCommentExtension);
  Add(bsApplicationExtension, TGIFApplicationExtension);
end;

destructor TExtensionList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Dispose(PExtRec(Items[i]));
  inherited Destroy;
end;

procedure TExtensionList.Add(eLabel: BYTE; eClass: TGIFExtensionClass);
var
  NewRec: PExtRec;
begin
  New(NewRec);
  with NewRec^ do
  begin
    ExtLabel := eLabel;
    ExtClass := eClass;
  end;
  inherited Add(NewRec);
end;

function TExtensionList.FindExt(eLabel: BYTE): TGIFExtensionClass;
var
  i: Integer;
begin
  for i := Count-1 downto 0 do
    with PExtRec(Items[i])^ do
      if ExtLabel = eLabel then
      begin
        Result := ExtClass;
        Exit;
      end;
  Result := nil;
end;

procedure TExtensionList.Remove(eClass: TGIFExtensionClass);
var
  i: Integer;
  P: PExtRec;
begin
  for i := Count-1 downto 0 do
  begin
    P := PExtRec(Items[i]);
    if (P^.ExtClass = eClass) then
    begin
      Dispose(P);
      Delete(i);
    end;
  end;
end;

var
  ExtensionList: TExtensionList = nil;

function GetExtensionList: TExtensionList;
begin
  if (ExtensionList = nil) then
    ExtensionList := TExtensionList.Create;
  Result := ExtensionList;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFExtension
//
////////////////////////////////////////////////////////////////////////////////
function TGIFExtension.GetVersion: TGIFVersion;
begin
  Result := gv89a;
end;

class procedure TGIFExtension.RegisterExtension(eLabel: BYTE; eClass: TGIFExtensionClass);
begin
  GetExtensionList.Add(eLabel, eClass);
end;

class function TGIFExtension.FindExtension(Stream: TStream): TGIFExtensionClass;
var
  eLabel: BYTE;
  SubClass: TGIFExtensionClass;
  Pos: LongInt;
begin
  Pos := Stream.Position;
  if (Stream.Read(eLabel, 1) <> 1) then
  begin
    Result := nil;
    exit;
  end;
  Result := GetExtensionList.FindExt(eLabel);
  while (Result <> nil) do
  begin
    SubClass := Result.FindSubExtension(Stream);
    if (SubClass = Result) then
      break;
    Result := SubClass;
  end;
  Stream.Position := Pos;
end;

class function TGIFExtension.FindSubExtension(Stream: TStream): TGIFExtensionClass;
begin
  Result := Self;
end;

constructor TGIFExtension.Create(AFrame: TGIFFrame);
begin
  inherited Create(AFrame.Image);
  FFrame := AFrame;
  FFrame.Extensions.Add(Self);
end;

destructor TGIFExtension.Destroy;
begin
  if (FFrame <> nil) then
    FFrame.Extensions.Remove(Self);
  inherited Destroy;
end;

procedure TGIFExtension.Assign(Source: TPersistent);
var
  MemoryStream: TMemoryStream;
begin
  if (Source is TGIFExtension) then
  begin
    // Copy extension via stream.
    // Note: No validation is performed on the copied extension (e.g. color
    // index values against color maps).
    MemoryStream := TMemoryStream.Create;
    try
      TGIFExtension(Source).SaveToStream(MemoryStream);
      MemoryStream.Seek(0, soFromBeginning);
      LoadFromStream(MemoryStream);
    finally
      MemoryStream.Free;
    end;
  end else
    inherited Assign(Source);
end;

procedure TGIFExtension.SaveToStream(Stream: TStream);
var
  ExtensionLeadIn: TExtensionLeadIn;
begin
  ExtensionLeadIn.Introducer := bsExtensionIntroducer;
  ExtensionLeadIn.ExtensionLabel := ExtensionType;
  Stream.Write(ExtensionLeadIn, SizeOf(ExtensionLeadIn));
end;

function TGIFExtension.DoReadFromStream(Stream: TStream): TGIFExtensionType;
var
  ExtensionLeadIn: TExtensionLeadIn;
begin
  ReadCheck(Stream, ExtensionLeadIn, SizeOf(ExtensionLeadIn));
  if (ExtensionLeadIn.Introducer <> bsExtensionIntroducer) then
    Error(sBadExtensionLabel);
  Result := ExtensionLeadIn.ExtensionLabel;
end;

procedure TGIFExtension.LoadFromStream(Stream: TStream);
begin
  // Seek past lead-in
  // Stream.Seek(SizeOf(TExtensionLeadIn), soFromCurrent);
  if (DoReadFromStream(Stream) <> ExtensionType) then
    Error(sBadExtensionInstance);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFGraphicControlExtension
//
////////////////////////////////////////////////////////////////////////////////
const
  { Extension flag bit masks }
  efInputFlag		= $02;		{ 00000010 }
  efDisposal		= $1C;		{ 00011100 }
  efTransparent		= $01;		{ 00000001 }
  efReserved		= $E0;		{ 11100000 }

constructor TGIFGraphicControlExtension.Create(AFrame: TGIFFrame);
begin
  inherited Create(AFrame);

  FGCExtension.BlockSize := 4;
  FGCExtension.PackedFields := $00;
  FGCExtension.DelayTime := 0;
  FGCExtension.TransparentColorIndex := 0;
  FGCExtension.Terminator := 0;

  if (Frame <> nil) then
  begin
    if (Frame.GCE = nil) then
      Frame.GCE := Self
    else
      Warning(gsWarning, sMultipleGCE);
  end;
end;

destructor TGIFGraphicControlExtension.Destroy;
begin
  if (Frame <> nil) and (Frame.GCE = Self) then
    Frame.GCE := nil;

  inherited Destroy;
end;

function TGIFGraphicControlExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsGraphicControlExtension;
end;

function TGIFGraphicControlExtension.GetTransparent: boolean;
begin
  Result := ((FGCExtension.PackedFields and efTransparent) = efTransparent);
end;

procedure TGIFGraphicControlExtension.SetTransparent(Value: boolean);
begin
  if (Value) then
  begin
    FGCExtension.PackedFields := FGCExtension.PackedFields or efTransparent;
    // Set transparency color to current value to validate it
    TransparentColorIndex := TransparentColorIndex;
  end else
    FGCExtension.PackedFields := FGCExtension.PackedFields and not(efTransparent);
end;

function TGIFGraphicControlExtension.GetTransparentColor: TColor;
begin
  Result := Frame.ActiveColorMap[TransparentColorIndex];
end;

procedure TGIFGraphicControlExtension.SetTransparentColor(Color: TColor);
begin
  FGCExtension.TransparentColorIndex := Frame.ActiveColorMap.AddUnique(Color);
end;

function TGIFGraphicControlExtension.GetTransparentColorIndex: BYTE;
begin
  Result := FGCExtension.TransparentColorIndex;
end;

procedure TGIFGraphicControlExtension.SetTransparentColorIndex(Value: BYTE);
begin
  if ((Value >= Frame.ActiveColorMap.Count) and (Frame.ActiveColorMap.Count > 0)) then
  begin
    // Some GIFs specify a transparency color index larger
    // than the number of colors in the color map. Although
    // this can possible reduce the filesize, it is not compliant
    // with the GIF specifications and TGIFImage can't handle
    // it in all situations. To work around this problem, we
    // try to increase color map size if the specified
    // transparency color index can be made valid by adding a
    // single color to the color map.
    if ((Value = Frame.ActiveColorMap.Count) and (Frame.ActiveColorMap.Count < 256)) then
    begin
      Frame.ActiveColorMap.Add(GIFDefaultTransparentColor);
      Warning(gsWarning, sBadColorIndexFixed);
    end else
    begin
      Warning(gsWarning, sBadColorIndex);
      Value := 0;
    end;
  end;
  FGCExtension.TransparentColorIndex := Value;
end;

function TGIFGraphicControlExtension.GetDelay: WORD;
begin
  Result := FGCExtension.DelayTime;
end;
procedure TGIFGraphicControlExtension.SetDelay(Value: WORD);
begin
  FGCExtension.DelayTime := Value;
end;

function TGIFGraphicControlExtension.GetUserInput: boolean;
begin
  Result := ((FGCExtension.PackedFields and efInputFlag) = efInputFlag);
end;

procedure TGIFGraphicControlExtension.SetUserInput(Value: boolean);
begin
  if (Value) then
    FGCExtension.PackedFields := FGCExtension.PackedFields or efInputFlag
  else
    FGCExtension.PackedFields := FGCExtension.PackedFields and not(efInputFlag);
end;

function TGIFGraphicControlExtension.GetDisposal: TDisposalMethod;
begin
  Result := TDisposalMethod((FGCExtension.PackedFields and efDisposal) shr 2);
end;

procedure TGIFGraphicControlExtension.SetDisposal(Value: TDisposalMethod);
begin
  FGCExtension.PackedFields := FGCExtension.PackedFields and not(efDisposal)
    or ((ord(Value) shl 2) and efDisposal);
end;

procedure TGIFGraphicControlExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FGCExtension, SizeOf(FGCExtension));
end;

procedure TGIFGraphicControlExtension.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  if (Stream.Read(FGCExtension, SizeOf(FGCExtension)) <> SizeOf(FGCExtension)) then
  begin
    Warning(gsWarning, sOutOfData);
    exit;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFTextExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFTextExtension.Create(ASubImage: TGIFFrame);
begin
  inherited Create(ASubImage);
  FText := TStringList.Create;
  FPlainTextExtension.BlockSize := 12;
  FPlainTextExtension.Left := 0;
  FPlainTextExtension.Top := 0;
  FPlainTextExtension.Width := 0;
  FPlainTextExtension.Height := 0;
  FPlainTextExtension.CellWidth := 0;
  FPlainTextExtension.CellHeight := 0;
  FPlainTextExtension.TextFGColorIndex := 0;
  FPlainTextExtension.TextBGColorIndex := 0;
end;

destructor TGIFTextExtension.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

function TGIFTextExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsPlainTextExtension;
end;

function TGIFTextExtension.GetForegroundColor: TColor;
begin
  Result := Frame.ColorMap[ForegroundColorIndex];
end;

procedure TGIFTextExtension.SetForegroundColor(Color: TColor);
begin
  ForegroundColorIndex := Frame.ActiveColorMap.AddUnique(Color);
end;

function TGIFTextExtension.GetBackgroundColor: TColor;
begin
  Result := Frame.ActiveColorMap[BackgroundColorIndex];
end;

procedure TGIFTextExtension.SetBackgroundColor(Color: TColor);
begin
  BackgroundColorIndex := Frame.ColorMap.AddUnique(Color);
end;

procedure TGIFTextExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FPlainTextExtension, SizeOf(FPlainTextExtension));
  WriteStrings(Stream, FText);
end;

procedure TGIFTextExtension.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  ReadCheck(Stream, FPlainTextExtension, SizeOf(FPlainTextExtension));
  ReadStrings(Stream, FText);
end;

procedure TGIFTextExtension.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFCommentExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFCommentExtension.Create(AFrame: TGIFFrame);
begin
  inherited Create(AFrame);
  FText := TStringList.Create;
end;

destructor TGIFCommentExtension.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

function TGIFCommentExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsCommentExtension;
end;

procedure TGIFCommentExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  WriteStrings(Stream, FText);
end;

procedure TGIFCommentExtension.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  ReadStrings(Stream, FText);
end;

procedure TGIFCommentExtension.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		TGIFApplicationExtension registration database
//
////////////////////////////////////////////////////////////////////////////////
type
  PAppExtRec = ^TAppExtRec;
  TAppExtRec = record
    AppClass: TGIFAppExtensionClass;
    Ident: TGIFApplicationRec;
  end;

  TAppExtensionList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(eIdent: TGIFApplicationRec; eClass: TGIFAppExtensionClass);
    function FindExt(eIdent: TGIFApplicationRec): TGIFAppExtensionClass;
    procedure Remove(eClass: TGIFAppExtensionClass);
  end;

constructor TAppExtensionList.Create;
const
  NSLoopIdent: array[0..1] of TGIFApplicationRec =
    ((Identifier: AnsiString('NETSCAPE'); Authentication: AnsiString('2.0')),    // Do not localize
     (Identifier: AnsiString('ANIMEXTS'); Authentication: AnsiString('1.0')));   // Do not localize
begin
  inherited Create;
  Add(NSLoopIdent[0], TGIFAppExtNSLoop);
  Add(NSLoopIdent[1], TGIFAppExtNSLoop);
end;

destructor TAppExtensionList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PAppExtRec(Items[I]));
  inherited Destroy;
end;

procedure TAppExtensionList.Add(eIdent: TGIFApplicationRec; eClass: TGIFAppExtensionClass);
var
  NewRec: PAppExtRec;
begin
  New(NewRec);
  NewRec^.Ident := eIdent;
  NewRec^.AppClass := eClass;
  inherited Add(NewRec);
end;

function TAppExtensionList.FindExt(eIdent: TGIFApplicationRec): TGIFAppExtensionClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    with PAppExtRec(Items[I])^ do
      if CompareMem(@Ident, @eIdent, SizeOf(TGIFApplicationRec)) then
      begin
        Result := AppClass;
        Exit;
      end;
  Result := nil;
end;

procedure TAppExtensionList.Remove(eClass: TGIFAppExtensionClass);
var
  I: Integer;
  P: PAppExtRec;
begin
  for I := Count-1 downto 0 do
  begin
    P := PAppExtRec(Items[I]);
    if (P^.AppClass = eClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

var
  AppExtensionList: TAppExtensionList = nil;

function GetAppExtensionList: TAppExtensionList;
begin
  if (AppExtensionList = nil) then
    AppExtensionList := TAppExtensionList.Create;
  Result := AppExtensionList;
end;

class procedure TGIFApplicationExtension.RegisterExtension(eIdent: TGIFApplicationRec;
  eClass: TGIFAppExtensionClass);
begin
  GetAppExtensionList.Add(eIdent, eClass);
end;

class function TGIFApplicationExtension.FindSubExtension(Stream: TStream): TGIFExtensionClass;
var
  eIdent: TGIFApplicationRec;
  OldPos: longInt;
  Size: BYTE;
begin
  OldPos := Stream.Position;
  Result := nil;
  if (Stream.Read(Size, 1) <> 1) then
    exit;

  // Some old Adobe export filters mistakenly uses a value of 10
  if (Size = 10) then
  begin
                                                                                          
    if (Stream.Read(eIdent, 10) <> 10) then
      exit;
    Result := TGIFUnknownAppExtension;
    exit;
  end else
  if (Size <> SizeOf(TGIFApplicationRec)) or
    (Stream.Read(eIdent, SizeOf(eIdent)) <> SizeOf(eIdent)) then
  begin
    Stream.Position := OldPos;
    Result := inherited FindSubExtension(Stream);
  end else
  begin
    Result := GetAppExtensionList.FindExt(eIdent);
    if (Result = nil) then
      Result := TGIFUnknownAppExtension;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFApplicationExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFApplicationExtension.Create(ASubImage: TGIFFrame);
begin
  inherited Create(ASubImage);
  FillChar(FIdent, SizeOf(FIdent), 0);
end;

constructor TGIFApplicationExtension.Create(ASubImage: TGIFFrame;
  const Ident: TGIFApplicationRec);
begin
  inherited Create(ASubImage);
  FIdent := Ident;
end;

destructor TGIFApplicationExtension.Destroy;
begin
  inherited Destroy;
end;

function TGIFApplicationExtension.GetExtensionType: TGIFExtensionType;
begin
  Result := bsApplicationExtension;
end;

function TGIFApplicationExtension.GetAuthentication: string;
begin
  Result := string(FIdent.Authentication);
end;

procedure StrCopyPad(Target: PAnsiChar; const Source: string; MaxLength: integer; Pad: Byte);
var
  pSource: PAnsiChar;
begin
  pSource := PAnsiChar(AnsiString(Source));
  while (MaxLength > 0) and (pSource^ <> #0) do
  begin
    Target^ := pSource^;
    inc(Target);
    inc(pSource);
    dec(MaxLength);
  end;
  while (MaxLength > 0) do
  begin
    Target^ := AnsiChar(Pad);
    inc(Target);
    dec(MaxLength);
  end;
end;

procedure TGIFApplicationExtension.SetAuthentication(const Value: string);
begin
  StrCopyPad(@FIdent.Authentication[0], Value, SizeOf(TGIFAuthenticationCode), 32);
end;

function TGIFApplicationExtension.GetIdentifier: string;
begin
  Result := string(FIdent.Identifier);
end;

procedure TGIFApplicationExtension.SetIdentifier(const Value: string);
begin
  StrCopyPad(@FIdent.Identifier[0], Value, SizeOf(TGIFIdentifierCode), 32);
end;

procedure TGIFApplicationExtension.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  WriteByte(Stream, SizeOf(FIdent)); // Block size
  Stream.Write(FIdent, SizeOf(FIdent));
  SaveData(Stream);
end;

procedure TGIFApplicationExtension.LoadFromStream(Stream: TStream);
var
  i: integer;
begin
  inherited LoadFromStream(Stream);
  i := ReadByte(Stream);
  // Some old Adobe export filters mistakenly uses a value of 10
  if (i = 10) then
    FillChar(FIdent, SizeOf(FIdent), 0)
  else
    if (i < 11) then
      Error(sBadBlockSize);

  ReadCheck(Stream, FIdent, SizeOf(FIdent));

  Dec(i, SizeOf(FIdent));
  // Ignore extra data
  Stream.Seek(i, soFromCurrent);

  // ***FIXME***
  // If Self class is TGIFApplicationExtension, this will cause an "abstract
  // error".
  // TGIFApplicationExtension.LoadData should read and ignore rest of block.
  LoadData(Stream);
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFUnknownAppExtension
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFBlock.Create(ASize: integer);
begin
  inherited Create;
  FSize := ASize;
  GetMem(FData, FSize);
  FillChar(FData^, FSize, 0);
end;

destructor TGIFBlock.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

procedure TGIFBlock.SaveToStream(Stream: TStream);
begin
  Stream.Write(FSize, 1);
  Stream.Write(FData^, FSize);
end;

procedure TGIFBlock.LoadFromStream(Stream: TStream);
begin
  ReadCheck(Stream, FData^, FSize);
end;

constructor TGIFUnknownAppExtension.Create(ASubImage: TGIFFrame);
begin
  inherited Create(ASubImage);
  FBlocks := TList.Create;
end;

destructor TGIFUnknownAppExtension.Destroy;
var
  i: integer;
begin
  for i := 0 to FBlocks.Count-1 do
    TGIFBlock(FBlocks[i]).Free;
  FBlocks.Free;
  inherited Destroy;
end;


procedure TGIFUnknownAppExtension.SaveData(Stream: TStream);
var
  i: integer;
begin
  for i := 0 to FBlocks.Count-1 do
    TGIFBlock(FBlocks[i]).SaveToStream(Stream);
  // Terminating zero
  WriteByte(Stream, 0);
end;

procedure TGIFUnknownAppExtension.LoadData(Stream: TStream);
var
  b: BYTE;
  Block: TGIFBlock;
  i: integer;
begin
  // Zap old blocks
  for i := 0 to FBlocks.Count-1 do
    TGIFBlock(FBlocks[i]).Free;
  FBlocks.Clear;

  // Read blocks
  while (Stream.Read(b, 1) = 1) and (b <> 0) do
  begin
    Block := TGIFBlock.Create(b);
    try
      Block.LoadFromStream(Stream);
    except
      Block.Free;
      raise;
    end;
    FBlocks.Add(Block);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//                      TGIFAppExtNSLoop
//
////////////////////////////////////////////////////////////////////////////////
const
  // Netscape sub block types
  nbLoopExtension	= 1;
  nbBufferExtension	= 2;

constructor TGIFAppExtNSLoop.Create(AFrame: TGIFFrame);
const
  NSLoopIdent: TGIFApplicationRec = (Identifier: AnsiString('NETSCAPE'); Authentication: AnsiString('2.0'));  // Do not localize
begin
  inherited Create(AFrame, NSLoopIdent);

  FLoops := 65535;
end;

procedure TGIFAppExtNSLoop.SaveData(Stream: TStream);
begin
  // Write loop count
  WriteByte(Stream, 1 + SizeOf(FLoops)); // Size of block
  WriteByte(Stream, nbLoopExtension); // Identify sub block as looping extension data

  Stream.Write(FLoops, SizeOf(FLoops)); // Loop count

  // Write buffer size if specified
  if (FBufferSize > 0) then
  begin
    WriteByte(Stream, 1 + SizeOf(FBufferSize)); // Size of block
    WriteByte(Stream, nbBufferExtension); // Identify sub block as buffer size data
    Stream.Write(FBufferSize, SizeOf(FBufferSize)); // Buffer size
  end;

  WriteByte(Stream, 0); // Terminating zero
end;

procedure TGIFAppExtNSLoop.LoadData(Stream: TStream);
var
  BlockSize: integer;
  BlockType: integer;
begin
  // Read size of first block or terminating zero
  BlockSize := ReadByte(Stream);
  while (BlockSize <> 0) do
  begin
    BlockType := ReadByte(Stream);
    dec(BlockSize);

    case (BlockType and $07) of
      nbLoopExtension:
        begin
          if (BlockSize < SizeOf(FLoops)) then
            Error(sInvalidData);
          // Read loop count
          ReadCheck(Stream, FLoops, SizeOf(FLoops));
          dec(BlockSize, SizeOf(FLoops));
        end;
      nbBufferExtension:
        begin
          if (BlockSize < SizeOf(FBufferSize)) then
            Error(sInvalidData);
          // Read buffer size
          ReadCheck(Stream, FBufferSize, SizeOf(FBufferSize));
          dec(BlockSize, SizeOf(FBufferSize));
        end;
    end;

    // Skip/ignore unread data
    if (BlockSize > 0) then
      Stream.Seek(BlockSize, soFromCurrent);

    // Read size of next block or terminating zero
    BlockSize := ReadByte(Stream);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//			TGIFImageList
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFImageList.Create(AImage: TGIFImage);
begin
  inherited Create;
  FImage := AImage;
end;

function TGIFImageList.GetImage: TGIFImage;
begin
  Result := FImage;
end;

function TGIFImageList.GetFrame(Index: Integer): TGIFFrame;
begin
  Result := TGIFFrame(Items[Index]);
end;

procedure TGIFImageList.SetFrame(Index: Integer; AFrame: TGIFFrame);
begin
  Items[Index] := AFrame;
end;

procedure TGIFImageList.LoadFromStream(Stream: TStream);
var
  b: BYTE;
  Frame: TGIFFrame;
begin
  // Peek ahead to determine block type
  repeat
    if (Stream.Read(b, 1) <> 1) then
      exit;
  until (b <> 0); // Ignore 0 padding (non-compliant)

  while (b <> bsTrailer) do
  begin
    Stream.Seek(-1, soFromCurrent);
    if (b in [bsExtensionIntroducer, bsImageDescriptor]) then
    begin
      Frame := TGIFFrame.Create(Image);
      try
        Frame.LoadFromStream(Stream);
        Image.Progress(Self, psRunning, MulDiv(Stream.Position, 100, Stream.Size),
          GIFImageRenderOnLoad, Rect(0,0,0,0), sProgressLoading);
        // Zap frame in case it failed to load
        if (Frame.Empty) and (Frame.Extensions.Count = 0) then
          Frame.Free;
      except
        Frame.Free;
        raise;
      end;
    end else
    begin
      Warning(gsWarning, sBadBlock);
      break;
    end;
    repeat
      if (Stream.Read(b, 1) <> 1) then
        exit;
    until (b <> 0); // Ignore 0 padding (non-compliant)
  end;
  Stream.Seek(-1, soFromCurrent);
end;

procedure TGIFImageList.SaveToStream(Stream: TStream);
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    Frames[i].SaveToStream(Stream);
    Image.Progress(Self, psRunning, MulDiv(i+1, 100, Count), False, Rect(0,0,0,0), sProgressSaving);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//			TColorMapOptimizer
//
////////////////////////////////////////////////////////////////////////////////
// Used by TGIFImage to optimize local color maps to a single global color map.
// The following algorithm is used:
// 1) Build a histogram for each image
// 2) Merge histograms
// 3) Sum equal colors and adjust max # of colors
// 4) Map entries > max to entries <= 256
// 5) Build new color map
// 6) Map images to new color map
////////////////////////////////////////////////////////////////////////////////

type

  POptimizeEntry = ^TOptimizeEntry;
  TColorRec = record
  case byte of
    0: (Value: integer);
    1: (Color: TGIFColor);
    2: (SameAs: POptimizeEntry); // Used if TOptimizeEntry.Count = 0
  end;

  TOptimizeEntry = record
    Count: integer;	// Usage count
    OldIndex: integer;	// Color OldIndex
    NewIndex: integer;	// NewIndex color OldIndex
    Color: TColorRec;	// Color value
  end;

  TOptimizeEntries = array[0..255] of TOptimizeEntry;
  POptimizeEntries = ^TOptimizeEntries;

  THistogram = class(TObject)
  strict private
    PHistogram: POptimizeEntries;
    FCount: integer;
    FColorMap: TGIFColorMap;
    FList: TList;
    FImages: TList;
  public
    constructor Create(AColorMap: TGIFColorMap);
    destructor Destroy; override;
    function ProcessFrame(Image: TGIFFrame): boolean;
    function Prune: integer;
    procedure MapImages(UseTransparency: boolean; NewTransparentColorIndex: byte);
    property Count: integer read FCount;
    property ColorMap: TGIFColorMap read FColorMap;
    property List: TList read FList;
  end;

  TColorMapOptimizer = class(TObject)
  strict private
    FImage: TGIFImage;
    FHistogramList: TList;
    FHistogram: TList;
    FColorMap: TColorMap;
    FFinalCount: integer;
    FUseTransparency: boolean;
    FNewTransparentColorIndex: byte;
  strict protected
    procedure ProcessImage;
    procedure MergeColors;
    procedure MapColors;
    procedure ReplaceColorMaps;
  public
    constructor Create(AImage: TGIFImage);
    destructor Destroy; override;
    procedure Optimize;
  end;

function CompareColor(Item1, Item2: Pointer): integer;
begin
  Result := POptimizeEntry(Item2)^.Color.Value - POptimizeEntry(Item1)^.Color.Value;
end;

function CompareCount(Item1, Item2: Pointer): integer;
begin
  Result := POptimizeEntry(Item2)^.Count - POptimizeEntry(Item1)^.Count;
end;

constructor THistogram.Create(AColorMap: TGIFColorMap);
var
  i: integer;
begin
  inherited Create;

  FCount := AColorMap.Count;
  FColorMap := AColorMap;

  FImages := TList.Create;

  // Allocate memory for histogram
  GetMem(PHistogram, FCount * SizeOf(TOptimizeEntry));
  FList := TList.Create;

  FList.Capacity := FCount;

  // Move data to histogram and initialize
  for i := 0 to FCount-1 do
    with PHistogram^[i] do
    begin
      FList.Add(@PHistogram^[i]);
      OldIndex := i;
      Count := 0;
      Color.Value := 0;
      Color.Color := AColorMap.Data[i];
      NewIndex := 256; // Used to signal unmapped
    end;
end;

destructor THistogram.Destroy;
begin
  FImages.Free;
  FList.Free;
  FreeMem(PHistogram);
  inherited Destroy;
end;

//: Build a color histogram
function THistogram.ProcessFrame(Image: TGIFFrame): boolean;
var
  Size: integer;
  Pixel: PByte;
  IsTransparent, WasTransparent: boolean;
  OldTransparentColorIndex: byte;
begin
  Result := False;
  if (Image.Empty) then
    exit;

  FImages.Add(Image);

  Pixel := Image.data;
  Size := Image.Width * Image.Height;

  IsTransparent := Image.Transparent;
  if (IsTransparent) then
    OldTransparentColorIndex := Image.GraphicControlExtension.TransparentColorIndex
  else
    OldTransparentColorIndex := 0; // To avoid compiler warning
  WasTransparent := False;

  (*
  ** Sum up usage count for each color
  *)
  while (Size > 0) do
  begin
    // Ignore transparent pixels
    if (not IsTransparent) or (ord(Pixel^) <> OldTransparentColorIndex) then
    begin
      // Check for invalid color index
      if (ord(Pixel^) >= FCount) then
      begin
        Pixel^ := 0; // ***FIXME*** Isn't this an error condition?
        Image.Warning(gsWarning, sInvalidColor);
      end;

      with PHistogram^[ord(Pixel^)] do
      begin
        // Stop if any color reaches the max count
        if (Count = high(integer)) then
          break;
        inc(Count);
      end;
    end else
      WasTransparent := WasTransparent or IsTransparent;
    inc(Pixel);
    dec(Size);
  end;

  (*
  ** Clear frames transparency flag if the frame claimed to
  ** be transparent, but wasn't
  *)
  if (IsTransparent and not WasTransparent) then
  begin
    Image.GraphicControlExtension.TransparentColorIndex := 0;
    Image.GraphicControlExtension.Transparent := False;
  end;

  Result := WasTransparent;
end;

//: Removed unused color entries from the histogram
function THistogram.Prune: integer;
var
  i, j: integer;
begin
  (*
  **  Sort by usage count
  *)
  FList.Sort(CompareCount);

  (*
  **  Determine number of used colors
  *)
  for i := 0 to FCount-1 do
    // Find first unused color entry
    if (POptimizeEntry(FList[i])^.Count = 0) then
    begin
      // Zap unused colors
      for j := i to FCount-1 do
        POptimizeEntry(FList[j])^.Count := -1; // Use -1 to signal unused entry
      // Remove unused entries
      FCount := i;
      FList.Count := FCount;
      break;
    end;

  Result := FCount;
end;

//: Convert images from old color map to new color map
procedure THistogram.MapImages(UseTransparency: boolean; NewTransparentColorIndex: byte);
var
  i: integer;
  Size: integer;
  Pixel: PByte;
  ReverseMap: array[byte] of byte;
  IsTransparent: boolean;
  OldTransparentColorIndex: byte;
begin
  (*
  ** Build NewIndex map
  *)
  for i := 0 to List.Count-1 do
    ReverseMap[POptimizeEntry(List[i])^.OldIndex] := POptimizeEntry(List[i])^.NewIndex;

  (*
  **  Reorder all images using this color map
  *)
  for i := 0 to FImages.Count-1 do
    with TGIFFrame(FImages[i]) do
    begin
      Pixel := Data;
      Size := Width * Height;

      // Determine frame transparency
      IsTransparent := (Transparent) and (UseTransparency);
      if (IsTransparent) then
      begin
        OldTransparentColorIndex := GraphicControlExtension.TransparentColorIndex;
        // Map transparent color
        GraphicControlExtension.TransparentColorIndex := NewTransparentColorIndex;
      end else
        OldTransparentColorIndex := 0; // To avoid compiler warning

      // Map all pixels to new color map
      while (Size > 0) do
      begin
        // Map transparent pixels to the new transparent color index and...
        if (IsTransparent) and (ord(Pixel^) = OldTransparentColorIndex) then
          Pixel^ := Byte(NewTransparentColorIndex)
        else
          // ... all other pixels to their new color index
          Pixel^ := Byte(ReverseMap[ord(Pixel^)]);
        dec(size);
        inc(Pixel);
      end;
    end;
end;

constructor TColorMapOptimizer.Create(AImage: TGIFImage);
begin
  inherited Create;
  FImage := AImage;
  FHistogramList := TList.Create;
  FHistogram := TList.Create;
end;

destructor TColorMapOptimizer.Destroy;
var
  i: integer;
begin
  FHistogram.Free;

  for i := FHistogramList.Count-1 downto 0 do
    THistogram(FHistogramList[i]).Free;
  FHistogramList.Free;

  inherited Destroy;
end;

procedure TColorMapOptimizer.ProcessImage;
var
  Hist: THistogram;
  i: integer;
  ProcessedImage: boolean;
begin
  FUseTransparency := False;
  (*
  ** First process images using global color map
  *)
  if (FImage.GlobalColorMap.Count > 0) then
  begin
    Hist := THistogram.Create(FImage.GlobalColorMap);
    ProcessedImage := False;
    // Process all images that are using the global color map
    for i := 0 to FImage.Images.Count-1 do
      if (FImage.Images[i].ColorMap.Count = 0) and (not FImage.Images[i].Empty) then
      begin
        ProcessedImage := True;
      // Note: Do not change order of statements. Shortcircuit evaluation not desired!
        FUseTransparency := Hist.ProcessFrame(FImage.Images[i]) or FUseTransparency;
      end;
    // Keep the histogram if any images used the global color map...
    if (ProcessedImage) then
      FHistogramList.Add(Hist)
    else // ... otherwise delete it
      Hist.Free;
  end;

  (*
  ** Next process images that have a local color map
  *)
  for i := 0 to FImage.Images.Count-1 do
    if (FImage.Images[i].ColorMap.Count > 0) and (not FImage.Images[i].Empty) then
    begin
      Hist := THistogram.Create(FImage.Images[i].ColorMap);
      FHistogramList.Add(Hist);
      // Note: Do not change order of statements. Shortcircuit evaluation not desired!
      FUseTransparency := Hist.ProcessFrame(FImage.Images[i]) or FUseTransparency;
    end;
end;

procedure TColorMapOptimizer.MergeColors;
var
  Entry, SameEntry: POptimizeEntry;
  i: integer;
begin
  // Check for empty histogram (100% transparent GIF)
  if (FHistogram.Count = 0) then
    exit;

  (*
  **  Sort by color value
  *)
  FHistogram.Sort(CompareColor);

  (*
  **  Merge same colors
  *)
  SameEntry := POptimizeEntry(FHistogram[0]);
  for i := 1 to FHistogram.Count-1 do
  begin
    Entry := POptimizeEntry(FHistogram[i]);
    ASSERT(Entry^.Count > 0, 'Unused entry exported from THistogram');
    if (Entry^.Color.Value = SameEntry^.Color.Value) then
    begin
      // Transfer usage count to first entry
      inc(SameEntry^.Count, Entry^.Count);
      Entry^.Count := 0; // Use 0 to signal merged entry
      Entry^.Color.SameAs := SameEntry; // Point to master
    end else
      SameEntry := Entry;
  end;
end;

procedure TColorMapOptimizer.MapColors;

  function IntSqr(x: integer): integer; inline;
  begin
    Result := x*x;
  end;

var
  i, j: integer;
  Delta, BestDelta: integer;
  BestIndex: integer;
  MaxColors: integer;
begin
  (*
  **  Sort by usage count
  *)
  FHistogram.Sort(CompareCount);

  (*
  ** Handle transparency
  *)
  if (FUseTransparency) then
    MaxColors := GIFMaxColors-1
  else
    MaxColors := GIFMaxColors;

  (*
  **  Determine number of colors used (max 256)
  *)
  FFinalCount := FHistogram.Count;
  for i := 0 to FFinalCount-1 do
    if (i >= MaxColors) or (POptimizeEntry(FHistogram[i])^.Count = 0) then
    begin
      FFinalCount := i;
      break;
    end;

  (*
  **  Build color map and reverse map for final entries
  *)
  SetLength(FColorMap, GIFMaxColors);
  for i := 0 to FFinalCount-1 do
  begin
    POptimizeEntry(FHistogram[i])^.NewIndex := i;
    FColorMap[i] := POptimizeEntry(FHistogram[i])^.Color.Color;
  end;

  (*
  **  Map colors > 256 to colors <= 256 and build NewIndex color map
  *)
  for i := FFinalCount to FHistogram.Count-1 do
    with POptimizeEntry(FHistogram[i])^ do
    begin
      // Entries with a usage count of -1 is unused
      ASSERT(Count <> -1, 'TColorMapOptimizer: Unused entry exported');
      // Entries with a usage count of 0 has been merged with another entry
      if (Count = 0) then
      begin
        // Use mapping of master entry
        ASSERT(Color.SameAs.NewIndex < 256, 'TColorMapOptimizer: Mapping to unmapped color');
        NewIndex := Color.SameAs.NewIndex;
      end else
      begin
        // Search for entry with nearest color value
        BestIndex := 0;
        BestDelta := 3*255*255;
        for j := 0 to FFinalCount-1 do
        begin
          Delta := IntSqr(POptimizeEntry(FHistogram[j])^.Color.Color.Red - Color.Color.Red) +
            IntSqr(POptimizeEntry(FHistogram[j])^.Color.Color.Green - Color.Color.Green) +
            IntSqr(POptimizeEntry(FHistogram[j])^.Color.Color.Blue - Color.Color.Blue);
          if (Delta < BestDelta) then
          begin
            BestDelta := Delta;
            BestIndex := j;
          end;
        end;
        NewIndex := POptimizeEntry(FHistogram[BestIndex])^.NewIndex;;
      end;
    end;

  (*
  ** Add transparency color to new color map
  *)
  if (FUseTransparency) then
  begin
    FNewTransparentColorIndex := FFinalCount;
    FColorMap[FFinalCount] := TGIFColorMap.Color2RGB(GIFDefaultTransparentColor);
    inc(FFinalCount);
  end;
end;

procedure TColorMapOptimizer.ReplaceColorMaps;
var
  i: integer;
begin
  // Zap all local color maps
  for i := 0 to FImage.Images.Count-1 do
    if (FImage.Images[i].ColorMap <> nil) then
      FImage.Images[i].ColorMap.Clear;
  // Store optimized global color map
  FImage.GlobalColorMap.ImportColorMap(FColorMap, FFinalCount);
  FImage.GlobalColorMap.Optimized := True;
end;

procedure TColorMapOptimizer.Optimize;
var
  Total: integer;
  i, j: integer;
begin
  FImage.SuspendDraw;
  try
    FImage.StopDraw;

    (*
    **  Process all sub images
    *)
    ProcessImage;

    // Prune histograms and calculate total number of colors
    Total := 0;
    for i := 0 to FHistogramList.Count-1 do
      inc(Total, THistogram(FHistogramList[i]).Prune);

    // Allocate global histogram
    FHistogram.Clear;
    FHistogram.Capacity := Total;

    // Move data pointers from local histograms to global histogram
    for i := 0 to FHistogramList.Count-1 do
      with THistogram(FHistogramList[i]) do
        for j := 0 to Count-1 do
        begin
          ASSERT(POptimizeEntry(List[j])^.Count > 0, 'Unused entry exported from THistogram');
          FHistogram.Add(List[j]);
        end;

    (*
    **  Merge same colors
    *)
    MergeColors;

    (*
    **  Build color map and NewIndex map for final entries
    *)
    MapColors;

    (*
    **  Replace local colormaps with global color map
    *)
    ReplaceColorMaps;

    (*
    **  Process images for each color map
    *)
    for i := 0 to FHistogramList.Count-1 do
      THistogram(FHistogramList[i]).MapImages(FUseTransparency, FNewTransparentColorIndex);

    (*
    **  Delete the frame's old bitmaps and palettes
    *)
    for i := 0 to FImage.Images.Count-1 do
    begin
      FImage.Images[i].HasBitmap := False;
      FImage.Images[i].Palette := 0;
    end;

  finally
    FImage.ResumeDraw;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			GIF renderer
//
////////////////////////////////////////////////////////////////////////////////
{ TCustomGIFRenderer }

procedure TCustomGIFRenderer.Changed;
begin
  StopAnimation;
  FChanged := True;
end;

procedure TCustomGIFRenderer.CheckChange;
begin
  if (HasChanged) then
  begin
    FChanged := False;
    Reset;
  end;
end;

constructor TCustomGIFRenderer.Create(AImage: TGIFImage);
begin
  inherited Create;
  FImage := AImage;
  FSpeed := 100;
  Changed;
end;

procedure TCustomGIFRenderer.DoNextFrame;
var
  OldFrameIndex: integer;
  OldFrame: TGIFFrame;
begin
  OldFrameIndex := FrameIndex;
  OldFrame := Frame;

  repeat
    if (FrameIndex >= Image.Images.Count-1) then
    begin
      inc(FLoopCount);
      if (LoopMax = 0) or (LoopCount < LoopMax) then
      begin
        InternalSetFrameIndex(0);
        Loop;
      end else
      begin
        HaltAnimation;

        // Revert to old frame if new one is empty
        if (Frame <> nil) and (Frame.Empty) and (OldFrame <> nil) and (not OldFrame.Empty) then
          InternalSetFrameIndex(OldFrameIndex);
        break;
      end;
    end else
      InternalSetFrameIndex(FFrameIndex+1);

    // Ignore empty frames (e.g. frames with only a comment)
    if (not Frame.Empty) then
      break;
  until (FrameIndex = OldFrameIndex);
end;

procedure TCustomGIFRenderer.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if (FCanvas <> ACanvas) or (not EqualRect(TargetRect, Rect)) then
  begin
    Reset;
    FCanvas := ACanvas;
    FDestRect := Rect;
    Initialize;
  end;
  ACanvas.StretchDraw(Rect, Bitmap);
//  ACanvas.CopyRect(Rect, Bitmap.Canvas, Bitmap.Canvas.ClipRect);
end;

procedure TCustomGIFRenderer.HaltAnimation;
begin

end;

procedure TCustomGIFRenderer.Initialize;
begin
  FBackgroundColor := Image.EffectiveBackgroundColor;
  FLoopMax := 1;
  Changed;
end;

procedure TCustomGIFRenderer.InternalSetFrameIndex(Value: integer);
begin
  if (Value <> FFrameIndex) then
  begin
    if (Value <> -1) then
      FFrame := Image.Images[Value]
    else
      FFrame := nil;
    FFrameIndex := Value;
  end;
end;

procedure TCustomGIFRenderer.Loop;
begin
end;

procedure TCustomGIFRenderer.NextFrame;
begin
  if (Image.Images.Count > 0) then
    DoNextFrame
  else
    InternalSetFrameIndex(-1);
end;

procedure TCustomGIFRenderer.Reset;
begin
  StopAnimation;

  FLoopCount := 0;
  InternalSetFrameIndex(-1);
  NextFrame;

  FChanged := False;
end;

procedure TCustomGIFRenderer.SetAnimate(const Value: boolean);
begin
  if (Value <> FAnimate) then
  begin
    FAnimate := Value;
    Changed;
  end;
end;

procedure TCustomGIFRenderer.SetBackgroundColor(const Value: TColor);
begin
  if (Value <> FBackgroundColor) then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

procedure TCustomGIFRenderer.SetFrameIndex(Value: integer);
begin
  if (Value <> FFrameIndex) then
  begin
    InternalSetFrameIndex(Value);
    Changed;
  end;
end;

procedure TCustomGIFRenderer.SetLoopMax(Value: integer);
begin
  FLoopMax := Value;
end;

procedure TCustomGIFRenderer.SetSpeed(const Value: integer);
begin
  if (Value <> FSpeed) then
  begin
    FSpeed := Value;
    Changed;
  end;
end;

procedure TCustomGIFRenderer.SetTransparent(const Value: boolean);
begin
  if (Value <> FTransparent) then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TCustomGIFRenderer.StartAnimation;
begin
  FAnimating := True;
end;

procedure TCustomGIFRenderer.StopAnimation;
begin
  if (Animating) then
    FAnimating := False;
end;

procedure TGIFRenderer.AnimationTimerEvent(Sender: TObject);
begin
  StopAnimationTimer;
  FNeedTimer := True;
  NextFrame;
  // Make image redraw itself
  Image.Changed(Self);
end;

procedure TGIFRenderer.Changed;
begin
  inherited Changed;
end;

procedure TGIFRenderer.Clear;
begin
  StopAnimation;
  FreeAndNil(FBackground);
  FreeAndNil(FPreviousBuffer);
end;

constructor TGIFRenderer.Create(AImage: TGIFImage);
begin
  inherited Create(AImage);
  FBuffer := TBitmap.Create;
end;

destructor TGIFRenderer.Destroy;
begin
  Clear;
  FBuffer.Free;
  inherited Destroy;
end;

procedure TGIFRenderer.UndoPreviousFrame;
var
  r: TRect;
begin
  ASSERT(PrevFrame <> nil);
  if (Transparent) and (Image.IsTransparent) then
  begin
    // If the frame is transparent, we must remove it by copying the
    // background buffer over it
    r := PrevFrame.ScaleRect(Buffer.Canvas.ClipRect);
    Buffer.Canvas.CopyMode := cmSrcCopy;
    Buffer.Canvas.CopyRect(r, Background.Canvas, r)
  end else
  begin
    // If the frame isn't transparent, we just clear the area covered by
    // it to the background color.
    Buffer.Canvas.Brush.Color := BackgroundColor;
    Buffer.Canvas.Brush.Style := bsSolid;
    Buffer.Canvas.FillRect(PrevFrame.ScaleRect(Buffer.Canvas.ClipRect));
  end;
end;

procedure TGIFRenderer.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  StartTime: DWORD;
  Delay: integer;
begin
  StartTime := timeGetTime;
  inherited Draw(ACanvas, Rect);

  if (Animate) and (Image.Images.Count > 0) then
  begin
    if (not Animating) then
      StartAnimation;

    if (Animating) and (FNeedTimer) then
    begin
      Delay := FrameDelay-integer(timeGetTime-StartTime);
      if (Delay < 1) then
        Delay := 1;
      StartAnimationTimer(Delay);
    end;
  end;
end;

function TGIFRenderer.GetBitmap: TBitmap;
begin
  RenderFrame;
  Result := FBuffer;
end;

procedure TGIFRenderer.HaltAnimation;
begin
  inherited HaltAnimation;
  StopAnimationTimer;
end;

procedure TGIFRenderer.Initialize;
var
  i, j: integer;
  Disposals: set of TDisposalMethod;
  Ext: TGIFExtension;
  CGE: TGIFGraphicControlExtension;
  LoopFound: Boolean;
begin
  inherited Initialize;

  Buffer.Height := TargetRect.Bottom-TargetRect.Top;
  Buffer.Width := TargetRect.Right-TargetRect.Left;
  Buffer.Canvas.CopyMode := cmSrcCopy;

  // Scan extensions to get max loop count from Netscape Extension
  // If the Netscape Extension is to be used, it is required by specs to be
  // present in the first frame, but lets ignore that and scan all frames for it.
  i := 0;
  LoopFound := False;
  while (not LoopFound) and (i < Image.Images.Count) do
    with Image.Images[i] do
    begin
      for j := 0 to Extensions.Count-1 do
      begin
        Ext := Extensions[j];
        if (Ext is TGIFAppExtNSLoop) then
        begin
          SetLoopMax(TGIFAppExtNSLoop(Ext).Loops);
          LoopFound := True;
          break;
        end;
      end;
      inc(i);
    end;

  // Preprocessing of extensions to determine if we need frame buffers
  Disposals := [];

  if (Animate) then
  begin
    // Need background buffer to clear on loop
    // The GIF specification does not require us to clear on loop, but
    // experience has shown this to bee a good idea.
    if (GIFClearOnLoop) then
      Include(Disposals, dmBackground);

    for i := 0 to Image.Images.Count-1 do
    begin
      CGE := Image.Images[i].GraphicControlExtension;
      if (CGE <> nil) then
        Include(Disposals, CGE.Disposal);
    end;

    // Need background buffer to draw transparent on background
    if (dmBackground in Disposals) and (Transparent) then
    begin
      FBackground := TBitmap.Create;
      Background.Height := Buffer.Height;
      Background.Width := Buffer.Width;

      if (Image.IsTransparent) then
      begin
        // Copy background immediately from target
        Background.Canvas.CopyMode := cmSrcCopy;
        Background.Canvas.CopyRect(Background.Canvas.ClipRect, TargetCanvas, TargetRect);
      end else
      begin
        // Clear background if no frames has transparency
        Background.Canvas.Brush.Style := bsSolid;
        Background.Canvas.Brush.Color := BackgroundColor;
        Background.Canvas.FillRect(Background.Canvas.ClipRect);
      end;
    end else
      FreeAndNil(FBackground);

    // Need backup buffer to restore to previous
    if (dmPrevious in Disposals) then
    begin
      FPreviousBuffer := TBitmap.Create;
      PreviousBuffer.Height := Buffer.Height;
      PreviousBuffer.Width := Buffer.Width;
      PreviousBuffer.Canvas.CopyMode := cmSrcCopy;
      PreviousBuffer.Canvas.Brush.Color := BackgroundColor;
      PreviousBuffer.Canvas.Brush.Style := bsSolid;
      // Initialize with background
      PreviousBuffer.Canvas.CopyRect(PreviousBuffer.Canvas.ClipRect, TargetCanvas, TargetRect);
    end else
      FreeAndNil(FPreviousBuffer);
  end;

  if (Transparent) and (Image.IsTransparent) then
  begin
    Buffer.Canvas.CopyMode := cmSrcCopy;
    Buffer.Canvas.CopyRect(Buffer.Canvas.ClipRect, TargetCanvas, TargetRect);
  end else
  begin
    Buffer.Canvas.Brush.Color := BackgroundColor;
    Buffer.Canvas.Brush.Style := bsSolid;
    Buffer.Canvas.FillRect(Buffer.Canvas.ClipRect);
  end;

end;

procedure TGIFRenderer.Loop;
begin
  inherited Loop;
  // Clear on loop
  if (GIFClearOnLoop) then
    FPrevDisposal := dmBackground;
end;

procedure TGIFRenderer.DoNextFrame;
var
  Ext: TGIFGraphicControlExtension;
  Delay: integer;
begin
  FPrevDisposal := Disposal;
  FPrevFrame := Frame;

  inherited DoNextFrame;

  FNeedRender := True;

  // Get frame options from GCE
  if (Frame <> nil) then
    Ext := Frame.GraphicControlExtension
  else
    Ext := nil;

  if (Ext <> nil) then
  begin
    FDisposal := Ext.Disposal;
    Delay := Ext.Delay;
    if (Delay = 0) then
      Delay := GIFDefaultDelay;
  end else
  begin
    FDisposal := dmNoDisposal;
    Delay := GIFDefaultDelay;
  end;
  // Enforce minimum animation delay in compliance with Mozilla
  if (Delay < GIFMinimumDelay) then
    Delay := GIFMinimumDelay;
  if (Delay > GIFMaximumDelay) then
    Delay := GIFMaximumDelay;

  if (Speed <= 0) then
    Delay := GIFMaximumDelay;

  FFrameDelay := MulDiv(Delay * GIFDelayExp, 100, Speed)
end;

procedure TGIFRenderer.RenderFrame;
var
  SavePal, SourcePal: HPALETTE;
begin
  CheckChange;
  if (not FNeedRender) or (Frame = nil) then
    exit;

  ASSERT((not Animate) or (Disposal <> dmPrevious) or (PreviousBuffer <> nil));
  ASSERT((not Animate) or (not Transparent) or (Disposal <> dmBackground) or (Background <> nil));


  // Save frame as previous
  if (Disposal = dmPrevious) and (PrevDisposal <> Disposal) then
    PreviousBuffer.Canvas.CopyRect(PreviousBuffer.Canvas.ClipRect, Buffer.Canvas, Buffer.Canvas.ClipRect);

  // Remove previous frame from buffer
  {
    Disposal methods:
    dmNone: Same as dmNodisposal
    dmNoDisposal: Do not dispose
    dmBackground: Clear with background color *)
    dmPrevious: Previous image
    *) Note: Background color should either be a BROWSER SPECIFIED Background
       color (DrawBackgroundColor) or the background image if any frames are
       transparent.
  }
  case (PrevDisposal) of
    dmNone, dmNoDisposal:
      // Nothing to do - just leave the buffer as is
      ;
    dmBackground:
      // Restore background
      UndoPreviousFrame;
    dmPrevious:
      // Restore previous frame
      Buffer.Canvas.CopyRect(Buffer.Canvas.ClipRect, PreviousBuffer.Canvas, PreviousBuffer.Canvas.ClipRect)
  end;

  // Draw new frame on buffer
  SourcePal := Frame.Palette;
  if (SourcePal = 0) then
    SourcePal := SystemPalette16; // This should never happen

  SavePal := SelectPalette(Buffer.Handle, SourcePal, False);
  try
    RealizePalette(Buffer.Canvas.Handle);
    Frame.Draw(Buffer.Canvas, Buffer.Canvas.ClipRect, True, False);
  finally
    if (SavePal <> 0) then
      SelectPalette(Buffer.Handle, SavePal, False);
  end;

  FNeedRender := False;
end;

procedure TGIFRenderer.Reset;
begin
  // Note: NextFrame sets FNeedRender if (Image.Images.Count > 0) so no need to do it here
  inherited Reset;

  FPrevDisposal := dmNoDisposal;
end;

procedure TGIFRenderer.StartAnimation;
begin
  if (not Animating) then
  begin
    inherited StartAnimation;
    ASSERT(FAnimationTimer = nil);
    FAnimationTimer := TTimer.Create(nil);
    FAnimationTimer.Enabled := False;
    FAnimationTimer.OnTimer := AnimationTimerEvent;
    FNeedTimer := True;
  end;
end;

procedure TGIFRenderer.StartAnimationTimer(Delay: integer);
begin
  if (Animating) then
  begin
    FAnimationTimer.Interval := Delay;
    FAnimationTimer.Enabled := True;
    FNeedTimer := False;
  end;
end;

procedure TGIFRenderer.StopAnimation;
begin
  if (Animating) then
  begin
    StopAnimationTimer;
    FreeAndNil(FAnimationTimer);
    inherited StopAnimation;
  end;
end;

procedure TGIFRenderer.StopAnimationTimer;
begin
  if (Animating) then
    FAnimationTimer.Enabled := False;
  FNeedTimer := False;
end;

////////////////////////////////////////////////////////////////////////////////
//
//			TGIFImage
//
////////////////////////////////////////////////////////////////////////////////
constructor TGIFImage.Create;
begin
  inherited Create;

  FImages := TGIFImageList.Create(Self);
  FHeader := TGIFHeader.Create(Self);
  FGlobalPalette := 0;
  // Load defaults
  FAnimate := GIFImageDefaultAnimate;
  FDithering := GIFImageDefaultDithering;
  FAnimateLoop := GIFImageDefaultAnimationLoop;
  ColorReduction := GIFImageDefaultColorReduction;
  FReductionBits := GIFImageDefaultColorReductionBits;
  FDitherMode := GIFImageDefaultDitherMode;
  FAnimationSpeed := GIFImageDefaultAnimationSpeed;

  FDrawBackgroundColor := clNone;
  IsDrawing := False;
  IsInsideGetPalette := False;
  NewImage;
  Transparent := True;
end;

destructor TGIFImage.Destroy;
begin
  InternalClear;
  FImages.Free;
  FHeader.Free;
  inherited Destroy;
end;

procedure TGIFImage.Clear;
begin
  InternalClear;
  Changed(Self);
end;

procedure TGIFImage.InternalClear;
begin
  StopDraw;
  FreeBitmap;
  FImages.Clear;
  FHeader.ColorMap.Clear;
  FHeader.Height := 0;
  FHeader.Width := 0;
  FHeader.Prepare;
  Palette := 0;
end;

procedure TGIFImage.NewImage;
begin
  InternalClear;
end;

function TGIFImage.GetVersion: TGIFVersion;
var
  v: TGIFVersion;
  i: integer;
begin
  Result := gvUnknown;
  i := 0;
  while (i < FImages.Count) and (Result < High(TGIFVersion)) do
  begin
    v := FImages[i].Version;
    if (v > Result) then
      Result := v;
    inc(i);
  end;
end;

function TGIFImage.GetColorResolution: integer;
var
  i: integer;
begin
  Result := FHeader.ColorResolution;
  for i := 0 to FImages.Count-1 do
    if (FImages[i].ColorResolution > Result) then
      Result := FImages[i].ColorResolution;
end;

function TGIFImage.GetBitsPerPixel: integer;
var
  i: integer;
begin
  Result := FHeader.BitsPerPixel;
  for i := 0 to FImages.Count-1 do
    if (FImages[i].BitsPerPixel > Result) then
      Result := FImages[i].BitsPerPixel;
end;

function TGIFImage.GetBackgroundColorIndex: BYTE;
begin
  Result := FHeader.BackgroundColorIndex;
end;

procedure TGIFImage.SetBackgroundColorIndex(const Value: BYTE);
begin
  if (Value <> FHeader.BackgroundColorIndex) then
  begin
    StopDraw;
    FHeader.BackgroundColorIndex := Value;
    // Zap all bitmaps
    Pack;
    // Signal change to redraw
    Changed(Self);
  end;
end;

function TGIFImage.GetBackgroundColor: TColor;
begin
  Result := FHeader.BackgroundColor;
end;

procedure TGIFImage.SetBackgroundColor(const Value: TColor);
begin
  if (Value <> FHeader.BackgroundColor) then
  begin
    StopDraw;
    FHeader.BackgroundColor := Value;
    // Zap all bitmaps
    Pack;
    // Signal change to redraw
    Changed(Self);
  end;
end;

function TGIFImage.GetAspectRatio: BYTE;
begin
  Result := FHeader.AspectRatio;
end;

procedure TGIFImage.SetAspectRatio(const Value: BYTE);
begin
  if (Value <> FHeader.AspectRatio) then
  begin
    StopDraw;
    FHeader.AspectRatio := Value;
    // Zap all bitmaps
    Pack;
    // Signal change to redraw
    Changed(Self);
  end;
end;

procedure TGIFImage.SetDithering(const Value: TGIFDithering);
begin
  if (Value <> FDithering) then
  begin
    StopDraw;
    FDithering := Value;
    // Zap all bitmaps
    Pack;
    // Signal change to redraw
    Changed(Self);
  end;
end;

procedure TGIFImage.SetAnimate(const Value: boolean);
begin
  if (Value <> FAnimate) then
  begin
    StopDraw;
    FAnimate := Value;
    Changed(Self);
  end;
end;

procedure TGIFImage.SetAnimateLoop(const Value: TGIFAnimationLoop);
begin
  if (Value <> FAnimateLoop) then
  begin
    FAnimateLoop := Value;
    StopDraw;
    Changed(Self);
  end;
end;

procedure TGIFImage.SetAnimationSpeed(Value: integer);
begin
  if (Value < 0) then
    Value := 0
  else if (Value > 1000) then
    Value := 1000;

  if (Value <> FAnimationSpeed) then
  begin
    SuspendDraw;
    try
      FAnimationSpeed := Value;
    finally
      ResumeDraw;
    end;
  end;
end;

procedure TGIFImage.SetReductionBits(Value: integer);
begin
  if (Value < 3) or (Value > 8) then
    Error(sInvalidBitSize);
  FReductionBits := Value;
end;

procedure TGIFImage.SetTransparent(Value: boolean);
begin
  if (Value <> Transparent) then
  begin
    StopDraw;
    inherited SetTransparent(Value);
    Changed(Self);
  end;
end;

procedure TGIFImage.OptimizeColorMap;
var
  ColorMapOptimizer: TColorMapOptimizer;
begin
  ColorMapOptimizer := TColorMapOptimizer.Create(Self);
  try
    ColorMapOptimizer.Optimize;
  finally
    ColorMapOptimizer.Free;
  end;
  Changed(Self);
end;

procedure TGIFImage.Optimize(Options: TGIFOptimizeOptions;
  ColorReduction: TColorReduction; DitherMode: TDitherMode;
  ReductionBits: integer);

  function Scan(Buf: PByte; Value: Byte; Count: integer): boolean;{$IFNDEF PUREPASCAL} assembler;{$ENDIF}
  {$IFDEF PUREPASCAL}
  var
    C: Integer;
  begin
    for C := 0 to Count - 1 do
    begin
      if Buf[C] = Value then
      begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;
  {$ELSE !PUREPASCAL}
  {$IFDEF CPUX86}
  asm
    PUSH	EDI
    MOV		EDI, Buf
    MOV		ECX, Count
    MOV		AL, Value
    REPNE	SCASB
    MOV		EAX, False
    JNE		@@1
    MOV		EAX, True
@@1:POP		EDI
  end;
  {$ENDIF CPUX86}
  {$ENDIF !PUREPASCAL}

var
  i, j: integer;
  Delay: integer;
  GCE: TGIFGraphicControlExtension;
  ThisRect, NextRect, MergeRect: TRect;
  Prog, MaxProg: integer;
begin
  if (Empty) then
    exit;

  // Prevent any new renderers from starting while we are doing our thing
  SuspendDraw;
  try
    // And stop active renderers
    StopDraw;

    Progress(Self, psStarting, 0, False, Rect(0,0,0,0), sProgressOptimizing);
    try

      Prog := 0;
      MaxProg := Images.Count*8;

      // Sort color map by usage and remove unused entries
      if (ooColorMap in Options) then
      begin
        // Optimize global color map
        if (GlobalColorMap.Count > 0) then
          GlobalColorMap.Optimize;
        // Optimize local color maps
        for i := 0 to Images.Count-1 do
        begin
          inc(Prog);
          if (Images[i].ColorMap.Count > 0) then
          begin
            Images[i].ColorMap.Optimize;
            Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
              Rect(0,0,0,0), sProgressOptimizing);
          end;
        end;
      end;

      // Remove passive elements, pass 1
      if (ooCleanup in Options) then
      begin
        // Check for transparency flag without any transparent pixels
        for i := 0 to Images.Count-1 do
        begin
          inc(Prog);
          if (Images[i].Transparent) then
          begin
            if not(Scan(Images[i].Data,
                        Images[i].GraphicControlExtension.TransparentColorIndex,
                        Images[i].DataSize)) then
            begin
              Images[i].GraphicControlExtension.Transparent := False;
              Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
                Rect(0,0,0,0), sProgressOptimizing);
            end;
          end;
        end;

        // Change redundant disposal modes
        for i := 0 to Images.Count-2 do
        begin
          inc(Prog);
          if (Images[i].GraphicControlExtension <> nil) and
            (Images[i].GraphicControlExtension.Disposal in [dmPrevious, dmBackground]) and
            (not Images[i+1].Transparent) then
          begin
            ThisRect := Images[i].BoundsRect;
            NextRect := Images[i+1].BoundsRect;
            if (not IntersectRect(MergeRect, ThisRect, NextRect)) then
              continue;
            // If the next frame completely covers the current frame,
            // change the disposal mode to dmNone
            if (EqualRect(MergeRect, NextRect)) then
              Images[i].GraphicControlExtension.Disposal := dmNone;
            Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
              Rect(0,0,0,0), sProgressOptimizing);
          end;
        end;
      end else
        inc(Prog, 2*Images.Count);

      // Merge layers of equal pixels (remove redundant pixels)
      if (ooMerge in Options) then
      begin
        // Merge from last to first to avoid interfering with merge
        for i := Images.Count-1 downto 1 do
        begin
          inc(Prog);
          j := i-1;
          // If the "previous" frames uses dmPrevious disposal mode, we must
          // instead merge with the frame before the previous
          while (j > 0) and
            ((Images[j].GraphicControlExtension <> nil) and
             (Images[j].GraphicControlExtension.Disposal = dmPrevious)) do
            dec(j);
          // Merge
          if (j >= 0) then
            Images[i].Merge(Images[j]);
          Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
            Rect(0,0,0,0), sProgressOptimizing);
        end;
      end else
        inc(Prog, Images.Count);

      // Remove passive elements, pass 2
      inc(Prog, Images.Count);
      if (ooCleanup in Options) then
      begin
        for i := Images.Count-1 downto 0 do
        begin
          // Remove comments and application extensions
          for j := Images[i].Extensions.Count-1 downto 0 do
            if (Images[i].Extensions[j] is TGIFCommentExtension) or
              (Images[i].Extensions[j] is TGIFTextExtension) or
              (Images[i].Extensions[j] is TGIFUnknownAppExtension) or
              ((Images[i].Extensions[j] is TGIFAppExtNSLoop) and
               ((i > 0) or (Images.Count = 1))) then
              Images[i].Extensions.Delete(j);
          // Zap frame if it has become empty
          if (Images[i].Empty) and (Images[i].Extensions.Count = 0) then
            Images[i].Free;
        end;
        Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
          Rect(0,0,0,0), sProgressOptimizing);
      end;

      // Crop transparent areas
      if (ooCrop in Options) then
      begin
        for i := Images.Count-1 downto 0 do
        begin
          inc(Prog);
          if (not Images[i].Empty) and (Images[i].Transparent) then
          begin
            // Remember frame's delay in case frame is deleted
            Delay := Images[i].GraphicControlExtension.Delay;
            // Crop
            Images[i].Crop;
            // If the frame was completely transparent we remove it
            if (Images[i].Empty) then
            begin
              // Transfer delay to previous frame in case frame was deleted
              if (i > 0) and (Images[i-1].Transparent) then
                Images[i-1].GraphicControlExtension.Delay :=
                  Images[i-1].GraphicControlExtension.Delay + Delay;
              Images.Delete(i);
            end;
            Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
              Rect(0,0,0,0), sProgressOptimizing);
          end;
        end;
      end else
        inc(Prog, Images.Count);

      // Remove passive elements, pass 3
      inc(Prog, Images.Count);
      if (ooCleanup in Options) then
      begin
        for i := Images.Count-1 downto 0 do
        begin
          if (Images[i].GraphicControlExtension <> nil) then
          begin
            GCE := Images[i].GraphicControlExtension;
            // Zap GCE if all of the following are true:
            // * No delay or only one image
            // * Not transparent
            // * No prompt
            // * No disposal or only one image
            if ((GCE.Delay = 0) or (Images.Count = 1)) and
              (not GCE.Transparent) and
              (not GCE.UserInput) and
              ((GCE.Disposal in [dmNone, dmNoDisposal]) or (Images.Count = 1)) then
            begin
              GCE.Free;
            end;
          end;
          // Zap frame if it has become empty
          if (Images[i].Empty) and (Images[i].Extensions.Count = 0) then
            Images[i].Free;
        end;
        Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
          Rect(0,0,0,0), sProgressOptimizing);
      end;

      // Reduce color depth
                                               
      (*
      inc(Prog, Images.Count);
      if (ooReduceColors in Options) then
      begin
        { TODO -oanme -cBug :
        Quantizer doesn't handle transparency.
        Merge assumes that frames will not be modified.
        Option is not non-destructive!
        Transparency color is not remapped. }
        if (ColorReduction = rmPalette) then
          Error(sInvalidReduction);
        ReduceGIFColors(Self, ColorReduction, DitherMode, ReductionBits, 0);
        Progress(Self, psRunning, MulDiv(Prog, 100, MaxProg), False,
          Rect(0,0,0,0), sProgressOptimizing);
      end;
      *)
    finally
      if ExceptObject = nil then
        i := 100
      else
        i := 0;
      Progress(Self, psEnding, i, False, Rect(0,0,0,0), sProgressOptimizing);
    end;
  finally
    ResumeDraw;
  end;
  Changed(Self);
end;

procedure TGIFImage.Pack;
var
  i: integer;
begin
  // Zap bitmaps and palettes
  FreeBitmap;
  Palette := 0;
  for i := 0 to FImages.Count-1 do
  begin
    FImages[i].Bitmap := nil;
    FImages[i].Palette := 0;
  end;

  // Only pack if no global colormap and a single image
  if (FHeader.ColorMap.Count > 0) or (FImages.Count <> 1) then
    exit;

  // Copy local colormap to global
  FHeader.ColorMap.Assign(FImages[0].ColorMap);
  // Zap local colormap
  FImages[0].ColorMap.Clear;
  Changed(Self);
end;

procedure TGIFImage.ResumeDraw;
begin
  dec(FDrawSuspendCount);

  if (FDrawSuspendCount = 0) then
  begin
    if (FRenderer <> nil) and (FSuspendedAnimation) then
      FRenderer.StartAnimation;
    FSuspendedAnimation := False;
  end;
end;

procedure TGIFImage.SaveToStream(Stream: TStream);
var
  Prog: Integer;
begin
  Prog := 0;
  Progress(Self, psStarting, Prog, False, Rect(0,0,0,0), sProgressSaving);
  try
    // Write header
    FHeader.SaveToStream(Stream);
    // Write images
    FImages.SaveToStream(Stream);
    // Write trailer
    with TGIFTrailer.Create(Self) do
      try
        SaveToStream(Stream);
      finally
        Free;
      end;
    // Signal that image changes has been saved
    Modified := False;
    Prog := 100;
  finally
    Progress(Self, psEnding, Prog, True, Rect(0,0,0,0), sProgressSaving);
  end;
end;

class function TGIFImage.CanLoadFromStream(Stream: TStream): Boolean;
var
  GifHeader: TGIFHeaderRec;
  P: Int64;
begin
  P := Stream.Position;
  try
    Result := (Stream.Read(GifHeader, SizeOf(GifHeader)) = SizeOf(GifHeader)) and
      (uppercase(string(GifHeader.Signature)) = 'GIF');
  finally
    Stream.Position := P;
  end;
end;

procedure TGIFImage.LoadFromStream(Stream: TStream);
var
  Prog: Integer;
  Position: integer;
begin
  Prog := 0;
  Progress(Self, psStarting, Prog, False, Rect(0,0,0,0), sProgressLoading);
  try
    // Zap old image
    InternalClear;
    Position := Stream.Position;
    try
      // Read header
      FHeader.LoadFromStream(Stream);
      // Read images
      FImages.LoadFromStream(Stream);
      // Read trailer
      with TGIFTrailer.Create(Self) do
        try
          LoadFromStream(Stream);
        finally
          Free;
        end;
      // Signal that image has changed
      Changed(Self);
    except
      // Restore stream position in case of error.
      // Not required, but "a nice thing to do"
      Stream.Position := Position;
      raise;
    end;
    Prog := 100;
  finally
    Progress(Self, psEnding, Prog, True, Rect(0,0,0,0), sProgressLoading);
  end;
end;

function TGIFImage.GetBitmap: TBitmap;
var
  Color: TColor;
begin
  if not(Empty) then
  begin
    Result := FBitmap;
    if (Result <> nil) then
      exit;
    FBitmap := TBitmap.Create;
    Result := FBitmap;
    FBitmap.OnChange := Changed;
    // Use first image as default
    if (Images.Count > 0) then
    begin
      if (Images[0].Width = Width) and (Images[0].Height = Height) and
        (not Images[0].Transparent) then
      begin
        // Use first image as it has same dimensions
        FBitmap.Assign(Images[0].Bitmap);
      end else
      begin
        // Draw first image on bitmap
        FBitmap.Palette := 0;
        FBitmap.Height := Height;
        FBitmap.Width := Width;
        FBitmap.Palette := CopyPalette(Images[0].Bitmap.Palette);
        // Clear background
        if (Images[0].Transparent) then
        begin
          Color := EffectiveBackgroundColor;
          FBitmap.Canvas.Brush.Style := bsSolid;
          FBitmap.Canvas.Brush.Color := Color;
                                                                                                                               
                                                                   
          FBitmap.Canvas.FillRect(FBitmap.Canvas.ClipRect);
        end;
                                                             
                                                                                
        Images[0].Draw(FBitmap.Canvas, FBitmap.Canvas.ClipRect, True, False);
      end;
    end;
  end else
    Result := nil
end;

// Create a new (empty) bitmap
function TGIFImage.NewBitmap: TBitmap;
begin
  Result := FBitmap;
  if (Result <> nil) then
    exit;
  FBitmap := TBitmap.Create;
  Result := FBitmap;
  FBitmap.OnChange := Changed;
  // Draw first image on bitmap
  FBitmap.Palette := CopyPalette(Palette);
  FBitmap.Height := Height;
  FBitmap.Width := Width;
end;

procedure TGIFImage.Dormant;
var
  i: integer;
begin
  FreeBitmap;
  FreePalette;
  for i := 0 to FImages.Count-1 do
    FImages[i].Dormant;
end;

procedure TGIFImage.FreePalette;
begin
  if (FGlobalPalette <> 0) then
  begin
    DeleteObject(FGlobalPalette);
    FGlobalPalette := 0;
  end;
end;

procedure TGIFImage.FreeBitmap;
begin
  StopDraw;

  FreeAndNil(FBitmap);
end;

function TGIFImage.Add(Source: TPersistent): TGIFFrame;
var
  i: integer;
begin
  Result := nil; // To avoid compiler warning

  if (Source is TGIFImage) then
  begin
    // Add frames of source GIF
    for i := 0 to TGIFImage(Source).Images.Count-1 do
    begin
      Result := TGIFFrame.Create(Self);
      try
        // Copy frame data
        Result.Assign(TGIFImage(Source).Images[i]);
      except
        Result.Free;
        raise;
      end;
    end;
  end else

  if (Source is TGraphic) then
  begin
    Result := TGIFFrame.Create(Self);
    try
      Result.Assign(Source);
    except
      Result.Free;
      raise;
    end;
  end else

  if (Source is TGIFFrame) then
  begin
    Result := TGIFFrame.Create(Self);
    try
      Result.Assign(TGIFFrame(Source));
    except
      Result.Free;
      raise;
    end;
  end else
    Error(sUnsupportedClass);

  FreeBitmap;
  Changed(Self);
end;

function TGIFImage.GetEmpty: Boolean;
begin
  Result := (Images.Count = 0);
end;

function TGIFImage.GetHeight: Integer;
begin
  Result := FHeader.Height;
end;

function TGIFImage.GetWidth: Integer;
begin
  Result := FHeader.Width;
end;

function TGIFImage.GetIsTransparent: Boolean;
var
  i: integer;
begin
  Result := False;
  i := 0;
  while (not Result) and (i < Images.Count) do
  begin
    Result := Images[i].Transparent;
    inc(i);
  end;
end;

function TGIFImage.Equals(Graphic: TGraphic): Boolean;
begin
  Result := (Graphic = Self);
end;

function TGIFImage.GetPalette: HPALETTE;
begin
  // Check for recursion
  // (TGIFImage.GetPalette->TGIFFrame.GetPalette->TGIFImage.GetPalette etc...)
  if (IsInsideGetPalette) then
    Error(sNoColorTable);
  IsInsideGetPalette := True;
  try
    Result := 0;
    if (FBitmap <> nil) and (FBitmap.Palette <> 0) then
      // Use bitmaps own palette if possible
      Result := FBitmap.Palette
    else if (FGlobalPalette <> 0) then
      // Or a previously exported global palette
      Result := FGlobalPalette
    else if (ShouldDither) then
    begin
      // or create a new dither palette
      FGlobalPalette := WebPalette;
      Result := FGlobalPalette;
    end else
    if (FHeader.ColorMap.Count > 0) then
    begin
      // or create a new if first time
      FGlobalPalette := FHeader.ColorMap.ExportPalette;
      Result := FGlobalPalette;
    end else
    if (FImages.Count > 0) then
      // This can cause a recursion if no global palette exist and image[0]
      // hasn't got one either. Checked by the IsInsideGetPalette semaphor.
      Result := FImages[0].Palette;
  finally
    IsInsideGetPalette := False;
  end;
end;

procedure TGIFImage.SetPalette(Value: HPalette);
var
  NeedNewBitmap: boolean;
begin
  if (Value <> FGlobalPalette) then
  begin
    StopDraw;
    // Zap old palette
    if (FGlobalPalette <> 0) then
      DeleteObject(FGlobalPalette);

    // Zap bitmap unless new palette is same as bitmaps own
    NeedNewBitmap := (FBitmap <> nil) and (Value <> FBitmap.Palette);

    // Use new palette
    FGlobalPalette := Value;

    if (NeedNewBitmap) then
    begin
      // Need to create new bitmap and repaint
      FreeBitmap;
      PaletteModified := True;
      Changed(Self);
    end;
  end;
end;

procedure TGIFImage.SetHeight(Value: Integer);
var
  i: integer;
  n: integer;
begin
  if (Value < 0) or (Value > High(Word)) then
    Value := 0;
  for i := 0 to Images.Count-1 do
  begin
    n := Images[i].Top + Images[i].Height;
    if (n > Value) then
      Value := n;
  end;

  if (Value <> FHeader.Height) then
  begin
    StopDraw;
    FHeader.Height := Value;
    FreeBitmap;
    Changed(Self);
  end;
end;

procedure TGIFImage.SetWidth(Value: Integer);
var
  i: integer;
  n: integer;
begin
  if (Value < 0) or (Value > High(Word)) then
    Value := 0;
  for i := 0 to Images.Count-1 do
  begin
    n := Images[i].Left + Images[i].Width;
    if (n > Value) then
      Value := n;
  end;

  if (Value <> FHeader.Width) then
  begin
    StopDraw;
    FHeader.Width := Value;
    FreeBitmap;
    Changed(Self);
  end;
end;

procedure TGIFImage.StopDraw;
begin
  if (FRenderer <> nil) then
  begin
    FRenderer.StopAnimation;
    FSuspendedAnimation := False;
    FreeAndNil(FRenderer);
  end;
end;

procedure TGIFImage.SuspendDraw;
begin
  inc(FDrawSuspendCount);

  if (FDrawSuspendCount = 1) then
  begin
    if (FRenderer <> nil) then
    begin
      FSuspendedAnimation := FRenderer.Animating;
      FRenderer.StopAnimation;
    end else
      FSuspendedAnimation := False;
  end;
end;

procedure TGIFImage.WriteData(Stream: TStream);
begin
  // Perform non destructive optimization on image when
  // saved to stream (usually at design time)
  if (GIFImageOptimizeOnStream) then
    Optimize([ooCrop, ooMerge, ooCleanup, ooColorMap], rmNone);

  inherited WriteData(Stream);
end;

procedure TGIFImage.AssignTo(Dest: TPersistent);
begin
  if (Dest is TBitmap) then
    Dest.Assign(Bitmap)
  else
    try
      // Attempt conversion from TBitmap to destination format...
      Dest.Assign(Bitmap)
    except
      // ...If that fails, we give up and let our ancestor give it a try
      on E: EConvertError do
        inherited AssignTo(Dest);
    end;
end;

procedure TGIFImage.Assign(Source: TPersistent);
var
  i: integer;
  Image: TGIFFrame;
  ClipPic: TPicture;
  Prog: integer;
begin
  if (Source = Self) then
    exit;
  if (Source = nil) then
  begin
    InternalClear;
  end else
  //
  // TGIFImage import
  //
  if (Source is TGIFImage) then
  begin
    InternalClear;
    // Temporarily copy event handlers to be able to generate progress events
    // during the copy and handle copy errors
    OnProgress := TGIFImage(Source).OnProgress;
    try
      FOnWarning := TGIFImage(Source).OnWarning;
      Prog := 0;
      Progress(Self, psStarting, Prog, False, Rect(0,0,0,0), sProgressCopying);
      try
        try
          FHeader.Assign(TGIFImage(Source).FHeader);
          FDrawBackgroundColor := TGIFImage(Source).DrawBackgroundColor;
          FAnimate := TGIFImage(Source).Animate;
          FAnimationSpeed := TGIFImage(Source).AnimationSpeed;
          Transparent := TGIFImage(Source).Transparent;
          FDithering := TGIFImage(Source).Dithering;
          FAnimateLoop := TGIFImage(Source).AnimateLoop;
          FColorReduction := TGIFImage(Source).ColorReduction;
          FDitherMode := TGIFImage(Source).DitherMode;

          for i := 0 to TGIFImage(Source).Images.Count-1 do
          begin
            Add(TGifImage(Source).Images[i]);
            Progress(Self, psRunning, MulDiv(i+1, 100, TGIFImage(Source).Images.Count),
              False, Rect(0,0,0,0), sProgressCopying);
          end;
          Prog := 100
        except
          Prog := 0;
          raise;
        end;
      finally
        Progress(Self, psEnding, Prog, False, Rect(0,0,0,0), sProgressCopying);
      end;
    finally
      // Reset event handlers
      FOnWarning := nil;
      OnProgress := nil;
    end;
  end else
  //
  // Import from clipboard
  //
  if (Source is TClipboard) then
  begin
    TClipboard(Source).Open;
    try
      // Test for image on clipboard
      if (TClipboard(Source).HasFormat(CF_PICTURE)) then
      begin
        ClipPic := TPicture.Create;
        try
          // Load TGraphic from clipboard
          ClipPic.Assign(Source);
          // Recurse to assign TGraphic
          Assign(ClipPic.Graphic);
        finally
          ClipPic.Free;
        end;
      end else
        inherited Assign(Source);
    finally
      TClipboard(Source).Close;
    end;
  end else
  //
  // TPicture import
  //
  if (Source is TPicture) then
  begin
    // Recursively import TGraphic
    Assign(TPicture(Source).Graphic);
  end else
  //
  // Everything else is handled via TGIFFrame.Assign
  //
  begin
    InternalClear;
    Image := TGIFFrame.Create(Self);
    try
      try
      Image.Assign(Source);
                                                                              
    except
        Image.Free;
        raise;
      end;
    except
      on E: EConvertError do
        // Unsupported format - fall back to Source.AssignTo
        inherited Assign(Source);
      end;
  end;
  Changed(Self);
end;

procedure TGIFImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  Size: Longint;
  Buffer: Pointer;
  Stream: TMemoryStream;
  Bmp: TBitmap;
begin
  if (AData = 0) then
    AData := GetClipboardData(AFormat);
  if (AData <> 0) and (AFormat = CF_GIF) then
  begin
    // Get size and pointer to data
    Size := GlobalSize(AData);
    Buffer := GlobalLock(AData);
    try
      Stream := TMemoryStream.Create;
      try
        // Copy data to a stream
        Stream.SetSize(Size);
        Move(Buffer^, Stream.Memory^, Size);
        // Load GIF from stream
        LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    finally
      GlobalUnlock(AData);
    end;
  end else
  if (AData <> 0) and (AFormat = CF_BITMAP) then
  begin
    // No GIF on clipboard - try loading a bitmap instead
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromClipboardFormat(AFormat, AData, APalette);
      Assign(Bmp);
    finally
      Bmp.Free;
    end;
  end else
    Error(sUnknownClipboardFormat);
end;

procedure TGIFImage.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
var
  Stream: TMemoryStream;
  Buffer: Pointer;
begin
  APalette := 0;
  if (Empty) then
  begin
    AFormat := 0;
    AData := 0;
    exit;
  end;
  AFormat := CF_GIF;
  // Store GIF in clipboard format
  Stream := TMemoryStream.Create;
  try
    // Save the GIF to a memory stream
    SaveToStream(Stream);
    // Allocate some memory for the GIF data
    AData := GlobalAlloc(HeapAllocFlags, Stream.Size);
    try
      if (AData <> 0) then
      begin
        Buffer := GlobalLock(AData);
        try
          // Copy GIF data from stream memory to clipboard memory
          Move(Stream.Memory^, Buffer^, Stream.Size);
        finally
          GlobalUnlock(AData);
        end;
      end;
    except
      GlobalFree(AData);
      raise;
    end;
  finally
    Stream.Free;
  end;
end;

function TGIFImage.GetColorMap: TGIFColorMap;
begin
  Result := FHeader.ColorMap;
end;

function TGIFImage.GetDoDither: boolean;
begin
  Result := (Dithering = gdEnabled) or ((Dithering = gdAuto) and DoAutoDither);
end;

function TGIFImage.EffectiveBackgroundColor: TColor;
var
  ColorLookup: TNetscapeColorLookup;
  RGB: TGIFColor;
begin
  if (DrawBackgroundColor = clNone) then
  begin
    if (GlobalColorMap.Count > 0) then
      Result := BackgroundColor
    else
      Result := ColorToRGB(clWindow);
  end else
    Result := ColorToRGB(DrawBackgroundColor);
  if (ShouldDither) then
  begin
                                                                                            
    // Map color to Netscape 216 color palette
    ColorLookup := TNetscapeColorLookup.Create(0);
    try
      RGB := TGIFColorMap.Color2RGB(Result);
      ColorLookup.Lookup(RGB.Red, RGB.Green, RGB.Blue,
        RGB.Red, RGB.Green, RGB.Blue);
      Result := TGIFColorMap.RGB2Color(RGB);
    finally
      ColorLookup.Free;
    end;
  end;
end;

function TGIFImage.CreateRenderer: TCustomGIFRenderer;
begin
  Result := TGIFRenderer.Create(Self);
  Result.Speed := AnimationSpeed;
  Result.Transparent := Transparent;
  Result.BackgroundColor := EffectiveBackgroundColor;
  Result.Animate := Animate;
end;

procedure TGIFImage.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  // Prevent recursion(s(s(s)))
  if (IsDrawing) or (Images.Count = 0) then
    exit;

  IsDrawing := True;
  try
    if (Images.Count = 1) or (not Animate) then
    begin
      if (not Transparent) and (Images[0].Transparent) then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := EffectiveBackgroundColor;
        ACanvas.FillRect(Rect);
      end;
      Images[0].Draw(ACanvas, Rect, Transparent, False);
    end else
    begin
      if (FRenderer = nil) then
        FRenderer := CreateRenderer;
      FRenderer.Draw(ACanvas, Rect);
    end;
  finally
    IsDrawing := False;
  end;
end;

procedure TGIFImage.Warning(Sender: TObject; Severity: TGIFSeverity; const Msg: string);
begin
  if (Assigned(FOnWarning)) then
    FOnWarning(Sender, Severity, Msg)
  else
    if (Severity = gsError) then
      Error(Msg);
end;

var
  DesktopDC: HDC;

////////////////////////////////////////////////////////////////////////////////
//
//			Initialization
//
////////////////////////////////////////////////////////////////////////////////

initialization
  TPicture.RegisterFileFormat('GIF', sGIFImageFile, TGIFImage);  // Do not localize
  CF_GIF := RegisterClipboardFormat(PChar(sGIFImageFile));
  TPicture.RegisterClipboardFormat(CF_GIF, TGIFImage);

  DesktopDC := GetDC(0);
  try
    PaletteDevice := (GetDeviceCaps(DesktopDC, BITSPIXEL) * GetDeviceCaps(DesktopDC, PLANES) <= 8);
    DoAutoDither := PaletteDevice;
  finally
    ReleaseDC(0, DesktopDC);
  end;

////////////////////////////////////////////////////////////////////////////////
//
//			Finalization
//
////////////////////////////////////////////////////////////////////////////////
finalization
  ExtensionList.Free;
  AppExtensionList.Free;
  TPicture.UnregisterGraphicClass(TGIFImage);
end.


