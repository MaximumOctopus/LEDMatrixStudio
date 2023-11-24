// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2023
//   www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
//   https://github.com/MaximumOctopus/LEDMatrixStudio
//
//   https://maximumoctopus.hashnode.dev/
//
//   C++ Rewrite October 11th 2023
//
// ===================================================================

#include <algorithm>
#include <fstream>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Imaging.GIFImg.hpp>

#include "CalcUtility.h"
#include "ColourUtility.h"
#include "Convert.h"
#include "Formatting.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "TheMatrix.h"
#include "Utility.h"

extern LanguageHandler *GLanguageHandler;


TheMatrix::TheMatrix(TComponent *owner, TWinControl *Zig)
{
	Owner = owner; // cache it for later!
	Canvas = Zig;

	PaintBox = new TPaintBox(owner);
	PaintBox->Parent = Canvas;
   //	PaintBox->OnPaint = PaintBoxUpdate;

	TextFont = new Font();

	InitPreviewBox(owner, Zig, false);

	// ===========================================================================

	MatrixBackup = new Matrix(__MaxWidth, __MaxHeight, Details.Mode, RGBBackground);
	MatrixCopy = new Matrix(__MaxWidth, __MaxHeight, Details.Mode, RGBBackground);
	MatrixRender = new Matrix(__MaxWidth, __MaxHeight, Details.Mode, RGBBackground);
	DisplayBuffer = new Matrix(__MaxWidth, __MaxHeight, Details.Mode, RGBBackground);
	MatrixMerge = new Matrix(__MaxWidth, __MaxHeight, Details.Mode, RGBBackground);
	MatrixDeadLayout = new MatrixDead(__MaxWidth, __MaxHeight);

	// ===========================================================================

	Layer *layer = new Layer(GLanguageHandler->Text[kBottomLayer]);
	MatrixLayers.push_back(layer);

	// ===========================================================================

	ScrollHorizontal = new TScrollBar(Owner);
	ScrollHorizontal->Parent   = Canvas;
	ScrollHorizontal->Align    = alBottom;
	ScrollHorizontal->Kind     = sbHorizontal;
	ScrollHorizontal->Name     = "FSH";
	ScrollHorizontal->Min      = 0;
	ScrollHorizontal->OnChange = ScrollBarHorizontalChange;
	ScrollHorizontal->Visible  = False;

	ScrollVertical = new TScrollBar(Owner);
	ScrollVertical->Parent   = Canvas;
	ScrollVertical->Align    = alRight;
	ScrollVertical->Kind     = sbVertical;
	ScrollVertical->Name     = "FSV";
	ScrollVertical->Min      = 0;
	ScrollVertical->OnChange = ScrollBarVerticalChange;
	ScrollVertical->Visible  = False;

	// ===========================================================================


	for (int x = 0; x < 10; x++)
	{
		Matrix *m = new Matrix(__MaxWidth, __MaxHeight, Details.Mode, RGBBackground);

		MatrixUser.push_back(m);
	}

	// ===========================================================================

	//PaintBox->OnMouseDown = ClickPixel;
	//PaintBox->OnMouseMove = Shape1MouseMove;
	//PaintBox->OnMouseUp   = Shape1MouseUp;

  // ===========================================================================

	CurrentFrame = 0;
	CurrentLayer = 0;

	ClearAllMatrixData(false);
}


TheMatrix::~TheMatrix()
{
	delete PreviewBox;

	delete TextFont;
}


void TheMatrix::InitPreviewBox(TComponent *Owner, TWinControl *WinControl, bool Visible)
{
	PreviewBox = new TPaintBox(Owner);
	PreviewBox->Parent = WinControl;
	PreviewBox->Visible = Visible;
	PreviewBox->Top = 0;
	PreviewBox->Left = 0;
	PreviewBox->OnMouseDown = &OnPreviewBoxMouseDown;

	PreviewBox->Canvas->Pen->Color = clBtnFace;

	PreviewCanvas = WinControl;

	SetPreviewBoxSize(Preview.Size);
}


void TheMatrix::NewMatrix(MatrixMode matrixmode, int framecount, int top, int left, int width, int height, int pixelsize,
						  PixelShape pixelshape, bool grid, bool readonly, bool clearall,
						  int backgroundcolour)
{
	CurrentFrame        = 0;
	LightBox            = 0;
	DeadPixelsMode      = false;

	AnimPlaying          = false;

	Render.TopLeft.X      = 0;
	Render.TopLeft.Y      = 0;
	Render.BottomRight.X  = width - 1;
	Render.BottomRight.Y  = height - 1;
	Render.ViewWindow.X   = width - 1;
	Render.ViewWindow.Y   = height - 1;

	Render.Draw.Mode        = DrawMode::kNone;
	Render.Draw.Point       = CDrawPointNone;
	Render.Draw.Colour      = 0;
	Render.Draw.Coords[0].X = -1;
	Render.Draw.Coords[0].Y = -1;
	Render.Draw.CopyPos.X   = 0;
	Render.Draw.CopyPos.Y   = 0;

	LastX                = -1;
	LastY                = -1;

	PaintBox->Top        = top;
	PaintBox->Left       = left;
	PaintBox->Width      = width * pixelsize;
	PaintBox->Height     = height * pixelsize;

	PreviewBox->Top       = top;

	Details.Width         = width;
	Details.Height        = height;
	Details.Mode          = matrixmode;
	Render.Gradient.Option = GradientOption::kOff;
	Render.Shape     = pixelshape;
	BrushSize Brush = BrushSize::kSmall;
	MatrixReadOnly      = readonly;
	RGBBackground       = backgroundcolour;

	Render.PixelSize = pixelsize;

	Details.Grid          = grid;

	if (grid)
	{
		Render.PixelSizeZ = Render.PixelSize - 1;
	}
	else
	{
		Render.PixelSizeZ = Render.PixelSize;
	}

	// ===========================================================================

	if (clearall)
	{
		SetDeadPixels(PixelAlive);
	}

	// ===========================================================================

	if (Details.Mode == MatrixMode::kRGB)
	{
		Render.Gradient.Clear(RGBBackground);
	}
	else
	{
		Render.Gradient.Clear(0);
	}

	// ===========================================================================

	ConfigurePaintboxDrawing();

	// ===========================================================================

	if (clearall)
	{
		ClearAllMatrixData(true);

		Details.Comment = L"";
	}

	while (MatrixLayers[CPermanentLayer]->Cells.size() < framecount)
	{
		InsertBlankFrameAt(0);
	}

	Details.Available = true;

	SetPreviewBoxSize(Preview.Size);

	if (OnChange) OnChange(this);
}


void TheMatrix::Refresh()
{
	PaintBox->Invalidate();
}


#pragma region Rendering
// merges all layers to a single layer. pixels "rain" down from top (highest index)
// to bottom (lowest index)
// colours = 0 ; retain grid value
// colours = 1 ; convert to colour for render
// colours = 2 ; convert for file output
void TheMatrix::BuildMergedFrame(int frame, int colours)
{
	MatrixMerge->ClearColour(RGBBackground); // to do check that works for single colour

	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		if (MatrixLayers[layer]->Visible)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				for (int y = 0; y < Details.Height; y++)
				{
					switch(Details.Mode)
					{
					case MatrixMode::kMono:
					case MatrixMode::kBiSequential:
					case MatrixMode::kBiBitplanes:
					{
						switch (MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x])
						{
						case 0:
							break;
						case 1:
						case 2:
						case 3:
							switch (colours)
							{
							case 0:
								MatrixMerge->Grid[y * Details.Width + x] = MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x];
								break;
							case 1:
								MatrixMerge->Grid[y * Details.Width + x] = LEDColours[MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x]];
								break;
							case 2:
								switch (MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x])
								{
								case 0:
									MatrixMerge->Grid[y * Details.Width + x] = 0x00000000;
									break;
								case 1:
									MatrixMerge->Grid[y * Details.Width + x] = 0x00ffffff;
									break;
								case 2:
									MatrixMerge->Grid[y * Details.Width + x] = LEDColours[CMouseMiddle];
									break;
								case 3:
									MatrixMerge->Grid[y * Details.Width + x] = LEDColours[CMouseRight];
									break;
								}
							}
							break;
						}
						break;
					}
					case MatrixMode::kRGB:
						if (MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] != RGBBackground)
						{
							MatrixMerge->Grid[y * Details.Width + x] = MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x];
						}
						break;
					case MatrixMode::kRGB3BPP:
					{
						if (MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] != RGBBackground)
						{
							switch (colours)
							{
							case 0:
								MatrixMerge->Grid[y * Details.Width + x] = MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x];
								break;
							case 1:
							case 2:
								MatrixMerge->Grid[y * Details.Width + x] = LEDRGB3BPPColours[MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x]];
								break;
							}
						}
						break;
					}
					}
				}
			}
		}
	}
}


void TheMatrix::CopyCurrentFrameToDrawBuffer()
{
	if (Busy) return;

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			DisplayBuffer->Grid[y * Details.Width + x] = MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x];
		}
	}

	PaintBox->Invalidate();
}


void TheMatrix::CopyDrawBufferToCurrentFrame()
{
	if (Busy) return;

	if (!MatrixLayers[CurrentLayer]->Visible) return;

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = DisplayBuffer->Grid[y * Details.Width + x];
		}
	}

	PaintBox->Invalidate();

	if (OnDisplayBufferCopied && Details.Available)
	{
		OnDisplayBufferCopied(this);
	}
}
#pragma end_region


#pragma region Clear
void TheMatrix::ClearCurrentFrame()
{
	for (int l = 0; l < MatrixLayers.size(); l++)
	{
		if (!IsThisFrameLocked(l, CurrentFrame))
		{
			if (l == CurrentLayer)
			{
				DisplayBuffer->Clear(Details.Mode, RGBBackground);
			}
		}

		MatrixLayers[l]->Cells[CurrentFrame]->Clear(Details.Mode, RGBBackground);

		MatrixLayers[l]->Cells[CurrentFrame]->AddToHistory();
	}

	PaintBox->Invalidate();

	if (OnChange) OnChange(this);
}


void TheMatrix::ClearCurrentLayer()
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame)) return;

	DisplayBuffer->Clear(Details.Mode, RGBBackground);

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Clear(Details.Mode, RGBBackground);

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();

	PaintBox->Invalidate();

	if (OnChange) OnChange(this);
}


void TheMatrix::ClearFrame(int frame)
{
	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		if (!IsThisFrameLocked(layer, frame))
		{
			if (layer == CurrentLayer && frame == CurrentFrame)
			{
				DisplayBuffer->Clear(Details.Mode, RGBBackground);
			}

			MatrixLayers[layer]->Cells[frame]->Clear(Details.Mode, RGBBackground);

			MatrixLayers[layer]->Cells[frame]->AddToHistory();
		}
	}

	PaintBox->Invalidate();

	if (OnChange) OnChange(this);
}


void TheMatrix::ClearAllMatrixData(bool addfirstframe)
{
	DisplayBuffer->Clear(Details.Mode, RGBBackground);

	while (MatrixLayers.size() > 1)
	{
		MatrixLayers.pop_back();
	}

	MatrixLayers[CPermanentLayer]->Cells.clear();
	MatrixLayers[CPermanentLayer]->Name = GLanguageHandler->Text[kBottomLayer];

	if (addfirstframe)
	{
    	Matrix *m1 = new Matrix(__MaxWidth, __MaxHeight, Details.Mode, RGBBackground);
		MatrixLayers[CPermanentLayer]->Cells.push_back(m1);
	}

	CurrentFrame = 0;
	CurrentLayer = 0;

   //	MatrixLayers[CPermanentLayer]->Cells[1]->History.clear();
	//MatrixLayers[CPermanentLayer]->Cells[1]->AddToHistory();

   //	MatrixLayers[CPermanentLayer]->Cells[1]->Locked = False;

	if (OnChange) OnChange(this);

	if (OnLayerChange) OnLayerChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::WipeAllFramesCurrentLayer()
{
	for (int frame = 0; frame < MatrixLayers[CurrentLayer]->Cells.size(); frame++)
	{
		if (!IsThisFrameLocked(CurrentLayer, frame))
		{
			MatrixLayers[CurrentLayer]->Cells[frame]->Clear(Details.Mode, RGBBackground);
		}
	}

	if (OnChange) OnChange(this);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}


void TheMatrix::WipeAllFramesAllLayers()
{
	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		for (int frame = 0; frame < MatrixLayers[layer]->Cells.size(); frame++)
		{
			if (!IsThisFrameLocked(layer, frame))
			{
				MatrixLayers[layer]->Cells[frame]->Clear(Details.Mode, RGBBackground);
			}
		}
	}

	if (OnChange) OnChange(this);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}


void TheMatrix::ClearAllFramesGradient(int mode)
{
	for (int frame = 0; frame < MatrixLayers[CurrentLayer]->Cells.size(); frame++)
	{
		if (!IsThisFrameLocked(CurrentLayer, frame))
		{
			for (int x = 0; x < Details.Width; x++)
			{
				for (int y = 0; y < Details.Height; y++)
				{
					if (mode == 1)
					{
						if (Details.Mode == MatrixMode::kRGB || Details.Mode == MatrixMode::kRGB3BPP)
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + x] = Render.Gradient.IY[y];
						}
						else
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + x] = LEDColours[Render.Gradient.IY[y]];
						}
					}
					else
					{
						if (Details.Mode == MatrixMode::kRGB || Details.Mode == MatrixMode::kRGB3BPP)
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + x] = Render.Gradient.IX[x];
						}
						else
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + x] = LEDColours[Render.Gradient.IX[x]];
						}
					}
				}
			}
		}
	}

	if (OnChange) OnChange(this);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region Preview
// radial, semi-circle only
int TheMatrix::GetPreviewPixelSize(int ROffset)
{
	// calculate circumference at ROffSet pixels from centre = 2 * pi * ROffSet
	// this is the circumference of the smallest part of the display, we need
	// to make sure that the pixels fit in this distance
	// Divide this by the number of pixels and we get the maximium pixel size.

	int pixel_size = 1;

	double c = (2 * 3.1415926535 * ROffset);

	switch (Preview.View)
	{
	case ViewShape::kSquare:
		pixel_size = std::round(c / Details.Width);
		break;
	case ViewShape::kRadial:
		pixel_size = std::round(c / Details.Width);
		break;
	case ViewShape::kRadial3Q:
		pixel_size = std::round(c / Details.Width);
		break;
	case ViewShape::kSemiCircle:
		pixel_size = std::round(c / (2 * Details.Width));
		break;
	case ViewShape::kSemiCircleInverted:
		pixel_size = std::round(c / (2 * Details.Width));
		break;
	}

	if (pixel_size == 0)
	{
		pixel_size = 1;
    }

	return pixel_size;
}


void TheMatrix::SetPreviewBoxSize(int size)
{
	if (PreviewPopout)
	{
		TPanel *panel = (TPanel*)PreviewBox->Parent;

		PreviewBox->Width     = panel->Width;
		PreviewBox->Height    = panel->Height;

		int s = std::min(std::round(PreviewBox->Width / Details.Width), std::round(PreviewBox->Height / Details.Height));

		Preview.Size = s;
	}
	else
	{
		Preview.Size = size;

		PreviewBox->Width = (Details.Width) * Preview.Size;
		PreviewBox->Height = (Details.Height) * Preview.Size;
	}

	if (!Details.Available) return;

	switch (Preview.View)
	{
	case ViewShape::kSquare:
		PreviewBox->OnPaint     = pbPreviewPaint;
		break;
	case ViewShape::kRadial:
		PreviewBox->OnPaint     = pbPreviewPaintRadial;

		Preview.RPixel = GetPreviewPixelSize(Preview.ROffset);
		break;
	case ViewShape::kRadial3Q:
		PreviewBox->OnPaint     = pbPreviewPaintRadialThreeQuarters;

		Preview.RPixel = GetPreviewPixelSize(Preview.ROffset);
		break;
	case ViewShape::kSemiCircle:
		PreviewBox->OnPaint     = pbPreviewPaintSemiCircle;

		Preview.RPixel = GetPreviewPixelSize(Preview.ROffset);
		break;
	case ViewShape::kSemiCircleInverted:
		PreviewBox->OnPaint     = pbPreviewPaintSemiCircleInverted;

		Preview.RPixel = GetPreviewPixelSize(Preview.ROffset);
		break;
	}

	if (Preview.View != ViewShape::kSquare && !PreviewPopout)
	{
		int s = std::max(PreviewBox->Width, PreviewBox->Height);

		PreviewBox->Width = s;
		PreviewBox->Height = s;
	}

	if (PreviewPopout)
	{
		PreviewBox->Left = 0;
	}
	else
	{
		PreviewBox->Left = CLeftOffset + (Render.PixelSize * (Details.Width)) + 20;
	}

	PreviewBox->Invalidate();
}


void TheMatrix::SetPreviewIncrementRadially(bool increment)
{
	Preview.IncrementRadially = increment;

	PreviewBox->Invalidate();
}


void TheMatrix::SetPreviewActive(bool active)
{
	Preview.Active = active;
	PreviewBox->Visible = active;

	if (active)
	{
		SetPreviewBoxSize(Preview.Size);
	}

	PreviewBox->Invalidate();
}


void TheMatrix::SetPreviewViewMode(ViewShape mode)
{
	Preview.View = mode;

	SetPreviewBoxSize(Preview.Size);
}


void TheMatrix::SetPreviewVoid(int offset)
{
	Preview.ROffset = offset;

	SetPreviewBoxSize(Preview.Size);
}


void TheMatrix::SetPreviewPopout(bool Popout)
{
	PreviewPopout = Popout;

	delete PreviewBox;

	if (Popout)
	{
		Preview.OldSize = Preview.Size;

		InitPreviewBox(PreviewOwner, PreviewCanvas, true);

		TPanel *panel = (TPanel*)PreviewCanvas;

		panel->OnResize = &OnPreviewBoxCanvasResize;
	}
	else
	{
		InitPreviewBox(Owner, Canvas, Preview.Active);

		SetPreviewBoxSize(Preview.OldSize);
	}
}


void __fastcall TheMatrix::pbPreviewPaint(TObject *Sender)
{
	switch (Details.Mode)
	{
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		BuildMonoBiRenderFrame();
		break;
	case MatrixMode::kRGB:
		BuildRGBRenderFrame();
		break;
	case MatrixMode::kRGB3BPP:
		BuildRGB3BPPRenderFrame();
		break;
	}

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[y * Details.Width + x]);

			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PreviewBox->Canvas->FillRect(Rect(x * Preview.Size,
												  y * Preview.Size,
												 (x * Preview.Size) + Preview.Size,
												 (y * Preview.Size) + Preview.Size));
				break;
			case PixelShape::kCircle:
				PreviewBox->Canvas->Ellipse(x * Preview.Size,
											y * Preview.Size,
										   (x * Preview.Size) + Preview.Size,
										   (y * Preview.Size) + Preview.Size);
				break;
			case PixelShape::kRoundRect:
				PreviewBox->Canvas->RoundRect(x * Preview.Size,
											  y * Preview.Size,
											 (x * Preview.Size) + Preview.Size,
											 (y * Preview.Size) + Preview.Size,
											  Preview.Size - (std::round(Preview.Size / CRoundRectCoeff)),
											  Preview.Size - (std::round(Preview.Size / CRoundRectCoeff)));
				break;
			}
		}
	}

	// ===========================================================================
	// ===========================================================================
	// ===========================================================================

	if (Render.Draw.Mode != DrawMode::kNone)
	{
		if (Render.Draw.Coords[0].X != - 1)
		{
			switch (Details.Mode)
			{
			case MatrixMode::kRGB:
			case MatrixMode::kRGB3BPP:
				PreviewBox->Canvas->Brush->Color = TColor(Render.Draw.Colour);
				break;

			default:
				PreviewBox->Canvas->Brush->Color = TColor(LEDColours[Render.Draw.Colour]);
			}

			// need a preview version of draw shape
			//DrawShape(true, PreviewBox->Canvas, Preview.Size, Preview.Size, 1, false);

			// =======================================================================

			PreviewBox->Canvas->Brush->Color = TColor(LEDColours[CDisplayMarker]);

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PreviewBox->Canvas->FillRect(Rect(Render.Draw.Coords[0].X * Preview.Size,
												 Render.Draw.Coords[0].Y * Preview.Size,
												(Render.Draw.Coords[0].X * Preview.Size) + Preview.Size,
												(Render.Draw.Coords[0].Y * Preview.Size) + Preview.Size));
				break;
			case PixelShape::kCircle:
				PreviewBox->Canvas->Ellipse(Render.Draw.Coords[0].X * Preview.Size,
										   Render.Draw.Coords[0].Y * Preview.Size,
										  (Render.Draw.Coords[0].X * Preview.Size) + Preview.Size,
										  (Render.Draw.Coords[0].Y * Preview.Size) + Preview.Size);
				break;
			case PixelShape::kRoundRect:
				PreviewBox->Canvas->RoundRect(Render.Draw.Coords[0].X * Preview.Size,
											 Render.Draw.Coords[0].Y * Preview.Size,
											(Render.Draw.Coords[0].X * Preview.Size) + Preview.Size,
											(Render.Draw.Coords[0].Y * Preview.Size) + Preview.Size,
											 Preview.Size - (std::round(Preview.Size / CRoundRectCoeff)),
											 Preview.Size - (std::round(Preview.Size / CRoundRectCoeff)));
				break;
			}
		}
	}

	// ===========================================================================
	// ===========================================================================
	// ===========================================================================

	if (Render.Draw.CopyPos.X != 0)
	{
		for (int x = 0; x < Render.Draw.CopyPos.X; x++)
		{
			for (int y = 0; y < Render.Draw.CopyPos.Y; y++)
			{
				if (x + LastX >= 0 && x + LastX < Details.Width &&
					y + LastY >= 0 && y + LastY < Details.Height)
				{
					if (Details.Mode == MatrixMode::kRGB)
					{
						if (MatrixDeadLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
						{
							PreviewBox->Canvas->Brush->Color = TColor(MatrixCopy->Grid[y * Details.Width + x]);
						}
						else
						{
							PreviewBox->Canvas->Brush->Color = TColor(RGBBackground);
						}
					}
					else
					{
						if (MatrixDeadLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
						{
							PreviewBox->Canvas->Brush->Color = TColor(LEDColours[MatrixCopy->Grid[y * Details.Width + x]]);
						}
						else
						{
							PreviewBox->Canvas->Brush->Color = clBtnFace;
						}
					}

					switch (Render.Shape)
					{
					case PixelShape::kSquare:
						PreviewBox->Canvas->FillRect(Rect((x + LastX) * Preview.Size,
														 (y + LastY) * Preview.Size,
														((x + LastX) * Preview.Size) + Preview.Size,
														((y + LastY) * Preview.Size) + Preview.Size));
						break;
					case PixelShape::kCircle:
						PreviewBox->Canvas->Ellipse((x + LastX) * Preview.Size,
												   (y + LastY) * Preview.Size,
												  ((x + LastX) * Preview.Size) + Preview.Size,
												  ((y + LastY) * Preview.Size) + Preview.Size);
						break;
					case PixelShape::kRoundRect:
						PreviewBox->Canvas->RoundRect((x + LastX) * Preview.Size,
													 (y + LastY) * Preview.Size,
													((x + LastX) * Preview.Size) + Preview.Size,
													((y + LastY) * Preview.Size) + Preview.Size,
													  Preview.Size - (std::round(Preview.Size / CRoundRectCoeff)),
													  Preview.Size - (std::round(Preview.Size / CRoundRectCoeff)));
						break;
					}
				}
			}
		}
	}
}


void __fastcall TheMatrix::pbPreviewPaintRadial(TObject *Sender)
{
	int cx = std::round(std::min(PreviewBox->Width, PreviewBox->Height) / 2);
	int cy = std::round(std::min(PreviewBox->Width, PreviewBox->Height) / 2);

	switch (Details.Mode)
	{
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		BuildMonoBiRenderFrame();
		break;
	case MatrixMode::kRGB:
		BuildRGBRenderFrame();
		break;
	case MatrixMode::kRGB3BPP:
		BuildRGB3BPPRenderFrame();
		break;
	}

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[y * Details.Width + x]);

			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			double ac = std::cos(CalcUtility::DegToRadians(RadialOffsetDegrees + ((Details.Width - 1 - x) / Details.Width - 1) * 360));
			double as = std::sin(CalcUtility::DegToRadians(RadialOffsetDegrees + ((Details.Width - 1 - x) / Details.Width - 1) * 360));

			double d = (cx - Preview.ROffset) / Details.Height;

			int xp =  cx + std::round((Preview.ROffset + (d * (Details.Height - 1 - y))) * ac);
			int yp =  cy - std::round((Preview.ROffset + (d * (Details.Height - 1 - y))) * as);

			if (Preview.IncrementRadially)
			{
				PreviewBox->Canvas->Ellipse(xp,
										   yp,
										   xp + Preview.RPixel + (Details.Height - 1 - y),
										   yp + Preview.RPixel + (Details.Height - 1 - y));
			}
			else
			{
				PreviewBox->Canvas->Ellipse(xp,
										   yp,
										   xp + Preview.RPixel,
										   yp + Preview.RPixel);
			}
		}
	}
}


void __fastcall TheMatrix::pbPreviewPaintRadialThreeQuarters(TObject *Sender)
{
	int cx = std::round(std::min(PreviewBox->Width, PreviewBox->Height) / 2);
	int cy = std::round(std::min(PreviewBox->Width, PreviewBox->Height) / 2);

	switch (Details.Mode)
	{
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		BuildMonoBiRenderFrame();
		break;
	case MatrixMode::kRGB:
		BuildRGBRenderFrame();
		break;
	case MatrixMode::kRGB3BPP:
		BuildRGB3BPPRenderFrame();
		break;
	}

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[y * Details.Width + x]);

			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			double ac = std::cos(CalcUtility::DegToRadians(RadialOffsetDegrees + 225 - (x / (Details.Width - 1)) * 270));
			double as = std::sin(CalcUtility::DegToRadians(RadialOffsetDegrees + 225 - (x / (Details.Width - 1)) * 270));

			double d =  (cx - Preview.ROffset) / Details.Height;

			int xp =  cx + std::round((Preview.ROffset + (d * (Details.Height - 1 - y))) * ac);
			int yp =  cy - std::round((Preview.ROffset + (d * (Details.Height - 1 - y))) * as);

			if (Preview.IncrementRadially)
			{
				PreviewBox->Canvas->Ellipse(xp,
										   yp,
										   xp + Preview.RPixel + (Details.Height - 1 - y),
										   yp + Preview.RPixel + (Details.Height - 1 - y));
			}
			else
			{
				PreviewBox->Canvas->Ellipse(xp,
										   yp,
										   xp + Preview.RPixel,
										   yp + Preview.RPixel);
			}
		}
	}
}


void __fastcall TheMatrix::pbPreviewPaintSemiCircle(TObject *Sender)
{
	int cx = std::round(std::min(PreviewBox->Width, PreviewBox->Height) / 2);
	int cy = std::round(std::min(PreviewBox->Width, PreviewBox->Height) / 2);

	switch (Details.Mode)
	{
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		BuildMonoBiRenderFrame();
		break;
	case MatrixMode::kRGB:
		BuildRGBRenderFrame();
		break;
	case MatrixMode::kRGB3BPP:
		BuildRGB3BPPRenderFrame();
		break;
	}

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[y * Details.Width + x]);

			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			double ac = std::cos(CalcUtility::DegToRadians(RadialOffsetDegrees + 180 - (x / (Details.Width - 1)) * 180));
			double as = std::sin(CalcUtility::DegToRadians(RadialOffsetDegrees + 180 - (x / (Details.Width - 1)) * 180));

			double d =  (cx - Preview.ROffset) / Details.Height;

			int xp = cx + std::round((Preview.ROffset + (d * (Details.Height - 1 - y))) * ac);
			int yp = cy - std::round((Preview.ROffset + (d * (Details.Height - 1 - y))) * as);

			if (Preview.IncrementRadially)
			{
				PreviewBox->Canvas->Ellipse(xp,
										   yp,
										   xp + Preview.RPixel + (Details.Height - 1 - y),
										   yp + Preview.RPixel + (Details.Height - 1 - y));
			}
			else
			{
				PreviewBox->Canvas->Ellipse(xp,
										   yp,
										   xp + Preview.RPixel,
										   yp + Preview.RPixel);
			}
		}
	}
}


void __fastcall TheMatrix::pbPreviewPaintSemiCircleInverted(TObject *Sender)
{
	int cx = std::round(std::min(PreviewBox->Width, PreviewBox->Height) / 2);
	int cy = 4;

	switch (Details.Mode)
	{
	case MatrixMode::kMono:
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		BuildMonoBiRenderFrame();
		break;
	case MatrixMode::kRGB:
		BuildRGBRenderFrame();
		break;
	case MatrixMode::kRGB3BPP:
		BuildRGB3BPPRenderFrame();
		break;
	}

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[y * Details.Width + x]);

			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			double ac = std::cos(CalcUtility::DegToRadians(RadialOffsetDegrees + 180 + (x / (Details.Width - 1)) * 180));
			double as = std::sin(CalcUtility::DegToRadians(RadialOffsetDegrees + 180 + (x / (Details.Width - 1)) * 180));

			double d = (cx - Preview.ROffset) / Details.Height;

			int xp = cx + std::round((Preview.ROffset + (d * (y))) * ac);
			int yp = cy - std::round((Preview.ROffset + (d * (y))) * as);

			if (Preview.IncrementRadially)
			{
				PreviewBox->Canvas->Ellipse(xp,
										   yp,
										   xp + Preview.RPixel + y,
										   yp + Preview.RPixel + y);
			}
			else
			{
				PreviewBox->Canvas->Ellipse(xp,
										   yp,
										   xp + Preview.RPixel,
										   yp + Preview.RPixel);
			}
		}
    }
}


void __fastcall TheMatrix::OnPreviewBoxCanvasResize(TObject *Sender)
{
	SetPreviewBoxSize(previewPixelSizeAuto);
}


void __fastcall TheMatrix::OnPreviewBoxMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (Shift.Contains(ssRight))
	{
		if (OnPreviewMouseDown)
		{
			OnPreviewMouseDown(PreviewBox->Left + X, PreviewBox->Top + Y);
		}
	}
}
#pragma end_region


#pragma region Mode_Mono
void __fastcall TheMatrix::ClickPixel(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!MatrixLayers[CurrentLayer]->Visible) return;

	int x1 = std::floor((double)X / (double)Render.PixelSize);
	int y1 = std::floor((double)Y / (double)Render.PixelSize);

	if (x1 < 0 || y1 < 0) return;

	x1 = std::floor((double)X / (double)Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor((double)Y / (double)Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = CMouseLeft;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			DrawWithBrush(1, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(1, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kPaste:
			DrawWithBrushPaste(x1, y1, !Shift.Contains(ssShift));

			if (OnChange) OnChange(this);
			break;

		default:
			UpdateDrawTool(x1, y1, 1, false);
		}

		CopyDrawBufferToCurrentFrame();
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = CMouseRight;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			DrawWithBrush(0, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(0, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;

		default:
			UpdateDrawTool(x1, y1, 0, false);
		}

		CopyDrawBufferToCurrentFrame();
	}

	PreviewBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseMove(TObject *Sender, TShiftState Shift, int X, int Y)
{
	int x1 = std::floor((double)X / (double)Render.PixelSize);
	int y1 = std::floor((double)Y / (double)Render.PixelSize);

	if (x1 < 0 || y1 < 0) return;

	x1 = std::floor((double)X / (double)Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor((double)Y / (double)Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (OnMouseOver) OnMouseOver(x1, y1);

	// ===========================================================================
	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = CMouseLeft;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(1, x1, y1);
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti :
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrushMulti(1, x1, y1);
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		}
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = CMouseRight;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(0, x1, y1);
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrushMulti(0, x1, y1);
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		}
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (Render.Draw.Mode == DrawMode::kNone)
	{
		CopyDrawBufferToCurrentFrame();
	}
}


void __fastcall TheMatrix::PaintBoxUpdate(TObject *Sender)
{
	BuildMonoBiRenderFrame();

	for (int x = 0; x <= Render.ViewWindow.X; x++)
	{
		for (int y = 0; y <= Render.ViewWindow.Y; y++)
		{
			PaintBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[(Render.TopLeft.Y + y) * Details.Width + (Render.TopLeft.X + x)]);

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
												y * Render.PixelSize,
											   (x * Render.PixelSize) + Render.PixelSizeZ,
											   (y * Render.PixelSize) + Render.PixelSizeZ));
				break;
			case PixelShape::kCircle:
				PaintBox->Canvas->Ellipse(x * Render.PixelSize,
												 y * Render.PixelSize,
												(x * Render.PixelSize) + Render.PixelSizeZ,
												(y * Render.PixelSize) + Render.PixelSizeZ);
				break;
			case PixelShape::kRoundRect : PaintBox->Canvas->RoundRect(x * Render.PixelSize,
												   y * Render.PixelSize,
												  (x * Render.PixelSize) + Render.PixelSizeZ,
												  (y * Render.PixelSize) + Render.PixelSizeZ,
												   Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
												   Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
				break;

			default:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
										 y * Render.PixelSize,
										(x * Render.PixelSize) + Render.PixelSizeZ,
										(y * Render.PixelSize) + Render.PixelSizeZ));
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Draw.Mode != DrawMode::kNone)
	{
		if ((Render.Draw.SinglePoint) or (Render.Draw.Coords[0].X != - 1))
		{
			PaintBox->Canvas->Brush->Color = TColor(LEDColours[Render.Draw.Colour]);
			DrawShape(true, 1, false);

			// =======================================================================

			// single point modes don't require "first click" marker
			if (Render.Draw.SinglePoint)
			{
				PaintBox->Canvas->Brush->Color = TColor(LEDColours[CDisplayMarker]);

				switch (Render.Shape)
				{
				case PixelShape::kSquare:
					PaintBox->Canvas->FillRect(Rect(Render.Draw.Coords[0].X * Render.PixelSize,
													Render.Draw.Coords[0].Y * Render.PixelSize,
												   (Render.Draw.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
												   (Render.Draw.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ));
					break;
				case PixelShape::kCircle:
					PaintBox->Canvas->Ellipse(Render.Draw.Coords[0].X * Render.PixelSize,
											  Render.Draw.Coords[0].Y * Render.PixelSize,
											 (Render.Draw.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
											 (Render.Draw.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ);
					break;
				case PixelShape::kRoundRect:
					PaintBox->Canvas->RoundRect(Render.Draw.Coords[0].X * Render.PixelSize,
												Render.Draw.Coords[0].Y * Render.PixelSize,
											   (Render.Draw.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
											   (Render.Draw.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ,
												Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
												Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
					break;
				}
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Draw.CopyPos.X != 0)
	{
		for (int x = 0; x < Render.Draw.CopyPos.X; x++)
		{
			for (int y = 0; y < Render.Draw.CopyPos.Y; y++)
			{
				if (x + LastX >= 0 && x + LastX <= Details.Width - 1 &&
					y + LastY >= 0 && y + LastY <= Details.Height - 1)
				{
					if (MatrixDeadLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
					{
						PaintBox->Canvas->Brush->Color = TColor(LEDColours[MatrixCopy->Grid[y * Details.Width + x]]);      // ? to do (from original source!)
					}
					else
					{
						PaintBox->Canvas->Brush->Color = TColor(CanvasBackground);
					}

					switch (Render.Shape)
					{
					case PixelShape::kSquare:
						PaintBox->Canvas->FillRect(Rect((x + LastX) * Render.PixelSize,
														(y + LastY) * Render.PixelSize,
													   ((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
													   ((y + LastY) * Render.PixelSize) + Render.PixelSizeZ));
						break;
					case PixelShape::kCircle:
						PaintBox->Canvas->Ellipse((x + LastX) * Render.PixelSize,
												  (y + LastY) * Render.PixelSize,
												 ((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
												 ((y + LastY) * Render.PixelSize) + Render.PixelSizeZ);
						break;
					case PixelShape::kRoundRect : PaintBox->Canvas->RoundRect((x + LastX) * Render.PixelSize,
													  (y + LastY) * Render.PixelSize,
													 ((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
													 ((y + LastY) * Render.PixelSize) + Render.PixelSizeZ,
													   Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
													   Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
						break;
					}
				}
			}
		}
	}

	PreviewBox->Invalidate();
}


void TheMatrix::BuildMonoBiRenderFrame()
{
	MatrixRender->ClearColour(LEDColours[CDisplayClear]);

	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		if (MatrixLayers[layer]->Visible)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				for (int y = 0; y < Details.Height; y++)
				{
					if (MatrixDeadLayout->Grid[y * Details.Width + x] == PixelDead)
					{
						MatrixRender->Grid[y * Details.Width + x] = CanvasBackground;
					}
					else
					{
						if (layer == CurrentLayer)
						{
							switch (DisplayBuffer->Grid[y * Details.Width + x])
							{
							case 0:
								if (MatrixRender->Grid[y * Details.Width + x] == LEDColours[0])
								{
									if (LightBox == 1 && CurrentFrame != 0)
									{
										if (MatrixLayers[layer]->Cells[CurrentFrame - 1]->Grid[y * Details.Width + x] == 1)
										{
											MatrixRender->Grid[y * Details.Width + x] = LEDColours[CLightBoxShade];
										}
									}
								}
								break;
							case 1:
								MatrixRender->Grid[y * Details.Width + x] = LEDColours[CMouseLeft];
								break;
							case 2:
								MatrixRender->Grid[y * Details.Width + x] = LEDColours[CMouseMiddle];
								break;
							case 3:
								MatrixRender->Grid[y * Details.Width + x] = LEDColours[CMouseRight];
								break;
							}
						}
						else
						{
							switch (MatrixLayers[layer]->Cells[CurrentFrame]->Grid[y * Details.Width + x])
							{
							case 0:
								if (MatrixRender->Grid[y * Details.Width + x] == LEDColours[CDisplayClear])
								{
									if (LightBox == 1 && CurrentFrame != 0)
									{
										if (MatrixLayers[layer]->Cells[CurrentFrame - 1]->Grid[y * Details.Width + x] == 1)
										{
											MatrixRender->Grid[y * Details.Width + x] = LEDColours[CLightBoxShade];
										}
									}
								}
								break;
							case 1:
								MatrixRender->Grid[y * Details.Width + x] = LEDColours[CMouseLeft];
								break;
							case 2:
								MatrixRender->Grid[y * Details.Width + x] = LEDColours[CMouseMiddle];
								break;
							case 3:
								MatrixRender->Grid[y * Details.Width + x] = LEDColours[CMouseRight];
								break;
							}
						}
					}
				}
            }
		}
	}
}
#pragma end_region


#pragma region Mode_Bi
void __fastcall TheMatrix::Shape1MouseUpBiColour(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	switch (Render.Draw.Mode)
	{
	case DrawMode::kNone:
	case DrawMode::kGradientBrush:
	case DrawMode::kMulti:
	case DrawMode::kRandom:
		CopyDrawBufferToCurrentFrame();
		break;
	}
}


void __fastcall TheMatrix::ClickPixelBiColour(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!MatrixLayers[CurrentLayer]->Visible) return;

	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = CMouseLeft;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			DrawWithBrush(SelectionLMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionLMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kPaste:
			DrawWithBrushPaste(x1, y1, !Shift.Contains(ssShift));

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kRandom:
		{
			int i = 1 + random(3);

			DrawWithBrush(i, x1, y1);

			if (OnChange) OnChange(this);
			break;
        }
		default:
			UpdateDrawTool(x1, y1, SelectionLMB, False);
		}

		CopyDrawBufferToCurrentFrame();
	}
	else if (Shift.Contains(ssMiddle))
	{
		LastMouseButton = CMouseMiddle;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			DrawWithBrush(SelectionMMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionMMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		default:
			UpdateDrawTool(x1, y1, SelectionMMB, True);
		}

		CopyDrawBufferToCurrentFrame();
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = CMouseRight;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			DrawWithBrush(SelectionRMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionRMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;

		default:
			UpdateDrawTool(x1, y1, SelectionRMB, False);
		}

		CopyDrawBufferToCurrentFrame();
	}

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseMoveBiColour(TObject *Sender, TShiftState Shift, int X, int Y)
{
	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (OnMouseOver) OnMouseOver(x1, y1);

	// ===========================================================================
	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = CMouseLeft;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			DrawWithBrush(SelectionLMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionLMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		}
	}
	else if (Shift.Contains(ssMiddle))
	{
		LastMouseButton = CMouseMiddle;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			if (Render.Gradient.Option == GradientOption::kVertical && Render.Gradient.IY[y1] != 0 && SelectionMMB != 0)
			{
				DrawWithBrush(Render.Gradient.IY[y1], x1, y1);
			}
			else if (Render.Gradient.Option == GradientOption::kHorizontal && Render.Gradient.IX[x1] != 0 && SelectionMMB != 0)
			{
				DrawWithBrush(Render.Gradient.IX[x1], x1, y1);
			}
			else
			{
				DrawWithBrush(SelectionMMB, x1, y1);
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionMMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		}
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = CMouseRight;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			DrawWithBrush(SelectionRMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionRMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		}
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region Mode_RGB
void TheMatrix::BuildRGBRenderFrame()
{
	MatrixRender->ClearColour(RGBBackground);

	for (int l = 0; l < MatrixLayers.size(); l++)
	{
		if (MatrixLayers[l]->Visible)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				for (int y = 0; y < Details.Height; y++)
				{
					if (MatrixDeadLayout->Grid[y * Details.Width + x] != PixelAlive)
					{
						MatrixRender->Grid[y * Details.Width + x] = CanvasBackground;
					}
					else
					{
						if (l == CurrentLayer)
						{
							if (LightBox == 1 && CurrentFrame != 0)
							{
								if (DisplayBuffer->Grid[y * Details.Width + x] == RGBBackground)
								{
									MatrixRender->Grid[y * Details.Width + x] = ColourUtility::DarkenRGB(MatrixLayers[l]->Cells[CurrentFrame - 1]->Grid[y * Details.Width + x]);
								}
								else
								{
									MatrixRender->Grid[y * Details.Width + x] = DisplayBuffer->Grid[y * Details.Width + x];
								}
							}
							else if (DisplayBuffer->Grid[y * Details.Width + x] != RGBBackground)
							{
								MatrixRender->Grid[y * Details.Width + x] = DisplayBuffer->Grid[y * Details.Width + x];
							}
						}
						else
						{
							if (LightBox == 1 && CurrentFrame != 0)
							{
								if (DisplayBuffer->Grid[y * Details.Width + x] == RGBBackground)
								{
									MatrixRender->Grid[y * Details.Width + x] = ColourUtility::DarkenRGB(MatrixLayers[l]->Cells[CurrentFrame - 1]->Grid[y * Details.Width + x]);
								}
								else
								{
									MatrixRender->Grid[y * Details.Width + x] = DisplayBuffer->Grid[y * Details.Width + x];
								}
							}
							else if (MatrixLayers[l]->Cells[CurrentFrame]->Grid[y * Details.Width + x] != RGBBackground)
							{
								MatrixRender->Grid[y * Details.Width + x] = MatrixLayers[l]->Cells[CurrentFrame]->Grid[y * Details.Width + x];
							}
						}
					}
				}
			  }
		}
	  }
}


void __fastcall TheMatrix::PaintBoxUpdateRGB(TObject *Sender)
{
	BuildRGBRenderFrame();

	for (int x = 0; x <= Render.ViewWindow.X; x++)
	{
		for (int y = 0; y <= Render.ViewWindow.Y; y++)
		{
			PaintBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[(Render.TopLeft.Y + y) * Details.Width + (Render.TopLeft.X + x)]);

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
													 y * Render.PixelSize,
													(x * Render.PixelSize) + Render.PixelSizeZ,
													(y * Render.PixelSize) + Render.PixelSizeZ));
				break;
			case PixelShape::kCircle    : PaintBox->Canvas->Ellipse(x * Render.PixelSize,
											   y * Render.PixelSize,
											  (x * Render.PixelSize) + Render.PixelSizeZ,
											  (y * Render.PixelSize) + Render.PixelSizeZ);
				break;
			case PixelShape::kRoundRect : PaintBox->Canvas->RoundRect(x * Render.PixelSize,
												 y * Render.PixelSize,
												(x * Render.PixelSize) + Render.PixelSizeZ,
												(y * Render.PixelSize) + Render.PixelSizeZ,
												 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
												 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
				break;

			default:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
									   y * Render.PixelSize,
									  (x * Render.PixelSize) + Render.PixelSizeZ,
									  (y * Render.PixelSize) + Render.PixelSizeZ));
				break;
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Draw.Mode != DrawMode::kNone)
	{
		if (Render.Draw.SinglePoint || Render.Draw.Coords[0].X != - 1)
		{
			PaintBox->Canvas->Brush->Color = TColor(Render.Draw.Colour);
			DrawShape(True, Render.Draw.Colour, false);

			// =======================================================================

			PaintBox->Canvas->Brush->Color = TColor(LEDColours[CDisplayMarker]);

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PaintBox->Canvas->FillRect(Rect(Render.Draw.Coords[0].X * Render.PixelSize,
													 Render.Draw.Coords[0].Y * Render.PixelSize,
													(Render.Draw.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
													(Render.Draw.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ));
				break;
			case PixelShape::kCircle:
				PaintBox->Canvas->Ellipse(Render.Draw.Coords[0].X * Render.PixelSize,
											   Render.Draw.Coords[0].Y * Render.PixelSize,
											  (Render.Draw.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
											  (Render.Draw.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ);
				break;
			case PixelShape::kRoundRect:
				PaintBox->Canvas->RoundRect(Render.Draw.Coords[0].X * Render.PixelSize,
												 Render.Draw.Coords[0].Y * Render.PixelSize,
												(Render.Draw.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
												(Render.Draw.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ,
												 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
												 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
				break;
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Draw.CopyPos.X != 0)
	{
		for (int x = 0; x < Render.Draw.CopyPos.X; x++)
		{
			for (int y = 0; y < Render.Draw.CopyPos.Y; y++)
			{
				if (x + LastX >= 0 && x + LastY <= Details.Width - 1 &&
					y + LastY >= 0 && y + LastY <= Details.Height - 1)
				{
					if (MatrixDeadLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
					{
						PaintBox->Canvas->Brush->Color = TColor(MatrixCopy->Grid[y * Details.Width + x]);
					}
					else
					{
						PaintBox->Canvas->Brush->Color = TColor(CanvasBackground);
					}

					switch (Render.Shape)
					{
					case PixelShape::kSquare:
						PaintBox->Canvas->FillRect(Rect((x + LastX) * Render.PixelSize,
														  (y + LastY) * Render.PixelSize,
														 ((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
														 ((y + LastY) * Render.PixelSize) + Render.PixelSizeZ));
						break;
					case PixelShape::kCircle:
						PaintBox->Canvas->Ellipse((x + LastX) * Render.PixelSize,
												  (y + LastY) * Render.PixelSize,
												 ((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
												 ((y + LastY) * Render.PixelSize) + Render.PixelSizeZ);
						break;
					case PixelShape::kRoundRect:
						PaintBox->Canvas->RoundRect((x + LastX) * Render.PixelSize,
													(y + LastY) * Render.PixelSize,
												   ((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
												   ((y + LastY) * Render.PixelSize) + Render.PixelSizeZ,
													 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
													 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
						break;
					}
				}
			}
		}
	}

	PreviewBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseUpRGB(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
 {
	switch (Render.Draw.Mode)
	{
	case DrawMode::kNone:
	case DrawMode::kGradientBrush:
	case DrawMode::kMulti:
	case DrawMode::kRandom:
		CopyDrawBufferToCurrentFrame();
		break;
	}
}


void __fastcall TheMatrix::ClickPixelRGB(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!MatrixLayers[CurrentLayer]->Visible) return;

	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = CMouseLeft;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
		case DrawMode::kGradientBrush:
			DrawWithBrush(SelectionLMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kRandom:
			DrawWithBrush(ColourUtility::RandomColour(SelectionLMB, RandomCoeff), x1, y1);

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionLMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kPicker:
			ChangeSelectionColour(MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y1 * Details.Width + x1], SelectionMMB, SelectionRMB);
			break;
		case DrawMode::kPaste:
			DrawWithBrushPaste(x1, y1, !Shift.Contains(ssShift));

			if (OnChange) OnChange(this);
			break;
		default:
			UpdateDrawTool(x1, y1, SelectionLMB, False);
		}

		CopyDrawBufferToCurrentFrame();
	}
	else if (Shift.Contains(ssMiddle))
	{
		LastMouseButton = CMouseMiddle;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			switch (Render.Gradient.Option)
			{
			case GradientOption::kOff:
				DrawWithBrush(SelectionMMB, x1, y1);
				break;
			case GradientOption::kVertical:
				DrawWithBrush(Render.Gradient.IY[y1], x1, y1);
				break;
			case GradientOption::kHorizontal:
				DrawWithBrush(Render.Gradient.IX[x1], x1, y1);
				break;
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
            break;
		case DrawMode::kRandom:
			DrawWithBrush(ColourUtility::RandomColour(SelectionMMB, RandomCoeff), x1, y1);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionMMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kPicker:
			ChangeSelectionColour(SelectionLMB, MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y1 * Details.Width + x1], SelectionRMB);
			break;
		case DrawMode::kGradientBrush:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithGradientBrush(x1, y1);

				if (OnChange) OnChange(this);

				LastX = x1;
				LastY = y1;
			}
			break;

		default:
			UpdateDrawTool(x1, y1, SelectionMMB, True);
		}

		CopyDrawBufferToCurrentFrame();
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = CMouseRight;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
		case DrawMode::kGradientBrush:
			DrawWithBrush(SelectionRMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kRandom:
			DrawWithBrush(ColourUtility::RandomColour(SelectionRMB, RandomCoeff), x1, y1);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionRMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kPicker:
			ChangeSelectionColour(SelectionLMB, SelectionMMB, MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y1 * Details.Width + x1]);
			break;

		default:
			UpdateDrawTool(x1, y1, SelectionRMB, False);
		}

		CopyDrawBufferToCurrentFrame();
	}

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseMoveRGB(TObject *Sender, TShiftState Shift, int X, int Y)
{
	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (OnMouseOver) OnMouseOver(x1, y1);

	// ===========================================================================
	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = CMouseLeft;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
		case DrawMode::kGradientBrush:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(SelectionLMB, x1, y1);
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrushMulti(SelectionLMB, x1, y1);
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kRandom:
			DrawWithBrush(ColourUtility::RandomColour(SelectionLMB, RandomCoeff), x1, y1);
			break;
		}
	}
	else if (Shift.Contains(ssMiddle))
	{
		LastMouseButton = CMouseMiddle;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
			switch (Render.Gradient.Option)
			{
			case GradientOption::kOff:
				DrawWithBrush(SelectionMMB, x1, y1);
				break;
			case GradientOption::kVertical:
				DrawWithBrush(Render.Gradient.IY[y1], x1, y1);
				break;
			case GradientOption::kHorizontal:
				DrawWithBrush(Render.Gradient.IX[x1], x1, y1);
				break;
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			DrawWithBrushMulti(SelectionMMB, x1, y1);

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kRandom:
			DrawWithBrush(ColourUtility::RandomColour(SelectionMMB, RandomCoeff), x1, y1);
			break;
		case DrawMode::kGradientBrush:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithGradientBrush(x1, y1);

				if (OnChange) OnChange(this);

				LastX = x1;
				LastY = y1;
			}
			break;
		}
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = CMouseRight;

		switch (Render.Draw.Mode)
		{
		case DrawMode::kNone:
		case DrawMode::kGradientBrush:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(SelectionRMB, x1, y1);
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kMulti:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrushMulti(SelectionRMB, x1, y1);
			}

			LastX = x1;
			LastY = y1;

			if (OnChange) OnChange(this);
			break;
		case DrawMode::kRandom:
			DrawWithBrush(ColourUtility::RandomColour(SelectionRMB, RandomCoeff), x1, y1);
			break;
		}
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region Mode_RGB3BPP
void TheMatrix::BuildRGB3BPPRenderFrame()
{
	MatrixRender->ClearColour(RGBBackground);

	for (int l = 0; l < MatrixLayers.size(); l++)
	{
		if (MatrixLayers[l]->Visible)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				for (int y = 0; y < Details.Height; y++)
				{
					if (MatrixDeadLayout->Grid[y * Details.Width + x] != PixelAlive)
					{
						MatrixRender->Grid[y * Details.Width + x] = CanvasBackground;
					}
					else
					{
						if (l == CurrentLayer)
						{
							if (LightBox == 1 && CurrentFrame != 0)
							{
								if (DisplayBuffer->Grid[y * Details.Width + x] == RGBBackground)
								{
									MatrixRender->Grid[y * Details.Width + x] = ColourUtility::DarkenRGB(LEDRGB3BPPColours[MatrixLayers[l]->Cells[CurrentFrame - 1]->Grid[y * Details.Width + x]]);
								}
								else
								{
									MatrixRender->Grid[y * Details.Width + x] = LEDRGB3BPPColours[DisplayBuffer->Grid[y * Details.Width + x]];
								}
							}
							else if (DisplayBuffer->Grid[y * Details.Width + x] != RGBBackground)
							{
								MatrixRender->Grid[y * Details.Width + x] = LEDRGB3BPPColours[DisplayBuffer->Grid[y * Details.Width + x]];
							}
						}
						else
						{
							if (LightBox == 1 && CurrentFrame != 0)
							{
								if (DisplayBuffer->Grid[y * Details.Width + x] == RGBBackground)
								{
									MatrixRender->Grid[y * Details.Width + x] = ColourUtility::DarkenRGB(MatrixLayers[l]->Cells[CurrentFrame - 1]->Grid[y * Details.Width + x]);
								}
								else
								{
									MatrixRender->Grid[y * Details.Width + x] = LEDRGB3BPPColours[MatrixLayers[l]->Cells[CurrentFrame]->Grid[y * Details.Width + x]];
								}
							}
							else if (MatrixLayers[l]->Cells[CurrentFrame]->Grid[y * Details.Width + x] != RGBBackground)
							{
								MatrixRender->Grid[y * Details.Width + x] = LEDRGB3BPPColours[MatrixLayers[l]->Cells[CurrentFrame]->Grid[y * Details.Width + x]];
							}
						}
					}
				}
			}
		}
	}
}


void __fastcall TheMatrix::PaintBoxUpdateRGB_3BPP(TObject *Sender)
{
	BuildRGB3BPPRenderFrame();

	for (int x = 0; x <= Render.ViewWindow.X; x++)
	{
		for (int y = 0; y <= Render.ViewWindow.Y; y++)
		{
			PaintBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[(Render.TopLeft.Y + y) * Details.Width + (Render.TopLeft.X + x)]);

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
												y * Render.PixelSize,
											   (x * Render.PixelSize) + Render.PixelSizeZ,
											   (y * Render.PixelSize) + Render.PixelSizeZ));
				break;
			case PixelShape::kCircle:
				PaintBox->Canvas->Ellipse(x * Render.PixelSize,
										  y * Render.PixelSize,
										 (x * Render.PixelSize) + Render.PixelSizeZ,
										 (y * Render.PixelSize) + Render.PixelSizeZ);
				break;
			case PixelShape::kRoundRect:
				PaintBox->Canvas->RoundRect(x * Render.PixelSize,
											y * Render.PixelSize,
										   (x * Render.PixelSize) + Render.PixelSizeZ,
										   (y * Render.PixelSize) + Render.PixelSizeZ,
											Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
											Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
				break;
			default:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
										 y * Render.PixelSize,
										(x * Render.PixelSize) + Render.PixelSizeZ,
										(y * Render.PixelSize) + Render.PixelSizeZ));
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Draw.Mode != DrawMode::kNone)
	{
		if (Render.Draw.SinglePoint || Render.Draw.Coords[0].X != - 1)
		{
			PaintBox->Canvas->Brush->Color = TColor(Render.Draw.Colour);
			DrawShape(True, Render.Draw.Colour, False);

			// =======================================================================

			PaintBox->Canvas->Brush->Color = TColor(LEDColours[CDisplayMarker]);

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PaintBox->Canvas->FillRect(Rect(Render.Draw.Coords[0].X * Render.PixelSize,
													 Render.Draw.Coords[0].Y * Render.PixelSize,
													(Render.Draw.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
													(Render.Draw.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ));
				break;
			case PixelShape::kCircle:
				PaintBox->Canvas->Ellipse(Render.Draw.Coords[0].X * Render.PixelSize,
											   Render.Draw.Coords[0].Y * Render.PixelSize,
											  (Render.Draw.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
											  (Render.Draw.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ);
				break;
			case PixelShape::kRoundRect:
					PaintBox->Canvas->RoundRect(Render.Draw.Coords[0].X * Render.PixelSize,
												 Render.Draw.Coords[0].Y * Render.PixelSize,
												(Render.Draw.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
												(Render.Draw.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ,
												 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
												 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
				break;
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Draw.CopyPos.X != 0)
	{
		for (int x = 0; x <  Render.Draw.CopyPos.X; x++)
		{
			for (int y = 0; y < Render.Draw.CopyPos.Y; y++)
			{
				if (x + LastX >= 0 && x + LastX <= Details.Width - 1 &&
				    y + LastY >= 0 && y + LastY <= Details.Height - 1)
				{
					if (MatrixDeadLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
					{
						PaintBox->Canvas->Brush->Color = TColor(LEDRGB3BPPColours[MatrixCopy->Grid[y * Details.Width + x]]); // ? to do
					}
					else
					{
						PaintBox->Canvas->Brush->Color = TColor(CanvasBackground);
					}

					switch(Render.Shape)
					{
					case PixelShape::kSquare:
						PaintBox->Canvas->FillRect(Rect((x + LastX) * Render.PixelSize,
														 (y + LastY) * Render.PixelSize,
														((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
														((y + LastY) * Render.PixelSize) + Render.PixelSizeZ));
						break;
					case PixelShape::kCircle: PaintBox->Canvas->Ellipse((x + LastX) * Render.PixelSize,
												   (y + LastY) * Render.PixelSize,
												  ((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
												  ((y + LastY) * Render.PixelSize) + Render.PixelSizeZ);
						break;
					case PixelShape::kRoundRect:
						PaintBox->Canvas->RoundRect((x + LastX) * Render.PixelSize,
													 (y + LastY) * Render.PixelSize,
													((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
													((y + LastY) * Render.PixelSize) + Render.PixelSizeZ,
													  Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
													  Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
						break;
					}
				}
			}
		}
	}

	PreviewBox->Invalidate();
}
#pragma end_region


#pragma region DeadPixels
void __fastcall TheMatrix::PaintBoxUpdateDeadPixel(TObject *Sender)
{
	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			if (MatrixDeadLayout->Grid[(Render.TopLeft.Y + y) * Details.Width + (Render.TopLeft.X + x)] == PixelAlive)
			{
				PaintBox->Canvas->Brush->Color = TColor(0x000000);
			}
			else
			{
				PaintBox->Canvas->Brush->Color = TColor(0xffffff);
			}

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
													 y * Render.PixelSize,
													(x * Render.PixelSize) + Render.PixelSizeZ,
													(y * Render.PixelSize) + Render.PixelSizeZ));
				break;
			case PixelShape::kCircle:
				PaintBox->Canvas->Ellipse(x * Render.PixelSize,
											   y * Render.PixelSize,
											  (x * Render.PixelSize) + Render.PixelSizeZ,
											  (y * Render.PixelSize) + Render.PixelSizeZ);
				break;
			case PixelShape::kRoundRect:
				PaintBox->Canvas->RoundRect(x * Render.PixelSize,
												 y * Render.PixelSize,
												(x * Render.PixelSize) + Render.PixelSizeZ,
												(y * Render.PixelSize) + Render.PixelSizeZ,
												 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)),
												 Render.PixelSize - (std::round(Render.PixelSize / CRoundRectCoeff)));
				break;
			}
		}
	}
}


void TheMatrix::SetDeadPixels(int deadness)
{
	for (int x = 0; x < __MaxWidth; x++)
	{
		for (int y = 0; y < __MaxHeight; y++)
		{
			MatrixDeadLayout->Grid[y * __MaxWidth + x] = deadness;
		}
	}
}


void TheMatrix::SetDeadPixelsFromCustomShape(CustomShape shape, int parameter)
{
	MatrixDeadLayout->SetFromCustomShape(Details.Width, Details.Height, shape, parameter);

	PaintBox->Invalidate();
}


void TheMatrix::SetDeadPixelsFromFileName(const std::wstring file_name)
{
	MatrixDeadLayout->Load(file_name);

	PaintBox->Invalidate();
}


void TheMatrix::SaveDeadPixels(const std::wstring file_name)
{
	MatrixDeadLayout->Save(file_name, Details.Width, Details.Height);
}


void __fastcall TheMatrix::ClickPixelDeadPixel(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)    // to do
{
	if (IsThisFrameLocked(0, CurrentFrame)) return;

	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		if (MatrixDeadLayout->Grid[y1 * Details.Width + x1] == PixelAlive)
		{
			MatrixDeadLayout->Grid[y1 * Details.Width + x1] = PixelDead;
		}
		else
		{
			MatrixDeadLayout->Grid[y1 * Details.Width + x1] = PixelAlive;
		}

		LastX = x1;
		LastY = y1;
	}

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseMoveDeadPixel(TObject *Sender, TShiftState Shift, int X, int Y)
{
	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (OnMouseOver) OnMouseOver(x1, y1);

	// ===========================================================================
	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		if (!(LastX == x1 && LastY == y1))
		{
			if (MatrixDeadLayout->Grid[y1 * Details.Width + x1] == PixelAlive)
			{
				MatrixDeadLayout->Grid[y1 * Details.Width + x1] = PixelDead;
			}
			else
			{
				MatrixDeadLayout->Grid[y1 * Details.Width + x1] = PixelAlive;
			}
		}
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseUpDeadPixel(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{

}
#pragma end_region


#pragma region Drawing
int TheMatrix::GetPixelFrom(MatrixMode matrixformat, MatrixMode importformat, int pixel, int background)
{
	switch (matrixformat)
	{
	case MatrixMode::kMono:
		switch (importformat)
		{
		case MatrixMode::kMono:
			return pixel;
		case MatrixMode::kBiSequential:
		case MatrixMode::kBiBitplanes:
		case MatrixMode::kRGB:
		case MatrixMode::kRGB3BPP:
			if (pixel != background)
			{
				return 1;
			}
			break;
		}
        break;
	case MatrixMode::kBiSequential:
	case MatrixMode::kBiBitplanes:
		switch (importformat)
		{
		case MatrixMode::kMono:
		case MatrixMode::kBiSequential:
		case MatrixMode::kBiBitplanes:
			return pixel;
		case MatrixMode::kRGB:
		case MatrixMode::kRGB3BPP:
			if (pixel != background)
			{
				return 1;
			}
			break;
		}
		break;
	case MatrixMode::kRGB:
		switch (importformat)
		{
		case MatrixMode::kMono:
		case MatrixMode::kBiSequential:
		case MatrixMode::kBiBitplanes:
			if (pixel != background)
			{
				return 0xffffff;
			}
			break;
		case MatrixMode::kRGB:
			return pixel;
		case MatrixMode::kRGB3BPP:
			return LEDRGB3BPPColours[pixel];
		}
		break;
	case MatrixMode::kRGB3BPP:
		switch (importformat)
		{
		case MatrixMode::kMono:
		case MatrixMode::kBiSequential:
		case MatrixMode::kBiBitplanes:
			if (pixel != background)
			{
				return 0xffffff;
			}
		case MatrixMode::kRGB:
			for (int t = 0; t < 8; t++)
			{
				if (LEDRGB3BPPColours[t] == pixel)
				{
					return LEDRGB3BPPColours[t];
				}
			}
			break;
		case MatrixMode::kRGB3BPP:
			return pixel;
		}
        break;
	}

	return 0;
}


void TheMatrix::ConfigurePaintboxDrawing()
{
	if (MatrixReadOnly)
	{
		PaintBox->OnMouseDown = nullptr;
		PaintBox->OnMouseMove = nullptr;
		PaintBox->OnMouseUp   = nullptr;

		switch (Details.Mode)
		{
		case MatrixMode::kMono:
		case MatrixMode::kBiSequential:
		case MatrixMode::kBiBitplanes:
			PaintBox->OnPaint = PaintBoxUpdate;
			break;
		case MatrixMode::kRGB:
			PaintBox->OnPaint = PaintBoxUpdateRGB;
			break;
		case MatrixMode::kRGB3BPP:
			PaintBox->OnPaint = PaintBoxUpdateRGB_3BPP;
			break;
		}
	}
	else
	{
		if (DeadPixelsMode)
		{
			PaintBox->OnMouseDown = ClickPixelDeadPixel;
			PaintBox->OnMouseMove = Shape1MouseMoveDeadPixel;
			PaintBox->OnMouseUp   = Shape1MouseUpDeadPixel;

			PaintBox->OnPaint     = PaintBoxUpdateDeadPixel;
		}
		else
		{
			switch (Details.Mode)
			{
			case MatrixMode::kMono:
				PaintBox->OnMouseDown = ClickPixel;
				PaintBox->OnMouseMove = Shape1MouseMove;
				PaintBox->OnMouseUp   = Shape1MouseUp;

				PaintBox->OnPaint     = PaintBoxUpdate;
				break;
			case MatrixMode::kBiSequential:
				PaintBox->OnMouseDown = ClickPixelBiColour;
				PaintBox->OnMouseMove = Shape1MouseMoveBiColour;
				PaintBox->OnMouseUp   = Shape1MouseUpBiColour;

				PaintBox->OnPaint     = PaintBoxUpdate;
				break;
			case MatrixMode::kBiBitplanes:
				PaintBox->OnMouseDown = ClickPixelBiColour;
				PaintBox->OnMouseMove = Shape1MouseMoveBiColour;
				PaintBox->OnMouseUp   = Shape1MouseUpBiColour;

				PaintBox->OnPaint     = PaintBoxUpdate;
				break;
			case MatrixMode::kRGB:
				PaintBox->OnMouseDown = ClickPixelRGB;
				PaintBox->OnMouseMove = Shape1MouseMoveRGB;
				PaintBox->OnMouseUp   = Shape1MouseUpRGB;

				PaintBox->OnPaint     = PaintBoxUpdateRGB;
				break;
			case MatrixMode::kRGB3BPP:
				PaintBox->OnMouseDown = ClickPixelRGB;
				PaintBox->OnMouseMove = Shape1MouseMoveRGB;
				PaintBox->OnMouseUp   = Shape1MouseUpRGB;

				PaintBox->OnPaint     = PaintBoxUpdateRGB_3BPP;
				break;
			}
		}
	}
}


void TheMatrix::UpdateDrawTool(int setx, int sety, int setcolour, bool isgradient)
{
	Render.Draw.Coords[Render.Draw.Point].X = setx;
	Render.Draw.Coords[Render.Draw.Point].Y = sety;

	if (Render.Draw.Point == CDrawPointNone)
	{
		Render.Draw.Colour = setcolour;
	}

	BackupMatrix(CurrentLayer, CurrentFrame);

	switch (Render.Draw.Mode)
	{
	case DrawMode::kFilledBox:
	case DrawMode::kEmptyBox:
	case DrawMode::kLine:
	case DrawMode::kEmptyCircle:
	case DrawMode::kFilledCircle:
		Render.Draw.Point++;

		if (Render.Draw.Point == CDrawPointLast)
		{
			DrawShape(false, Render.Draw.Colour, isgradient);

			CopyDrawBufferToCurrentFrame();
		}
		break;
	case DrawMode::kCopy:
		Render.Draw.Point++;

		if (Render.Draw.Point == CDrawPointLast)
		{
			CopyCurrentFrameToDrawBuffer();

			CopyShape();
		}
		break;
	case DrawMode::kFloodFill:
		FloodFill(setx, sety, Render.Draw.Colour);
		break;
	case DrawMode::kSpiral:
	case DrawMode::kRing:
	case DrawMode::kSplitRing:
	case DrawMode::kPetals:
	case DrawMode::kGrid:
	case DrawMode::kPyramid:
	case DrawMode::kLeftTriangle:
	case DrawMode::kRightTriangle:
		DrawShape(False, Render.Draw.Colour, isgradient);

		CopyDrawBufferToCurrentFrame();
		break;
	}
}


void TheMatrix::DrawWithBrush(int index, int x, int y)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!MatrixLayers[CurrentLayer]->Visible) return;

	switch (Render.Brush)
	{
	case BrushSize::kSmall:
		PlotPixelMatrix(x, y, index);

		MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
		break;
	case BrushSize::kMedium:
		PlotPixelMatrix(x,     y,     index);
		PlotPixelMatrix(x + 1, y,     index);
		PlotPixelMatrix(x,     y + 1, index);
		PlotPixelMatrix(x + 1, y + 1, index);

		MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
		break;
	case BrushSize::kLarge:
		for (int a = 0; a < 3; a++)
		{
			for (int b = 0; b < 3; b++)
			{
				PlotPixelMatrix(x + a, y + b, index);
			}
		}

		MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
		break;

	default:
        ShowMessage(L"error brush size");
	}
}


// draws identical pixels on every frame
void TheMatrix::DrawWithBrushMulti(int index, int x, int y)
{
	for (int frame = 0; frame < Render.Draw.Special; frame++)
	{
		if (!IsThisFrameLocked(CurrentLayer, frame) &&
			MatrixLayers[CurrentLayer]->Visible)
		{
			switch (Render.Brush)
			{
			case BrushSize::kSmall:
				PlotPixelMatrixFrame(frame, x, y, index);

				MatrixLayers[CurrentLayer]->Cells[frame]->AddToHistory(DisplayBuffer);
				break;
			case BrushSize::kMedium:
				PlotPixelMatrixFrame(frame, x, y,         index);
				PlotPixelMatrixFrame(frame, x + 1, y,     index);
				PlotPixelMatrixFrame(frame, x, y + 1,     index);
				PlotPixelMatrixFrame(frame, x + 1, y + 1, index);

				MatrixLayers[CurrentLayer]->Cells[frame]->AddToHistory(DisplayBuffer);
				break;
			case BrushSize::kLarge:
				for (int a = 0; a < 3; a++)
				{
					for (int b = 0; b < 3; b++)
					{
						PlotPixelMatrixFrame(frame, x + a, y + b, index);
					}
				}

				MatrixLayers[CurrentLayer]->Cells[frame]->AddToHistory(DisplayBuffer);
				break;
			}
		}
	}
}


void TheMatrix::DrawWithGradientBrush(int x, int y)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!MatrixLayers[CurrentLayer]->Visible ||
		Gradient.size() == 0) return;

	PlotPixelMatrixFrame(CurrentFrame, x, y, Gradient[Render.Draw.Parameter]);

	if (Render.Draw.Parameter == Gradient.size() - 1)
	{
		Render.Draw.Parameter = 0;
	}
	else
	{
		Render.Draw.Parameter++;
	}

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
}


void TheMatrix::DrawWithBrushPaste(int x1, int y1, bool transparent)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!MatrixLayers[CurrentLayer]->Visible) return;

	switch (Details.Mode)
	{
	case MatrixMode::kRGB:
	case MatrixMode::kRGB3BPP:
		for (int x2 = 0; x2 < Render.Draw.CopyPos.X; x2++)
		{
			for (int y2 = 0; y2 < Render.Draw.CopyPos.Y; y2++)
			{
				if (x2 + x1 >= 0 && x2 + x1 < Details.Width &&
					y2 + y1 >= 0 && y2 + y1 < Details.Height)
				{
					if (MatrixCopy->Grid[y2 * Details.Width + x2] != RGBBackground)
					{
						PlotPixelMatrix(x2 + x1, y2 + y1, MatrixCopy->Grid[y2 * Details.Width + x2]);
					}
					else
					{
						if (transparent)
						{
							PlotPixelMatrix(x2 + x1, y2 + y1, RGBBackground);
						}
                    }
				}
			}
        }
		break;
	default:
		for (int x2 = 0; x2 < Render.Draw.CopyPos.X; x2++)
		{
			for (int y2 = 0; y2 < Render.Draw.CopyPos.Y; y2++)
			{
				if (x2 + x1 >= 0 && x2 + x1 < Details.Width &&
					y2 + y1 >= 0 && y2 + y1 < Details.Height)
				{
					if (MatrixCopy->Grid[y2 * Details.Width + x2] == 1)
					{
						PlotPixelMatrix(x2 + x1, y2 + y1, 1);
					}
					else
					{
						if (transparent)
						{
							PlotPixelMatrix(x2 + x1, y2 + y1, 0);
						}
					}
				}
			}
		}
	}

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
}


void TheMatrix::DrawWithBrushPasteEveryFrame(int x1, int y1, bool transparent)
{
	switch (Details.Mode)
	{
	case MatrixMode::kRGB:
	case MatrixMode::kRGB3BPP:
		for (int x2 = 0; x2 < Render.Draw.CopyPos.X; x2++)
		{
			for (int y2 = 0; x2 < Render.Draw.CopyPos.Y; y2++)
			{
				if (x2 + x1 >= 0 && x2 + x1 < Details.Width &&
					y2 + y1 >= 0 && y2 + y1 < Details.Height)
				{
					for (int frame = 0; frame < MatrixLayers[CurrentLayer]->Cells.size(); frame++)
					{
						if (!IsThisFrameLocked(CurrentLayer, frame))
						{
							if (MatrixCopy->Grid[y2 * Details.Width + x2] != RGBBackground)
							{
								PlotPixelMatrixFrame(frame, x2 + x1, y2 + y1, MatrixCopy->Grid[y2 * Details.Width + x2]);
							}
							else
							{
								if (!transparent)
								{
									PlotPixelMatrixFrame(frame, x2 + x1, y2 + y1, RGBBackground);
								}
							}
						}
					}
				}
			}
		}
		break;
	default:
		for (int x2 = 0; x2 < Render.Draw.CopyPos.X; x2++)
		{
			for (int y2 = 0; y2 < Render.Draw.CopyPos.Y; y2++)
			{
				if (x2 + x1 >= 0 && x2 + x1 < Details.Width &&
					y2 + y1 >= 0 && y2 + y1 < Details.Height)
				{
					for (int frame = 0; frame < MatrixLayers[CurrentLayer]->Cells.size(); frame++)
					{
						if (!IsThisFrameLocked(CurrentLayer, frame))
						{
							if (MatrixCopy->Grid[y2 * Details.Width + x2] == 1)
							{
								PlotPixelMatrixFrame(frame, x2 + x1, y2 + y1, 1);
							}
							else
							{
								if (!transparent)
								{
									PlotPixelMatrixFrame(frame, x2 + x1, y2 + y1, 0);
								}
							}
						}
					}
				}
			}
		}
    }

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
}


void TheMatrix::PlotInBounds(int x, int y, int colour)
{
	if (x >= 0 && x < Details.Width &&
		y >= 0 && y < Details.Height)
	{
		PlotPixelMatrix(x, y, colour);
	}
}


// this and PlotPixelMatrixFrame() are the only two safe methods of drawing on the matrix
// this takes in to account the gradient status and allows for the drawing buffer and
// various other drawing modes.
void TheMatrix::PlotPixelMatrix(int x, int y, int defaultcolour)
{
	int colour = defaultcolour;
	int newcoord = 0;

	if (LastMouseButton == CMouseMiddle)
	{
		switch (Render.Gradient.Option)
		{
		// goOff        : lColour = aDefaultColour;
		case GradientOption::kVertical:
			colour = Render.Gradient.IY[y];
			break;
		case GradientOption::kHorizontal:
			colour = Render.Gradient.IX[x];
			break;
		}
	}

	switch (Mirror)
	{
	case MirrorMode::kHorizontal:
		newcoord = Details.Height - y - 1;
		break;
	case MirrorMode::kVertical:
		newcoord = Details.Width - x - 1;
		break;
	default:
		newcoord = Details.Height - y - 1;
	}

	DisplayBuffer->Grid[y * Details.Width + x] = colour;

	switch (Mirror)
	{
	case MirrorMode::kHorizontal:
		DisplayBuffer->Grid[newcoord * Details.Width + x] = colour;
		break;
	case MirrorMode::kVertical:
		DisplayBuffer->Grid[y * Details.Width + newcoord] = colour;
		break;
	}
}


// this and PlotPixelMatrix() are the only two safe methods of drawing on the matrix
// this takes in to account the gradient status and allows for the drawing buffer and
// various other drawing modes.
void TheMatrix::PlotPixelMatrixFrame(int frame, int x, int y, int defaultcolour) // check currentlayer is okay to o
{
	int colour = defaultcolour;
	int newcoord = 0;

	switch (Render.Gradient.Option)
	{
	case GradientOption::kVertical:
		colour = Render.Gradient.IY[y];
		break;
	case GradientOption::kHorizontal:
		colour = Render.Gradient.IX[x];
		break;
	}

	switch (Mirror)
	{
	case MirrorMode::kHorizontal:
		newcoord = Details.Height - y - 1;
		break;
	case MirrorMode::kVertical:
		newcoord = Details.Width - x - 1;
		break;
	default:
		newcoord = Details.Height - y - 1;
	}

	if (frame == CurrentFrame)
	{
		DisplayBuffer->Grid[y * Details.Width + x] = colour;

		switch (Mirror)
		{
		case MirrorMode::kHorizontal:
			DisplayBuffer->Grid[newcoord * Details.Width + x] = colour;
			break;
		case MirrorMode::kVertical:
			DisplayBuffer->Grid[y * Details.Width + newcoord] = colour;
			break;
		}
	}
	else
	{
		MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + x] = colour;

		switch (Mirror)
		{
		case MirrorMode::kHorizontal:
			MatrixLayers[CurrentLayer]->Cells[frame]->Grid[newcoord * Details.Width + x] = colour;
			break;
		case MirrorMode::kVertical:
			MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + newcoord] = colour;
			break;
		}
	}
}


void TheMatrix::SimpleLine(int x1, int y1, int x2, int y2, int old_colour, bool gradient)
{
	int column = x1;
	int colour = old_colour;

	if (gradient)
	{
		switch (Render.Gradient.Option)
		{
		case GradientOption::kVertical:
			colour = Render.Gradient.IY[y1];
			break;
		case GradientOption::kHorizontal:
			colour = Render.Gradient.IX[x1];
			break;
		}
	}

	PaintBox->Canvas->Brush->Color = TColor(colour);

	while (column <= x2)
	{
		PlotInBounds(column, y1, colour);

		column++;
	}
}


void TheMatrix::DrawShape(bool realtime, int colour, bool isgradient)
{
	CopyCurrentFrameToDrawBuffer();

	if (!realtime)
	{
		BackupMatrix(CurrentLayer, CurrentFrame);
	}
	else
	{
		Render.Draw.Coords[1].X = LastX;
		Render.Draw.Coords[1].Y = LastY;
	}

	int x1 = Render.Draw.Coords[0].X;
	int y1 = Render.Draw.Coords[0].Y;
	int x2 = Render.Draw.Coords[1].X;
	int y2 = Render.Draw.Coords[1].Y;

  	// ===========================================================================

	switch (Render.Draw.Mode)
	{
	// =========================================================================
	// == Filled Box ===========================================================
	// =========================================================================
	case DrawMode::kFilledBox:
	{
		if (x1 > x2)
		{
			std::swap(x1, x2);
		}

		if (y1 > y2)
		{
			std::swap(y1, y2);
		}

		for (int x = x1; x <= x2; x++)
		{
			for (int y = y1; y <= y2; y++)
			{
				PlotPixelMatrix(x, y, colour);
			}
		}

		break;
	}
	// =========================================================================
	// == Empty Box ============================================================
	// =========================================================================
	case DrawMode::kEmptyBox:
	{
		if (x1 > x2)
		{
			std::swap(x1, x2);
		}

		if (y1 > y2)
		{
			std::swap(y1, y2);
		}

		for (int x = x1; x <= x2; x++)
		{
			for (int y = y1; y <= y2; y++)
			{
				if (x == x1 || x == x2 || y == y1 || y == y2)
				{
					PlotPixelMatrix(x, y, colour);
				}
			}
		}

		break;
	}

	// =========================================================================
	// == Straight Line ========================================================
	// =========================================================================
	case DrawMode::kLine:
	{
		int x = x1;             // line starting point
		int y = y1;

		// Determine drawing direction and step to the next pixel.
		int a = x2 - x1;       	// difference in x dimension
		int b = y2 - y1;       	// difference in y dimension
		int d = 0;
        int tc = 0;

		int dx_diag = 0;        // diagonal x step for next pixel
		int dy_diag = 0;        // diagonal y step for next pixel
		int dx_nondiag = 0;     // nondiagonal x step for next pixel
		int dy_nondiag = 0;     // nondiagonal y step for next pixel

		int nondiag_inc = 0;      // set initial d increment values
		int diag_inc = 0;

		// Determine whether end point lies to right or left of start point.
		if (a < 0)
		{
			a = -a;                // make 'a' positive
			dx_diag = -1;
		}
		else
		{
			dx_diag = 1;
		}

		// Determine whether end point lies above or below start point.
		if (b < 0)
		{
			b = -b;                // make 'a' positive
			dy_diag = -1;
		}
		else
		{
			dy_diag = 1;
		}

		// Identify octant containing end point.
		if (a < b)
		{
			tc = a;
			a = b;
			b = tc;
			dx_nondiag = 0;
			dy_nondiag = dy_diag;
		}
		else
		{
			dx_nondiag = dx_diag;
			dy_nondiag = 0;
		}

		d = b + b - a;            // initial value for d is 2*b - a
		nondiag_inc = b + b;      // set initial d increment values
		diag_inc    = b + b - a - a;

		for (int i = 0; i <= a; i++)
		{
			PlotPixelMatrix(x, y, colour);

			if (d < 0)          	// step nondiagonally
			{
				x += dx_nondiag;
				y += dy_nondiag;
				d += nondiag_inc;// update decision variable
			}
			else
			{               		// midpoint is above the line; step diagonally
				x += dx_diag;
				y += dy_diag;
				d += diag_inc;
			}
		}

		break;
	}

    // =========================================================================
    // == Empty Circle =========================================================
	// =========================================================================
	case DrawMode::kEmptyCircle:
	{
		// c^2 = a^2 + b^2
		int tc = std::round(std::sqrt(std::pow(std::abs(x1 - x2), 2) + std::pow(std::abs(y1 - y2), 2))); // radius of circle

		// midpoint algorithm: http://en.wikipedia.org/wiki/Midpoint_circle_algorithm
		int a = 0;
		int b = 1 - tc;

		while (tc >= a)
		{
			PlotInBounds( tc + x1,   a + y1, colour);
			PlotInBounds(  a + x1,  tc + y1, colour);
			PlotInBounds(-tc + x1,   a + y1, colour);
			PlotInBounds( -a + x1,  tc + y1, colour);
			PlotInBounds(-tc + x1,  -a + y1, colour);
			PlotInBounds( -a + x1, -tc + y1, colour);
			PlotInBounds( tc + x1,  -a + y1, colour);
			PlotInBounds(  a + x1, -tc + y1, colour);

			a++;

			if (b < 0)
			{
				b += 2 * a + 1;
			}
			else
			{
				tc--;

				b += 2 * (a - tc + 1);
			}
		}

		break;
	}

    // =========================================================================
    // == Filled Circle ========================================================
    // =========================================================================
	case DrawMode::kFilledCircle:
	{
		// c^2 = a^2 + b^2
		int tc = std::round(std::sqrt(std::pow(std::abs(x1 - x2), 2) + std::pow(std::abs(y1 - y2), 2))); // radius of circle

		// midpoint algorithm: http://en.wikipedia.org/wiki/Midpoint_circle_algorithm
		int a = 0;
		int b = 1 - tc;

		while (tc >= a)
		{
			if (realtime)
			{
				SimpleLine(-tc + x1,   a + y1, tc + x1,   a + y1, colour, isgradient);
				SimpleLine( -a + x1,  tc + y1,  a + x1,  tc + y1, colour, isgradient);
				SimpleLine(-tc + x1,  -a + y1, tc + x1,  -a + y1, colour, isgradient);
				SimpleLine( -a + x1, -tc + y1,  a + x1, -tc + y1, colour, isgradient);
			}
			else
			{
				SimpleLine(-tc + x1,   a + y1, tc + x1,   a + y1, colour, isgradient);
				SimpleLine( -a + x1,  tc + y1,  a + x1,  tc + y1, colour, isgradient);
				SimpleLine(-tc + x1,  -a + y1, tc + x1,  -a + y1, colour, isgradient);
				SimpleLine( -a + x1, -tc + y1,  a + x1, -tc + y1, colour, isgradient);
			}

			a++;

			if (b < 0)
			{
				b += 2 * a + 1;
			}
			else
			{
				tc--;

				b += 2 * (a - tc + 1);
			}
		}

		break;
	}

	 // =========================================================================
	 // == Copy Lasso thing =====================================================
	 // =========================================================================
	case DrawMode::kCopy:
	{
		if (!realtime) return;

		if (x1 > x2)
		{
			std::swap(x1, x2);
		}

		if (y1 > y2)
		{
			std::swap(y1, y2);
		}

		PaintBox->Canvas->Brush->Color = TColor(LEDColours[CDisplayMarker]);

		for (int x = x1; x <= x2; x++)
		{
			for (int y = y1; y <= y2; y++)
			{
				if (x == x1 || x == x2 || y == y1 || y == y2)
				{
					PlotPixelMatrix(x, y, colour);
				}
			}
		}

        break;
	}

	// =========================================================================
	// == Patterns: Spiral =====================================================
	// =========================================================================
	case DrawMode::kSpiral:
	{
		int a = LastX;
		int b = 0;

		while (b < Details.Height)
		{
			PlotPixelMatrix(a, b, colour);

			if (a == Details.Width - 1)
			{
				a = 0;
			}
			else
			{
				a++;

				b += Render.Draw.Parameter;
			}
		}

		break;
	}

	case DrawMode::kRing:
	{
		int y = LastY;

		for (int x = 0; x < Details.Width; x++)
		{
			PlotPixelMatrix(x, y, colour);
		}

		break;
	}

	case DrawMode::kSplitRing:
	{
		int x = LastX;
		int y = LastY;

		int a = 0;
		int d = 0;

		if (x == 0)
		{
			int a = Details.Width - 1;
		}
		else
		{
			a = x - 1;
		}

		d = Render.Draw.Parameter; // count between pixels X000X = 4

		while (x != a)
		{
			if (d == Render.Draw.Parameter)
			{
				PlotPixelMatrix(x, y, colour);

				d = 0;
			}
			else
			{
				d++;
			}

			if (x == Details.Width - 1)
			{
				x = 0;
			}
			else
			{
				x++;
			}
		}

		break;
	}

	case DrawMode::kPetals:
	{
		int x = LastX;
		int a = 0;
		int i = 0;
		int j = 0;

		if (x == 0)
		{
			a = Details.Width - 1;
		}
		else
		{
			a = x - 1;
		}

		int d = Render.Draw.Parameter;

		while (x != a)
		{
			if (d == Render.Draw.Parameter)
			{
				i = x; // left part
				j = x; // right part

				for (int y = Details.Height - 1; y >= 0; y--)
				{
					PlotPixelMatrix(i, y, colour);
					PlotPixelMatrix(j, y, colour);

					if (i == 0)
					{
						i = Details.Width - 1;
					}
					else
					{
						i--;
					}

					if (j == Details.Width - 1)
					{
						j = 0;
					}
					else
					{
						j++;
					}

					d = 1;
				}
			}
			else
			{
				d++;
			}

			if (x == Details.Width - 1)
			{
				x = 0;
			}
			else
			{
				x++;
			}
		}

		break;
	}

	case DrawMode::kGrid:
	{
		int x = LastX;
		int y = LastY;

		int a = 0;
		int b = 0;

		if (x == 0)
		{
			a = Details.Width - 1;
		}
		else
		{
			a = x - 1;
		}

		if (y == 0)
		{
			b = Details.Height - 1;
		}
		else
		{
			b = y - 1;
		}

		int d = Render.Draw.Parameter; // count between pixels X000X = 4

		while (x != a)
		{
			if (d == Render.Draw.Parameter)
			{
				for (int i = 0; i < Details.Height; i++)
				{
					PlotPixelMatrix(x, i, colour);
				}

				d = 0;
			}
			else
			{
				d++;
			}

			if (x == Details.Width - 1)
			{
				x = 0;
			}
			else
			{
				x++;
			}
		}

		d = Render.Draw.Parameter;

		while (y != b)
		{
			if (d == Render.Draw.Parameter)
			{
				for (int i = 0; i < Details.Width; i++)
				{
					 PlotPixelMatrix(i, y, colour);
				}

				d = 0;
			}
			else
			{
				d++;
			}

			if (y == Details.Height - 1)
			{
				y = 0;
			}
			else
			{
				y++;
			}
		}

		break;
	}

	case DrawMode::kPyramid:
	{
		int i = 1;
		int x = LastX - 1;
		int y = LastY;

		while (y < Details.Height)
		{
			for (int a = 1; a <= i; a++)
			{
				PlotInBounds(x + a, y, colour);
			}

			x--;
			i += 2;
			y += Render.Draw.Parameter;
		}

		break;
	}

	case DrawMode::kLeftTriangle:
	{
		int i = 1;
		int x = LastX - 1;
		int y = LastY;

		while (y < Details.Height)
		{
			for (int a = 1; a <= i; a++)
			{
				PlotInBounds(x + a, y, colour);
			}

			i++;
			y += Render.Draw.Parameter;
		}

		break;
	}

	case DrawMode::kRightTriangle:
	{
		int i = 1;
		int x = LastX - 1;
		int y = LastY;

		while (y < Details.Height)
		{
			for (int a = 1; a <= i; a++)
			{
				PlotInBounds(x + a, y, colour);
			}

			x--;
			i++;
			y += Render.Draw.Parameter;
		}

		break;
	}
	}

	if (!realtime)
	{
		MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);

		if (OnChange) OnChange(this);

		Render.Draw.Point       = CDrawPointNone;
		Render.Draw.Coords[0].X = -1;
		Render.Draw.Coords[0].Y = -1;
	}

	PaintBox->Invalidate();
}


void TheMatrix::FloodFill(int x, int y, int fillcolour)
{
	if (fillcolour != DisplayBuffer->Grid[y * Details.Width + x])
	{
		Busy = true;

		DoFill(x, y, fillcolour);

		Busy = false;

		Render.Draw.Coords[0].X = - 1;

		CopyDrawBufferToCurrentFrame();

		MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();

		PaintBox->Invalidate();
	}
}


// based on code from here:
// https://stackoverflow.com/questions/53247243/how-should-i-implement-a-flood-fill-function-to-my-c-program
void TheMatrix::DoFill(int x, int y, int fillcolour)
{
	int initialcolour = DisplayBuffer->Grid[y * Details.Width + x];

	DisplayBuffer->Grid[y * Details.Width + x] = fillcolour;

	if (x > 0 && DisplayBuffer->Grid[y * Details.Width + (x - 1)] == initialcolour)
	{
		DoFill(x - 1, y, fillcolour);
	}

	if (x + 1 < Details.Width && DisplayBuffer->Grid[y * Details.Width + (x + 1)] == initialcolour)
	{
		DoFill(x + 1, y, fillcolour);
	}

	if (y > 0 && DisplayBuffer->Grid[(y - 1) * Details.Width + x] == initialcolour)
	{
		DoFill(x, y - 1, fillcolour);
	}

	if (y + 1 < Details.Height && DisplayBuffer->Grid[(y + 1) * Details.Width + x] == initialcolour)
	{
		DoFill(x, y + 1, fillcolour);
    }
}


void TheMatrix::CancelDrawMode()
{
	Render.Draw.Mode = DrawMode::kNone;
	Render.Draw.Point = CDrawPointNone;
	Render.Draw.Coords[0].X = -1;
	Render.Draw.Coords[0].Y = -1;
	Render.Draw.CopyPos.X = 0;
	Render.Draw.CopyPos.Y = 0;

	CopyCurrentFrameToDrawBuffer();
}


void TheMatrix::CopyShape()
{
	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			if (Details.Mode == MatrixMode::kRGB)
			{
				MatrixCopy->Grid[y * Details.Width + x] = RGBBackground;
			}
			else
			{
				MatrixCopy->Grid[y * Details.Width + x] = 0;
			}
		}
	}

	if (Render.Draw.Coords[0].X > Render.Draw.Coords[1].X)
	{
		int tc = Render.Draw.Coords[0].X;

		Render.Draw.Coords[0].X = Render.Draw.Coords[1].X;
		Render.Draw.Coords[1].X = tc;
	}

	if (Render.Draw.Coords[0].Y > Render.Draw.Coords[1].Y)
	{
		int tc = Render.Draw.Coords[0].Y;

		Render.Draw.Coords[0].Y = Render.Draw.Coords[1].Y;
		Render.Draw.Coords[1].Y = tc;
	}

	Render.Draw.CopyPos.X = Render.Draw.Coords[1].X - Render.Draw.Coords[0].X;
	Render.Draw.CopyPos.Y = Render.Draw.Coords[1].Y - Render.Draw.Coords[0].Y;

	for (int x = Render.Draw.Coords[0].X; x < Render.Draw.Coords[1].X; x++)
	{
		for (int y = Render.Draw.Coords[0].Y; y < Render.Draw.Coords[1].Y; y++)
		{
			MatrixCopy->Grid[(y - Render.Draw.Coords[0].Y) * Details.Width + (x - Render.Draw.Coords[0].X)] = MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x];
		}
	}

	Render.Draw.Point       = CDrawPointNone;
	Render.Draw.Mode        = DrawMode::kPaste;
	Render.Draw.Coords[0].X = -1;
	Render.Draw.Coords[0].Y = -1;
}
#pragma end_region


#pragma region Brush
void TheMatrix::RotateCopyBrush(int mode)
{
	if (Render.Draw.CopyPos.X == Render.Draw.CopyPos.Y)
	{
		BackupMatrix(-1, -1);

		switch (mode)
		{
		case modeRotateCW:
			for (int x = 0; x <= Render.Draw.CopyPos.X; x++)
			{
				for (int y = 0; y <= Render.Draw.CopyPos.Y; y++)
				{
					MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[(Render.Draw.CopyPos.X - x) * Details.Width + y];
				}
			}
			break;
		case modeRotateACW:
			for (int x = 0; x <= Render.Draw.CopyPos.X; x++)
			{
				for (int y = 0; y <= Render.Draw.CopyPos.Y; y++)
				{
					MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[x * Details.Width + (Render.Draw.CopyPos.Y - y)];
				}
			}
			break;
		}

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformEffectOnBrush(int mode)
{
	BackupMatrix(-1, -1);

	switch (mode)
	{
	case modeFlip:
		for (int x = 0; x <= Render.Draw.CopyPos.X; x++)
		{
			for (int y = 0; y <= Render.Draw.CopyPos.Y; y++)
			{
				MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (Render.Draw.CopyPos.X - x)];
			}
		}
		break;
	case modeMirror:
		for (int y = 0; y <= Render.Draw.CopyPos.X; y++)
		{
			for (int x = 0; x <= Render.Draw.CopyPos.Y; x++)
			{
				MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[(Render.Draw.CopyPos.Y - y) * Details.Width + x];
			}
		}
		break;
	case modeInvert:
		for (int x = 0; x <= Render.Draw.CopyPos.X; x++)
		{
			for (int y = 0; y <= Render.Draw.CopyPos.Y; y++)
			{
				switch (Details.Mode)
				{
				case MatrixMode::kMono:
					MatrixCopy->Grid[y * Details.Width + x] = 1 - MatrixBackup->Grid[y * Details.Width + x];
					break;
				case MatrixMode::kBiSequential:
				case MatrixMode::kBiBitplanes:
					MatrixCopy->Grid[y * Details.Width + x] = 3 - MatrixBackup->Grid[y * Details.Width + x];
					break;
				case MatrixMode::kRGB:
					MatrixCopy->Grid[y * Details.Width + x] = 0xFFFFFF - MatrixBackup->Grid[y * Details.Width + x];
					break;
				case MatrixMode::kRGB3BPP:
					MatrixCopy->Grid[y * Details.Width + x] = 0x000004 - MatrixBackup->Grid[y * Details.Width + x];
					break;
				}
			}
		}
		break;
	}

	PaintBox->Invalidate();
}


void TheMatrix::SetPixelBrush(BrushSize newbrushsize)
{
	Render.Brush = newbrushsize;
}
#pragma end_region


#pragma region Font
void TheMatrix::DrawFontCharacter(int ascii, int frame)
{
	const int __FontWidth = 8;
	const int __FontHeight = 8;

	int startY = Render.Draw.Coords[0].Y;
	std::wstring temp = L"";
	bool canwrite = true;

	for (int x = TextFont->Start[ascii]; x <= TextFont->End[ascii]; x++)
	{
		for (int y = 0; y < 8; y++)
		{
			int outputx = Render.Draw.Coords[0].X;
			int outputy = startY - y;

			if (FontWrap)
			{
				if (outputx > Details.Width - 1)
				{
					outputx = outputx - Details.Width;
					Render.Draw.Coords[0].X = outputx;
				}

				if (outputy < 0)
				{
					outputy = outputy + Details.Height;
				}
			}
			else
			{
				canwrite = (Render.Draw.Coords[0].X >= 0 &&
					Render.Draw.Coords[0].X <= Details.Width - 1 &&
					y >= 0 && startY - y <= Details.Height - 1);
			}

			if (canwrite)
			{
				int data_index = (ascii * __FontWidth * __FontHeight) + (y * __FontWidth + x);

				if (Details.Mode == MatrixMode::kRGB)
				{
					switch (TextFont->Mode)
					{
					case MatrixMode::kMono:
						if (TextFont->Data[data_index] == 1)
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[outputy * Details.Width + outputx] = Render.Draw.Colour;
						}
						break;
					case MatrixMode::kRGB:
						if (TextFont->Data[data_index] != -1)
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[outputy * Details.Width + outputx] = TextFont->Data[data_index];
						}
						break;
					}
				}
			   else
			   {
					switch (TextFont->Mode)
					{
					case MatrixMode::kMono:
						if (TextFont->Data[data_index] == 1)
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[outputy * Details.Width + outputx] = Render.Draw.Colour;
						}
						break;
					case MatrixMode::kRGB:
						if (TextFont->Data[data_index] != -1)
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[outputy * Details.Width + outputx] = Render.Draw.Colour;
						}
					}
			   }
			}
		}

		Render.Draw.Coords[0].X++;
	}

	Render.Draw.Coords[0].X++; // adds single column spacing between chars

	if (OnChange) OnChange(this);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}


void TheMatrix::DeleteFontCharacter(int frame)
{
	Render.Draw.Coords[0].X--;

	for (int y = Render.Draw.Coords[0].Y; y >= Render.Draw.Coords[0].Y - 7; y--)
	{
		if (Render.Draw.Coords[0].X >= 0 &&
			Render.Draw.Coords[0].X <= Details.Width - 1 &&
			y >= 0 && y <= Details.Height - 1)
		{
			MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + Render.Draw.Coords[0].X] = 0;
		}
	}

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::LoadTextToolFont(const std::wstring file_name)
{
	TextFont->Load(file_name);
}
#pragma end_region


#pragma region Frame
void TheMatrix::InsertBlankFrameAt(int insertat)
{
	if (!AutomateMode) Busy = True;


	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		Matrix *m = new Matrix(Details.Width, Details.Height, Details.Mode, RGBBackground);

		if (insertat >= MatrixLayers[layer]->Cells.size())
		{
			MatrixLayers[layer]->Cells.push_back(m);
		}
		else
		{
			MatrixLayers[layer]->Cells.insert(MatrixLayers[layer]->Cells.begin() + insertat, m);
		}
	}

	if (!AutomateMode)
	{
		if (OnSizeChange) OnSizeChange(this);

		Busy = false;
    }
}


void TheMatrix::InsertCopyFrameAt(int insertat)
{
	if (!AutomateMode)
	{
		Busy = true;
    }

	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		Matrix *m = new Matrix(Details.Width, Details.Height, Details.Mode, RGBBackground);

		for (int x = 0; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				m->Grid[y * Details.Width + x] = MatrixLayers[layer]->Cells[CurrentFrame]->Grid[y * Details.Width + x];
			}
		}

		if (insertat >= MatrixLayers[layer]->Cells.size())
		{
			MatrixLayers[layer]->Cells.push_back(m);
		}
		else
		{
			MatrixLayers[layer]->Cells.insert(MatrixLayers[layer]->Cells.begin() + insertat, m);
		}
	}

	if (!AutomateMode)
	{
		if (OnSizeChange) OnSizeChange(this);

		Busy = false;
	}
}


void TheMatrix::AddFrameMultiple(int count, int current)
{
	int oldframe = current;

	for (int frame = 0; frame < count; frame++)
	{
		InsertBlankFrameAt(oldframe);

		oldframe++;
	}
}


void TheMatrix::DeleteFrame(int frame)
{
	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		MatrixLayers[layer]->Cells.erase(MatrixLayers[layer]->Cells.begin() + frame);
	}

	if (frame >= MatrixLayers[CPermanentLayer]->Cells.size())
	{
		CurrentFrame = MatrixLayers[CPermanentLayer]->Cells.size() - 1;
	}

	if (OnNewFrameDisplayed) OnNewFrameDisplayed(this);

	if (OnSizeChange) OnSizeChange(this);

	PaintBox->Invalidate();
}


bool TheMatrix::IsThisFrameLocked(int layer, int frame)
{
	return (IsLayerLocked(layer) || MatrixLayers[layer]->Cells[frame]->Locked);
}


bool TheMatrix::IsLocked()
{
	return MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Locked;
}


void TheMatrix::UnLockCurrentFrame()
{
	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Locked = false;
}


void TheMatrix::LockCurrentFrame()
{
	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Locked = true;
}


void TheMatrix::LockUnLockRange(int start, int end, bool status)
{
	for (int f = start; f <= end; f++)
	{
		MatrixLayers[CurrentLayer]->Cells[f]->Locked = status;
	}
}


void TheMatrix::CopyFromPrevious(int frame_to)
{
	if (frame_to > 0)
	{
		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				for (int x = 0; x < Details.Width; x++)
				{
					MatrixLayers[layer]->Cells[frame_to]->Grid[y * Details.Width + x] = MatrixLayers[layer]->Cells[frame_to - 1]->Grid[y * Details.Width + x];
				}
			}
		}

		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::CopyAllLayersFromTo(int frame_from, int frame_to)
{
	if (!AutomateMode)
	{
		if (frame_from == CurrentFrame)
		{
			CopyDrawBufferToCurrentFrame();
		}

		Busy = True;
	}

	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixLayers[layer]->Cells[frame_to]->Grid[y * Details.Width + x] = MatrixLayers[layer]->Cells[frame_from]->Grid[y * Details.Width + x];
			}
		}
	}

	if (!AutomateMode)
	{
		Busy = false;

		if (frame_from == CurrentFrame)
		{
			CopyCurrentFrameToDrawBuffer();

			if (OnChange) OnChange(this);

			PaintBox->Invalidate();
		}
	}
}
#pragma end_region


#pragma region Layers
bool TheMatrix::AddLayer(const std::wstring name)
{
	if (Software == SoftwareMode::kAnimation && Details.Width > 0 && Details.Height > 0)
	{
		Busy = true;

		Layer *layer = new Layer(name);

		MatrixLayers.push_back(layer);

		for (int t = 0; t < MatrixLayers[CPermanentLayer]->Cells.size(); t++)
		{
			Matrix *m = new Matrix(Details.Width, Details.Height, Details.Mode, RGBBackground);

			MatrixLayers.back()->Cells.push_back(m);
		}

		Busy = false;

		SetCurrentLayer(MatrixLayers.size() - 1);

		if (OnLayerChange) OnLayerChange(this);

		return true;
	}

	return false;
}


bool TheMatrix::AddLayerAsCopy(const std::wstring name, int copylayer)
{
	if (Software == SoftwareMode::kAnimation && Details.Width > 0 && Details.Height > 0)
	{
		Busy = true;

		Layer *layer = new Layer(name);

		MatrixLayers.push_back(layer);

		for (int t = 0; t < MatrixLayers[CPermanentLayer]->Cells.size(); t++)
		{
			Matrix *m = new Matrix(Details.Width, Details.Height, Details.Mode, RGBBackground);

			for (int x = 0; x < Details.Width; x++)
			{
				for (int y = 0; y < Details.Height; y++)
				{
					m->Grid[y * Details.Width + x] = MatrixLayers[copylayer]->Cells[t]->Grid[y * Details.Width + x];
				}
			}

			MatrixLayers.back()->Cells.push_back(m);
		}

		Busy = false;

		SetCurrentLayer(MatrixLayers.size() - 1);

		if (OnLayerChange) OnLayerChange(this);

        return true;
	}

	return false;
}


bool TheMatrix::DeleteLayer(int index)
{
	if (MatrixLayers.size() > 1)
	{
		CopyDrawBufferToCurrentFrame();

		MatrixLayers.erase(MatrixLayers.begin() + index);

		CurrentLayer = 0;

		if (OnLayerChange) OnLayerChange(this);

		return true;
	}

	return false;
}


void TheMatrix::ClearCurrentLayerAllFrames()
{
	DisplayBuffer->Clear(Details.Mode, RGBBackground);

	for (int frame = 0; frame < MatrixLayers[CurrentLayer]->Cells.size(); frame++)
	{
		if (!IsThisFrameLocked(CurrentLayer, frame))
		{
			MatrixLayers[CurrentLayer]->Cells[frame]->Clear(Details.Mode, RGBBackground);

			MatrixLayers[CurrentLayer]->Cells[frame]->AddToHistory();
		}
	}

	PaintBox->Invalidate();

	if (OnChange) OnChange(this);
}


void TheMatrix::FlattenAllLayers()
{
	CopyDrawBufferToCurrentFrame();

	Busy = true;

	MatrixMerge = new Matrix(Details.Width, Details.Height, Details.Mode, RGBBackground);

	for (int l = 0; l < MatrixLayers.size(); l++)
	{
		MatrixLayers[l]->Visible = true;
	}

	for (int f = 0; f < MatrixLayers[CPermanentLayer]->Cells.size(); f++)
	{
		BuildMergedFrame(f, 0);

		for (int x = 0; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[CPermanentLayer]->Cells[f]->Grid[y * Details.Width + x] = MatrixMerge->Grid[y * Details.Width + x];
			}
		}
	}

	while (MatrixLayers.size() > 1)
	{
		MatrixLayers.pop_back();
	}

	CurrentFrame = 0;
	CurrentLayer = 0;

    delete MatrixMerge;

	Busy = false;

	CopyCurrentFrameToDrawBuffer();

	if (OnLayerChange) OnLayerChange(this);
}


int TheMatrix::GetLayerCount()
{
	return MatrixLayers.size();
}


std::wstring TheMatrix::GetLayerName(int layerindex)
{
	return MatrixLayers[layerindex]->Name;
}


void TheMatrix::SetLayerName(const std::wstring name, int layerindex)
{
	MatrixLayers[layerindex]->Name = name;
}


bool TheMatrix::IsVisible(int index)
{
	return MatrixLayers[index]->Visible;
}


void TheMatrix::SetVisibility(int LayerIndex, bool Visibility)
{
	MatrixLayers[LayerIndex]->Visible = Visibility;
}


void TheMatrix::MoveUp(int LayerIndex)
{
	if (LayerIndex == CurrentFrame)
	{
		CopyCurrentFrameToDrawBuffer();
	}

	Busy = true;

	std::swap(MatrixLayers[LayerIndex], MatrixLayers[LayerIndex + 1]);

	CurrentLayer = LayerIndex + 1;

	Busy = false;

	PaintBox->Invalidate();

	if (OnLayerChange) OnLayerChange(this);
}


void TheMatrix::MoveDown(int LayerIndex)
{
	if (LayerIndex == CurrentFrame)
	{
		CopyCurrentFrameToDrawBuffer();
	}

	Busy = true;

	std::swap(MatrixLayers[LayerIndex], MatrixLayers[LayerIndex - 1]);

	CurrentLayer = LayerIndex - 1;

	Busy = false;

	PaintBox->Invalidate();

	if (OnLayerChange) OnLayerChange(this);
}


void TheMatrix::UnlockLayer(int layer)
{
	MatrixLayers[layer]->Locked = false;
}


void TheMatrix::LockLayer(int layer)
{
	MatrixLayers[layer]->Locked = true;
}


bool TheMatrix::IsLayerLocked(int layer)
{
	return MatrixLayers[layer]->Locked;
}


// ensures that all layers have the same number of frames, crashes will occur
// if this is not the case!
void TheMatrix::EnsureLayerCoherence()
{
	int max = 0;

	for (int t = 0; t < MatrixLayers.size(); t++)
	{
		if (MatrixLayers[t]->Cells.size() > max)
		{
			max = MatrixLayers[t]->Cells.size();
		}
	}

	for (int t = 0; t < MatrixLayers.size(); t++)
	{
		if (MatrixLayers[t]->Cells.size() != max)
		{
			for (int frame = MatrixLayers[t]->Cells.size() + 1; frame <= max; frame++)
			{
				Matrix *m = new Matrix(Details.Width, Details.Height, Details.Mode, RGBBackground);

				MatrixLayers[t]->Cells.push_back(m);
			}
		}
	}
}


bool TheMatrix::AreLayersIdentical(int layer1, int layer2, int frame)
{
	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			if (MatrixLayers[layer1]->Cells[frame]->Grid[y * Details.Width + x] != MatrixLayers[layer2]->Cells[frame]->Grid[y * Details.Width + x])
			{
				return false;
			}
		}
	}

	return true;
}


void TheMatrix::CopyLayerFromTo(int source, int destination, int frame_from, int frame_to)
{
	if (!AutomateMode)
	{
		if (frame_from == CurrentFrame)
		{
			CopyDrawBufferToCurrentFrame();
		}

		Busy = true;
	}

	for (int y = 0; y < Details.Height; y++)
	{
		for (int x = 0; x < Details.Width; x++)
		{
			MatrixLayers[destination]->Cells[frame_to]->Grid[y * Details.Width + x] = MatrixLayers[source]->Cells[frame_from]->Grid[y * Details.Width + x];
		}
	}

	if (!AutomateMode)
	{
		Busy = false;

		if (frame_from == CurrentFrame)
		{
			CopyCurrentFrameToDrawBuffer();
		}

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}
#pragma end_region


#pragma region ColourStuff
void TheMatrix::CopyLEDColours()
{
	for (int t = 0; t < 6; t++)
	{
		switch (Details.Mode)
		{
		case MatrixMode::kMono:
			LEDColours[t] = LEDColoursSingle[t];
			break;
		case MatrixMode::kBiSequential:
		case MatrixMode::kBiBitplanes:
			LEDColours[t] = LEDColoursBi[t];
			break;
		}
	}

	PaintBox->Invalidate();
}


void TheMatrix::ChangeSelectionColour(int LMB, int MMB, int RMB)
{
	SetMouseButtonColours(LMB, MMB, RMB);

	LEDRGBColours[CMouseLeft]   = LMB;
	LEDRGBColours[CMouseMiddle] = MMB;
	LEDRGBColours[CMouseRight]  = RMB;

	if (OnColourChange) OnColourChange(this);
}


void TheMatrix::GradientFillFrame()
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame)) return;

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			if (Render.Gradient.Option == GradientOption::kVertical)
			{
				if (Details.Mode == MatrixMode::kRGB || Details.Mode == MatrixMode::kRGB3BPP)
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = Render.Gradient.IY[y];
				}
				else
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = LEDColours[Render.Gradient.IY[y]];
				}
			}
			else
			{
				if (Details.Mode == MatrixMode::kRGB || Details.Mode == MatrixMode::kRGB3BPP)
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = Render.Gradient.IX[x];
				}
				else
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = LEDColours[Render.Gradient.IX[x]];
				}
			}
		}
	}

	if (OnChange) OnChange(this);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}


void TheMatrix::ChangePixels(int colour_from, int colour_to)
{
	for (int frame = 0; frame < GetFrameCount(); frame++)
	{
		MatrixLayers[CurrentLayer]->Cells[frame]->ChangePixels(colour_from, colour_to);
	}

	if (OnChange) OnChange(this);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}


void TheMatrix::FadeFirstToLast()
{
	for (int y = 0; y < Details.Height; y++)
	{
		for (int x = 0; x < Details.Width; x++)
		{
			int colstart   = MatrixLayers[CurrentLayer]->Cells[0]->Grid[y * Details.Width + x];
			int colend     = MatrixLayers[CurrentLayer]->Cells[GetFrameCount() - 1]->Grid[y * Details.Width + x];

			int gradheight = GetFrameCount();

			int rdy  = (colend & 0x0000FF) - (colstart & 0x0000FF);
			int gdy  = ((colend & 0x00FF00) >> 8) - ((colstart & 0x00FF00) >> 8);
			int bdy  = ((colend & 0xFF0000) >> 16) - ((colstart & 0xFF0000) >> 16);

			double newr = (colstart & 0x0000FF);
			double newg = (colstart & 0x00FF00) >> 8;
			double newb = (colstart & 0xFF0000) >> 16;

			double rdx  = rdy / gradheight;
			double gdx  = gdy / gradheight;
			double bdx  = bdy / gradheight;

			for (int frame = 1; frame < GetFrameCount(); frame++)
			{
				newr  = newr + rdx;
				newg  = newg + gdx;
				newb  = newb + bdx;

				int newri = std::floor(newr);
				int newgi = std::floor(newg);
				int newbi = std::floor(newb);

				MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + x] = (newbi << 16) + (newgi << 8) + newri;
			}
		}
	}

	PaintBox->Invalidate();
}


// change colours in the current layer of the currently frame only
void TheMatrix::ChangeColourCurrent(int colour_from, int colour_to)
{
	CopyDrawBufferToCurrentFrame();

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->ChangePixels(colour_from, colour_to);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}


// change colours in all frames of the current layer
void TheMatrix::ChangeColourCurrentLayer(int colour_from, int colour_to)
{
	CopyDrawBufferToCurrentFrame();

	for (int t = 0; t < MatrixLayers[CurrentLayer]->Cells.size(); t++)
	{
		MatrixLayers[CurrentLayer]->Cells[t]->ChangePixels(colour_from, colour_to);
	}

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}


// change the colours in all layers and all frames
void TheMatrix::ChangeColourAll(int colour_from, int colour_to)
{
	CopyDrawBufferToCurrentFrame();

	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		for (int frame = 0; frame < MatrixLayers[layer]->Cells.size(); frame++)
		{
			MatrixLayers[layer]->Cells[frame]->ChangePixels(colour_from, colour_to);
		}
	}

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region MatrixIO
void TheMatrix::ImportRowData(bool hex, int sourcedirection, int sourcelsb, const std::wstring s)
//var
//  t, x, rowindex : integer;
//  zig, rowvalue : int64;
//  temp : string;
//
{ /*
  if aSourceDirection = 0 then
    rowindex = 0
  else
    rowindex = Details.Height - 1;

  temp     = '';

  s = UpperCase(s);
  s = StringReplace(s, '0X', '$', [rfReplaceAll]);                       					m = Utility::ReplaceString(m, unique_items[t], std::to_wstring(t));
  s = StringReplace(s, ',',  ' ', [rfReplaceAll]);
  s = StringReplace(s, '[',  '',  [rfReplaceAll]);
  s = StringReplace(s, ']',  '',  [rfReplaceAll]);
  s = StringReplace(s, '{',  '',  [rfReplaceAll]);
  s = StringReplace(s, '}',  '',  [rfReplaceAll]);
  s = StringReplace(s, '(',  '',  [rfReplaceAll]);
  s = StringReplace(s, ')',  '',  [rfReplaceAll]);
  s = s + ' ';

  for t = 1 to length(s) do {
    if s[t] = ' ' then {
      if temp <> '' then {

        if aHex then
          rowvalue = StrToInt64('$' + temp)
        else
          rowvalue = StrToInt64(temp);

        if (rowindex >= 0) and (rowindex <= Details.Height - 1) then {
          for x = 0 to Details.Width - 1 do {
            case aSourceLSB of
              lsbLeft  : {
                           zig = (rowvalue and Powers[x]);

                           if zig = Powers[x] then {
                             MatrixLayers[CurrentLayer]->Frames[CurrentFrame]->Grid[x, rowindex] = 1;
                           end
                           else {
                             MatrixLayers[CurrentLayer]->Frames[CurrentFrame]->Grid[x, rowindex] = 0;
                           }
                         }
              lsbRight : {
                           zig=(rowvalue and Powers[x]);

                           if zig = Powers[x] then {
                             MatrixLayers[CurrentLayer]->Frames[CurrentFrame]->Grid[Details.Width - x - 1, rowindex] = 1;
                           end
                           else {
                             MatrixLayers[CurrentLayer]->Frames[CurrentFrame]->Grid[Details.Width - x - 1, rowindex] = 0;
                           }
                         }
            }
          }
        }

		temp = '';

		if aSourceDirection = 0 then
		  inc(rowindex)
		else
		  dec(rowindex);
	  }
	end
	else if (ord(s[t]) >= 48) and (ord(s[t]) <= 57) then {
	  temp = temp + s[t];
	end
	else if (ord(s[t]) >= 65) and (ord(s[t]) <= 90) then {
	  temp = temp + s[t];
	end
	else if (s[t] = '$') then {
	  aHex = True;
	}

	if (OnChange) OnChange();
  }

  PaintBox->Invalidate(); */
}


void TheMatrix::ImportColumnData(bool hex, int sourcedirection, int sourcelsb, const std::wstring s)
//var
//  t, y, colindex  : integer;
//  zig, colvalue : int64;
//  temp : string;
{ /*
  if aSourceDirection = 0 then
	colindex = 0
  else
	colindex = Details.Width - 1;

  temp = '';

  s = UpperCase(s);
  s = StringReplace(s, '0X', '$', [rfReplaceAll]);
  s = StringReplace(s, ',',  ' ', [rfReplaceAll]);
  s = StringReplace(s, '[',  '', [rfReplaceAll]);
  s = StringReplace(s, ']',  '', [rfReplaceAll]);
  s = StringReplace(s, '{',  '', [rfReplaceAll]);
  s = StringReplace(s, '}',  '', [rfReplaceAll]);
  s = StringReplace(s, '(',  '', [rfReplaceAll]);
  s = StringReplace(s, ')',  '', [rfReplaceAll]);
  s = s + ' ';

  for t = 1 to length(s) do {
	if s[t] = ' ' then {
	  if temp <> '' then {

		if aHex then
		  colvalue = StrToInt64('$' + temp)
		else
		  colvalue = StrToInt64(temp);

		if (colindex >= 0) and (colindex <= Details.Width - 1) then {
		  for y = 0 to Details.Height - 1 do {
			case aSourceLSB of
			  lsbLeft  : {
						   zig = (colvalue and Powers[y]);

						  if zig = Powers[y] then {
							 MatrixLayers[CurrentLayer]->Frames[CurrentFrame]->Grid[colindex, y] = 1;
						   end
						   else {
							 MatrixLayers[CurrentLayer]->Frames[CurrentFrame]->Grid[colindex, y] = 0;
						   }
						 }
			  lsbRight : {
						   zig = (colvalue and Powers[y]);

						   if zig = Powers[y] then {
							 MatrixLayers[CurrentLayer]->Frames[CurrentFrame]->Grid[colindex, Details.Height - y] = 1;
						   end
						   else {
							 MatrixLayers[CurrentLayer]->Frames[CurrentFrame]->Grid[colindex, Details.Height - y] = 0;
						   }
						 }
			}
		  }
		}

		temp = '';

		if aSourceDirection = 0 then
		  inc(colindex)
		else
		  dec(colindex);
	  }
	end
	else if (ord(s[t]) >= 48) and (ord(s[t]) <= 57) then {
	  temp = temp + s[t];
	end
	else if (ord(s[t]) >= 65) and (ord(s[t]) <= 90) then {
	  temp = temp + s[t];
	end
	else if (s[t] = '$') then {
	  aHex = True;
	}

	if (OnChange) OnChange();
  }

  PaintBox->Invalidate(); */
}








std::wstring TheMatrix::RowToString(int frame, int row)
{
	std::wstring s = L"";

	for (int x = 0; x < Details.Width; x++)
	{
		s += IntToHex(MatrixLayers[CurrentLayer]->Cells[frame]->Grid[row * Details.Width + x], 6).c_str();

		s += L" ";
	}

	return s;
}


void TheMatrix::StringToRow(bool copybrush, const std::wstring s, int frame, int row, int transparentcolour, bool transparent)
{
	int x = 0;
	int colour = 0;
	std::wstring input = L"";

	for (int i = 0; i < s.length(); i++)
	{
		if (s[i] == L' ' || i == s.length() - 1)
		{
			colour = Convert::HexToInt(input);

			if (copybrush)
			{
				MatrixCopy->Grid[row * Details.Width + x] = colour;
			}
			else
			{
				if (transparent)
				{
					if (colour != transparentcolour)
					{
						MatrixLayers[CurrentLayer]->Cells[frame]->Grid[row * Details.Width + x] = colour;
					}
				}
				else
				{
					MatrixLayers[CurrentLayer]->Cells[frame]->Grid[row * Details.Width + x] = colour;
				}
			}

			x++;

			input = L"";
		}
		else
		{
			input += s[i];
		}
	}
}


int TheMatrix::RightBounds()
{
	int bound = 0;

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			if (Details.Mode == MatrixMode::kRGB || Details.Mode == MatrixMode::kRGB3BPP)
			{
				if (MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] != RGBBackground)
				{
					if (x > bound) bound = x;
				}
			}
			else
			{
				if (MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] == 1)
				{
					if (x > bound) bound = x;
				}
			}
		}
	}

	return bound;
}


int TheMatrix::BottomBounds()
{
	int bound = 0;

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			if (Details.Mode == MatrixMode::kRGB || Details.Mode == MatrixMode::kRGB3BPP)
			{
				if (MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] != RGBBackground)
				{
					if (y > bound) bound = y;
				}
			}
			else
			{
				if (MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] == 1)
				{
					if (y > bound) bound = y;
				}
			}
		}
	}

	return bound;
}
#pragma end_region


#pragma region FileIO_Bitmap
ImportData TheMatrix::ImportFromBMPSingleImage(const std::wstring file_name, int count, int width, int height, bool rgbimport, bool createnew)
{
	ImportData import;

	TBitmap *bmp = new TBitmap();
	bmp->LoadFromFile(file_name.c_str());

	if (!bmp->Empty)
	{
		int FrameStart = 0;
		int FrameEnd = 0;

		if (createnew)
		{
			FrameStart = 0;
			FrameEnd   = FrameStart + (count - 1);
		}
		else
		{
			FrameStart = CurrentFrame;
			FrameEnd   = CurrentFrame + count - 1;
		}

		// ===========================================================================

		for (int frame = FrameStart; frame <= FrameEnd; frame++)
		{
			int wo = (frame - FrameStart) * width;

			if (MatrixLayers[CurrentLayer]->Cells.size() < frame + 1)
			{
				Matrix *matrix = new Matrix(width,height, Details.Mode, RGBBackground);
				MatrixLayers[CurrentLayer]->Cells.push_back(matrix);
			}

			for (int x = 0; x < width; x++)
			{
				for (int y = 0; y < height; y++)
				{
					if (rgbimport)
					{
						MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * width + x] = bmp->Canvas->Pixels[wo + x][y];
					}
					else
					{
						if (bmp->Canvas->Pixels[wo + x][y] == clBlack)
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * width + x] = 0;
						}
						else
						{
							MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * width + x] = 1;
						}
					}
				}
			}

			if (frame == CurrentFrame)
			{
				CopyCurrentFrameToDrawBuffer();
			}
		}

		// ===========================================================================

        import.ImportOk = true;
		import.NewWidth    = width;
		import.NewHeight   = height;
		//import.NewFrames   = FrameCount;
		import.RGBImport   = rgbimport;

		PaintBox->Invalidate();

		Details.Available = true;

		if (OnChange) OnChange(this);

		if (OnLayerChange) OnLayerChange(this);
	}

	delete bmp;

	return import;
}


ImportData TheMatrix::ImportFromBMPMultipleImage(std::wstring pattern, int startframe, int count, int padlength, int fwidth, int fheight, bool RGBimport, bool createnew)
{
	ImportData import;

	std::wstring file_name = L"";

	for (int i = 0; i < count; i++)
	{
		int frame = 0;

		if (padlength == 0)
		{
			file_name = Utility::ReplaceString(pattern, L"$$", std::to_wstring(startframe + i));
		}
		else
		{
			file_name = Utility::ReplaceString(pattern, L"$$", Formatting::PadZeroes(std::to_wstring(startframe + i), padlength));
		}

		TBitmap *bmp = new TBitmap();
		bmp->LoadFromFile(file_name.c_str());

		if (createnew)
		{
			frame = startframe + i;
		}
		else
		{
			frame = CurrentFrame + i;
		}

		// ===================================================================

		if (frame > MatrixLayers[CurrentLayer]->Cells.size() - 1)
		{
			Matrix *matrix = new Matrix(fwidth, fheight, Details.Mode, RGBBackground); // to do

			MatrixLayers[CurrentLayer]->Cells.push_back(matrix);
		}

		for (int x = 0; x < fwidth; x++)
		{
			for (int y = 0; y < fheight; y++)
			{
				if (RGBimport)
				{
					MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * fwidth + x] = bmp->Canvas->Pixels[x][y];
				}
				else
				{
					if (bmp->Canvas->Pixels[x][y] == clBlack)
					{
						MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * fwidth + x] = 0;
					}
					else
					{
						MatrixLayers[CurrentLayer]->Cells[frame]->Grid[y * fwidth + x] = 1;
					}
				}
			}
		}

		delete bmp;
	}

	// =======================================================================

	import.NewWidth    = fwidth;
	import.NewHeight   = fheight;
	import.NewFrames   = count;
	import.RGBImport   = RGBimport;

	PaintBox->Invalidate();

	Details.Available = true;

	if (OnLayerChange) OnLayerChange(this);

	if (OnChange) OnChange(this);

	return import;
}


bool TheMatrix::ExportToBitmap(const std::wstring file_name)
{
	TBitmap *bitmap = new TBitmap();
	bitmap->PixelFormat = pf24bit;
	bitmap->Width = GetFrameCount() * Details.Width;
	bitmap->Height = Details.Height;

	try
	{
		TRGBTriple *ptr;

		for (int frame = 0; frame < GetFrameCount(); frame++)
		{
			BuildMergedFrame(frame, 2);

			for (int y = 0; y < Details.Height; y++)
			{
				ptr = reinterpret_cast<TRGBTriple *>(bitmap->ScanLine[y]);

				for (int x = 0; x < Details.Width; x++)
				{
					int colour = MatrixMerge->Grid[y * Details.Width + x];

					ptr[(frame * Details.Width) + x].rgbtRed = (colour & 0x0000ff);
					ptr[(frame * Details.Width) + x].rgbtGreen = (colour & 0x00ff00) >> 8;
					ptr[(frame * Details.Width) + x].rgbtBlue = (colour & 0xff0000) >> 16;
				}
			}
		}

		bitmap->SaveToFile(file_name.c_str());
	}
	catch(...)
	{
	}

	if (bitmap != nullptr)
	{
		delete bitmap;
	}

	return true;
}


bool TheMatrix::ExportAnimationToBitmap(const std::wstring file_name)
{
	bool success = true;

	std::wstring prefix = Utility::GetFileNameNoExt(file_name);

	TBitmap *bitmap = new TBitmap();
	bitmap->PixelFormat = pf24bit;
	bitmap->Width = Details.Width;
	bitmap->Height = Details.Height;

	try
	{
		TRGBTriple *ptr;

		for (int frame = 0; frame < GetFrameCount(); frame++)
		{
			BuildMergedFrame(frame, 2);

			for (int y = 0; y < Details.Height; y++)
			{
				ptr = reinterpret_cast<TRGBTriple *>(bitmap->ScanLine[y]);

				for (int x = 0; x < Details.Width; x++)
				{
					int colour = MatrixMerge->Grid[y * Details.Width + x];

					ptr[x].rgbtRed = (colour & 0x0000ff);
					ptr[x].rgbtGreen = (colour & 0x00ff00) >> 8;
					ptr[x].rgbtBlue = (colour & 0xff0000) >> 16;
				}
			}

			std::wstring output = prefix + L"_" + Formatting::PadZeroes(std::to_wstring(frame + 1), 6) + L".bmp";

			bitmap->SaveToFile(output.c_str());
		}
	}
	catch(...)
	{
        success = false;
	}

	if (bitmap != nullptr)
	{
		delete bitmap;
	}

	return success;
}
#pragma end_region


#pragma region FileIO_GIF
// based on code from this stack overflow question
// https://stackoverflow.com/questions/36444024/how-to-extract-frames-from-this-gif-image-access-violation-in-tgifrenderer-dra
ImportData TheMatrix::ImportFromGIF(const std::wstring file_name)
{
	ClearAllMatrixData(false);

	// ===========================================================================

	ImportData import;

	import.ImportOk        = True;
	import.Source          = -1;
	import.SourceLSB       = -1;
	//  Result.SourceDirection = -1;
	import.Mode      = MatrixMode::kMono;
	import.RGBImport       = False;
	import.Preview.Enabled = Preview.Active;

  // ===========================================================================

	TGIFImage *lGIF = new TGIFImage();

	try
	{
		try
		{
			lGIF->Animate = false;
			lGIF->LoadFromFile(file_name.c_str());
		}
		catch(...)
		{
			import.ImportOk    = False;
			import.ErrorString = GLanguageHandler->Text[kErrorWhileLoadingThisGIF];

			return import;
		}

		TBitmap *lTempFrame = new TBitmap();
		lTempFrame->PixelFormat = pf24bit;

		TGIFRenderer *lGR = new TGIFRenderer(lGIF);

		int gifheight    = lGIF->Height;
		int gifWidth     = lGIF->Width;

		if (gifheight > 256 || gifheight > 256)
		{
			import.ImportOk    = false;
			import.ErrorString = GLanguageHandler->Text[kGIFDimensionsAreTooLarge] + L" " + std::to_wstring(gifWidth) + L" x " + std::to_wstring(gifheight) + L").";

			delete lGIF;
			delete lGR;
			delete lTempFrame;

			return import;
		}

		Details.Width  = gifWidth;
		Details.Height = gifheight;

		try
		{
			lTempFrame->SetSize(gifWidth, gifheight);

			TRGBTriple *ptr;

			for (int t = 0; t < lGIF->Images->Count; t++)
			{
				if (lGIF->Images->Frames[t]->Empty) continue;	// ignore bad frames

				for (int layer = 0; layer < MatrixLayers.size(); layer++)
				{
					Matrix *m = new Matrix(lGIF->Width, lGIF->Height, MatrixMode::kRGB, RGBBackground);

					MatrixLayers[layer]->Cells.push_back(m);
                }

				lGR->Draw(lTempFrame->Canvas, lTempFrame->Canvas->ClipRect);

				for (int y = 0; y < lGIF->Height; y++)
				{
					ptr = reinterpret_cast<TRGBTriple *>(lTempFrame->ScanLine[y]);

					for (int x = 0; x < lGIF->Width; x++)
					{
						MatrixLayers[CurrentLayer]->Cells.back()->Grid[y * Details.Width + x] = (ptr[x].rgbtBlue << 16) + (ptr[x].rgbtGreen << 8) + (ptr[x].rgbtRed);
					}
				}

				lGR->NextFrame();
			}

			delete lGR;
			delete lTempFrame;
		}
		catch(...)
		{

		}
	}
	catch(...)
	{

	}

	delete lGIF;

	CurrentFrame = 0;

	CopyCurrentFrameToDrawBuffer();

	import.Mode       = MatrixMode::kRGB;
	import.NewWidth         = Details.Width;
	import.NewHeight        = Details.Height;
	import.BackgroundColour = RGBBackground;

	import.MaxFrames        = MatrixLayers[CPermanentLayer]->Cells.size();
	import.FontMode         = False;

	Details.Available = true;

	if (OnLayerChange) OnLayerChange(this);

	PaintBox->Invalidate();

	return import;
}


// if you decide to tweak the export yourself then don't bother with the Embarcadero docs, they are worse
// than useless. open Vcl.Imaging.GIFImg and examine the code to see how things are done!
void TheMatrix::ExportToGIF(const std::wstring file_name, int background, int pixelsize, int pixelshape, int animationspeed)
//var
//  lGIF, lTGI : TGIFImage;
//  lTempFrame : TBitmap;
//  lFrame, lColumn, lRow : integer;
{
	TGIFImage *lGIF = new TGIFImage();

	lGIF->Animate     = true;
	lGIF->AnimateLoop = glContinously;

	//FMatrixMerge = TMatrix.Create(Details.Width, Details.Height, Details.Mode, RGBBackground);

	try
	{
		for (int frame = 0; frame < MatrixLayers[CPermanentLayer]->Cells.size(); frame++)
		{
			TBitmap *lTempFrame = new TBitmap();

			lTempFrame->Width  = Details.Width * pixelsize;
			lTempFrame->Height = Details.Height * pixelsize;

			lTempFrame->Canvas->Brush->Color = TColor(background);
			lTempFrame->Canvas->FillRect(Rect(0, 0, lTempFrame->Width, lTempFrame->Height));

			BuildMergedFrame(frame, 2);

			if (pixelsize == 1)
			{
				for (int column = 0; column < Details.Width; column++)
				{
					for (int row = 0; row < Details.Height; row++)
					{
						lTempFrame->Canvas->Pixels[column][row] = TColor(MatrixMerge->Grid[row * Details.Width + column]);
					}
				}
			}
			else
			{
				for (int column = 0; column < Details.Width; column++)
				{
					for (int row = 0; row < Details.Height; row++)
					{
						lTempFrame->Canvas->Brush->Color = TColor(MatrixMerge->Grid[row * Details.Width + column]);

						switch (pixelshape)
						{
						case 0:
							lTempFrame->Canvas->FillRect(Rect(column * pixelsize,
																  row * pixelsize,
																 (column * pixelsize) + pixelsize,
																 (row * pixelsize) + pixelsize));
							break;
						case 1:
							lTempFrame->Canvas->Ellipse(column * pixelsize,
															row * pixelsize,
														   (column * pixelsize) + pixelsize,
														   (row * pixelsize) + pixelsize);
							break;
						case 2:
							lTempFrame->Canvas->RoundRect(column * pixelsize,
															  row * pixelsize,
															 (column * pixelsize) + pixelsize,
															 (row * pixelsize) + pixelsize,
															  pixelsize - (std::round(pixelsize / CRoundRectCoeff)),
															  pixelsize - (std::round(pixelsize / CRoundRectCoeff)));
							break;
						}
					}
				}
			}

			TGIFImage *lTGI = new TGIFImage();
			lTGI->Assign(lTempFrame);

			if (animationspeed != 0)
			{
				TGIFGraphicControlExtension *gce = new TGIFGraphicControlExtension(lTGI->Images->Frames[0]);

				gce->Delay = animationspeed;

				lTGI->Images->Frames[0]->Extensions->Add(gce);
			}

			lGIF->Add(lTGI);

			//      lTGI.Free;         to do
			//      lTempFrame.Free;
		}

		TGIFAppExtNSLoop *aeloop = new TGIFAppExtNSLoop(lGIF->Images->Frames[0]);

		aeloop->Loops = 0;

		lGIF->Images->Frames[0]->Extensions->Add(aeloop);

		lGIF->SaveToFile(file_name.c_str());
	}
	catch(...)
	{

	}

	delete lGIF;
}
#pragma end_region


#pragma region FileIO_LMSFormats
ImportData TheMatrix::LoadLEDMatrixData(const std::wstring file_name, ExportOptions &eeo, LoadMode loadmode, int startframe)
{
	auto SafeStringToBool = [](const std::wstring s) -> bool
	{
		if (s.empty() || s == L"0")
		{
			return false;
		}

        return true;
	};


	int importLayer = CurrentLayer;
	int importFrame = CurrentFrame;
	int initialframe = 0;

	switch (loadmode)
	{
	case LoadMode::kNew:
		ClearAllMatrixData(false);

		importFrame = 0;
		importLayer = 0;
		break;
	case LoadMode::kMergeBottomPriority:
	case LoadMode::kMergeTopPriority:
		importFrame = startframe;
		break;
	case LoadMode::kAppend:
		importFrame = MatrixLayers[CPermanentLayer]->Cells.size();
		break;
	case LoadMode::kMergeNewLayer:
	{
		importFrame = startframe;

		std::wstring name = ExtractFileName(file_name.c_str()).c_str();

		AddLayer(L"Merge from " + name);

		importLayer = MatrixLayers.size() - 1;
		break;
	}
	case LoadMode::kMergeCurrentLayer:
		importLayer = CurrentLayer;
		importFrame = 1;
		break;
	}

	initialframe = importFrame;

	// =======================================================================
	// =======================================================================

	ImportData import;
	import.ImportOk        = true;
	import.Mode      = MatrixMode::kMono;
	import.RGBImport       = false;
	import.RGBBrightness   = 100;

	import.Colours.HasData = false;

	// clear rest of preview data?
	import.Preview.Enabled           = Preview.Active;
	import.Preview.IncrementRadially = false;

	// =======================================================================

	std::wifstream file(file_name);

	if (file)
	{
		bool headermode           = false;
		bool fontmode           = false;
		bool deadpixelmode          = false;
		bool matrixdatamode     = false;
		bool layermode          = false;
		bool coloursmode        = false;
		int layercount = 0;

		int row                   = 0;
		MatrixMode mode        = MatrixMode::kMono;
		int background         = -1; // was -1 !!!!
		int colour                = 0;
		int palette               = 0;
		int importRGBbackground = 0;

		int tempMaxWidth           = -1;
		int tempMaxHeight          = -1;
		int newwidth = 0;
		int newheight = 0;

		if (loadmode == LoadMode::kNew)
		{
		  SetDeadPixels(PixelAlive);
		}

		if (loadmode == LoadMode::kAppend)
		{
			for (int i = 0; i < MatrixLayers.size(); i++)
			{
				Matrix *m = new Matrix(Details.Width, Details.Height, Details.Mode, RGBBackground);

				MatrixLayers[i]->Cells.push_back(m);
			}
		}

		// ===========================================================================
		// ===========================================================================

		std::wstring s(L"");

		while (std::getline(file, s))
		{
			if (s != L"")
			{
				if (s[0] == L'/' || s[0] == L'#')
				{
					// comment, do nothing
				}
				else
				{
					std::wstring v = L"";

					if (s.length() >= 3) v = s.substr(2);

					std::transform(s.begin(), s.end(), s.begin(), ::tolower);

					switch (LoadDataParameterType(s, headermode, matrixdatamode, deadpixelmode, layermode, coloursmode))
					{
					case LoadData::kLoadBlockStartHeader:
						if (s == L"{" + kFileHeaderFontHeader)
						{
							fontmode = true;
						}
						else
						{
							fontmode = false;
						}

						headermode = true;
						break;
					case LoadData::kLoadBlockStartDeadPixel:// dead pixel mode
						deadpixelmode  = true;
						matrixdatamode = false;

						row = 0;
                        break;
					case LoadData::kLoadBlockBegin:
						row = 0;

						switch (v[v.length() - 1])
						{
						case L'2':
							mode = MatrixMode::kBiSequential;
							break;
						case L'3':
							mode = MatrixMode::kBiBitplanes;
							break;
						case L'4':
							mode = MatrixMode::kRGB;
							break;
						case L'5':
							mode = MatrixMode::kRGB3BPP;
							break;

						default:
							mode = MatrixMode::kMono;
						}

						if (loadmode == LoadMode::kNew)
						{
							Details.Mode = mode;
						}

						headermode     = false;
						matrixdatamode = true;
						break;
					case LoadData::kLoadBlockEnd:
						 if (matrixdatamode)
						 {
							importFrame++;
						 }
						 break;
					case LoadData::kLoadBlockBeginLayout:
						coloursmode    = False;
						layermode      = True;
						matrixdatamode = False;
						headermode     = False;

						importLayer++;

						importFrame = initialframe;
						break;
					case LoadData::kLoadBlockEndLayout:
						layermode = False;

						switch (loadmode)
						{
						case LoadMode::kNew:
							Details.Height = tempMaxHeight;
							Details.Width  = tempMaxWidth;
							break;
						}

						if (layercount > 0)
						{
							for (int i = 1; i < layercount; i++)
							{
								AddLayer(L"");
							}
						}

						layercount = -1;
						break;
					case LoadData::kLoadBlockStartColours:
						headermode = false;
						coloursmode = true;

						import.Colours.HasData = true;
						break;

					 // ====================================================================

					case LoadData::kLoadHeaderSource:
						break;
					case LoadData::kLoadHeaderSourceLSB:
						break;
					case LoadData::kLoadHeaderSourceDirection:
						break;
					case LoadData::kLoadHeaderPadMode:
						import.PadModeFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderHexFormat:
						import.HexFormatFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderHexOutput:
						import.HexOutputFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderBrackets:
						import.BracketsFromInt(stoi(v));
						break;

					case LoadData::kLoadHeaderDataSource:
						eeo.SourceFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderOrientation:
						eeo.OrientationFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderScanDirection:
						eeo.ScanDirectionFromInt(eeo.Code.Source, stoi(v));
						break;
					case LoadData::kLoadHeaderLSB:
						eeo.LSBFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderLanguage:
						eeo.LSBFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderNumberFormat:
						eeo.NumberFormatFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderNumberSize:
						eeo.NumberSizeFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderLineContent:
						eeo.LineContentFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderLineCount:
						eeo.Code.LineCount = stoi(v);
						break;
					case LoadData::kLoadHeaderRGBMode:
						eeo.RGBModeFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderRGBChangePixels:
						eeo.Code.RGBChangePixels    = SafeStringToBool(v);
						break;
					case LoadData::kLoadHeaderRGBChangeColour:
						eeo.Code.RGBChangeColour    = stoi(v);
						break;

					case LoadData::kLoadHeaderOptimise:
						eeo.Optimise           = SafeStringToBool(v);
						break;

					case LoadData::kLoadHeaderMatrixComment:
						Details.Comment = v;
						break;
					case LoadData::kLoadHeaderRGBBackground:
						importRGBbackground = stoi(v);
						break;
					case LoadData::kLoadHeaderASCIIIndex:
						import.ASCIIIndex = stoi(v);
						break;
					case LoadData::kLoadHeaderAutomationFile:
						import.AutomationFileName = v;
						break;

					case LoadData::kLoadHeaderRGBBrightness:
						eeo.Code.RGBBrightness = stoi(v);
						import.RGBBrightness = eeo.Code.RGBBrightness;
						break;

					 // ======================================================================

					case LoadData::kLoadHeaderPreviewEnabled:
						import.Preview.Enabled = SafeStringToBool(v);
						break;
					case LoadData::kLoadHeaderPreviewSize:
						import.Preview.Size = stoi(v);
						break;
					case LoadData::kLoadHeaderPreviewView:
						import.Preview.ViewShapeFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderPreviewVoid:
						import.Preview.Void = stoi(v);
						break;
					case LoadData::kLoadHeaderPreviewOffset:
						import.Preview.Offset = stoi(v);
						break;
					case LoadData::kLoadHeaderPreviewOffsetDir:
						import.Preview.OffsetDirection = SafeStringToBool(v);
						break;
					case LoadData::kLoadHeaderPreviewIncRadially:
						import.Preview.IncrementRadially = SafeStringToBool(v);
						break;

					case LoadData::kLoadHeaderLayerCount:
						layercount = stoi(v);

						// layers have been saved in the file, so we know the first will be 0
						// set this to -1 so that when the [layer data is reached we increment from -1 to 0 ;)
						importLayer = -1;
						break;

					 // ======================================================================

					case LoadData::kLoadMatrixWidth:
						tempMaxWidth  = stoi(v);
						break;
					case LoadData::kLoadMatrixHeight:
						tempMaxHeight = stoi(v);
						break;
					case LoadData::kLoadMatrixData:
					{
						if (row == 0 && MatrixLayers[importLayer]->Cells.size() < importFrame + 1)
						{
							switch (loadmode)
							{
							case LoadMode::kNew:
								newwidth  = tempMaxWidth;
								newheight = tempMaxHeight;
								break;
							case LoadMode::kAppend:
							case LoadMode::kMergeBottomPriority:
							case LoadMode::kMergeTopPriority:
								newwidth  = Details.Width;
								newheight = Details.Height;
								break;
							}

							tempMaxWidth = newwidth;
							tempMaxHeight = newheight;

							Matrix *m = new Matrix(newwidth, newheight, Details.Mode, RGBBackground);
							MatrixLayers[importLayer]->Cells.push_back(m);
						}

						int x = 0;
						std::wstring pixel = L"";

						if (importRGBbackground == -1)
						{
							importRGBbackground = RGBBackground;
						}

						for (int i = 0; i < v.length(); i++)
						{
							if (v[i] == L' ' || i == v.length() - 1)
							{
								switch (loadmode)
								{
								case LoadMode::kMergeBottomPriority:
									if (mode == MatrixMode::kRGB)
									{
										if (MatrixLayers[importLayer]->Cells[importFrame]->Grid[row * tempMaxWidth + x] == importRGBbackground)
										{
											MatrixLayers[importLayer]->Cells[importFrame]->SafePlot(x, row, Convert::HexToInt(pixel));
										}
									}
									else
									{
										if (MatrixLayers[importLayer]->Cells[importFrame]->Grid[row * tempMaxWidth + x] == 0)
										{
											MatrixLayers[importLayer]->Cells[importFrame]->SafePlot(x, row, Convert::HexToInt(pixel));
										}
									}
									break;
								case LoadMode::kMergeTopPriority:
									if (mode == MatrixMode::kRGB)
									{
										if (Convert::HexToInt(pixel) != importRGBbackground)
										{
											MatrixLayers[importLayer]->Cells[importFrame]->SafePlot(x, row, Convert::HexToInt(pixel));
										}
									}
									else
									{
										if (Convert::HexToInt(pixel) != 0)
										{
											MatrixLayers[importLayer]->Cells[importFrame]->SafePlot(x, row, Convert::HexToInt(pixel));
										}
									}
									break;

								default:
									MatrixLayers[importLayer]->Cells[importFrame]->SafePlot(x, row, GetPixelFrom(Details.Mode, mode, Convert::HexToInt(pixel), importRGBbackground));
								}

								x++;

								pixel = L"";
							}
							else
							{
								pixel += v[i];
							}
						}

						row++;
						break;
					}
					case LoadData::kLoadMatrixLocked:
						MatrixLayers[importLayer]->Cells[importFrame]->Locked = stoi(v);
						break;

					 // ======================================================================

					case LoadData::kLoadDeadPixelData:
					{
						int x     = 0;
						std::wstring pixel = L"";

						for (int i = 0; i < v.length(); i++)
						{
							if (v[i] == L' ' || i == v.length() - 1)
							{
								if (pixel == L"0")
								{
									MatrixDeadLayout->Grid[row * tempMaxWidth + x] = PixelAlive;
								}
								else
								{
									MatrixDeadLayout->Grid[row * tempMaxWidth + x] = PixelDead;
								}

								x++;

								pixel = L"";
							}
							else
							{
								pixel += pixel + v[i];
							}
						}

						row++;
						break;
					}

					 // ====================================================================

					case LoadData::kLoadLayoutName:
						MatrixLayers[importLayer]->Name = v;
						break;
					case LoadData::kLoadLayoutWidth:
						tempMaxWidth  = stoi(v);
						break;
					case LoadData::kLoadLayoutHeight:
						tempMaxHeight = stoi(v);
						break;
					case LoadData::kLoadLayoutLocked:
						MatrixLayers[importLayer]->Locked = stoi(v);
						break;

					 // ====================================================================

					case LoadData::kLoadColoursCustom:
						import.Colours.CustomColours[colour] = stoi(v);

						colour++;
						break;
					case LoadData::kLoadColoursDraw0:
						import.Colours.DrawColours[CMouseLeft]   = stoi(v);
						break;
					case LoadData::kLoadColoursDraw1:
						import.Colours.DrawColours[CMouseMiddle] = stoi(v);
						break;
					case LoadData::kLoadColoursDraw2:
						import.Colours.DrawColours[CMouseRight]  = stoi(v);
						break;
					case LoadData::kLoadColoursPaletteHistory:
						import.Colours.PaletteHistory[palette] = stoi(v);
						break;

						palette++;
					}
				}
			}
		}

		file.close();

	//for i = 0 to MatrixLayers.Count - 1 do {
//      MatrixLayers[i]->Frames.Delete(MatrixLayers[0]->Frames.Count - 1); // do all
//    }

	EnsureLayerCoherence();

	Details.Height = tempMaxHeight;       // to do, maybe a problem if append a larger matrix?! no idea!
	Details.Width  = tempMaxWidth;

	Details.Available   = true;

	CurrentFrame = 0;

	Busy = false;

	CopyCurrentFrameToDrawBuffer();

	if (loadmode == LoadMode::kNew)
	{
		import.Mode = mode;
		import.NewWidth         = tempMaxWidth;
		import.NewHeight        = tempMaxHeight;
		import.BackgroundColour = importRGBbackground;
	}

	import.MaxFrames        = MatrixLayers[0]->Cells.size() - 1;
	import.FontMode         = fontmode;

	eeo.ExportMode = ExportSource::kAnimation;
//  except
//	on E: Exception do {
//	  Matrix.Available         = False;

//	  Result.ImportOk    = False;
//	  Result.ErrorString = GLanguageHandler.Text[kErrorLoadingProject] + ': "' + E.Message + '"';
//	}
	}

	if (OnLayerChange) OnLayerChange(this);

	PaintBox->Invalidate();

	return import;
}


ImportData TheMatrix::ImportLEDMatrixDataSingleFrame(const std::wstring file_name) // to do or cull ;)
{
	BackupMatrix(CurrentLayer, CurrentFrame);

	bool addedSingleFrame       = false;
	MatrixMode lMatrixMode      = MatrixMode::kMono;
	bool headermode             = false;
	bool deadpixelmode          = false;
	bool fontmode               = false;
	bool lMatrixDataMode        = false;
	bool lLayerMode             = false;
	bool lColoursMode           = false;
	int lRGBBackground         = -1;

	int lCurrentLayer          = 0;

	ImportData import;
	import.Source          = -1;
	import.SourceLSB       = -1;
	//  Result.SourceDirection = -1;
	import.Mode      = MatrixMode::kMono;
	import.RGBImport       = False;

	// ===========================================================================
	// ===========================================================================

	std::wifstream file(file_name);

	if (file)
	{
		int MemSlot    = CurrentFrame;
		int Row        = 0;

		std::wstring s(L"");

		while (std::getline(file, s))
		{
			if (s != L"")
			{
				if (s[0] == L'/' || s[0] == L'#')
				{
					// comment, do nothing
				}
				else
				{
					std::wstring v = s.substr(2);

					switch (LoadDataParameterType(s, headermode, lMatrixDataMode, deadpixelmode, lLayerMode, lColoursMode))  // TO DO layermode
					{
					case LoadData::kLoadBlockStartHeader:
						if (s == L"{" + kFileHeaderFontHeader)
						{
							fontmode = true;
						}
						else
						{
							fontmode = false;
						}

						headermode = true;
						break;

					case LoadData::kLoadBlockStartDeadPixel:	// dead pixel mode
						deadpixelmode   = true;
						lMatrixDataMode = false;

						Row = 0;
						break;

					case LoadData::kLoadBlockBegin:
						Row = 0;

						switch (v.length() - 1)
						{
						case L'2':
							lMatrixMode = MatrixMode::kBiSequential;
							break;
						case L'3':
							lMatrixMode = MatrixMode::kBiBitplanes;
							break;
						case L'4':
							lMatrixMode = MatrixMode::kRGB;
							break;
						case L'5':
							lMatrixMode = MatrixMode::kRGB3BPP;
							break;

						default:
							lMatrixMode = MatrixMode::kMono;
						}

						headermode = false;
						lMatrixDataMode = true;
						break;

					case LoadData::kLoadBlockEnd:
						if (lMatrixDataMode)
						{
							MemSlot++;
						}
						break;
					case LoadData::kLoadBlockBeginLayout:
						lLayerMode = True;
						break;
					case LoadData::kLoadBlockEndLayout:
						lLayerMode = False;
						break;

					   // ======================================================================

					case LoadData::kLoadHeaderSource:
						import.Source          = stoi(v);
						break;
					case LoadData::kLoadHeaderSourceLSB:
						import.SourceLSB       = stoi(v);
                        break;
					case LoadData::kLoadHeaderSourceDirection:
						import.SourceDirection = stoi(v);
						break;
					case LoadData::kLoadHeaderPadMode:
						import.PadModeFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderHexFormat:
						import.HexFormatFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderHexOutput:
						import.HexOutputFromInt(stoi(v));
						break;
					case LoadData::kLoadHeaderBrackets:
						import.BracketsFromInt(stoi(v));
						break;

					   // ======================================================================

					//       50 : tempMaxWidth  = stoi(v);
					//       51 : tempMaxHeight = stoi(v);
					case LoadData::kLoadMatrixData:
					{
						int	x = 0;
						std::wstring pixel = L"";

						for (int i = 0; i < v.length(); i++)
						{
							if (v[i] == ' ' || i == v.length() - 1)
							{
								if (import.RGBImport)
								{
									if (lRGBBackground != -1)
									{
										if (Convert::HexToInt(pixel) == lRGBBackground)
										{
											MatrixLayers[lCurrentLayer]->Cells[MemSlot]->Grid[Row * Details.Width + x] = RGBBackground;
										}
									}
								}
								else
								{
									MatrixLayers[lCurrentLayer]->Cells[MemSlot]->Grid[Row * Details.Width + x] = Convert::HexToInt(pixel);
								}

								x++;

								pixel = L"";
							}
							else
							{
								pixel += v[i];
							}
						}

						Row++;
						break;
					}

					   // ======================================================================

					case LoadData::kLoadDeadPixelData:
					{
						int x = 0;
						std::wstring pixel = L"";

						for (int i = 0; i < v.length(); i++)
						{
							if (v[i] == L' ' || i == v.length() - 1)
							{
								if (pixel == L"0")
								{
									MatrixDeadLayout->Grid[Row * Details.Width + x] = PixelAlive;
								}
								else
								{
									MatrixDeadLayout->Grid[Row * Details.Width + x] = PixelDead;
								}

								x++;

								pixel = L"";
							}
							else
							{
								pixel += v[i];
							}
						}

						Row++;
						break;
					}
					   // ======================================================================

					case LoadData::kLoadLayoutName:
						MatrixLayers[lCurrentLayer]->Name = v;
						break;
					}
				}
			}
		}

		file.close();
	}

	import.Mode = lMatrixMode;

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();

	return import;
}


bool TheMatrix::SaveAnimation(const std::wstring file_name, ImportData &tid, ExportOptions &eeo, ProjectColours &colours)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kFileHeaderHeader + L"\n");

		file << Formatting::to_utf8(kAnimPadModeF +            std::to_wstring(tid.PadModeToInt()) + L"\n");
		file << Formatting::to_utf8(kAnimHexFormatF +          std::to_wstring(tid.HexFormatToInt()) + L"\n");
		file << Formatting::to_utf8(kAnimHexOutputF +          std::to_wstring(tid.HexOutputToInt()) + L"\n");
		file << Formatting::to_utf8(kAnimBracketsF +           std::to_wstring(tid.BracketsToInt()) + L"\n");

		file << Formatting::to_utf8(kAnimPreviewEnabledF +     std::to_wstring(tid.Preview.Enabled) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewSizeF +        std::to_wstring(tid.Preview.Size) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewViewF +        std::to_wstring(tid.Preview.ViewToInt()) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewVoidF +        std::to_wstring(tid.Preview.Void) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewOffsetF +      std::to_wstring(tid.Preview.Offset) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewDirectionF +   std::to_wstring(tid.Preview.OffsetDirection) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewIncRadiallyF + std::to_wstring(tid.Preview.IncrementRadially) + L"\n");

		eeo.SaveToFile(file);

		file << Formatting::to_utf8(kAnimAutomationFileNameF + tid.AutomationFileName + L"\n");
		file << Formatting::to_utf8(kAnimCommentF +            Details.Comment + L"\n");
		file << Formatting::to_utf8(kAnimRGBBackgroundF +      std::to_wstring(RGBBackground) + L"\n");
		file << Formatting::to_utf8(kAnimFrameRangeF +         std::to_wstring(tid.StartFrame) + L"," + std::to_wstring(tid.EndFrame) + L"\n");
		file << Formatting::to_utf8(kAnimLayerCountF +         std::to_wstring(MatrixLayers.size()) + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		if (tid.Mode == MatrixMode::kRGB)
		{
			file << Formatting::to_utf8(L'{' + kFileHeaderColours + L"\n");

			for (int i = 0; i < 16; i++)
			{
				file << Formatting::to_utf8(kAnimColoursCustomF + std::to_wstring(colours.CustomColours[i]) + L"\n");
			}

			for (int i = 0; i < 28; i++)
			{
				file << Formatting::to_utf8(kAnimColoursPaletteHistoryF + std::to_wstring(colours.PaletteHistory[i]) + L"\n");
			}

			file << Formatting::to_utf8(kAnimColoursLeftF +   std::to_wstring(colours.DrawColours[CMouseLeft]) + L"\n");
			file << Formatting::to_utf8(kAnimColoursMiddleF + std::to_wstring(colours.DrawColours[CMouseMiddle]) + L"\n");
			file << Formatting::to_utf8(kAnimColoursRightF +  std::to_wstring(colours.DrawColours[CMouseRight]) + L"\n");

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		// ===================================================================

		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			file << Formatting::to_utf8(L"[" + kFileHeaderLayer + L"\n");
			file << Formatting::to_utf8(kAnimLayerNameF +   MatrixLayers[layer]->Name + L"\n");
			file << Formatting::to_utf8(kAnimLayerWidthF +  std::to_wstring(Details.Width) + L"\n");
			file << Formatting::to_utf8(kAnimLayerHeightF + std::to_wstring(Details.Height) + L"\n");
			file << Formatting::to_utf8(kAnimLayerLockedF + std::to_wstring(MatrixLayers[layer]->Locked) + L"\n");
			file << Formatting::to_utf8(L"]\n");

			// ===============================================================

			for (int frame = tid.StartFrame; frame <= tid.EndFrame; frame++)
			{
				switch (tid.Mode)
				{
				case MatrixMode::kMono:
					file << Formatting::to_utf8(L"{" + kFilePrefixMono + L"\n");
					break;
				case MatrixMode::kBiSequential:
					file << Formatting::to_utf8(L"{" + kFilePrefixBiSequential + L"\n");
					break;
				case MatrixMode::kBiBitplanes:
					file << Formatting::to_utf8(L"{" + kFilePrefixBiBitPlanes + L"\n");
					break;
				case MatrixMode::kRGB:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB + L"\n");
					break;
				case MatrixMode::kRGB3BPP:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB3BPP + L"\n");
					break;
				}

				file << Formatting::to_utf8(kAnimWidthF  + std::to_wstring(Details.Width) + L"\n");
				file << Formatting::to_utf8(kAnimHeightF + std::to_wstring(Details.Height) + L"\n");

				for (int y = 0; y < Details.Height; y++)
				{
					std::wstring s = L"";

					for (int x = 0; x < Details.Width; x++)
					{
						s += IntToHex(MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x], 6) + L" ";
					}

					file << Formatting::to_utf8(kAnimRowDataF + s + L"\n");
				}

				file << Formatting::to_utf8(kAnimFrameLockedF + std::to_wstring(MatrixLayers[layer]->Cells[frame]->Locked) + L"\n");

				file << Formatting::to_utf8(kDataBlockEndS + L"\n");
			}
		}


		// ===========================================================================

		file << Formatting::to_utf8(L"{" + kFileHeaderDeadPixel + L"\n");

		for (int y = 0; y < Details.Height; y++)
		{
			std::wstring s = L"";

			for (int x = 0; x < Details.Width; x++)
			{
				s += std::to_wstring(MatrixDeadLayout->Grid[y * Details.Width + x]) + L" ";
			}

			file << Formatting::to_utf8(kAnimDeadPixelDataF + s + L"\n");
		}

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		file.close();

		return true;
	}

	return false;
}


void TheMatrix::SaveFont(const std::wstring file_name, ImportData &tid, ExportOptions &eeo)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kFileHeaderFontHeader);

		file << Formatting::to_utf8(kAnimPreviewEnabledF +     std::to_wstring(tid.Preview.Enabled) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewSizeF +        std::to_wstring(tid.Preview.Size) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewViewF +        std::to_wstring(tid.Preview.ViewToInt()) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewVoidF +        std::to_wstring(tid.Preview.Void) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewOffsetF +      std::to_wstring(tid.Preview.Offset) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewDirectionF +   std::to_wstring(tid.Preview.OffsetDirection) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewIncRadiallyF + std::to_wstring(tid.Preview.IncrementRadially) + L"\n");

		eeo.SaveToFile(file);

		file << Formatting::to_utf8(kAnimAutomationFileNameF + tid.AutomationFileName + L"\n");
		file << Formatting::to_utf8(kAnimCommentF +            Details.Comment + L"\n");
		file << Formatting::to_utf8(kAnimRGBBackgroundF +      std::to_wstring(RGBBackground) + L"\n");
		file << Formatting::to_utf8(kAnimFrameRangeF +         std::to_wstring(tid.StartFrame) + L"," + std::to_wstring(tid.EndFrame) + L"\n");
		file << Formatting::to_utf8(kAnimLayerCountF +         std::to_wstring(MatrixLayers.size()) + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			file << Formatting::to_utf8(L"[" + kFileHeaderLayer);
			file << Formatting::to_utf8(kAnimLayerNameF +   MatrixLayers[layer]->Name + L"\n");
			file << Formatting::to_utf8(kAnimLayerWidthF +  std::to_wstring(Details.Width) + L"\n");
			file << Formatting::to_utf8(kAnimLayerHeightF + std::to_wstring(Details.Height) + L"\n");
			file << Formatting::to_utf8(kAnimLayerLockedF + std::to_wstring(MatrixLayers[layer]->Locked) + L"\n");
			file << Formatting::to_utf8(L"]");

			for (int i = 1; i <= FontCharacterCount; i++)
			{
				switch (tid.Mode)
				{
				case MatrixMode::kMono:
					file << Formatting::to_utf8(L"{" + kFilePrefixMono + L"\n");
					break;
				case MatrixMode::kBiSequential:
					file << Formatting::to_utf8(L"{" + kFilePrefixBiSequential + L"\n");
					break;
				case MatrixMode::kBiBitplanes:
					file << Formatting::to_utf8(L"{" + kFilePrefixBiBitPlanes + L"\n");
					break;
				case MatrixMode::kRGB:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB + L"\n");
					break;
				case MatrixMode::kRGB3BPP:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB3BPP + L"\n");
					break;
				}

				file << Formatting::to_utf8(kAnimWidthF +  std::to_wstring(Details.Width) + L"\n");
				file << Formatting::to_utf8(kAnimHeightF + std::to_wstring(Details.Height) + L"\n");

				for (int y = 0; y < Details.Height; y++)
				{
					std::wstring s = L"";

					for (int x = 0; x < Details.Width; x++)
					{
						s += IntToHex(MatrixLayers[layer]->Cells[i]->Grid[y * Details.Width + x], 6) + L" ";    // 6 was 4, to do check!
					}

					file << Formatting::to_utf8(kAnimRowDataF + s + L"\n");
				}

				file << Formatting::to_utf8(kDataBlockEndS + L"\n");
			}
		}

		// ===========================================================================

		file << Formatting::to_utf8(L"{" + kFileHeaderDeadPixel + L"\n");

		for (int y = 0; y < Details.Height; y++)
		{
			std::wstring s = L"";

			for (int x = 0; x < Details.Width; x++)
			{
				s += std::to_wstring(MatrixDeadLayout->Grid[y * Details.Width + x]) + L" ";
			}

			file << Formatting::to_utf8(kAnimDeadPixelDataF + s + L"\n");
		}

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		file.close();
	}
}


void TheMatrix::SaveSingleFrame(const std::wstring file_name, ImportData tid, int frame)
{
	std::ofstream file(file_name);

	if (file)
	{
		switch (tid.Mode)
		{
		case MatrixMode::kMono:
			file << Formatting::to_utf8(L"{" + kFramePrefixMono);
			break;
		case MatrixMode::kBiSequential:
			file << Formatting::to_utf8(L"{" + kFramePrefixBiSequential);
			break;
		case MatrixMode::kBiBitplanes:
			file << Formatting::to_utf8(L"{" + kFramePrefixBiBitPlanes);
			break;
		case MatrixMode::kRGB:
			file << Formatting::to_utf8(L"{" + kFramePrefixRGB);
			break;
		case MatrixMode::kRGB3BPP:
			file << Formatting::to_utf8(L"{" + kFramePrefixRGB3BPP);
			break;
		}

		file << Formatting::to_utf8(kAnimWidthF + std::to_wstring(Details.Width));
		file << Formatting::to_utf8(kAnimHeightF + std::to_wstring(Details.Height));
		file << Formatting::to_utf8(kAnimCommentF + Details.Comment);
		file << Formatting::to_utf8(kAnimRGBBackgroundF + std::to_wstring(RGBBackground));

		file << Formatting::to_utf8(kDataBlockEndS);

		// ===========================================================================

		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			file << Formatting::to_utf8(L"[" + kFileHeaderLayer);
			file << Formatting::to_utf8(kAnimLayerNameF +   MatrixLayers[layer]->Name);
			file << Formatting::to_utf8(kAnimLayerWidthF +  std::to_wstring(Details.Width));
			file << Formatting::to_utf8(kAnimLayerHeightF + std::to_wstring(Details.Height));
			file << Formatting::to_utf8(L"]");

			file << Formatting::to_utf8(kDataBlockStartS);

			for (int y = 0; y < Details.Height; y++)
			{
				std::wstring s = L"";

				for (int x = 0; x < Details.Width; x++)
				{
					s += IntToHex(MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x], 6) + L" ";
				}

				file << Formatting::to_utf8(kAnimRowDataF + s);
			}

			file << Formatting::to_utf8(kDataBlockEndS);
		}

		// ===========================================================================

		file << Formatting::to_utf8(L"{" + kFileHeaderDeadPixel);

		for (int y = 0; y < Details.Height; y++)
		{
			std::wstring s = L"";

			for (int x = 0; x < Details.Width; x++)
			{
				s += std::to_wstring(MatrixDeadLayout->Grid[y * Details.Width + x]) + L" ";
			}

			file << Formatting::to_utf8(kAnimDeadPixelDataF + s);
		}

		file << Formatting::to_utf8(kDataBlockEndS);

		// ===================================================================

		file.close();
	}
}


void TheMatrix::SaveAsTextToolFont(const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		for (int t = 1; t < FontCharacterCount; t++)
		{
			std::wstring s = L"";

			for (int x = 0; x < Details.Width; x++)
			{
				int mydata = 0;

				for (int y = 0; y < Details.Height; y++)
				{
					if (MatrixLayers[CPermanentLayer]->Cells[t]->Grid[y * Details.Width + x] == 1)
					{
						mydata = mydata + (powers[Details.Height - y - 1]);
					}
				}

				if (x != Details.Width - 1)
				{
					s += std::to_wstring(mydata) + L", ";
				}
				else
				{
					s += std::to_wstring(mydata);
				}
			}

			file << Formatting::to_utf8(s + L" // " + Char(32 + t) + L"\n");
		}

		file.close();
	}
}


void TheMatrix::SaveAsRGBFont(const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kFileHeaderFontRGB + L"\n");
		file << Formatting::to_utf8(kRGBFontWidthF  + std::to_wstring(Details.Width) + L"\n");
		file << Formatting::to_utf8(kRGBFontHeightF + std::to_wstring(Details.Height) + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		for (int t = 0; t < FontCharacterCount; t++)
		{
			file << Formatting::to_utf8(L"{" + kFontPrefixChar + L"\n");

			for (int x = 0; x < Details.Width; x++)
			{
				std::wstring mydata = L"";

				for (int y = 0; y < Details.Height; y++)
				{
					if (MatrixLayers[CPermanentLayer]->Cells[t]->Grid[y * Details.Width + x] != RGBBackground)
					{
						mydata += IntToHex(MatrixLayers[CPermanentLayer]->Cells[t]->Grid[y * Details.Width + x], 6).c_str();

						mydata += L" ";
					}
					else
					{
						mydata += + L"-1 ";
					}
				}

				file << Formatting::to_utf8(kRGBFontDataF + L":" + mydata + L"\n");
			}

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		file.close();
	}
}


LoadData TheMatrix::LoadDataParameterType(const std::wstring s, bool headermode, bool matrixmode, bool deadpixelmode, bool layermode, bool coloursmode)
{
	if (s.find(L"{header") != std::wstring::npos)
		return LoadData::kLoadBlockStartHeader;
	else if (s.find(L"{deadpixel") != std::wstring::npos)
		return LoadData::kLoadBlockStartDeadPixel;
	else if (s.find(L"{colours") != std::wstring::npos)
		return LoadData::kLoadBlockStartColours;
	else if (s[0] == kDataBlockStart)
		return LoadData::kLoadBlockBegin;
	else if (s[0] == kDataBlockEnd)
		return LoadData::kLoadBlockEnd;
	else if (s[0] == L'[')
		return LoadData::kLoadBlockBeginLayout;
	else if (s[0] == L']')
		return LoadData::kLoadBlockEndLayout;
	else if (headermode)
	{
		switch (s[0])
		{
		case kAnimDataSource:
			return LoadData::kLoadHeaderSource;
		case kAnimSourceLSB:
			return LoadData::kLoadHeaderSourceLSB;
		case kAnimSourceDirection:
			return LoadData::kLoadHeaderSourceDirection;
		case kAnimPadMode:
			return LoadData::kLoadHeaderPadMode;
		case kAnimHexFormat:
			return LoadData::kLoadHeaderHexFormat;
		case kAnimHexOutput:
			return LoadData::kLoadHeaderHexOutput;
		case kAnimBrackets:
			return LoadData::kLoadHeaderBrackets;
		case kAnimSource:
			return LoadData::kLoadHeaderDataSource;
		case kAnimOrientation:
			return LoadData::kLoadHeaderOrientation;
		case kAnimScanDirection:
			return LoadData::kLoadHeaderScanDirection;
		case kAnimLSB:
			return LoadData::kLoadHeaderLSB;
		case kAnimLanguage:
			return LoadData::kLoadHeaderLanguage;
		case kAnimNumberFormat:
			return LoadData::kLoadHeaderNumberFormat;

		case kAnimNumberSize:
			return LoadData::kLoadHeaderNumberSize;
		case kAnimLineContent:
			return LoadData::kLoadHeaderLineContent;
		case kAnimLineCount:
			return LoadData::kLoadHeaderLineCount;
		case kAnimRGBMode:
			return LoadData::kLoadHeaderRGBMode;
		case kAnimRGBChangePixels:
			return LoadData::kLoadHeaderRGBChangePixels;
		case kAnimRGBChangeColour:
			return LoadData::kLoadHeaderRGBChangeColour;
		case kAnimOptimise:
			return LoadData::kLoadHeaderOptimise;
		case kAnimRGBBrightness:
			return LoadData::kLoadHeaderRGBBrightness;

		case kAnimAutomationFileName:
			return LoadData::kLoadHeaderAutomationFile;
		case kAnimComment:
			return LoadData::kLoadHeaderMatrixComment;
		case kAnimASCIIIndex:
			return LoadData::kLoadHeaderASCIIIndex;
		case kAnimRGBBackground:
			return LoadData::kLoadHeaderRGBBackground;

		case kAnimPreviewEnabled:
			return LoadData::kLoadHeaderPreviewEnabled;
		case kAnimPreviewSize:
			return LoadData::kLoadHeaderPreviewSize;
		case kAnimPreviewView:
			return LoadData::kLoadHeaderPreviewView;
		case kAnimPreviewVoid:
			return LoadData::kLoadHeaderPreviewVoid;
		case kAnimPreviewOffset:
			return LoadData::kLoadHeaderPreviewOffset;
		case kAnimPreviewDirection:
			return LoadData::kLoadHeaderPreviewOffsetDir;
		case kAnimPreviewIncRadially:
			return LoadData::kLoadHeaderPreviewIncRadially;
		case kAnimLayerCount:
			return LoadData::kLoadHeaderLayerCount;
		case kAnimBinary:
            return LoadData::kLoadHeaderBinaryData;

		case kAnimBlockEnd:
			return LoadData::kLoadHeaderEnd;
		}
	}
	else if (deadpixelmode)
	{
		switch (s[0])
		{
		casekAnimDeadPixelData:
			return LoadData::kLoadDeadPixelData;
		}
	}
	else if (matrixmode)
	{
		switch (s[0])
		{
		case kAnimWidth:
			return LoadData::kLoadMatrixWidth;
		case kAnimHeight:
			return LoadData::kLoadMatrixHeight;
		case kAnimRowData:
			return LoadData::kLoadMatrixData;
		case kAnimFrameLocked:
			return LoadData::kLoadMatrixLocked;
		}
	}
	else if (layermode)
	{
		switch (s[0])
		{
		case kAnimLayerName:
			return LoadData::kLoadLayoutName;
		case kAnimLayerWidth:
			return LoadData::kLoadLayoutWidth;
		case kAnimLayerHeight:
			return LoadData::kLoadLayoutHeight;
		case kAnimLayerLocked:
			return LoadData::kLoadLayoutLocked;
		}
	}
	else if (coloursmode)
	{
		switch (s[0])
		{
		case kAnimColoursCustom:
			return LoadData::kLoadColoursCustom;
		case kAnimColoursLeft:
			return LoadData::kLoadColoursDraw0;
		case kAnimColoursMiddle:
			return LoadData::kLoadColoursDraw1;
		case kAnimColoursRight:
			return LoadData::kLoadColoursDraw2;
		case kAnimColoursPaletteHistory:
			return LoadData::kLoadColoursPaletteHistory;
		}
	}

	return LoadData::kUnknown;
}
#pragma end_region


#pragma region MatrixActions
void TheMatrix::PerformEffectController(int mode, int multipleoptionmode)
{
	switch (multipleoptionmode)
	{
	case CMOMCurrentOnly:
		PerformEffect(mode, CurrentLayer, CurrentFrame);
		break;
	case CMOMCurrentFrameLayers:
		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			PerformEffect(mode, layer, CurrentFrame);
		}
		break;
	case CMOMCurrentLayerFrames:
		for (int frame = 0; frame < MatrixLayers[CurrentLayer]->Cells.size(); frame++)
		{
			PerformEffect(mode, CurrentLayer, frame);
		}
		break;
	case CMOMAll:
		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			for (int frame = 0; frame < MatrixLayers[layer]->Cells.size(); frame++)
			{
				PerformEffect(mode, layer, frame);
			}
		}
		break;
	}

	if (!AutomateMode)
	{
		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformEffect(int mode, int layer, int frame)
{
	if (IsThisFrameLocked(layer, frame) || !MatrixLayers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
	{
	case modeFlip:
		for (int x = 0; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (Details.Width - x - 1)];
			}
		}
		break;
	case modeMirror:
		for (int y = 0; y < Details.Height; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(Details.Height - y - 1) * Details.Width + x];
			}
		}
		break;
	case modeInvert:
		for (int x = 0; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				switch (Details.Mode)
				{
				case MatrixMode::kMono:
					MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = 1 - MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x];
					break;
				case MatrixMode::kBiSequential:
				case MatrixMode::kBiBitplanes:
					MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = 3 - MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x];
					break;
				case MatrixMode::kRGB:
					MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = 0xFFFFFF - MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x];
					break;
				case MatrixMode::kRGB3BPP:
					MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = 0x4 - MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x];
					break;
                }
			}
		}
		break;
	case modeGradientAll:
		for (int x = 0; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				if (Render.Gradient.Option == GradientOption::kVertical && Render.Gradient.IY[y] != 0)
				{
					if (MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] != 0)
					{
						MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = Render.Gradient.IY[y];
					}
				}
			}
		}
		break;
	}

	MatrixLayers[layer]->Cells[frame]->AddToHistory();
}


void TheMatrix::PerformScrollController(int mode, int multipleoptionmode)
{
	switch (multipleoptionmode)
	{
	case CMOMCurrentOnly:
		PerformScroll(mode, CurrentLayer, CurrentFrame);
		break;
	case CMOMCurrentFrameLayers:
		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			PerformScroll(mode, layer, CurrentFrame);
		}
		break;
	case CMOMCurrentLayerFrames:
		for (int frame = 0; frame < MatrixLayers[CurrentLayer]->Cells.size(); frame++)
		{
			PerformScroll(mode, CurrentLayer, frame);
		}
		break;
	case CMOMAll:
		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			for (int frame = 0; frame < MatrixLayers[layer]->Cells.size(); frame++)
			{
				PerformScroll(mode, layer, frame);
			}
		}
		break;
	}

	if (!AutomateMode)
	{
		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformScroll(int mode, int layer, int frame)
{
	if (IsThisFrameLocked(layer, frame) || !MatrixLayers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
	{
	case modeScrollLeft:
		for (int x = 0; x <= Details.Width - 2; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x + 1)];
			}
		}

		for (int y = 0; y < Details.Height; y++)
		{
			MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + (Details.Width - 1)] = MatrixBackup->Grid[y * Details.Width];
		}
		break;
	case modeScrollRight:
		for (int x = 1; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				 MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x - 1)];
			}
		}

		for (int y = 0; y < Details.Height; y++)
		{
			MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width] = MatrixBackup->Grid[y * Details.Width + (Details.Width - 1)];
		}
		break;
	case modeScrollUp:
		for (int y = 0; y < Details.Height - 1; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				 MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y + 1) * Details.Width + x];
			}
		}

		for (int x = 0; x < Details.Width; x++)
		{
			MatrixLayers[layer]->Cells[frame]->Grid[(Details.Height - 1) * Details.Width + x] = MatrixBackup->Grid[x];
		}
		break;
	case modeScrollDown:
		for (int y = 1; y < Details.Height; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
			 MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y - 1) * Details.Width + x];
			}
		}

		for (int x = 0; x < Details.Width; x++)
		{
			MatrixLayers[layer]->Cells[frame]->Grid[x] = MatrixBackup->Grid[(Details.Height - 1) * Details.Width + x];
		}
		break;
	}

	MatrixLayers[layer]->Cells[frame]->AddToHistory();
}


void TheMatrix::PerformSplitScroll(int mode, int layer, int frame)
{
	if (IsThisFrameLocked(layer, frame) || !MatrixLayers[layer]->Visible) return;

	switch (mode)
	{
	case modeSplitScrollLeftRight:
	case modeSplitScrollRightLeft:
	{
		int mid = std::round(Details.Height / 2) - 1;

		int	a = modeScrollRowLeft;
		int b = modeScrollRowRight;

		switch (mode)
		{
		case modeSplitScrollLeftRight:
			a = modeScrollRowLeft;
			b = modeScrollRowRight;
			break;
		case modeSplitScrollRightLeft:
			a = modeScrollRowRight;
			b = modeScrollRowLeft;
			break;
		}

		for (int row = 0; row <= mid; row++)
		{
			ScrollRow(layer, frame, a, row);
		}

		for (int row = mid + 1; row < Details.Height; row++)
		{
			ScrollRow(layer, frame, b, row);
		}
		break;
	}
	case modeSplitScrollUpDown:
	case modeSplitScrollDownUp:
	{
		int mid = std::round(Details.Width / 2) - 1;
		int a = modeScrollColumnUp;
		int b = modeScrollColumnDown;

		switch (mode)
		{
		case modeSplitScrollUpDown:
			a = modeScrollColumnUp;
			b = modeScrollColumnDown;
			break;
		case modeSplitScrollDownUp:
			a = modeScrollColumnDown;
			b = modeScrollColumnUp;
			break;
		}

		for (int row = 0; row <= mid; row++)
		{
			ScrollColumn(layer, frame, a,row);
		}

		for (int row = mid + 1; row < Details.Height; row++)
		{
			ScrollColumn(layer, frame, b, row);
		}
		break;
	}
	}
}


void TheMatrix::PerformAlternateScroll(int mode, int layer, int frame)
{
	if (IsThisFrameLocked(layer, frame) || !MatrixLayers[layer]->Visible) return;

	switch (mode)
	{
	case modeAlternateScrollUpDown:
	case modeAlternateScrollDownUp:
	{
		int coeff = std::round((double)Details.Width / 4);

		int count = 0;
		int mode  = modeScrollColumnUp;

		for (int t = 0; t < Details.Width; t++)
		{
			ScrollColumn(layer, frame, mode, t);

			count++;

			if (count == coeff)
			{
				count = 0;

				if (mode == modeScrollColumnUp)
				{
					mode = modeScrollColumnDown;
				}
				else
				{
					mode = modeScrollColumnUp;
				}
			}
		}
		break;
	}
	}
}


void TheMatrix::PerformWipeOnCurrentFrame(int mode, bool clear)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) || !MatrixLayers[CurrentLayer]->Visible) return;

	BackupMatrix(CurrentLayer, CurrentFrame);

	switch (mode)
    {
	case modeWipeVerticalOut:
	{
		int z = std::round(Details.Width / 2);

		for  (int x = 0; x <= z - 2; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x + 1)];
			}
		}

		for (int x = Details.Width - 1; z >= z + 1; z--)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x - 1)];
			}
		}

		for (int y = 0; y < Details.Height; y++)
		{
			if (clear)
			{

				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + (z - 1)] = RGBBackground;
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + z]     = RGBBackground;
			}
			else
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + (z - 1)] = MatrixBackup->Grid[y * Details.Width];
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + z]     = MatrixBackup->Grid[y * Details.Width + (Details.Width - 1)];
			}
		}
		break;
	}
	case modeWipeVerticalIn:
	{
		int z = std::round(Details.Width / 2);

		for (int x = 1; x <= z - 1; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x - 1)];
			}
		}

		for (int x = Details.Width - 1; x >= z; x--)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x + 1)];
			}
		}

		for (int y = 0; y < Details.Height; y++)
		{
			if (clear)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width] = RGBBackground;
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + (Details.Width - 1)] = RGBBackground;
			}
			else
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width] = MatrixBackup->Grid[y * Details.Width + (z - 1)];
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + (Details.Width - 1)] = MatrixBackup->Grid[y * Details.Width + z];
			}
		}
		break;
	}
	case modeWipeHorizontalOut:
	{
		int z = std::round(Details.Height / 2);

		for (int y = 0; y <= z - 2; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y + 1) * Details.Width + x];
			}
		}

		for (int y = Details.Height - 1; y >= z + 1; y--)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y - 1) * Details.Width + x];
			}
		}

		for (int x = 0; x < Details.Width; x++)
		{
			if (clear)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[(z - 1) * Details.Width + x] = RGBBackground;
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[z * Details.Width + x] = RGBBackground;
			}
			else
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[(z - 1) * Details.Width + x] = MatrixBackup->Grid[x];
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[z * Details.Width + z] = MatrixBackup->Grid[(Details.Height - 1) * Details.Width + x];
			}
		}
		break;
	}
	case modeWipeHorizontalIn:
	{
		int z = std::round(Details.Height / 2);

		for (int y = 1; y < z; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y - 1) * Details.Width + x];
			}
		}

		for (int y = Details.Height - 1; y >= z; y--)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y + 1) * Details.Width + x];
			}
		}

		for (int x = 0; x < Details.Width; x++)
		{
			if (clear)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[x] = RGBBackground;
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[(Details.Height - 1) * Details.Width + x] = RGBBackground;
			}
			else
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[x]                = MatrixBackup->Grid[(z - 1) * Details.Width + x];
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[(Details.Height - 1) * Details.Width + x] = MatrixBackup->Grid[z * Details.Width + x];
			}
		}
		break;
	}
	case modeWipeLeftToRight:
	{
		for (int x = 0; x <= Details.Width - 2; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x + 1)];
			}
		}

		for (int y = 0; y < Details.Height; y++)
		{
			if (clear)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + (Details.Width - 1)] = RGBBackground;
			}
			else
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + (Details.Width - 1)] = MatrixBackup->Grid[y * Details.Width];
			}
		}
		break;
	}
	case modeWipeRightToLeft:
	{
		for (int x = 1; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x - 1)];
			}
		}

		for (int y = 0; y < Details.Height; y++)
		{
			if (clear)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width] = RGBBackground;
			}
			else
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width] = MatrixBackup->Grid[y * Details.Width + (Details.Width - 1)];
			}
		}
		break;
	}
	case modeWipeUpToDown:
	{
		for (int y = 0; y <= Details.Height - 2; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y + 1) * Details.Width + x];
			}
		}

		for (int x = 0; x < Details.Width; x++)
		{
			if (clear)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[(Details.Height - 1) * Details.Width + x] = RGBBackground;
			}
			else
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[(Details.Height - 1) * Details.Width + x] = MatrixBackup->Grid[x];
			}
		}
		break;
	}
	case modeWipeDownToUp:
	{
		for (int y = 1; y < Details.Height; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y - 1) * Details.Width + x];
			}
		}

		for (int x = 0; x < Details.Width; x++)
		{
			if (clear)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[x] = RGBBackground;
			}
			else
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[x] = MatrixBackup->Grid[(Details.Height - 1) * Details.Width + x];
			}
		}
		break;
	}
	}

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();

	if (!AutomateMode)
	{
		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformRevealOnCurrentFrame(int mode, int colour, int &parameter)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) || !MatrixLayers[CurrentLayer]->Visible) return;

	BackupMatrix(CurrentLayer, CurrentFrame);

	switch (mode)
	{
	case modeRevealLeftRight:
		if (parameter <= Details.Width - 1)
		{
			for (int x = parameter; x < Details.Width; x++)
			{
				for (int y = 0; y < Details.Height; y++)
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = colour;
				}
			}

		 parameter++;
		}
		break;
	case modeRevealRightLeft:
		if (parameter >= 0)
		{
			for (int x = parameter; x >= 0; x++)
			{
				for (int y = 0; y < Details.Height; y++)
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = colour;
				}
			}

			parameter--;
		}
		break;
	case modeRevealTopBottom:
		if (parameter <= Details.Height - 1)
		{
			for (int y = parameter; y < Details.Height; y++)
			{
				for (int x = 0; x < Details.Width; x++)
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = colour;
				}
			}

			parameter++;
		}
		break;
	case modeRevealBottomTop:
		if (parameter >= 0)
		{
			for (int y = parameter; y >= 0; y--)
			{
				for (int x = 0; x < Details.Width; x++)
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = colour;
				}
			}

			parameter--;
		}
		break;
	modeRevealCentreIn:
	modeRevealCentreOut:
		break;
	}

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();

	if (!AutomateMode)
	{
		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformScrollOnCopyFrame(int mode)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) || !MatrixLayers[CurrentLayer]->Visible) return;

	BackupMatrix(-1, -1);

	switch (mode)
	{
	case modeScrollLeft:
		for (int x = 0; x <= Details.Width - 2; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x + 1)];
			}
		}

		for (int y = 0; y < Details.Height; y++)
		{
			MatrixCopy->Grid[y * Details.Width + (Details.Width - 1)] = MatrixBackup->Grid[y * Details.Width];
		}
		break;
	case modeScrollRight:
		for (int x = 1; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (x - 1)];
			}
		}

		for (int y = 0; y < Details.Height; y++)
		{
			MatrixCopy->Grid[y * Details.Width] = MatrixBackup->Grid[y * Details.Width + (Details.Width - 1)];
		}
		break;
	case modeScrollUp:
		for (int y = 0; y <= Details.Height - 2; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y + 1) * Details.Width + x];
			}
		}

		for (int x = 0; x < Details.Width; x++)
		{
			MatrixCopy->Grid[(Details.Height - 1) * Details.Width + x] = MatrixBackup->Grid[x];
		}
		break;
	case modeScrollDown:
		for (int y = 1; y < Details.Height; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[(y - 1) * Details.Width + x];
			}
		}

		for (int x = 0; x < Details.Width; x++)
		{
			MatrixCopy->Grid[x] = MatrixBackup->Grid[(Details.Height - 1) * Details.Width + x];
		}
		break;
	}
}


void TheMatrix::PerformColumnScrollOnCurrentFrame(int mode, int column, bool clear)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) || !MatrixLayers[CurrentLayer]->Visible) return;

	BackupMatrix(CurrentLayer, CurrentFrame);

	switch (mode)
	{
	case modeScrollUp:
		for (int y = 0; y <= Details.Height - 2; y++)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + column] = MatrixBackup->Grid[(y + 1) * Details.Width + column];
		}

		if (clear)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[(Details.Height - 1) * Details.Width + column] = 0;
		}
		else
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[(Details.Height - 1) * Details.Width + column] = MatrixBackup->Grid[column];
		}
		break;
	case modeScrollDown:
		for (int y = 1; y < Details.Height; y++)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + column] = MatrixBackup->Grid[(y - 1) * Details.Width + column];
		}

		if (clear)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[column] = 0;
		}
		else
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[column] = MatrixBackup->Grid[(Details.Height - 1) * Details.Width + column];
		}
		break;
	}

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();

	if (!AutomateMode)
	{
		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformRowScrollOnCurrentFrame(int mode, int row, bool clear)
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) || !MatrixLayers[CurrentLayer]->Visible) return;

	BackupMatrix(CurrentLayer, CurrentFrame);

	switch (mode)
	{
	case modeScrollLeft:
		for (int x = 0; x < Details.Width - 1; x++)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[row * Details.Width + x] = MatrixBackup->Grid[row * Details.Width + (x + 1)];
		}

		if (clear)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[row * Details.Width + (Details.Width - 1)] = 0;
		}
		else
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[row * Details.Width + (Details.Width - 1)] = MatrixBackup->Grid[row];
		}
		break;
	case modeScrollRight:
		for (int x = 1; x < Details.Width; x++)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[row * Details.Width + x] = MatrixBackup->Grid[row * Details.Width + (x - 1)];
		}

		if (clear)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[row] = 0;
		}
		else
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[row] = MatrixBackup->Grid[row * Details.Width + (Details.Width - 1)];
		}
		break;
	}

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();

	if (!AutomateMode)
	{
		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::RotateFrameController(int mode, int multipleoptionmode)
{
	CopyDrawBufferToCurrentFrame();

	Busy = True;

	switch (multipleoptionmode)
	{
	case CMOMCurrentOnly:
		RotateFrame(mode, CurrentLayer, CurrentFrame);
		break;
	case CMOMCurrentFrameLayers:
		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			RotateFrame(mode, layer, CurrentFrame);
		}
		break;
	case CMOMCurrentLayerFrames:
		for (int frame = 0; frame < MatrixLayers[CurrentLayer]->Cells.size(); frame++)
		{
			RotateFrame(mode, CurrentLayer, frame);
		}
		break;
	case CMOMAll:
		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			for (int frame = 0; frame < MatrixLayers[layer]->Cells.size(); frame++)
			{
				RotateFrame(mode, layer, frame);
			}
		}
		break;
	}

	Busy = False;

	CopyCurrentFrameToDrawBuffer();

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::RotateFrame(int mode, int layer, int frame)
{
	if (IsThisFrameLocked(layer, frame) || !MatrixLayers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
	{
	case modeRotateCW:
		for (int x = 0; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[(Details.Width - x - 1) * Details.Width + y];
			}
		}
		break;
	case modeRotateACW:
		for (int x = 0; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = MatrixBackup->Grid[x * Details.Width + (Details.Height - y - 1)];
			}
		}
		break;
	}

	MatrixLayers[layer]->Cells[frame]->AddToHistory();
}


void TheMatrix::RotateFrameAnyAngle(double angle, int toframe)   // to do for multilayer
{
	if (IsThisFrameLocked(CurrentLayer, toframe) || !MatrixLayers[CurrentLayer]->Visible) return;

	MatrixLayers[CurrentLayer]->Cells[toframe]->Clear(Details.Mode, RGBBackground);

	double myangle = (3.1415926535 * angle) / 180;
	int hx = std::round(((double)Details.Width - 1) / 2);
	int hy = std::round(((double)Details.Height - 1) / 2);

	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			int ox = x - hx;
			int oy = y - hy;

			int newx = hx + std::round((ox * std::cos(angle)) - (oy * std::sin(angle)));
			int newy = hy + std::round((ox * std::sin(angle)) + (oy * std::cos(angle)));

			switch (Details.Mode)
			{
			case MatrixMode::kRGB:
			case MatrixMode::kRGB3BPP:
				if (newx >= 0 && newx <= Details.Width - 1 && newy >= 0 && newy <= Details.Height - 1)
				{
					MatrixLayers[CurrentLayer]->Cells[toframe]->Grid[newy * Details.Width + newx] = MatrixBackup->Grid[y * Details.Width + x];
				}

			default:
				if (MatrixBackup->Grid[y * Details.Width + x] > 0)
				{
					if ((newx >= 0) and (newx <= Details.Width - 1) and (newy >= 0) and (newy <= Details.Height - 1))
					{
						MatrixLayers[CurrentLayer]->Cells[toframe]->Grid[newy * Details.Width + newx] = MatrixBackup->Grid[y * Details.Width + x];
					}
				}
			}
		}
	}
}


void TheMatrix::ScrollRow(int layer, int frame, int mode, int row)
{
	if (IsThisFrameLocked(layer, frame) || !MatrixLayers[layer]->Visible) return;

	switch (mode)
	{
	case modeScrollRowLeft:
	{
		int pixel = MatrixLayers[layer]->Cells[frame]->Grid[row * Details.Width];

		for (int x = 0; x <= Details.Width - 2; x++)
		{
			MatrixLayers[layer]->Cells[frame]->Grid[row * Details.Width + x] = MatrixLayers[layer]->Cells[frame]->Grid[row * Details.Width + (x + 1)];
		}

		MatrixLayers[layer]->Cells[frame]->Grid[row * Details.Width + (Details.Width - 1)] = pixel;
		break;
	}
	case modeScrollRowRight:
	{
		int pixel = MatrixLayers[layer]->Cells[frame]->Grid[row * Details.Width + (Details.Width - 1)];

		for (int x = Details.Width - 1; x >= 1; x--)
		{
			MatrixLayers[layer]->Cells[frame]->Grid[row * Details.Width + x] = MatrixLayers[layer]->Cells[frame]->Grid[row * Details.Width + (x - 1)];
		}

		MatrixLayers[layer]->Cells[frame]->Grid[row * Details.Width] = pixel;
		break;
	}
	}
}


void TheMatrix::ScrollColumn(int layer, int frame, int mode, int column)
{
	if (IsThisFrameLocked(layer, frame) || !MatrixLayers[layer]->Visible) return;

	switch (mode)
	{
	case modeScrollColumnUp:
	{
		int pixel = MatrixLayers[layer]->Cells[frame]->Grid[column];

		for (int y = 0; y < Details.Height; y++)
		{
			MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + column] = MatrixLayers[layer]->Cells[frame]->Grid[(y + 1) * Details.Width + column];
		}

		MatrixLayers[layer]->Cells[frame]->Grid[(Details.Height - 1) * Details.Width + column] = pixel;
		break;
	}
	case modeScrollColumnDown:
	{
		int pixel = MatrixLayers[layer]->Cells[frame]->Grid[(Details.Height - 1) + Details.Width + column];

		for (int y = Details.Height - 1; y >= 1; y--)
		{
			MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + column] = MatrixLayers[layer]->Cells[frame]->Grid[(y - 1) * Details.Width + column];
		}

		MatrixLayers[layer]->Cells[frame]->Grid[column * Details.Width] = pixel;
		break;
	}
	}
}
#pragma end_region


#pragma region ReadOnlyProperties
// value should be the same for all layers, so just return those of layer 0
int TheMatrix::GetFrameCount()
{
	return MatrixLayers[CPermanentLayer]->Cells.size();
}


bool TheMatrix::GetDeadPixelsMode()
{
	return DeadPixelsMode;
}


SoftwareMode TheMatrix::GetSoftwareMode()
{
	return Software;
}


int TheMatrix::GetAutoPixelSize(int canvas_width, int canvas_height, int gradient)
{
	if (Details.Available)
	{
		int xc = canvas_width - 70;
		int yc = canvas_height - 14;

		int pxc = 10;
		int pyc = 10;

		switch (gradient)
		{
		case 0:
			pxc = std::floor((double)xc / (double)Details.Width);
			pyc = std::floor((double)yc / (double)Details.Height);
			break;
		case 1:
			pxc = std::floor(xc / (Details.Width + 2));
			pyc = std::floor(yc / Details.Height);
			break;
		case 2:
			pxc = std::floor(xc / Details.Width);
			pyc = std::floor(yc / (Details.Height + 2));
			break;
		}

		return std::min(pxc, pyc);
	}

	return 1;
}


int TheMatrix::GetCurrentFrame()
{
	return CurrentFrame;
}


int TheMatrix::GetCurrentLayer()
{
	return CurrentLayer;
}


bool TheMatrix::GetPreviewActive()
{
	return Preview.Active;
}


int TheMatrix::GetPreviewBoxSize()
{
	return Preview.Size;
}


bool TheMatrix::GetPreviewIncRadially()
{
	return Preview.IncrementRadially;
}


ViewShape TheMatrix::GetPreviewView()
{
	return Preview.View;
}


int TheMatrix::GetPreviewVoid()
{
	return Preview.ROffset;
}


bool TheMatrix::GetPreviewPopout()
{
	return PreviewPopout;
}


int TheMatrix::GetRadialOffset()
{
    return RadialOffset;
}


int TheMatrix::GetRadialOffsetDirection()
{
	return RadialOffsetDirection;
}
#pragma end_region


#pragma region WriteProperties
void TheMatrix::SetYPos(int newypos)
{
	PaintBox->Top = newypos;
	PreviewBox->Top = newypos;

	PaintBox->Invalidate();
}


void TheMatrix::SetMouseButtonColours(int LMB, int MMB, int RMB)
{
	SelectionLMB = LMB;
	SelectionMMB = MMB;
	SelectionRMB = RMB;
}


void TheMatrix::SetAutomateMode(bool mode)
{
	AutomateMode = mode;
}


void TheMatrix::SetFontWrap(bool mode)
{
	FontWrap = mode;
}


void TheMatrix::SetRandomCoeff(int coeff)
{
	RandomCoeff = coeff;
}


void TheMatrix::SetBackgroundColour(int newcolour)
{
	CanvasBackground = newcolour;

	PaintBox->Canvas->Brush->Color = TColor(newcolour);
	PaintBox->Canvas->FillRect(Rect(0, 0, PaintBox->Width, PaintBox->Height));

	PreviewBox->Canvas->Brush->Color = TColor(newcolour);
	PreviewBox->Canvas->FillRect(Rect(0, 0, PaintBox->Width, PaintBox->Height));

	PaintBox->Invalidate();
}


void TheMatrix::ChangePixelSize(int newpixelsize)
{
	Render.PixelSize = newpixelsize;

	PaintBox->Width  = Details.Width * Render.PixelSize;
	PaintBox->Height = Details.Height * Render.PixelSize;

	if (Details.Grid)
	{
		Render.PixelSizeZ = Render.PixelSize - 1;
	}
	else
	{
		Render.PixelSizeZ = Render.PixelSize;
	}

	if (Preview.Active)
	{
		if (PreviewPopout)
		{
			PreviewBox->Left = 0;
		}
		else
		{
			PreviewBox->Left = CLeftOffset + (Render.PixelSize * (Details.Width)) + 20;
		}
	}

	// ===========================================================================

	ChangeZoomUI(newpixelsize);

	// ===========================================================================

	PaintBox->Invalidate();
}


void TheMatrix::ChangePixelShape(PixelShape newpixelshape)
{
	Render.Shape = newpixelshape;

	PaintBox->Invalidate();
}


void TheMatrix::ChangeMatrixMode(MatrixMode newmatrixnode)       // to do
{
	if (Details.Width != -1)
	{
		Details.Mode = newmatrixnode;

		ConfigurePaintboxDrawing();
	}

	// if we're moving to single colour matrix
	// make sure the matrix data fits!

	if (newmatrixnode == MatrixMode::kMono)
	{
		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			for (int frame = 0; frame < MatrixLayers[layer]->Cells.size(); frame++)
			{
				for (int x = 0; x < Details.Width; x++)
				{
					for (int y = 0; y < Details.Height; y++)
					{
						if (MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] > 0) // TO DO
						{
							MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x] = 1;
						}
					}
				}
			}
		}
	}

	// ===========================================================================

	if (Details.Mode == MatrixMode::kRGB)
	{
		Render.Gradient.Clear(RGBBackground);
	}
	else
	{
		Render.Gradient.Clear(0);
	}

	// ===========================================================================

	PaintBox->Invalidate();
}


void TheMatrix::SetSoftwareMode(SoftwareMode softwaremode)
{
	switch (softwaremode)
	{
	case SoftwareMode::kAnimation:
	{
		ClearAllMatrixData(false);
		break;
	}
	case SoftwareMode::kFont:
	{
		ClearAllMatrixData(false);

		for (int t = 0; t < 96; t++)
		{
			Matrix *m = new Matrix(__MaxWidth, __MaxHeight, Details.Mode, RGBBackground); // TO DO

			MatrixLayers[CPermanentLayer]->Cells.push_back(m);
		}

		break;
	}
	}

	Software = softwaremode;

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::SetRadialOffset(int radialoffset)
{
	RadialOffset = radialoffset;

	if (RadialOffsetDirection)
	{
		RadialOffsetDegrees = -RadialOffset;
	}
	else
	{
		RadialOffsetDegrees = RadialOffset;
	}

	PreviewBox->Invalidate();
}


void TheMatrix::SetRadialOffsetDirection(bool direction)
{
	RadialOffsetDirection = direction;

	if (RadialOffsetDirection)
	{
		RadialOffsetDegrees = -RadialOffset;
	}
	else
	{
		RadialOffsetDegrees = RadialOffset;
	}

	PreviewBox->Invalidate();
}


void TheMatrix::SetShapeParameter(int parameter)
{
	Render.Draw.Parameter = parameter;

	PaintBox->Invalidate();
}


void TheMatrix::SetMirrorMode(MirrorMode newmode)
{
	Mirror = newmode;
}


void TheMatrix::SetCurrentFrame(int frame)
{
	CurrentFrame = frame;

//	  PaintBox->Invalidate();

	CopyCurrentFrameToDrawBuffer();

	//if (OnNewFrameDisplayed) OnNewFrameDisplayed(this);      // to do, interferes with trackbar selection!!!
}


void TheMatrix::SetCurrentLayer(int layer)
{
	CopyDrawBufferToCurrentFrame();

	CurrentLayer = layer;

	CopyCurrentFrameToDrawBuffer();
}


void TheMatrix::SetLightBox(int lightboxmode)
{
	LightBox = lightboxmode;

	PaintBox->Invalidate();
}


void TheMatrix::ChangeGrid(bool grid)
{
	Details.Grid = grid;

	if (Details.Grid)
	{
		Render.PixelSizeZ = Render.PixelSize - 1;
	}
	else
	{
		Render.PixelSizeZ = Render.PixelSize;
	}
}


void TheMatrix::SetDeadPixelsMode(bool mode)
{
	DeadPixelsMode = mode;

	ConfigurePaintboxDrawing();
}


void TheMatrix::SetMatrixReadOnly(bool mode)
{
	MatrixReadOnly = mode;

	ConfigurePaintboxDrawing();
}
#pragma end_region


#pragma region Automation
void TheMatrix::AutomationPostProcessExecute(ActionObject &ao, int actionId)
{
	if (ao.Layer < 0 || ao.Layer > MatrixLayers.size() - 1) return;

	switch (actionId)
	{
		  // == colour cycling =================================================

	case 27:	// linear
	{
		int index = ao.CCTargetIndex;

		for (int x = 0; x < ao.SourceColours.size(); x++)
		{
			MatrixLayers[ao.Layer]->Cells[CurrentFrame]->ChangePixels(ao.SourceColours[x],
																	   ao.TargetColours[index]);

			if (index == ao.TargetColours.size() - 1)
			{
				index = 0;
			}
			else
			{
				index++;
			}
		}

		if (ao.TargetSkipIndex == 0)
		{
			if (ao.CCTargetIndex == ao.TargetColours.size() - 1)
			{
				ao.CCTargetIndex = 0;
			}
			else
			{
				ao.CCTargetIndex++;
			}
		}
		break;
	 }

	case 28:	// bounceybouncey
	{
		int index = ao.CCTargetIndex;
		CyclingDirection direction = ao.CCDirection;

		for (int x = 0; x < ao.SourceColours.size(); x++)
		{
			MatrixLayers[ao.Layer]->Cells[CurrentFrame]->ChangePixels(ao.SourceColours[x],
																	   ao.TargetColours[index]);
			if (direction == CyclingDirection::kForwards)
			{
				if (index == ao.TargetColours.size() - 1)
				{
					index = ao.TargetColours.size() - 2;

					direction = CyclingDirection::kBackwards;
				}
				else
				{
					index++;
				}
			}
			else
			{
				if (index == 0)
				{
					index = 1;

					direction = CyclingDirection::kForwards;
				}
				else
				{
					index--;
				}
			}
		}

		if (ao.TargetSkipIndex == 0)
		{
			if (ao.CCDirection == CyclingDirection::kForwards)
			{
				if (ao.CCTargetIndex == ao.TargetColours.size() - 1)
				{
					ao.CCTargetIndex = ao.TargetColours.size() - 2;

					ao.CCDirection = CyclingDirection::kBackwards;
				}
				else
				{
					ao.CCTargetIndex++;
				}
			}
			else
			{
				if (ao.CCTargetIndex == 0)
				{
					ao.CCTargetIndex = 1;

					ao.CCDirection = CyclingDirection::kForwards;
				}
				else
				{
					ao.CCTargetIndex--;
				}
			}
		}
		break;
	}

//	default:
//	MessageDlg('Error: unknown action ID "' + IntToStr(aActionID) + '".', mtError, [mbOK], 0);
	}
}


void TheMatrix::AutomationActionExecute(ActionObject &ao, int actionId)
{
	switch (actionId)
	{
	case 0:
		PerformEffect(modeMirror, ao.Layer, CurrentFrame);
		break;
	case 1:
		PerformEffect(modeFlip,   ao.Layer, CurrentFrame);
		break;
	case 2:
		PerformEffect(modeInvert, ao.Layer, CurrentFrame);
		break;

	case 3:
		PerformScroll(modeScrollLeft,  ao.Layer, CurrentFrame);
		break;
	case 4:
		PerformScroll(modeScrollRight, ao.Layer, CurrentFrame);
		break;
	case 5:
		PerformScroll(modeScrollUp,    ao.Layer, CurrentFrame);
		break;
	case 6:
		PerformScroll(modeScrollDown,  ao.Layer, CurrentFrame);
		break;

	case 7:
		RotateFrame(modeRotateACW, ao.Layer, CurrentFrame);
		break;
	case 8:
		RotateFrame(modeRotateCW,  ao.Layer, CurrentFrame);
		break;

	case 9:
		PerformWipeOnCurrentFrame(modeWipeVerticalOut,   ao.EraseBehind);
		break;
	case 10:
		PerformWipeOnCurrentFrame(modeWipeVerticalIn,    ao.EraseBehind);
		break;
	case 11:
		PerformWipeOnCurrentFrame(modeWipeHorizontalOut, ao.EraseBehind);
		break;
	case 12:
		PerformWipeOnCurrentFrame(modeWipeHorizontalIn,  ao.EraseBehind);
		break;

	case 36:
		PerformWipeOnCurrentFrame(modeWipeLeftToRight,   ao.EraseBehind);
		break;
	case 37:
		PerformWipeOnCurrentFrame(modeWipeRightToLeft,   ao.EraseBehind);
		break;
	case 38:
		PerformWipeOnCurrentFrame(modeWipeUpToDown,      ao.EraseBehind);
		break;
	case 39:
		PerformWipeOnCurrentFrame(modeWipeDownToUp,      ao.EraseBehind);
		break;

	case 13 :
		if (ao.ProcesingStage < Details.Height)
		{
			for (int x = 0; x <= ao.ProcesingStage % Details.Height; x++)
			{
				PerformRowScrollOnCurrentFrame(modeScrollLeft, x, ao.EraseBehind);
			}
		}
		else
		{
			for (int x = 0; x < Details.Height; x++)
			{
				PerformRowScrollOnCurrentFrame(modeScrollLeft, x, ao.EraseBehind);
			}
		}
		break;

	case 14:
		if (ao.ProcesingStage < Details.Height)
		{
			for (int x = 0; x <= ao.ProcesingStage % Details.Height; x++)
			{
				PerformRowScrollOnCurrentFrame(modeScrollRight, x, ao.EraseBehind);
			}
		}
		else
		{
			for (int x = 0; x < Details.Height; x++)
			{
				PerformRowScrollOnCurrentFrame(modeScrollRight, x, ao.EraseBehind);
			}
		}
		break;

	case 15:
		if (ao.ProcesingStage < Details.Width)
		{
			for (int x = 0; x <= ao.ProcesingStage % Details.Width; x++)
			{
				PerformColumnScrollOnCurrentFrame(modeScrollUp, x, ao.EraseBehind);
			}
		}
		else
		{
			for (int x = 0; x < Details.Width; x++)
			{
				PerformColumnScrollOnCurrentFrame(modeScrollUp, x, ao.EraseBehind);
			}
		}
		break;

	case 16:
		if (ao.ProcesingStage < Details.Width)
		{
			for (int x = 0; x <= ao.ProcesingStage % Details.Width; x++)
			{
				PerformColumnScrollOnCurrentFrame(modeScrollDown, x, ao.EraseBehind);
			}
		}
		else
		{
			for (int x = 0; x < Details.Width; x++)
			{
				PerformColumnScrollOnCurrentFrame(modeScrollDown, x, ao.EraseBehind);
			}
		}
		break;

		  // parameter1 is scroll count
		  // parameter2 is direction: 0 = right, 1 = left
	case 17 :
		if (ao.Parameter2 == 0)
		{
			PerformScroll(modeScrollRight, ao.Layer, CurrentFrame);
		}
		else
		{
			PerformScroll(modeScrollLeft, ao.Layer, CurrentFrame);
		}

		if (ao.Parameter1 == Details.Width - 1)
		{
			ao.Parameter1 = 0;

			if (ao.Parameter2 == 0)
			{
				ao.Parameter2 = 1;
			}
			else
			{
				ao.Parameter2 = 0;
			}
		}
		else
		{
			ao.Parameter1++;
		}
		break;

		  // parameter1 is scroll count
		  // parameter2 is direction: 0 = right, 1 = left
	case 18:
		if (ao.Parameter2 == 0)
		{
			PerformScroll(modeScrollUp, ao.Layer, CurrentFrame);
		}
		else
		{
			PerformScroll(modeScrollDown, ao.Layer, CurrentFrame);
		}

		if (ao.Parameter1 == Details.Width - 1)
		{
			ao.Parameter1 = 0;

			if (ao.Parameter2 == 0)
			{
				ao.Parameter2 = 1;
			}
			else
			{
				ao.Parameter2 = 0;
			}
		}
		else
		{
			ao.Parameter1++;
		}
		break;

		  // == paste brush in to every frame ==================================
	 case 19:
		for (int x = 0; x < ao.Brushes[0].BrushData.size(); x++)
		{
			StringToRow(False, ao.Brushes[0].BrushData[x], CurrentFrame, x,
						  ao.Brushes[0].TransparentColour,
						  ao.Brushes[0].Transparent);
		}
		break;

	case 20:
		if (CurrentFrame == ao.FrameStart)
		{
			for (int x = 0; x < ao.Brushes[0].BrushData.size(); x++)
			{
				StringToRow(False, ao.Brushes[0].BrushData[x], CurrentFrame, x,
							 ao.Brushes[0].TransparentColour,
							 ao.Brushes[0].Transparent);
			}
		}
		break;

	case 21:
		for (int x = 0; x < ao.Brushes[1].BrushData.size(); x++)
		{
			StringToRow(False, ao.Brushes[1].BrushData[x], CurrentFrame, x,
						  ao.Brushes[1].TransparentColour,
						  ao.Brushes[1].Transparent);
		}
		break;

	case 22:
		if (CurrentFrame == ao.FrameStart)
		{
			for (int x = 0; x < ao.Brushes[1].BrushData.size(); x++)
			{
				StringToRow(False, ao.Brushes[1].BrushData[x], CurrentFrame, x,
					 ao.Brushes[1].TransparentColour,
					 ao.Brushes[1].Transparent);
			}
		}
		break;

          // == split scroll
	case 23:
		PerformSplitScroll(modeSplitScrollLeftRight, ao.Layer, CurrentFrame);
		break;
	case 24:
		PerformSplitScroll(modeSplitScrollRightLeft, ao.Layer, CurrentFrame);
		break;
	case 25:
		PerformSplitScroll(modeSplitScrollUpDown,    ao.Layer, CurrentFrame);
		break;
	case 26:
		PerformSplitScroll(modeSplitScrollDownUp,    ao.Layer, CurrentFrame);
		break;

		  // alternate scrolls
	case 29:
		PerformAlternateScroll(modeAlternateScrollUpDown, ao.Layer, CurrentFrame);
		break;

	case 30:
		PerformRevealOnCurrentFrame(modeRevealLeftRight, ao.ParameterRevealColour, ao.ParameterReveal);
		break;
	case 31:
		PerformRevealOnCurrentFrame(modeRevealRightLeft, ao.ParameterRevealColour, ao.ParameterReveal);
		break;
		break;
	case 32:
		PerformRevealOnCurrentFrame(modeRevealTopBottom, ao.ParameterRevealColour, ao.ParameterReveal);
		break;
		break;
	case 33:
		PerformRevealOnCurrentFrame(modeRevealBottomTop, ao.ParameterRevealColour, ao.ParameterReveal);
		break;
	case 34:
		PerformRevealOnCurrentFrame(modeRevealCentreIn,  ao.ParameterRevealColour, ao.ParameterReveal);
		break;
	case 35:
		PerformRevealOnCurrentFrame(modeRevealCentreOut, ao.ParameterRevealColour, ao.ParameterReveal);
		break;

//	default:
//    MessageDlg('Error: unknown action ID "' + IntToStr(aActionID) + '".', mtError, [mbOK], 0);
	}
}


void TheMatrix::Automate(ActionObject &ao)
{
	AutomateMode = true;

	// ===========================================================================

	ao.ProcesingStage = 0;
	ao.CCSourceIndex  = 0;
	ao.CCTargetIndex  = 0;
	ao.CCDirection    = CyclingDirection::kForwards;
	int lIterationCount    = 1;

	// ===========================================================================

	int lOldLayer     = CurrentLayer;
	int action = 0;

	CurrentLayer = ao.Layer;

	// ===========================================================================

	Busy = true;

	switch (ao.Source)
	{
	case AutomateSource::kFirstFrame:	// first frame is source
		CurrentFrame = ao.FrameStart;

		for (int a = 0; a < ao.ActionList.size(); a++)
		{
			action = ao.ActionList[a];

			if (action >= 19 || action <= 22)
			{
				AutomationActionExecute(ao, action);
			}
		}

		for (int frame = ao.FrameStart + 1; frame <= ao.FrameEnd; frame++)
		{

			if (frame > GetFrameCount())
			{
				InsertBlankFrameAt(frame);
			}

			CopyLayerFromTo(ao.Layer, ao.Layer, frame - 1, frame);

			CurrentFrame = frame;

			for (int a = 0; a < ao.ActionList.size(); a++)
			{
				action = ao.ActionList[a];

				AutomationActionExecute(ao, action);
			}

			ao.ProcesingStage++;
		}
		break;
	case AutomateSource::kEachFrame:	// previous frame is source
		for (int frame = ao.FrameStart; frame <= ao.FrameEnd; frame++)
		{
			if (frame > GetFrameCount())
			{
				InsertBlankFrameAt(frame);
			}

			CurrentFrame = frame;

			for (int a = 0; a < ao.ActionList.size(); a++)
			{
				action = ao.ActionList[a];

				AutomationActionExecute(ao, action);
			}

			ao.ProcesingStage++;
		}
		break;
	case AutomateSource::kEachFrameInc:
		for (int frame = ao.FrameStart; frame <= ao.FrameEnd; frame++)
		{
			if (frame > GetFrameCount())
			{
				InsertBlankFrameAt(frame);
			}

			CurrentFrame = frame;

			for (int a = 0; a < ao.ActionList.size(); a++)
			{
				action = ao.ActionList[a];
			}

			for (int i = 1; i <= lIterationCount; i++)
			{
				AutomationActionExecute(ao, action);
			}

			ao.ProcesingStage++;
			lIterationCount++;
		}
	}

	// ===========================================================================

	if (ao.PostProcessList.size() != 0)
	{
		ao.TargetSkipIndex = 0;

		for (int frame = ao.FrameStart; frame <= ao.FrameEnd; frame++)
		{
			CurrentFrame = frame;

			for (int a = 0; a < ao.PostProcessList.size(); a++)
			{
				action = ao.PostProcessList[a];
			}

			AutomationPostProcessExecute(ao, action);
		}

		ao.TargetSkipIndex++;

		if (ao.TargetSkipIndex > ao.TargetSkip)
		{
			ao.TargetSkipIndex = 0;
		}
	}

	// ===========================================================================

	CurrentLayer = lOldLayer;

	Busy = false;

	// ===========================================================================

	AutomateMode = false;
}
#pragma end_region


#pragma region CopyPaste
void TheMatrix::CopyCurrentFrame()
{
	for (int y = 0; y < Details.Height; y++)
	{
		for (int x = 0; x < Details.Width; x++)
		{
			MatrixCopy->Grid[y * Details.Width + x] = MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x];
		}
	}
}


void TheMatrix::CopyBackupToCopyFrame()
{
	for (int y = 0; y < Details.Height; y++)
	{
		for (int x = 0; x < Details.Width; x++)
		{
			MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + x];
		}
	}
}


void TheMatrix::PasteSpecial(int mode)
{
	PerformScrollOnCopyFrame(mode);

	PasteCurrentFrame();
}


void TheMatrix::PasteCurrentFrame()
{
	if (IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!MatrixLayers[CurrentLayer]->Visible) return;

	BackupMatrix(CurrentLayer, CurrentFrame);

	for (int y = 0; y < Details.Height; y++)
	{
		for (int x = 0; x < Details.Width; x++)
		{
			MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixCopy->Grid[y * Details.Width + x];
		}
	}

	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();

	CopyCurrentFrameToDrawBuffer();

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region Statistics
int TheMatrix::GetUndoCount()
{
	return MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->History.size();
}


int TheMatrix::GetTotalUndos()
{
	int total = 0;

	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		for (int frame = 0; frame < MatrixLayers[layer]->Cells.size(); frame++)
		{
			total += MatrixLayers[layer]->Cells[frame]->History.size();
		}
	}

	return total;
}


int TheMatrix::CalculateMemoryUsage()
{
	switch (Details.Mode)
	{
	case MatrixMode::kRGB:
		return Details.Width * Details.Height * 4 * GetFrameCount();             // 4 bytes per pixel
	case MatrixMode::kRGB3BPP:
		return std::ceil((Details.Width * Details.Height * 3 * GetFrameCount()) / 8); // 3 bits per pixel

	default:
		int a = 0;
		int b = 0;
		int total = 0;

		if (Details.Height >= Details.Width)
		{
			a = std::div(Details.Height + 1, 8).quot;
			b = (Details.Width);
		}
		else
		{
			a = std::div(Details.Width + 1, 8).quot;
			b = (Details.Height);
		}

		if (Software == SoftwareMode::kFont)
		{
			total = (a * b) * (FontCharacterCount);         // always 96 frames in font mode
		}
		else
		{
			total = (a * b) * (GetFrameCount());
		}

		// if using any of the bicolour modes then double requirements
		if (Details.Mode > MatrixMode::kMono)
		{
			total *= 2;
		}

		return total;
	}
}


int TheMatrix::DataSizeBytes()
{
	switch (Details.Mode)
	{
	case MatrixMode::kRGB:
		return 4;
	case MatrixMode::kRGB3BPP:
		return std::ceil(Details.Height / 8) * 3;

	default:
		if (Details.Height >= 0 && Details.Height <= 8)
			return 1;
		else if (Details.Height >= 9 && Details.Height <= 16)
			return 2;
		else if (Details.Height >= 17 && Details.Height <= 32)
			return 4;
		else if (Details.Height >= 33 && Details.Height <= 64)
			return 8;
		else
			return 0;
	}
}
#pragma end_region


#pragma region UIScrollbars
// configures the render engine for scrolling the view window
void TheMatrix::ChangeZoomUI(int pixelsize)
{
	TPanel *panel = (TPanel*)Canvas;

	int containerwidth  = panel->Width - 95 - 25;
	int containerheight = panel->Height - 40;

	if (pixelsize * Details.Width > containerwidth)
	{
		if (!ScrollHorizontal->Visible)
		{
			ScrollHorizontal->Visible = true;
		}

		Render.ViewWindow.X        = std::floor(containerwidth / pixelsize);

		Render.TopLeft.X           = 0;
		Render.BottomRight.X       = Render.TopLeft.X + Render.ViewWindow.X - 1;

		ScrollHorizontal->Max      = Details.Width - Render.ViewWindow.X - 1;
		ScrollHorizontal->Position = 0;
	}
	else
	{
		if (ScrollHorizontal->Visible)
		{
			ScrollHorizontal->Visible = false;
		}
	}

	if (pixelsize * Details.Height > containerheight)
	{
		if (!ScrollVertical->Visible)
		{
			ScrollVertical->Visible = true;
		}

		Render.ViewWindow.Y      = std::floor(containerheight / pixelsize);

		Render.TopLeft.Y         = 0;
		Render.BottomRight.Y     = Render.TopLeft.Y + Render.ViewWindow.Y - 1;

		ScrollVertical->Max      = Details.Height - Render.ViewWindow.Y - 1;
		ScrollVertical->Position = 0;
	}
	else
	{
		if (ScrollVertical->Visible)
		{
			ScrollVertical->Visible = false;
		}
	}
}

void __fastcall TheMatrix::ScrollBarHorizontalChange(TObject *Sender)
{
	Render.TopLeft.X = ScrollHorizontal->Position;
	Render.BottomRight.X = Render.TopLeft.X + Render.ViewWindow.X - 1;

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::ScrollBarVerticalChange(TObject *Sender)
{
  Render.TopLeft.Y = ScrollVertical->Position;
  Render.BottomRight.Y = Render.TopLeft.Y + Render.ViewWindow.Y - 1;

  PaintBox->Invalidate();
}
#pragma end_region


#pragma region ColourCounting
int TheMatrix::CountColoursFrame()
{
	std::vector<int> Colours;

	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		for (int x = 0; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				if (std::find(Colours.begin(), Colours.end(), MatrixLayers[layer]->Cells[CurrentFrame]->Grid[y * Details.Width + x]) == Colours.end())
				{
					Colours.push_back(MatrixLayers[layer]->Cells[CurrentFrame]->Grid[y * Details.Width + x]);
				}
			}
		}
	}

	return Colours.size();
}


int TheMatrix::CountColoursAnimation()
{
	std::vector<int> Colours;

	for (int frame = 0; frame < GetFrameCount(); frame++)
	{
		for (int layer = 0; layer < MatrixLayers.size(); layer++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				for (int y = 0; y < Details.Height; y++)
				{
					if (std::find(Colours.begin(), Colours.end(), MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x]) == Colours.end())
					{
						Colours.push_back(MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x]);
					}
				}
			}
		}
	}

	return Colours.size();
}


void TheMatrix::GetFirst32Colours(std::vector<int> &colour_list)
{
	for (int layer = 0; layer < MatrixLayers.size(); layer++)
	{
		for (int x = 0; x < Details.Width; x++)
		{
			for (int y = 0; y < Details.Height; y++)
			{
				if (std::find(colour_list.begin(), colour_list.end(), MatrixLayers[layer]->Cells[CurrentFrame]->Grid[y * Details.Width + x]) == colour_list.end())
				{
					colour_list.push_back(MatrixLayers[layer]->Cells[CurrentFrame]->Grid[y * Details.Width + x]);

					if (colour_list.size() >= 32) return;
				}
			}
		}
	}
}
#pragma end_region


#pragma region UserBuffers
void TheMatrix::ClearUserBuffers()
{
	for (int i = 0; i < 10; i++)
	{
		MatrixUser[i]->Clear(Details.Mode, RGBBackground);
	}
}


void TheMatrix::CopyToUserBuffer(int frame)
{
	for (int y = 0; y < Details.Height; y++)
	{
		for (int x = 0; x < Details.Width; x++)
		{
			MatrixUser[frame]->Grid[y * Details.Width + x] = MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x];
		}
	}
}


void TheMatrix::RestoreFromUserBuffer(int frame)
{
	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			if (Details.Mode == MatrixMode::kRGB)
			{
				MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = MatrixUser[frame]->Grid[y * Details.Width + x];
			}
			else
			{
				if (MatrixUser[frame]->Grid[y * Details.Width + x] == 1)
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = 1;
				}
				else
				{
					MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x] = 0;
				}
			}
		}
	}

	CopyCurrentFrameToDrawBuffer();

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region UndoRedo
void TheMatrix::Undo()
{
	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Undo();

	CopyCurrentFrameToDrawBuffer();

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::Redo()
{
	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->Redo();

	CopyCurrentFrameToDrawBuffer();

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::SetFromUndo(int undo)
{
	MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->SetFromUndo(undo);

	CopyCurrentFrameToDrawBuffer();

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


bool TheMatrix::CanUndo()
{
	return MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->HistoryOffset != 0;
}


bool TheMatrix::CanRedo()
{
	return MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->HistoryOffset != MatrixLayers[CurrentLayer]->Cells[CurrentFrame]->History.size() - 1;
}
#pragma end_region


void TheMatrix::BackupMatrix(int layer, int frame)
{
	if (frame >= 0)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixBackup->Grid[y * Details.Width + x] = MatrixLayers[layer]->Cells[frame]->Grid[y * Details.Width + x];
			}
		}
	}
	else
	{
		for (int y = 0; y < Details.Height; y++)
		{
			for (int x = 0; x < Details.Width; x++)
			{
				MatrixBackup->Grid[y * Details.Width + x] = MatrixCopy->Grid[y * Details.Width + x];
			}
		}
	}
}


void TheMatrix::BackupMatrix()
{
	BackupMatrix(CurrentLayer, CurrentFrame);
}


void TheMatrix::ClearGradient()
{
	Gradient.clear();
}


void TheMatrix::AddGradient(int colour)
{
	Gradient.push_back(colour);
}


void TheMatrix::CreateMatrixMerge()
{
	// not sure if this is necessary
}

void TheMatrix::FreeMatrixMerge()
{
	// not sure if this is necessary
}
