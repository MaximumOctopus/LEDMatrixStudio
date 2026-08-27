// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2026
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

#include "AutomationConstants.h"
#include "CalcUtility.h"
#include "ColourUtility.h"
#include "Convert.h"
#include "FileUtility.h"
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

	MatrixBackup = new MatrixGrid(__MaxWidth, __MaxHeight, Details.ColourMode, RGBBackground);
	MatrixCopy = new MatrixGrid(__MaxWidth, __MaxHeight, Details.ColourMode, RGBBackground);
	MatrixRender = new MatrixGrid(__MaxWidth, __MaxHeight, Details.ColourMode, RGBBackground);
	DisplayBuffer = new MatrixGrid(__MaxWidth, __MaxHeight, Details.ColourMode, RGBBackground);
	MatrixMerge = new MatrixGrid(__MaxWidth, __MaxHeight, Details.ColourMode, RGBBackground);
	MatrixIgnoredLayout = new MatrixIgnored(__MaxWidth, __MaxHeight);

	// ===========================================================================

	Data = new LayerHandler(GLanguageHandler->Text[kBottomLayer]);

	// ===========================================================================

	ScrollHorizontal = new TScrollBar(Owner);
	ScrollHorizontal->Parent   = Canvas;
	ScrollHorizontal->Align    = alBottom;
	ScrollHorizontal->Kind     = sbHorizontal;
	ScrollHorizontal->Name     = "FSH";
	ScrollHorizontal->Min      = 0;
	ScrollHorizontal->OnChange = ScrollBarHorizontalChange;
	ScrollHorizontal->Visible  = false;

	ScrollVertical = new TScrollBar(Owner);
	ScrollVertical->Parent   = Canvas;
	ScrollVertical->Align    = alRight;
	ScrollVertical->Kind     = sbVertical;
	ScrollVertical->Name     = "FSV";
	ScrollVertical->Min      = 0;
	ScrollVertical->OnChange = ScrollBarVerticalChange;
	ScrollVertical->Visible  = false;

	// ===========================================================================

	for (int x = 0; x < 10; x++)
	{
		MatrixGrid *m = new MatrixGrid(__MaxWidth, __MaxHeight, Details.ColourMode, RGBBackground);   // user buffers

		MatrixUser.push_back(m);
	}

	// ===========================================================================

	//PaintBox->OnMouseDown = ClickPixel;
	//PaintBox->OnMouseMove = Shape1MouseMove;
	//PaintBox->OnMouseUp   = Shape1MouseUp;

	// ===========================================================================

	CurrentFrame = 0;
	CurrentLayer = 0;
    CurrentPixel = -1;

	ClearAllMatrixData(false, 0, 0);
}


TheMatrix::~TheMatrix()
{
	for (int t = 0; t < 10; t++)
	{
		delete MatrixUser[t];
	}

    MatrixUser.clear();

	delete TextFont;
	delete DisplayBuffer;

	delete PaintBox;

	delete PreviewBox;

	delete TextFont;

	delete ScrollHorizontal;
	delete ScrollVertical;

    delete Data;
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


void TheMatrix::NewMatrix(MatrixDrawMode drawmode, MatrixColourMode colourmode,
						  int framecount,
                          int top, int left, int width, int height, int pixelsize,
						  PixelShape pixelshape, bool grid, bool readonly, bool clearall,
						  int backgroundcolour)
{
	CurrentFrame = 0;
	LightBox = 0;
	IgnoredPixelsMode = false;
    HideIgnoredPixels = true;

	AnimPlaying = false;

	LastX = -1;
	LastY = -1;

	PaintBox->Top = top;
	PaintBox->Left = left;

	if (drawmode == MatrixDrawMode::kGrid)
	{
		PaintBox->Width = width * pixelsize;
		PaintBox->Height = height * pixelsize;
	}
	else
	{
		PaintBox->Width = Canvas->Width;
		PaintBox->Height = Canvas->Height;
	}

	PreviewBox->Top = top;

	Details.SetNew(width, height, drawmode, colourmode);
	Render.SetNew(pixelshape, width, height);

	BrushSize Brush = BrushSize::kSmall;

	MatrixReadOnly = readonly;
	RGBBackground = backgroundcolour;

	Data->SetSystem(width, height, backgroundcolour, Software, Details.DrawMode, Details.ColourMode);

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
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
	}
	else
	{
		Render.PixelSize = 20;
        Render.PixelSizeZ = 19;
	}

	// =======================================================================

	if (clearall)
	{
		SetIgnoredPixels(PixelAlive);
	}

	// =======================================================================

	if (Details.ColourMode == MatrixColourMode::kRGB)
	{
		Render.Gradient.Clear(RGBBackground);
	}
	else
	{
		Render.Gradient.Clear(0);
	}

	// =======================================================================

	ConfigurePaintboxDrawing();

	// =======================================================================

	if (clearall)
	{
		ClearAllMatrixData(true, width, height);

		Details.Comment = L"";
	}

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		while (Data->Layers[kPermanentLayer]->Cells.size() < framecount)
		{
			InsertBlankFrameAt(0);
		}
	}
	else
	{
		while (Data->Layers[kPermanentLayer]->Freeform->Frames.size() < framecount)
		{
			InsertBlankFrameAt(0);
		}
	}

	Details.Available = true;

	SetPreviewBoxSize(Preview.Size);

	if (OnChange) OnChange(this);
}


void TheMatrix::Refresh()
{
	PaintBox->Invalidate();
}


#pragma region RenderingGrid
// merges all layers to a single layer. pixels "rain" down from top (highest index)
// to bottom (lowest index)
void TheMatrix::BuildMergedFrame(int frame, MergeFrameMode merge)
{
	MatrixMerge->Clear(Details.ColourMode, RGBBackground);

	for (int layer = 0; layer < Data->Layers.size(); layer++)
	{
		if (Data->Layers[layer]->Visible)
		{
			for (int z = 0; z < Details.Width * Details.Height; z++)
			{
				switch (Details.ColourMode)
				{
				case MatrixColourMode::kMono:
				case MatrixColourMode::kBiSequential:
				case MatrixColourMode::kBiBitplanes:
				{
					switch (Data->Layers[layer]->Cells[frame]->Grid[z])
					{
					case 0:
						break;
					case 1:
					case 2:
					case 3:
						switch (merge)
						{
						case MergeFrameMode::kRetainGridValue:
							MatrixMerge->Grid[z] = Data->Layers[layer]->Cells[frame]->Grid[z];
							break;
						case MergeFrameMode::kConvertForRender:
							MatrixMerge->Grid[z] = LEDColours[Data->Layers[layer]->Cells[frame]->Grid[z]];
							break;
						case MergeFrameMode::kConvertForFileOutput:
							switch (Data->Layers[layer]->Cells[frame]->Grid[z])
							{
							case 0:
								MatrixMerge->Grid[z] = 0x00000000;
								break;
							case 1:
								MatrixMerge->Grid[z] = 0x00ffffff;
								break;
							case 2:
								MatrixMerge->Grid[z] = LEDColours[kMouseMiddle];
								break;
							case 3:
								MatrixMerge->Grid[z] = LEDColours[kMouseRight];
								break;
							}
						}
						break;
					}
					break;
				}
				case MatrixColourMode::kRGB:
					if (Data->Layers[layer]->Cells[frame]->Grid[z] != RGBBackground)
					{
						MatrixMerge->Grid[z] = Data->Layers[layer]->Cells[frame]->Grid[z];
					}
					break;
				case MatrixColourMode::kRGB3BPP:
				{
					if (Data->Layers[layer]->Cells[frame]->Grid[z] != RGBBackground)
					{
						switch (merge)
						{
						case MergeFrameMode::kRetainGridValue:
							MatrixMerge->Grid[z] = Data->Layers[layer]->Cells[frame]->Grid[z];
							break;
						case MergeFrameMode::kConvertForRender:
						case MergeFrameMode::kConvertForFileOutput:
							MatrixMerge->Grid[z] = LEDRGB3BPPColours[Data->Layers[layer]->Cells[frame]->Grid[z]];
							break;
						}
					}
					break;
				}

				default:
					break;
				}
			}
		}
	}
}


void TheMatrix::CopyCurrentFrameToDrawBuffer()
{
	if (Busy) return;

	std::memcpy(DisplayBuffer->Grid, Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid, Details.Width * Details.Height * sizeof(int));
}


void TheMatrix::CopyDrawBufferToCurrentFrame()
{
	if (Busy) return;

	if (!Data->Layers[CurrentLayer]->Visible) return;

	std::memcpy(Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid,
				DisplayBuffer->Grid,
				Details.Width * Details.Height * sizeof(int));

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
	for (int l = 0; l < Data->Layers.size(); l++)
	{
		if (!Data->IsThisFrameLocked(l, CurrentFrame))
		{
			if (l == CurrentLayer)
			{
				DisplayBuffer->Clear(Details.ColourMode, RGBBackground);
			}
		}

		if (Details.DrawMode == MatrixDrawMode::kGrid)
		{
			Data->Layers[l]->Cells[CurrentFrame]->Clear(Details.ColourMode, RGBBackground);
			Data->Layers[l]->Cells[CurrentFrame]->AddToHistory();
		}
		else
		{
			Data->Layers[l]->Freeform->Clear(CurrentFrame, Details.ColourMode, RGBBackground);
			// to do add to history
		}
	}

	PaintBox->Invalidate();

	if (OnChange) OnChange(this);
}


void TheMatrix::ClearCurrentLayer()
{
	if (Data->IsThisFrameLocked(CurrentLayer, CurrentFrame)) return;

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		DisplayBuffer->Clear(Details.ColourMode, RGBBackground);

		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Clear(Details.ColourMode, RGBBackground);

		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();
	}
	else
	{
		Data->Layers[CurrentLayer]->Freeform->Clear(CurrentFrame, Details.ColourMode, RGBBackground);
		// add to do history
	}

	PaintBox->Invalidate();

	if (OnChange) OnChange(this);
}


void TheMatrix::ClearFrame(int frame)
{
	for (int layer = 0; layer < Data->Layers.size(); layer++)
	{
		if (!Data->IsThisFrameLocked(layer, frame))
		{
			if (layer == CurrentLayer && frame == CurrentFrame)
			{
				DisplayBuffer->Clear(Details.ColourMode, RGBBackground);
			}

			if (Details.DrawMode == MatrixDrawMode::kGrid)
			{
				Data->Layers[layer]->Cells[frame]->Clear(Details.ColourMode, RGBBackground);

				Data->Layers[layer]->Cells[frame]->AddToHistory();
			}
			else
			{
				Data->Layers[layer]->Freeform->Clear(frame, Details.ColourMode, RGBBackground);

				// to do Data->Layers[layer]->Cells[frame]->AddToHistory();
			}
		}
	}

	PaintBox->Invalidate();

	if (OnChange) OnChange(this);
}


void TheMatrix::ClearAllMatrixData(bool addfirstframe, int width, int height)
{
	DisplayBuffer->Clear(Details.ColourMode, RGBBackground);

	while (Data->Layers.size() > 1)
	{
		Data->Layers.pop_back();
	}

	Data->Layers[kPermanentLayer]->Cells.clear();
	if (Data->Layers[kPermanentLayer]->Freeform != nullptr)
	{
		Data->Layers[kPermanentLayer]->Freeform->ClearAll();
    }
	Data->Layers[kPermanentLayer]->Name = GLanguageHandler->Text[kBottomLayer];

	if (Data->Layers[kPermanentLayer]->Freeform == nullptr)
	{
		Data->Layers[kPermanentLayer]->Freeform = new FreeformHandler();
	}

	if (addfirstframe)
	{
		if (Details.DrawMode == MatrixDrawMode::kGrid)
		{
			MatrixGrid *m1 = new MatrixGrid(width, height, Details.ColourMode, RGBBackground);
			Data->Layers[kPermanentLayer]->Cells.push_back(m1);
		}
		else
		{
			FreeformFrame *mf = new FreeformFrame();
            Data->Layers[kPermanentLayer]->Freeform->Frames.push_back(mf);
		}
	}

	CurrentFrame = 0;
	CurrentLayer = 0;

   //	Data->Layers[kPermanentLayer]->Cells[1]->History.clear();
	//Data->Layers[kPermanentLayer]->Cells[1]->AddToHistory();

   //	Data->Layers[kPermanentLayer]->Cells[1]->Locked = false;

	if (OnChange) OnChange(this);

	if (OnLayerChange) OnLayerChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::RemoveAllPixels()
{
	Busy = true;

	for (int t = 0; t < Data->Layers.size(); t++)
	{
		Data->Layers[t]->Freeform->Pixels.clear();
	}

	Busy = false;

	PaintBox->Invalidate();
}


void TheMatrix::WipeAllFramesCurrentLayer()
{
	Data->WipeAllFrames(CurrentLayer);

	if (OnChange) OnChange(this);

    if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
	}

	PaintBox->Invalidate();
}


void TheMatrix::WipeAllFramesAllLayers()
{
    Data->WipeAllFramesAllLayers();

	if (OnChange) OnChange(this);

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
    }

	PaintBox->Invalidate();
}


void TheMatrix::ClearAllFramesGradient(int mode)
{
	if (!Details.Available) return;

	if (Details.DrawMode == MatrixDrawMode::kFreeform) return;

	Data->ClearAllFramesGradient(mode, CurrentLayer, Render.Gradient, LEDColours);

	if (OnChange) OnChange(this);

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
    }

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

	double c = (2 * 3.1415926535 * (double)ROffset);

	switch (Preview.View)
	{
	case ViewShape::kSquare:
	case ViewShape::kRadial:
	case ViewShape::kRadial3Q:
		pixel_size = std::round(c / Details.Width);
		break;
	case ViewShape::kSemiCircle:
	case ViewShape::kSemiCircleInverted:
		pixel_size = std::round(c / (2 * Details.Width));
		break;
	}

	if (pixel_size <= 1)
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

		if (Preview.Size <= 2)
		{
			Preview.DisplayShape = PixelShape::kSquare;
		}
		else
		{
			Preview.DisplayShape = Preview.Shape;
		}
	}

	if (!Details.Available) return;

	switch (Preview.View)
	{
	case ViewShape::kSquare:
		PreviewBox->OnPaint = pbPreviewPaint;
		break;
	case ViewShape::kRadial:
		PreviewBox->OnPaint = pbPreviewPaintRadial;

		Preview.RPixel = GetPreviewPixelSize(Preview.ROffset);
		break;
	case ViewShape::kRadial3Q:
		PreviewBox->OnPaint = pbPreviewPaintRadialThreeQuarters;

		Preview.RPixel = GetPreviewPixelSize(Preview.ROffset);
		break;
	case ViewShape::kSemiCircle:
		PreviewBox->OnPaint = pbPreviewPaintSemiCircle;

		Preview.RPixel = GetPreviewPixelSize(Preview.ROffset);
		break;
	case ViewShape::kSemiCircleInverted:
		PreviewBox->OnPaint = pbPreviewPaintSemiCircleInverted;

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
		PreviewBox->Left = kLeftOffset + (Render.PixelSize * (Details.Width)) + 20;
	}

	PreviewBox->Invalidate();
}


void TheMatrix::SetPreviewIncrementRadially(bool increment)
{
	Preview.IncrementRadially = increment;

	PreviewBox->Invalidate();
}


void TheMatrix::SetPreviewDrawing(bool candraw)
{
    Preview.CanDraw = candraw;
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
	for (int y = 0; y < Details.Height; y++)
	{
		int ydw = y * Details.Width;
		int yps = y * Preview.Size;

		for (int x = 0; x < Details.Width; x++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[ydw + x]);
			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			switch (Preview.DisplayShape)
			{
			case PixelShape::kSquare:
				PreviewBox->Canvas->FillRect(Rect(x * Preview.Size,
												  yps,
												 (x * Preview.Size) + Preview.Size,
												  yps + Preview.Size));
				break;
			case PixelShape::kCircle:
				PreviewBox->Canvas->Ellipse(x * Preview.Size,
											yps,
										   (x * Preview.Size) + Preview.Size,
											yps + Preview.Size);
				break;
			case PixelShape::kRoundRect:
				PreviewBox->Canvas->RoundRect(x * Preview.Size,
											  yps,
											 (x * Preview.Size) + Preview.Size,
											  yps + Preview.Size,
											  Preview.Size - (std::round(Preview.Size / kRoundRectCoeff)),
											  Preview.Size - (std::round(Preview.Size / kRoundRectCoeff)));
				break;
			}
		}
	}

	// ===========================================================================
	// ===========================================================================
	// ===========================================================================

	if (Render.Action.Mode != ActionMode::kNone)
	{
		if (Render.Action.Coords[0].X != - 1)
		{
			// need a preview version of draw shape
			//DrawShape(true, PreviewBox->Canvas, Preview.Size, Preview.Size, 1, false);

			// =======================================================================

			PreviewBox->Canvas->Brush->Color = TColor(LEDColours[kDisplayMarker]);

			switch (Preview.DisplayShape)
			{
			case PixelShape::kSquare:
				PreviewBox->Canvas->FillRect(Rect(Render.Action.Coords[0].X * Preview.Size,
												  Render.Action.Coords[0].Y * Preview.Size,
												 (Render.Action.Coords[0].X * Preview.Size) + Preview.Size,
												 (Render.Action.Coords[0].Y * Preview.Size) + Preview.Size));
				break;
			case PixelShape::kCircle:
				PreviewBox->Canvas->Ellipse(Render.Action.Coords[0].X * Preview.Size,
											Render.Action.Coords[0].Y * Preview.Size,
										   (Render.Action.Coords[0].X * Preview.Size) + Preview.Size,
										   (Render.Action.Coords[0].Y * Preview.Size) + Preview.Size);
				break;
			case PixelShape::kRoundRect:
				PreviewBox->Canvas->RoundRect(Render.Action.Coords[0].X * Preview.Size,
											  Render.Action.Coords[0].Y * Preview.Size,
											 (Render.Action.Coords[0].X * Preview.Size) + Preview.Size,
											 (Render.Action.Coords[0].Y * Preview.Size) + Preview.Size,
											  Preview.Size - (std::round(Preview.Size / kRoundRectCoeff)),
											  Preview.Size - (std::round(Preview.Size / kRoundRectCoeff)));
				break;
			}
		}
	}

	// =======================================================================
	// =======================================================================
	// =======================================================================

	if (Render.Action.CopyPos.X != 0)
	{
		for (int x = 0; x <= Render.Action.CopyPos.X; x++)
		{
			for (int y = 0; y <= Render.Action.CopyPos.Y; y++)
			{
				if (x + LastX >= 0 && x + LastX <= Details.Width &&
					y + LastY >= 0 && y + LastY <= Details.Height)
				{
					if (Details.ColourMode == MatrixColourMode::kRGB)
					{
						if (MatrixIgnoredLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
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
						if (MatrixIgnoredLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
						{
							PreviewBox->Canvas->Brush->Color = TColor(LEDColours[MatrixCopy->Grid[y * Details.Width + x]]);
						}
						else
						{
							PreviewBox->Canvas->Brush->Color = clBtnFace;
						}
					}

					switch (Preview.DisplayShape)
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
													  Preview.Size - (std::round(Preview.Size / kRoundRectCoeff)),
													  Preview.Size - (std::round(Preview.Size / kRoundRectCoeff)));
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

	for (int y = 0; y < Details.Height; y++)
	{
		int ydw = y * Details.Width;

		for (int x = 0; x < Details.Width; x++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[ydw + x]);

			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			double dwx = (double)Details.Width - 1 - (double)x;

			double ac = std::cos(CalcUtility::DegToRadians(RadialOffsetDegrees + (dwx / (double)Details.Width - 1) * 360));
			double as = std::sin(CalcUtility::DegToRadians(RadialOffsetDegrees + (dwx / (double)Details.Width - 1) * 360));

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

	for (int y = 0; y < Details.Height; y++)
	{
		int ydw = y * Details.Width;

		for (int x = 0; x < Details.Width; x++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[ydw + x]);

			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			double ac = std::cos(CalcUtility::DegToRadians(RadialOffsetDegrees + 225 - ((double)x / ((double)Details.Width - 1)) * 270));
			double as = std::sin(CalcUtility::DegToRadians(RadialOffsetDegrees + 225 - ((double)x / ((double)Details.Width - 1)) * 270));

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

	for (int y = 0; y < Details.Height; y++)
	{
		int ydw = y * Details.Width;

		for (int x = 0; x < Details.Width; x++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[ydw + x]);

			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			double ac = std::cos(CalcUtility::DegToRadians((double)RadialOffsetDegrees + 180 - ((double)x / ((double)Details.Width - 1)) * 180));
			double as = std::sin(CalcUtility::DegToRadians((double)RadialOffsetDegrees + 180 - ((double)x / ((double)Details.Width - 1)) * 180));

			double d =  ((double)cx - (double)Preview.ROffset) / (double)Details.Height;

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

	for (int y = 0; y < Details.Height; y++)
	{
		int ydw = y * Details.Width;

		for (int x = 0; x < Details.Width; x++)
		{
			PreviewBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[ydw + x]);

			PreviewBox->Canvas->Pen->Color = PreviewBox->Canvas->Brush->Color;

			double ac = std::cos(CalcUtility::DegToRadians((double)RadialOffsetDegrees + 180 + ((double)x / ((double)Details.Width - 1)) * 180));
			double as = std::sin(CalcUtility::DegToRadians((double)RadialOffsetDegrees + 180 + ((double)x / ((double)Details.Width - 1)) * 180));

			double d = ((double)cx - (double)Preview.ROffset) / (double)Details.Height;

			int xp = cx + std::round(((double)Preview.ROffset + (d * (double)y)) * ac);
			int yp = cy - std::round(((double)Preview.ROffset + (d * (double)y)) * as);

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
	SetPreviewBoxSize(kPreviewPixelSizeAuto);
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
	else if (Preview.CanDraw)
	{
		if (Preview.View == ViewShape::kSquare)
		{
			int nx = std::round(((double)X / PreviewBox->Width) * PaintBox->Width);
			int ny = std::round(((double)Y / PreviewBox->Height) * PaintBox->Height);

			PaintBox->OnMouseDown(nullptr, Button, Shift, nx, ny);
		}
	}
}
#pragma end_region


#pragma region Mode_Mono_Grid
void __fastcall TheMatrix::ClickPixel(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (Data->IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!Data->Layers[CurrentLayer]->Visible) return;

	int x1 = std::floor((double)X / (double)Render.PixelSize);
	int y1 = std::floor((double)Y / (double)Render.PixelSize);

	if (x1 < 0 || y1 < 0 || x1 > Details.Width - 1 || y1 > Details.Height - 1) return;

	x1 = std::floor((double)X / (double)Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor((double)Y / (double)Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = kMouseLeft;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			DrawWithBrush(1, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(1, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kPaste:
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
		LastMouseButton = kMouseRight;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			DrawWithBrush(0, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(0, x1, y1);

			if (OnChange) OnChange(this);
			break;

		default:
			UpdateDrawTool(x1, y1, 0, false);
		}

		CopyDrawBufferToCurrentFrame();
	}

	LastX = x1;
	LastY = y1;

	PreviewBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseMove(TObject *Sender, TShiftState Shift, int X, int Y)
{
	int x1 = std::floor((double)X / (double)Render.PixelSize);
	int y1 = std::floor((double)Y / (double)Render.PixelSize);

	if (x1 < 0 || y1 < 0 || x1 > Details.Width - 1 || y1 > Details.Height - 1) return;

	x1 = std::floor((double)X / (double)Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor((double)Y / (double)Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (OnMouseOver) OnMouseOver(x1, y1);

	// ===========================================================================
	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = kMouseLeft;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(1, x1, y1);
			}

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti :
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrushMulti(1, x1, y1);
			}

			if (OnChange) OnChange(this);
			break;

		default:
			break;
		}
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = kMouseRight;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(0, x1, y1);
			}

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrushMulti(0, x1, y1);
			}

			if (OnChange) OnChange(this);
			break;

		default:
			break;
		}
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (Render.Action.Mode == ActionMode::kNone)
	{
		CopyDrawBufferToCurrentFrame();
	}
}


void __fastcall TheMatrix::PaintBoxUpdate(TObject *Sender)
{
	#if _FrameTimer == 1
	std::chrono::system_clock::time_point StartTime = std::chrono::system_clock::now();
	#endif

	BuildMonoBiRenderFrame();

	for (int y = 0; y <= Render.ViewWindow.Y; y++)
	{
		int irp = y * Render.PixelSize;
		int rtlyy = (Render.TopLeft.Y + y) * Details.Width;

		for (int x = 0; x <= Render.ViewWindow.X; x++)
		{
			PaintBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[rtlyy + x]);

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
												irp,
											   (x * Render.PixelSize) + Render.PixelSizeZ,
												irp + Render.PixelSizeZ));
				break;
			case PixelShape::kCircle:
				PaintBox->Canvas->Ellipse(x * Render.PixelSize,
										  irp,
										 (x * Render.PixelSize) + Render.PixelSizeZ,
										  irp + Render.PixelSizeZ);
				break;
			case PixelShape::kRoundRect:
				PaintBox->Canvas->RoundRect(x * Render.PixelSize,
											irp,
										   (x * Render.PixelSize) + Render.PixelSizeZ,
											irp + Render.PixelSizeZ,
											Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
											Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
				break;

			default:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
												irp,
											   (x * Render.PixelSize) + Render.PixelSizeZ,
											    irp + Render.PixelSizeZ));
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Action.Mode != ActionMode::kNone)
	{
		if (Render.Action.SinglePoint || Render.Action.Coords[0].X != - 1)
		{
			DrawShape(true, 1, false);

			// =======================================================================

			// single point modes don't require "first click" marker
			if (Render.Action.SinglePoint)
			{
				PaintBox->Canvas->Brush->Color = TColor(LEDColours[kDisplayMarker]);

				switch (Render.Shape)
				{
				case PixelShape::kSquare:
					PaintBox->Canvas->FillRect(Rect(Render.Action.Coords[0].X * Render.PixelSize,
													Render.Action.Coords[0].Y * Render.PixelSize,
												   (Render.Action.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
												   (Render.Action.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ));
					break;
				case PixelShape::kCircle:
					PaintBox->Canvas->Ellipse(Render.Action.Coords[0].X * Render.PixelSize,
											  Render.Action.Coords[0].Y * Render.PixelSize,
											 (Render.Action.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
											 (Render.Action.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ);
					break;
				case PixelShape::kRoundRect:
					PaintBox->Canvas->RoundRect(Render.Action.Coords[0].X * Render.PixelSize,
												Render.Action.Coords[0].Y * Render.PixelSize,
											   (Render.Action.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
											   (Render.Action.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ,
												Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
												Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
					break;
				}
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Action.CopyPos.X != 0)
	{
		for (int x = 0; x <= Render.Action.CopyPos.X; x++)
		{
			for (int y = 0; y <= Render.Action.CopyPos.Y; y++)
			{
				if (x + LastX >= 0 && x + LastX <= Details.Width &&
					y + LastY >= 0 && y + LastY <= Details.Height)
				{
					if (MatrixIgnoredLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
					{
						PaintBox->Canvas->Brush->Color = TColor(LEDColours[MatrixCopy->Grid[y * Details.Width + x]]);
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
					case PixelShape::kRoundRect :
						PaintBox->Canvas->RoundRect((x + LastX) * Render.PixelSize,
													(y + LastY) * Render.PixelSize,
												   ((x + LastX) * Render.PixelSize) + Render.PixelSizeZ,
												   ((y + LastY) * Render.PixelSize) + Render.PixelSizeZ,
													 Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
													 Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
						break;
					}
				}
			}
		}
	}

	PreviewBox->Invalidate();

	#if _FrameTimer == 1
	std::chrono::system_clock::time_point EndTime = std::chrono::system_clock::now();

	std::chrono::duration<double> elapsed_seconds = EndTime - StartTime;

	if (OnDebugEvent) OnDebugEvent(this, std::to_wstring(elapsed_seconds.count()) + L" seconds");
	#endif
}


void TheMatrix::BuildMonoBiRenderFrame()
{
	MatrixRender->ClearColour(LEDColours[kDisplayClear]);

	if (Data->Layers.size() == 1)
	{
		for (int z = 0; z < Details.Width * Details.Height; z++)
		{
			if (MatrixIgnoredLayout->Grid[z] == PixelAlive)
			{
				switch (DisplayBuffer->Grid[z])
				{
					case 1:
						MatrixRender->Grid[z] = LEDColours[kMouseLeft];
						break;
					case 2:
						MatrixRender->Grid[z] = LEDColours[kMouseMiddle];
						break;
					case 3:
						MatrixRender->Grid[z] = LEDColours[kMouseRight];
						break;
				}
			}
			else
			{
				MatrixRender->Grid[z] = CanvasBackground;
			}
		}

		return;
	}

	for (int layer = 0; layer < Data->Layers.size(); layer++)
	{
		if (Data->Layers[layer]->Visible)
		{
			for (int z = 0; z < Details.Width * Details.Height; z++)
			{
				if (MatrixIgnoredLayout->Grid[z] == PixelIgnored)
				{
					MatrixRender->Grid[z] = CanvasBackground;
				}
				else
				{
					if (layer == CurrentLayer)
					{
						switch (DisplayBuffer->Grid[z])
						{
						case 0:
							if (MatrixRender->Grid[z] == LEDColours[0])
							{
								if (LightBox == 1 && CurrentFrame != 0)
								{
									if (Data->Layers[layer]->Cells[CurrentFrame - 1]->Grid[z] == 1)
									{
										MatrixRender->Grid[z] = LEDColours[kLightBoxShade];
									}
								}
							}
							break;
						case 1:
							MatrixRender->Grid[z] = LEDColours[kMouseLeft];
							break;
						case 2:
							MatrixRender->Grid[z] = LEDColours[kMouseMiddle];
							break;
						case 3:
							MatrixRender->Grid[z] = LEDColours[kMouseRight];
							break;
						}
					}
					else
					{
						switch (Data->Layers[layer]->Cells[CurrentFrame]->Grid[z])
						{
						case 0:
							if (MatrixRender->Grid[z] == LEDColours[kDisplayClear])
							{
								if (LightBox == 1 && CurrentFrame != 0)
								{
									if (Data->Layers[layer]->Cells[CurrentFrame - 1]->Grid[z] == 1)
									{
										MatrixRender->Grid[z] = LEDColours[kLightBoxShade];
									}
								}
							}
							break;
						case 1:
							MatrixRender->Grid[z] = LEDColours[kMouseLeft];
							break;
						case 2:
							MatrixRender->Grid[z] = LEDColours[kMouseMiddle];
							break;
						case 3:
							MatrixRender->Grid[z] = LEDColours[kMouseRight];
							break;
						}
					}
				}
			}
		}
	}
}
#pragma end_region


#pragma region Mode_Bi_Grid
void __fastcall TheMatrix::Shape1MouseUpBiColour(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	switch (Render.Action.Mode)
	{
	case ActionMode::kNone:
	case ActionMode::kGradientBrush:
	case ActionMode::kMulti:
	case ActionMode::kRandom:
		CopyDrawBufferToCurrentFrame();
		break;

	default:
		break;
	}
}


void __fastcall TheMatrix::ClickPixelBiColour(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (Data->IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!Data->Layers[CurrentLayer]->Visible) return;

	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0 || x1 > Details.Width - 1 || y1 > Details.Height - 1) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = kMouseLeft;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			DrawWithBrush(SelectionLMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(SelectionLMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kPaste:
			DrawWithBrushPaste(x1, y1, !Shift.Contains(ssShift));

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kRandom:
		{
			int i = 1 + random(3);

			DrawWithBrush(i, x1, y1);

			if (OnChange) OnChange(this);
			break;
		}

		default:
			UpdateDrawTool(x1, y1, SelectionLMB, false);
		}

		CopyDrawBufferToCurrentFrame();
	}
	else if (Shift.Contains(ssMiddle))
	{
		LastMouseButton = kMouseMiddle;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			DrawWithBrush(SelectionMMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(SelectionMMB, x1, y1);

			if (OnChange) OnChange(this);
			break;

		default:
			UpdateDrawTool(x1, y1, SelectionMMB, true);
		}

		CopyDrawBufferToCurrentFrame();
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = kMouseRight;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			DrawWithBrush(SelectionRMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(SelectionRMB, x1, y1);

			if (OnChange) OnChange(this);
			break;

		default:
			UpdateDrawTool(x1, y1, SelectionRMB, false);
		}

		CopyDrawBufferToCurrentFrame();
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseMoveBiColour(TObject *Sender, TShiftState Shift, int X, int Y)
{
	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0 || x1 > Details.Width - 1 || y1 > Details.Height - 1) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (OnMouseOver) OnMouseOver(x1, y1);

	// ===========================================================================
	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = kMouseLeft;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			DrawWithBrush(SelectionLMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(SelectionLMB, x1, y1);

			if (OnChange) OnChange(this);
			break;

		default:
			break;
		}
	}
	else if (Shift.Contains(ssMiddle))
	{
		LastMouseButton = kMouseMiddle;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
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

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(SelectionMMB, x1, y1);

			if (OnChange) OnChange(this);
			break;

		default:
			break;
		}
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = kMouseRight;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			DrawWithBrush(SelectionRMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(SelectionRMB, x1, y1);

			if (OnChange) OnChange(this);
			break;

		default:
			break;
		}
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region Mode_RGB_Grid
void TheMatrix::BuildRGBRenderFrame()
{
	MatrixRender->ClearColour(RGBBackground);

	if (Data->Layers.size() == 1)
	{
		for (int z = 0; z < Details.Width * Details.Height; z++)
		{
			if (MatrixIgnoredLayout->Grid[z] == PixelAlive)
			{
				MatrixRender->Grid[z] = DisplayBuffer->Grid[z];
			}
			else
			{
				MatrixRender->Grid[z] = CanvasBackground;
			}
		}

		return;
	}

	for (int l = 0; l < Data->Layers.size(); l++)
	{
		if (Data->Layers[l]->Visible)
		{
			for (int z = 0; z < Details.Width * Details.Height; z++)
			{
				if (MatrixIgnoredLayout->Grid[z] != PixelAlive)
				{
					MatrixRender->Grid[z] = CanvasBackground;
				}
				else
				{
					if (l == CurrentLayer)
					{
						if (LightBox == 1 && CurrentFrame != 0)
						{
							if (DisplayBuffer->Grid[z] == RGBBackground)
							{
								MatrixRender->Grid[z] = ColourUtility::DarkenRGB(Data->Layers[l]->Cells[CurrentFrame - 1]->Grid[z]);
							}
							else
							{
								MatrixRender->Grid[z] = DisplayBuffer->Grid[z];
							}
						}
						else if (DisplayBuffer->Grid[z] != RGBBackground)
						{
							MatrixRender->Grid[z] = DisplayBuffer->Grid[z];
						}
					}
					else
					{
						if (LightBox == 1 && CurrentFrame != 0)
						{
							if (DisplayBuffer->Grid[z] == RGBBackground)
							{
								MatrixRender->Grid[z] = ColourUtility::DarkenRGB(Data->Layers[l]->Cells[CurrentFrame - 1]->Grid[z]);
							}
							else
							{
								MatrixRender->Grid[z] = DisplayBuffer->Grid[z];
							}
						}
						else if (Data->Layers[l]->Cells[CurrentFrame]->Grid[z] != RGBBackground)
						{
							MatrixRender->Grid[z] = Data->Layers[l]->Cells[CurrentFrame]->Grid[z];
						}
					}
				}
			}
		}
	}
}


void __fastcall TheMatrix::PaintBoxUpdateRGB(TObject *Sender)
{
	#if _FrameTimer == 1
	std::chrono::system_clock::time_point StartTime = std::chrono::system_clock::now();
	#endif

	BuildRGBRenderFrame();

	for (int y = 0; y <= Render.ViewWindow.Y; y++)
	{
		int irp = y * Render.PixelSize;
		int rtlyy = (Render.TopLeft.Y + y) * Details.Width;

		for (int x = 0; x <= Render.ViewWindow.X; x++)
		{
			PaintBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[rtlyy + (Render.TopLeft.X + x)]);

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
												irp,
											   (x * Render.PixelSize) + Render.PixelSizeZ,
												irp + Render.PixelSizeZ));
				break;
			case PixelShape::kCircle:
				PaintBox->Canvas->Ellipse(x * Render.PixelSize,
										  irp,
										 (x * Render.PixelSize) + Render.PixelSizeZ,
										 (irp) + Render.PixelSizeZ);
				break;
			case PixelShape::kRoundRect:
				PaintBox->Canvas->RoundRect(x * Render.PixelSize,
											irp,
										   (x * Render.PixelSize) + Render.PixelSizeZ,
										   (irp) + Render.PixelSizeZ,
											Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
											Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
				break;

			default:
				PaintBox->Canvas->FillRect(Rect(x * Render.PixelSize,
												irp,
											   (x * Render.PixelSize) + Render.PixelSizeZ,
											   (irp) + Render.PixelSizeZ));
				break;
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Action.Mode != ActionMode::kNone)
	{
		if (Render.Action.SinglePoint || Render.Action.Coords[0].X != - 1)
		{
			DrawShape(true, Render.Action.Colour, false);

			// =======================================================================

			// single point modes don't require "first click" marker
			if (Render.Action.SinglePoint)
			{
				PaintBox->Canvas->Brush->Color = TColor(LEDColours[kDisplayMarker]);

				switch (Render.Shape)
				{
				case PixelShape::kSquare:
					PaintBox->Canvas->FillRect(Rect(Render.Action.Coords[0].X * Render.PixelSize,
													Render.Action.Coords[0].Y * Render.PixelSize,
												   (Render.Action.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
												   (Render.Action.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ));
					break;
				case PixelShape::kCircle:
					PaintBox->Canvas->Ellipse(Render.Action.Coords[0].X * Render.PixelSize,
											  Render.Action.Coords[0].Y * Render.PixelSize,
											 (Render.Action.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
											 (Render.Action.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ);
					break;
				case PixelShape::kRoundRect:
					PaintBox->Canvas->RoundRect(Render.Action.Coords[0].X * Render.PixelSize,
												Render.Action.Coords[0].Y * Render.PixelSize,
											   (Render.Action.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
											   (Render.Action.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ,
												Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
												Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
					break;
				}
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Action.CopyPos.X != 0)
	{
		for (int x = 0; x <= Render.Action.CopyPos.X; x++)
		{
			for (int y = 0; y <= Render.Action.CopyPos.Y; y++)
			{
				if (x + LastX >= 0 && x + LastX <= Details.Width &&
					y + LastY >= 0 && y + LastY <= Details.Height)
				{
					if (MatrixIgnoredLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
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
													 Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
													 Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
						break;
					}
				}
			}
		}
	}

	PreviewBox->Invalidate();

	#if _FrameTimer == 1
	std::chrono::system_clock::time_point EndTime = std::chrono::system_clock::now();

	std::chrono::duration<double> elapsed_seconds = EndTime - StartTime;

	if (OnDebugEvent) OnDebugEvent(this, std::to_wstring(elapsed_seconds.count()) + L" seconds");
	#endif
}


void __fastcall TheMatrix::Shape1MouseUpRGB(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
 {
	switch (Render.Action.Mode)
	{
	case ActionMode::kNone:
	case ActionMode::kGradientBrush:
	case ActionMode::kMulti:
	case ActionMode::kRandom:
		CopyDrawBufferToCurrentFrame();
		break;

	default:
		break;
	}
}


void __fastcall TheMatrix::ClickPixelRGB(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (Data->IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!Data->Layers[CurrentLayer]->Visible) return;

	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0 || x1 > Details.Width - 1 || y1 > Details.Height - 1) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = kMouseLeft;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
		case ActionMode::kGradientBrush:
			DrawWithBrush(SelectionLMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kRandom:
			DrawWithBrush(ColourUtility::RandomColour(SelectionLMB, RandomCoeff), x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(SelectionLMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kPicker:
			ChangeSelectionColour(Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid[y1 * Details.Width + x1], SelectionMMB, SelectionRMB);
			break;
		case ActionMode::kPaste:
			DrawWithBrushPaste(x1, y1, !Shift.Contains(ssShift));

			if (OnChange) OnChange(this);
			break;

		default:
			UpdateDrawTool(x1, y1, SelectionLMB, false);
		}

		CopyDrawBufferToCurrentFrame();
	}
	else if (Shift.Contains(ssMiddle))
	{
		LastMouseButton = kMouseMiddle;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
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

			if (OnChange) OnChange(this);
            break;
		case ActionMode::kRandom:
			DrawWithBrush(ColourUtility::RandomColour(SelectionMMB, RandomCoeff), x1, y1);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(SelectionMMB, x1, y1);
			if (OnChange) OnChange(this);
			break;
		case ActionMode::kPicker:
			ChangeSelectionColour(SelectionLMB, Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid[y1 * Details.Width + x1], SelectionRMB);
			break;
		case ActionMode::kGradientBrush:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithGradientBrush(x1, y1);

				if (OnChange) OnChange(this);
			}
			break;

		default:
			UpdateDrawTool(x1, y1, SelectionMMB, true);
		}

		CopyDrawBufferToCurrentFrame();
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = kMouseRight;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
		case ActionMode::kGradientBrush:
			DrawWithBrush(SelectionRMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kRandom:
			DrawWithBrush(ColourUtility::RandomColour(SelectionRMB, RandomCoeff), x1, y1);
			break;
		case ActionMode::kMulti:
			DrawWithBrushMulti(SelectionRMB, x1, y1);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kPicker:
			ChangeSelectionColour(SelectionLMB, SelectionMMB, Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid[y1 * Details.Width + x1]);
			break;

		default:
			UpdateDrawTool(x1, y1, SelectionRMB, false);
		}

		CopyDrawBufferToCurrentFrame();
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseMoveRGB(TObject *Sender, TShiftState Shift, int X, int Y)
{
	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0 || x1 > Details.Width - 1 || y1 > Details.Height - 1) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (OnMouseOver) OnMouseOver(x1, y1);

	// ===========================================================================
	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = kMouseLeft;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
		case ActionMode::kGradientBrush:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(SelectionLMB, x1, y1);
			}

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrushMulti(SelectionLMB, x1, y1);
			}

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kRandom:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(ColourUtility::RandomColour(SelectionLMB, RandomCoeff), x1, y1);
            }

			if (OnChange) OnChange(this);
			break;

		default:
			break;
		}
	}
	else if (Shift.Contains(ssMiddle))
	{
		LastMouseButton = kMouseMiddle;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
			switch (Render.Gradient.Option)
			{
			case GradientOption::kOff:
				if (!(LastX == x1 && LastY == y1))
				{
					DrawWithBrush(SelectionMMB, x1, y1);
                }
				break;
			case GradientOption::kVertical:
				if (!(LastX == x1 && LastY == y1))
				{
					DrawWithBrush(Render.Gradient.IY[y1], x1, y1);
				}
				break;
			case GradientOption::kHorizontal:
                if (!(LastX == x1 && LastY == y1))
				{
					DrawWithBrush(Render.Gradient.IX[x1], x1, y1);
				}
				break;
			}

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrushMulti(SelectionMMB, x1, y1);
			}

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kRandom:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(ColourUtility::RandomColour(SelectionMMB, RandomCoeff), x1, y1);
			}

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kGradientBrush:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithGradientBrush(x1, y1);
			}

			if (OnChange) OnChange(this);
			break;

		default:
			break;
		}
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = kMouseRight;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
		case ActionMode::kGradientBrush:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(SelectionRMB, x1, y1);
			}

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrushMulti(SelectionRMB, x1, y1);
			}

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kRandom:
			if (!(LastX == x1 && LastY == y1))
			{
				DrawWithBrush(ColourUtility::RandomColour(SelectionRMB, RandomCoeff), x1, y1);
            }

            if (OnChange) OnChange(this);
			break;

		default:
			break;
		}
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region Mode_RGB_Freeform
void __fastcall TheMatrix::PaintBoxUpdateRGBFF(TObject *Sender)
{
	#if _FrameTimer == 1
	std::chrono::system_clock::time_point StartTime = std::chrono::system_clock::now();
	#endif

	for (int p = 0; p < Data->Layers[0]->Freeform->Pixels.size(); p++)
	{
		MatrixPixel *px = Data->Layers[0]->Freeform->Pixels[p];

		PaintBox->Canvas->Brush->Color = TColor(px->Colours[CurrentFrame]);

		switch (Render.Shape)
		{
		case PixelShape::kSquare:
			PaintBox->Canvas->FillRect(Rect(px->X,
											px->Y,
											px->X + Render.PixelSizeZ,
											px->Y + Render.PixelSizeZ));
			break;
		case PixelShape::kCircle:
			PaintBox->Canvas->Ellipse(px->X,
									  px->Y,
									  px->X + Render.PixelSizeZ,
									  px->Y + Render.PixelSizeZ);
			break;
		case PixelShape::kRoundRect:
			PaintBox->Canvas->RoundRect(px->X,
										px->Y,
										px->X + Render.PixelSizeZ,
										px->Y + Render.PixelSizeZ,
										Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
										Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
			break;

		default:
			PaintBox->Canvas->FillRect(Rect(px->X,
											px->Y,
											px->X + Render.PixelSizeZ,
											px->Y + Render.PixelSizeZ));
			break;
		}

		if (Render.ShowPixelOrder)
		{
			PaintBox->Canvas->Font->Color = TColor(px->Contrast);

			PaintBox->Canvas->TextOut(px->X + 1,
									  px->Y + 1,
									  IntToStr(px->Order));
		}
		if (Render.ShowPixelGroup)
		{
			PaintBox->Canvas->Font->Color = TColor(px->Contrast);

			PaintBox->Canvas->TextOut(px->X + 1,
									  px->Y + 1,
									  IntToStr(px->Group));
		}
		#if _DEBUG
		if (Render.ShowFrameCount)
		{
			PaintBox->Canvas->Font->Color = TColor(px->Contrast);

			PaintBox->Canvas->TextOut(px->X + 1,
									  px->Y + 1,
									  std::to_wstring(px->Colours.size()).c_str());
		}
		#endif
	}

	#if _FrameTimer == 1
	std::chrono::system_clock::time_point EndTime = std::chrono::system_clock::now();

	std::chrono::duration<double> elapsed_seconds = EndTime - StartTime;

	if (OnDebugEvent) OnDebugEvent(this, std::to_wstring(elapsed_seconds.count()) + L" seconds");
	#endif
}


void __fastcall TheMatrix::ClickPixelRGBFF(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	CurrentPixel = Data->Layers[CurrentLayer]->FindPixel(X, Y, Render.PixelSizeZ);

	if (CurrentPixel == -1)
	{
		Render.Action.Coords[0].X = X;
		Render.Action.Coords[0].Y = Y;

		if (Render.Action.Mode == ActionMode::kMovePixel && !Shift.Contains(ssShift))
		{
			if (Data->Layers[CurrentLayer]->Freeform->Selection.size() != 0)
			{
				Data->Layers[CurrentLayer]->Freeform->Selection.clear();
			}
		}

        if (OnMouseOverPixel) OnMouseOverPixel(X, Y, CurrentPixel);

		return;
	}

	if (Data->IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!Data->Layers[CurrentLayer]->Visible) return;

	// ===========================================================================

	if (OnMouseOverPixel) OnMouseOverPixel(X, Y, CurrentPixel);

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		LastMouseButton = kMouseLeft;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
		case ActionMode::kGradientBrush:
			ColourPixel(SelectionLMB);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kRandom:
			ColourPixel(ColourUtility::RandomColour(SelectionLMB, RandomCoeff));

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kMulti:
			ColourPixelMulti(SelectionLMB);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kPicker:
			ChangeSelectionColour(Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->Colours[CurrentFrame], SelectionMMB, SelectionRMB);
			break;
		case ActionMode::kMovePixel:
			if (Shift.Contains(ssShift))
			{
				if (Data->Layers[CurrentLayer]->Freeform->Selection.size() == 0)
				{
					LastX = X;
					LastY = Y;
				}

				Data->Layers[CurrentLayer]->Freeform->AddToSelection(CurrentPixel);
			}
			break;
		case ActionMode::kDrawOrder:
			Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->Order = NewOrder++;

			if (NewOrder >= Data->Layers[CurrentLayer]->Freeform->Pixels.size())
			{
				Render.Action.Mode = ActionMode::kNone;
			}
			break;

		default:
			UpdateDrawTool(X, Y, SelectionLMB, false);
		}
	}
	else if (Shift.Contains(ssMiddle))
	{
		LastMouseButton = kMouseMiddle;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
		case ActionMode::kGradientBrush:
			ColourPixel(SelectionMMB);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kRandom:
			ColourPixel(ColourUtility::RandomColour(SelectionMMB, RandomCoeff));
			break;
		case ActionMode::kMulti:
			ColourPixelMulti(SelectionMMB);
			if (OnChange) OnChange(this);
			break;
		case ActionMode::kPicker:
			ChangeSelectionColour(SelectionLMB, Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->Colours[CurrentFrame], SelectionRMB);
			break;

		default:
			UpdateDrawTool(X, Y, SelectionMMB, false);
		}
	}
	else if (Shift.Contains(ssRight))
	{
		LastMouseButton = kMouseRight;

		switch (Render.Action.Mode)
		{
		case ActionMode::kNone:
		case ActionMode::kGradientBrush:
			ColourPixel(SelectionRMB);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kRandom:
			ColourPixel(ColourUtility::RandomColour(SelectionRMB, RandomCoeff));
			break;
		case ActionMode::kMulti:
			ColourPixelMulti(SelectionRMB);

			if (OnChange) OnChange(this);
			break;
		case ActionMode::kPicker:
			ChangeSelectionColour(SelectionLMB, SelectionMMB, Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->Colours[CurrentFrame]);
			break;

		default:
			UpdateDrawTool(X, Y, SelectionRMB, false);
		}
	}

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseMoveRGBFF(TObject *Sender, TShiftState Shift, int X, int Y)
{
	// ===========================================================================
	if (OnMouseOverPixel) OnMouseOverPixel(X, Y, CurrentPixel);
	// ===========================================================================

	if (CurrentPixel == -1) return;

	if (Render.Action.Mode == ActionMode::kMovePixel && !Shift.Contains(ssShift))
	{
		if (Data->Layers[CurrentLayer]->Freeform->Selection.size() != 0)
		{
			for (int t = 0; t < Data->Layers[CurrentLayer]->Freeform->Selection.size(); t++)
			{
				Data->Layers[CurrentLayer]->Freeform->Move(Data->Layers[CurrentLayer]->Freeform->Selection[t], X - LastX, Y - LastY);
			}
		}
		else
		{
			Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->X = X;
			Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->Y = Y;
		}
	}

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseUpRGBFF(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
    CurrentPixel = -1;
}


void TheMatrix::AddPixelShape(ShapeObject so)
{
	switch (so.Shape)
	{
	case 0: // circle
		Data->Layers[CurrentLayer]->Freeform->AddShapeCircle(so.SizeX, so.Pixels, Render.PixelSizeZ, so.X, so.Y, so.Colour);
		break;
	case 1: // line (horizontal)
		Data->Layers[CurrentLayer]->Freeform->AddLineH(so.SizeX, Render.PixelSizeZ, so.X, so.Y, so.Colour);
		break;
	case 2: // line (vertical)
		Data->Layers[CurrentLayer]->Freeform->AddLineV(so.SizeX, Render.PixelSizeZ, so.X, so.Y, so.Colour);
		break;
	case 3: // square
		Data->Layers[CurrentLayer]->Freeform->AddShapeSquare(so.SizeX, Render.PixelSizeZ, so.X, so.Y, so.Colour);
		break;
	case 4: // square filled
		Data->Layers[CurrentLayer]->Freeform->AddShapeSquareFilled(so.SizeX, so.Direction, Render.PixelSizeZ, so.X, so.Y, so.Colour);
		break;
	case 5: // rectangle
		Data->Layers[CurrentLayer]->Freeform->AddShapeRectangle(so.SizeX, so.SizeY, Render.PixelSizeZ, so.X, so.Y, so.Colour);
		break;
	case 6: // rectangle filled
		Data->Layers[CurrentLayer]->Freeform->AddShapeRectangleFilled(so.SizeX, so.SizeY, so.Direction, Render.PixelSizeZ, so.X, so.Y, so.Colour);
		break;
	}

    PaintBox->Invalidate();
}
#pragma end_region


#pragma region Mode_RGB3BPP
void TheMatrix::BuildRGB3BPPRenderFrame()
{
	MatrixRender->ClearColour(RGBBackground);

	if (Data->Layers.size() == 1)
	{
		for (int z = 0; z < Details.Width * Details.Height; z++)
		{
			if (MatrixIgnoredLayout->Grid[z] == PixelAlive)
			{
				if (DisplayBuffer->Grid[z] != RGBBackground)
				{
					MatrixRender->Grid[z] = LEDRGB3BPPColours[DisplayBuffer->Grid[z]];
				}
			}
			else
			{
				MatrixRender->Grid[z] = CanvasBackground;
			}
		}

		return;
	}

	for (int l = 0; l < Data->Layers.size(); l++)
	{
		if (Data->Layers[l]->Visible)
		{
			for (int z = 0; z < Details.Width * Details.Height; z++)
			{
				if (MatrixIgnoredLayout->Grid[z] != PixelAlive)
				{
					MatrixRender->Grid[z] = CanvasBackground;
				}
				else
				{
					if (l == CurrentLayer)
					{
						if (LightBox == 1 && CurrentFrame != 0)
						{
							if (DisplayBuffer->Grid[z] == RGBBackground)
							{
								MatrixRender->Grid[z] = ColourUtility::DarkenRGB(LEDRGB3BPPColours[Data->Layers[l]->Cells[CurrentFrame - 1]->Grid[z]]);
							}
							else
							{
								MatrixRender->Grid[z] = LEDRGB3BPPColours[DisplayBuffer->Grid[z]];
							}
						}
						else if (DisplayBuffer->Grid[z] != RGBBackground)
						{
							MatrixRender->Grid[z] = LEDRGB3BPPColours[DisplayBuffer->Grid[z]];
						}
					}
					else
					{
						if (LightBox == 1 && CurrentFrame != 0)
						{
							if (DisplayBuffer->Grid[z] == RGBBackground)
							{
								MatrixRender->Grid[z] = ColourUtility::DarkenRGB(Data->Layers[l]->Cells[CurrentFrame - 1]->Grid[z]);
							}
							else
							{
								MatrixRender->Grid[z] = LEDRGB3BPPColours[Data->Layers[l]->Cells[CurrentFrame]->Grid[z]];
							}
						}
						else if (Data->Layers[l]->Cells[CurrentFrame]->Grid[z] != RGBBackground)
						{
							MatrixRender->Grid[z] = LEDRGB3BPPColours[Data->Layers[l]->Cells[CurrentFrame]->Grid[z]];
						}
					}
				}
			}
		}
	}
}


void __fastcall TheMatrix::PaintBoxUpdateRGB_3BPP(TObject *Sender)
{
	#if _FrameTimer == 1
	std::chrono::system_clock::time_point StartTime = std::chrono::system_clock::now();
	#endif

	BuildRGB3BPPRenderFrame();

	for (int y = 0; y <= Render.ViewWindow.Y; y++)
	{
		int rtyy = (Render.TopLeft.Y + y) * Details.Width;

		for (int x = 0; x <= Render.ViewWindow.X; x++)
		{
			PaintBox->Canvas->Brush->Color = TColor(MatrixRender->Grid[rtyy + (Render.TopLeft.X + x)]);

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
											Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
											Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
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

	if (Render.Action.Mode != ActionMode::kNone)
	{
		if (Render.Action.SinglePoint || Render.Action.Coords[0].X != - 1)
		{
			DrawShape(true, Render.Action.Colour, false);

			// =======================================================================

			PaintBox->Canvas->Brush->Color = TColor(LEDColours[kDisplayMarker]);

			switch (Render.Shape)
			{
			case PixelShape::kSquare:
				PaintBox->Canvas->FillRect(Rect(Render.Action.Coords[0].X * Render.PixelSize,
													 Render.Action.Coords[0].Y * Render.PixelSize,
													(Render.Action.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
													(Render.Action.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ));
				break;
			case PixelShape::kCircle:
				PaintBox->Canvas->Ellipse(Render.Action.Coords[0].X * Render.PixelSize,
											   Render.Action.Coords[0].Y * Render.PixelSize,
											  (Render.Action.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
											  (Render.Action.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ);
				break;
			case PixelShape::kRoundRect:
					PaintBox->Canvas->RoundRect(Render.Action.Coords[0].X * Render.PixelSize,
												 Render.Action.Coords[0].Y * Render.PixelSize,
												(Render.Action.Coords[0].X * Render.PixelSize) + Render.PixelSizeZ,
												(Render.Action.Coords[0].Y * Render.PixelSize) + Render.PixelSizeZ,
												 Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
												 Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
				break;
			}
		}
	}

	// ===========================================================================
	// ===========================================================================

	if (Render.Action.CopyPos.X != 0)
	{
		for (int x = 0; x <=  Render.Action.CopyPos.X; x++)
		{
			for (int y = 0; y <= Render.Action.CopyPos.Y; y++)
			{
				if (x + LastX >= 0 && x + LastX <= Details.Width &&
					y + LastY >= 0 && y + LastY <= Details.Height)
				{
					if (MatrixIgnoredLayout->Grid[(y + LastY) * Details.Width + (x + LastX)] == PixelAlive)
					{
						PaintBox->Canvas->Brush->Color = TColor(LEDRGB3BPPColours[MatrixCopy->Grid[y * Details.Width + x]]);
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
													  Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
													  Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
						break;
					}
				}
			}
		}
	}

	PreviewBox->Invalidate();

	#if _FrameTimer == 1
	std::chrono::system_clock::time_point EndTime = std::chrono::system_clock::now();

	std::chrono::duration<double> elapsed_seconds = EndTime - StartTime;

	if (OnDebugEvent) OnDebugEvent(this, std::to_wstring(elapsed_seconds.count()) + L" seconds");
	#endif
}
#pragma end_region


#pragma region IgnoredPixels
void __fastcall TheMatrix::PaintBoxUpdateIgnoredPixel(TObject *Sender)
{
	for (int x = 0; x < Details.Width; x++)
	{
		for (int y = 0; y < Details.Height; y++)
		{
			if (MatrixIgnoredLayout->Grid[(Render.TopLeft.Y + y) * Details.Width + (Render.TopLeft.X + x)] == PixelAlive)
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
											Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)),
											Render.PixelSize - (std::round(Render.PixelSize / kRoundRectCoeff)));
				break;
			}
		}
	}
}


void TheMatrix::SetIgnoredPixels(int ignoredness)
{
	for (int z = 0; z < __MaxWidth * __MaxHeight; z++)
	{
		MatrixIgnoredLayout->Grid[z] = ignoredness;
	}
}


void TheMatrix::SetIgnoredPixelsFromCustomShape(CustomShape shape, int parameter)
{
	MatrixIgnoredLayout->SetFromCustomShape(Details.Width, Details.Height, shape, parameter);

	PaintBox->Invalidate();
}


void TheMatrix::SetIgnoredPixelsFromFileName(const std::wstring file_name)
{
	MatrixIgnoredLayout->Load(file_name);

	PaintBox->Invalidate();
}


void TheMatrix::SaveIgnoredPixels(const std::wstring file_name)
{
	MatrixIgnoredLayout->Save(file_name, Details.Width, Details.Height);
}


void __fastcall TheMatrix::ClickPixelIgnoredPixel(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{
	if (Data->IsThisFrameLocked(0, CurrentFrame)) return;

	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0 || x1 > Details.Width - 1 || y1 > Details.Height - 1) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		if (MatrixIgnoredLayout->Grid[y1 * Details.Width + x1] == PixelAlive)
		{
			MatrixIgnoredLayout->Grid[y1 * Details.Width + x1] = PixelIgnored;
		}
		else
		{
			MatrixIgnoredLayout->Grid[y1 * Details.Width + x1] = PixelAlive;
		}

		LastX = x1;
		LastY = y1;
	}

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseMoveIgnoredPixel(TObject *Sender, TShiftState Shift, int X, int Y)
{
	int x1 = std::floor(X / Render.PixelSize);
	int y1 = std::floor(Y / Render.PixelSize);

	if (x1 < 0 || y1 < 0 || x1 > Details.Width - 1 || y1 > Details.Height - 1) return;

	x1 = std::floor(X / Render.PixelSize) + Render.TopLeft.X;
	y1 = std::floor(Y / Render.PixelSize) + Render.TopLeft.Y;

	// ===========================================================================

	if (OnMouseOver) OnMouseOver(x1, y1);

	// ===========================================================================

	if (Shift.Contains(ssLeft))
	{
		if (!(LastX == x1 && LastY == y1))
		{
			if (MatrixIgnoredLayout->Grid[y1 * Details.Width + x1] == PixelAlive)
			{
				MatrixIgnoredLayout->Grid[y1 * Details.Width + x1] = PixelIgnored;
			}
			else
			{
				MatrixIgnoredLayout->Grid[y1 * Details.Width + x1] = PixelAlive;
			}
		}
	}

	LastX = x1;
	LastY = y1;

	PaintBox->Invalidate();
}


void __fastcall TheMatrix::Shape1MouseUpIgnoredPixel(TObject *Sender, TMouseButton Button, TShiftState Shift, int X, int Y)
{

}


void TheMatrix::ToggleIgnoredPixels(bool hide)
{
    HideIgnoredPixels = hide;
}
#pragma end_region


#pragma region Drawing
int TheMatrix::GetPixelFrom(MatrixColourMode matrixformat, MatrixColourMode importformat, int pixel, int background)
{
	switch (matrixformat)
	{
	case MatrixColourMode::kMono:
		switch (importformat)
		{
		case MatrixColourMode::kMono:
			return pixel;
		case MatrixColourMode::kBiSequential:
		case MatrixColourMode::kBiBitplanes:
		case MatrixColourMode::kRGB:
		case MatrixColourMode::kRGB3BPP:
			if (pixel != background)
			{
				return 1;
			}
			break;

		default:
			break;
		}
        break;
	case MatrixColourMode::kBiSequential:
	case MatrixColourMode::kBiBitplanes:
		switch (importformat)
		{
		case MatrixColourMode::kMono:
		case MatrixColourMode::kBiSequential:
		case MatrixColourMode::kBiBitplanes:
			return pixel;
		case MatrixColourMode::kRGB:
		case MatrixColourMode::kRGB3BPP:
			if (pixel != background)
			{
				return 1;
			}
			break;

		default:
			break;
		}
		break;
	case MatrixColourMode::kRGB:
		switch (importformat)
		{
		case MatrixColourMode::kMono:
		case MatrixColourMode::kBiSequential:
		case MatrixColourMode::kBiBitplanes:
			if (pixel != background)
			{
				return 0xffffff;
			}
			break;
		case MatrixColourMode::kRGB:
			return pixel;
		case MatrixColourMode::kRGB3BPP:
			return LEDRGB3BPPColours[pixel];

		default:
			break;
		}
		break;
	case MatrixColourMode::kRGB3BPP:
		switch (importformat)
		{
		case MatrixColourMode::kMono:
		case MatrixColourMode::kBiSequential:
		case MatrixColourMode::kBiBitplanes:
			if (pixel != background)
			{
				return 0xffffff;
			}
			break;
		case MatrixColourMode::kRGB:
			for (int t = 0; t < 8; t++)
			{
				if (LEDRGB3BPPColours[t] == pixel)
				{
					return LEDRGB3BPPColours[t];
				}
			}
			break;
		case MatrixColourMode::kRGB3BPP:
			return pixel;

		default:
			break;
		}
		break;

	default:
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

		switch (Details.ColourMode)
		{
		case MatrixColourMode::kMono:
		case MatrixColourMode::kBiSequential:
		case MatrixColourMode::kBiBitplanes:
			PaintBox->OnPaint = PaintBoxUpdate;
			break;
		case MatrixColourMode::kRGB:
			PaintBox->OnPaint = PaintBoxUpdateRGB;
			break;
		case MatrixColourMode::kRGB3BPP:
			PaintBox->OnPaint = PaintBoxUpdateRGB_3BPP;
			break;

		default:
			break;
		}
	}
	else
	{
		if (IgnoredPixelsMode)
		{
			PaintBox->OnMouseDown = ClickPixelIgnoredPixel;
			PaintBox->OnMouseMove = Shape1MouseMoveIgnoredPixel;
			PaintBox->OnMouseUp   = Shape1MouseUpIgnoredPixel;

			PaintBox->OnPaint     = PaintBoxUpdateIgnoredPixel;
		}
		else
		{
			switch (Details.ColourMode)
			{
			case MatrixColourMode::kMono:
				PaintBox->OnMouseDown = ClickPixel;
				PaintBox->OnMouseMove = Shape1MouseMove;
				PaintBox->OnMouseUp   = Shape1MouseUp;

				PaintBox->OnPaint     = PaintBoxUpdate;
				break;
			case MatrixColourMode::kBiSequential:
				PaintBox->OnMouseDown = ClickPixelBiColour;
				PaintBox->OnMouseMove = Shape1MouseMoveBiColour;
				PaintBox->OnMouseUp   = Shape1MouseUpBiColour;

				PaintBox->OnPaint     = PaintBoxUpdate;
				break;
			case MatrixColourMode::kBiBitplanes:
				PaintBox->OnMouseDown = ClickPixelBiColour;
				PaintBox->OnMouseMove = Shape1MouseMoveBiColour;
				PaintBox->OnMouseUp   = Shape1MouseUpBiColour;

				PaintBox->OnPaint     = PaintBoxUpdate;
				break;
			case MatrixColourMode::kRGB:
				if (Details.DrawMode == MatrixDrawMode::kGrid)
				{
					PaintBox->OnMouseDown = ClickPixelRGB;
					PaintBox->OnMouseMove = Shape1MouseMoveRGB;
					PaintBox->OnMouseUp   = Shape1MouseUpRGB;

					PaintBox->OnPaint     = PaintBoxUpdateRGB;
				}
				else
				{
					PaintBox->OnMouseDown = ClickPixelRGBFF;
					PaintBox->OnMouseMove = Shape1MouseMoveRGBFF;
					PaintBox->OnMouseUp   = Shape1MouseUpRGBFF;

					PaintBox->OnPaint     = PaintBoxUpdateRGBFF;
				}
				break;
			case MatrixColourMode::kRGB3BPP:
				PaintBox->OnMouseDown = ClickPixelRGB;
				PaintBox->OnMouseMove = Shape1MouseMoveRGB;
				PaintBox->OnMouseUp   = Shape1MouseUpRGB;

				PaintBox->OnPaint     = PaintBoxUpdateRGB_3BPP;
				break;

			default:
				break;
			}
		}
	}
}


void TheMatrix::UpdateDrawTool(int setx, int sety, int setcolour, bool isgradient)
{
	Render.Action.Coords[Render.Action.Point].X = setx;
	Render.Action.Coords[Render.Action.Point].Y = sety;

	if (Render.Action.Point == CDrawPointNone)
	{
		Render.Action.Colour = setcolour;
	}

	BackupMatrix(CurrentLayer, CurrentFrame);

	switch (Render.Action.Mode)
	{
	case ActionMode::kFilledBox:
	case ActionMode::kEmptyBox:
	case ActionMode::kLine:
	case ActionMode::kEmptyCircle:
	case ActionMode::kFilledCircle:
		Render.Action.Point++;

		if (Render.Action.Point == CDrawPointLast)
		{
			DrawShape(false, Render.Action.Colour, isgradient);

			CopyDrawBufferToCurrentFrame();
		}
		break;
	case ActionMode::kCopy:
		Render.Action.Point++;

		if (Render.Action.Point == CDrawPointLast)
		{
			CopyCurrentFrameToDrawBuffer();

			CopyShape();
		}
		break;
	case ActionMode::kFloodFill:
		FloodFill(setx, sety, Render.Action.Colour);
		break;
	case ActionMode::kSpiral:
	case ActionMode::kRing:
	case ActionMode::kSplitRing:
	case ActionMode::kPetals:
	case ActionMode::kGrid:
	case ActionMode::kPyramid:
	case ActionMode::kLeftTriangle:
	case ActionMode::kRightTriangle:
		DrawShape(false, Render.Action.Colour, isgradient);

		CopyDrawBufferToCurrentFrame();
		break;

	default:
		break;
	}
}


void TheMatrix::ColourPixel(int colour)
{
	if (Render.ApplyToGroup)
	{
		Data->Layers[CurrentLayer]->Freeform->SetAllGroupTo(Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->Group,
															CurrentFrame,
                                                            colour);
	}
	else
	{
		Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->Colours[CurrentFrame] = colour;
	}
}


void TheMatrix::ColourPixelMulti(int colour)
{
	for (int f = 0; f < Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->Colours.size(); f++)
	{
		Data->Layers[CurrentLayer]->Freeform->Pixels[CurrentPixel]->Colours[f] = colour;
	}
}


void TheMatrix::DrawWithBrush(int index, int x, int y)
{
	if (Data->IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!Data->Layers[CurrentLayer]->Visible) return;

	if (x >= Details.Width || y >= Details.Height) return;

	switch (Render.Brush)
	{
	case BrushSize::kSmall:
		PlotPixelMatrix(x, y, index);

		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
		break;
	case BrushSize::kMedium:
		PlotPixelMatrix(x,     y,     index);
		PlotPixelMatrix(x + 1, y,     index);
		PlotPixelMatrix(x,     y + 1, index);
		PlotPixelMatrix(x + 1, y + 1, index);

		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
		break;
	case BrushSize::kLarge:
	case BrushSize::kBigLarge:
	case BrushSize::kSuperLarge:
	{
		int p = ConstantsHelper::PixelsFromBrushSize(Render.Brush);

		for (int a = 0; a < p; a++)
		{
			for (int b = 0; b < p; b++)
			{
				PlotPixelMatrix(x + a, y + b, index);
			}
		}

		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
		break;
	}

	default:
        ShowMessage(L"error brush size");
	}
}


// draws identical pixels on every frame
void TheMatrix::DrawWithBrushMulti(int index, int x, int y)
{
    if (x >= Details.Width || y >= Details.Height) return;

	for (int frame = 0; frame < Render.Action.Special; frame++)
	{
		if (!Data->IsThisFrameLocked(CurrentLayer, frame) &&
			Data->Layers[CurrentLayer]->Visible)
		{
			switch (Render.Brush)
			{
			case BrushSize::kSmall:
				PlotPixelMatrixFrame(frame, x, y, index);

				Data->Layers[CurrentLayer]->Cells[frame]->AddToHistory(DisplayBuffer);
				break;
			case BrushSize::kMedium:
				PlotPixelMatrixFrame(frame, x, y,         index);
				PlotPixelMatrixFrame(frame, x + 1, y,     index);
				PlotPixelMatrixFrame(frame, x, y + 1,     index);
				PlotPixelMatrixFrame(frame, x + 1, y + 1, index);

				Data->Layers[CurrentLayer]->Cells[frame]->AddToHistory(DisplayBuffer);
				break;
			case BrushSize::kLarge:
				for (int a = 0; a < 3; a++)
				{
					for (int b = 0; b < 3; b++)
					{
						PlotPixelMatrixFrame(frame, x + a, y + b, index);
					}
				}

				Data->Layers[CurrentLayer]->Cells[frame]->AddToHistory(DisplayBuffer);
				break;
            case BrushSize::kBigLarge:
			case BrushSize::kSuperLarge:
			{
				int p = ConstantsHelper::PixelsFromBrushSize(Render.Brush);

				for (int a = 0; a < p; a++)
				{
					for (int b = 0; b < p; b++)
					{
						PlotPixelMatrixFrame(frame, x + a, y + b, index);
					}
				}

				Data->Layers[CurrentLayer]->Cells[frame]->AddToHistory(DisplayBuffer);
				break;
			}
			}
		}
	}
}


void TheMatrix::DrawWithGradientBrush(int x, int y)
{
	if (Data->IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!Data->Layers[CurrentLayer]->Visible ||
		Gradient.size() == 0) return;

	if (x >= Details.Width || y >= Details.Height) return;

	PlotPixelMatrixFrame(CurrentFrame, x, y, Gradient[Render.Action.Parameter]);

	if (Render.Action.Parameter == Gradient.size() - 1)
	{
		Render.Action.Parameter = 0;
	}
	else
	{
		Render.Action.Parameter++;
	}

	Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
}


void TheMatrix::DrawWithBrushPaste(int x1, int y1, bool transparent)
{
	if (Data->IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!Data->Layers[CurrentLayer]->Visible) return;

	if (x1 >= Details.Width || y1 >= Details.Height) return;

	switch (Details.ColourMode)
	{
	case MatrixColourMode::kRGB:
	case MatrixColourMode::kRGB3BPP:
		for (int x2 = 0; x2 <= Render.Action.CopyPos.X; x2++)
		{
			for (int y2 = 0; y2 <= Render.Action.CopyPos.Y; y2++)
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
		for (int x2 = 0; x2 < Render.Action.CopyPos.X; x2++)
		{
			for (int y2 = 0; y2 < Render.Action.CopyPos.Y; y2++)
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

	Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
}


void TheMatrix::DrawWithBrushPasteEveryFrame(int x1, int y1, bool transparent)
{
    if (x1 >= Details.Width || y1 >= Details.Height) return;

	switch (Details.ColourMode)
	{
	case MatrixColourMode::kRGB:
	case MatrixColourMode::kRGB3BPP:
		for (int x2 = 0; x2 < Render.Action.CopyPos.X; x2++)
		{
			for (int y2 = 0; x2 < Render.Action.CopyPos.Y; y2++)
			{
				if (x2 + x1 >= 0 && x2 + x1 < Details.Width &&
					y2 + y1 >= 0 && y2 + y1 < Details.Height)
				{
					for (int frame = 0; frame < Data->Layers[CurrentLayer]->Cells.size(); frame++)
					{
						if (!Data->IsThisFrameLocked(CurrentLayer, frame))
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
		for (int x2 = 0; x2 < Render.Action.CopyPos.X; x2++)
		{
			for (int y2 = 0; y2 < Render.Action.CopyPos.Y; y2++)
			{
				if (x2 + x1 >= 0 && x2 + x1 < Details.Width &&
					y2 + y1 >= 0 && y2 + y1 < Details.Height)
				{
					for (int frame = 0; frame < Data->Layers[CurrentLayer]->Cells.size(); frame++)
					{
						if (!Data->IsThisFrameLocked(CurrentLayer, frame))
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

	Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);
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
// this takes into account the gradient status and allows for the drawing buffer and
// various other drawing modes.
void TheMatrix::PlotPixelMatrix(int x, int y, int defaultcolour)
{
	int colour = defaultcolour;
	int newcoord = 0;

	if ((true)) {

	}

	if (LastMouseButton == kMouseMiddle)
	{
		switch (Render.Gradient.Option)
		{
		case GradientOption::kOff:
			break;
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

	default:
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
	case GradientOption::kOff:
		break;
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

		default:
			break;
		}
	}
	else
	{
		Data->Layers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + x] = colour;

		switch (Mirror)
		{
		case MirrorMode::kHorizontal:
			Data->Layers[CurrentLayer]->Cells[frame]->Grid[newcoord * Details.Width + x] = colour;
			break;
		case MirrorMode::kVertical:
			Data->Layers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + newcoord] = colour;
			break;

		default:
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
		case GradientOption::kOff:
			break;
		case GradientOption::kVertical:
			colour = Render.Gradient.IY[y1];
			break;
		case GradientOption::kHorizontal:
			colour = Render.Gradient.IX[x1];
			break;

		default:
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
		Render.Action.Coords[1].X = LastX;
		Render.Action.Coords[1].Y = LastY;
	}

	int x1 = Render.Action.Coords[0].X;
	int y1 = Render.Action.Coords[0].Y;
	int x2 = Render.Action.Coords[1].X;
	int y2 = Render.Action.Coords[1].Y;

	// =======================================================================

	switch (Render.Action.Mode)
	{
	// =======================================================================
	// == Filled Box =========================================================
	// =======================================================================
	case ActionMode::kFilledBox:
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
	// =======================================================================
	// == Empty Box ==========================================================
	// =======================================================================
	case ActionMode::kEmptyBox:
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
	// =======================================================================
	// == Straight Line ======================================================
	// =======================================================================
	case ActionMode::kLine:
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
	// =======================================================================
	// == Empty Circle =======================================================
	// =======================================================================
	case ActionMode::kEmptyCircle:
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
	// =======================================================================
	// == Filled Circle ======================================================
	// =======================================================================
	case ActionMode::kFilledCircle:
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
	// =======================================================================
	// == Copy Lasso thing ===================================================
	// =======================================================================
	case ActionMode::kCopy:
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

		PaintBox->Canvas->Brush->Color = TColor(LEDColours[kDisplayMarker]);

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
	// =======================================================================
	// == Patterns: Spiral ===================================================
	// =======================================================================
	case ActionMode::kSpiral:
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

				b += Render.Action.Parameter;
			}
		}

		break;
	}
	case ActionMode::kRing:
	{
		int y = LastY;

		for (int x = 0; x < Details.Width; x++)
		{
			PlotPixelMatrix(x, y, colour);
		}

		break;
	}
	case ActionMode::kSplitRing:
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

		d = Render.Action.Parameter; // count between pixels X000X = 4

		while (x != a)
		{
			if (d == Render.Action.Parameter)
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
	case ActionMode::kPetals:
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

		int d = Render.Action.Parameter;

		while (x != a)
		{
			if (d == Render.Action.Parameter)
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
	case ActionMode::kGrid:
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

		int d = Render.Action.Parameter; // count between pixels X000X = 4

		while (x != a)
		{
			if (d == Render.Action.Parameter)
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

		d = Render.Action.Parameter;

		while (y != b)
		{
			if (d == Render.Action.Parameter)
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
	case ActionMode::kPyramid:
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
			y += Render.Action.Parameter;
		}

		break;
	}
	case ActionMode::kLeftTriangle:
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
			y += Render.Action.Parameter;
		}

		break;
	}
	case ActionMode::kRightTriangle:
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
			y += Render.Action.Parameter;
		}

		break;
	}
	case ActionMode::kLeftAngleLine:
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

			x++;
			y += Render.Action.Parameter;
		}
		break;
	}
	case ActionMode::kRightAngleLine:
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
			y += Render.Action.Parameter;
		}
		break;
	}

	default:
		break;
    }

	if (!realtime)
	{
		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory(DisplayBuffer);

		if (OnChange) OnChange(this);

		Render.Action.Point       = CDrawPointNone;
		Render.Action.Coords[0].X = -1;
		Render.Action.Coords[0].Y = -1;

		PaintBox->Invalidate();
	}
}


void TheMatrix::FloodFill(int x, int y, int fillcolour)
{
	if (fillcolour != DisplayBuffer->Grid[y * Details.Width + x])
	{
		Busy = true;

		DoFill(x, y, fillcolour);

		Busy = false;

		Render.Action.Coords[0].X = - 1;

		CopyDrawBufferToCurrentFrame();

		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();

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
	Render.Action.Mode = ActionMode::kNone;
	Render.Action.Point = CDrawPointNone;
	Render.Action.Coords[0].X = -1;
	Render.Action.Coords[0].Y = -1;
	Render.Action.CopyPos.X = 0;
	Render.Action.CopyPos.Y = 0;

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
    }
}


void TheMatrix::CopyShape()
{
	for (int z = 0; z < Details.Width * Details.Height; z++)
	{
		if (Details.ColourMode == MatrixColourMode::kRGB)
		{
			MatrixCopy->Grid[z] = RGBBackground;
		}
		else
		{
			MatrixCopy->Grid[z] = 0;
		}
	}

	if (Render.Action.Coords[0].X > Render.Action.Coords[1].X)
	{
		std::swap(Render.Action.Coords[0].X, Render.Action.Coords[1].X);
	}

	if (Render.Action.Coords[0].Y > Render.Action.Coords[1].Y)
	{
		std::swap(Render.Action.Coords[0].Y, Render.Action.Coords[1].Y);
	}

	Render.Action.CopyPos.X = Render.Action.Coords[1].X - Render.Action.Coords[0].X;
	Render.Action.CopyPos.Y = Render.Action.Coords[1].Y - Render.Action.Coords[0].Y;

	for (int x = Render.Action.Coords[0].X; x <= Render.Action.Coords[1].X; x++)
	{
		for (int y = Render.Action.Coords[0].Y; y <= Render.Action.Coords[1].Y; y++)
		{
			MatrixCopy->Grid[(y - Render.Action.Coords[0].Y) * Details.Width + (x - Render.Action.Coords[0].X)] = Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid[y * Details.Width + x];
		}
	}

	Render.Action.Point       = CDrawPointNone;
	Render.Action.Mode        = ActionMode::kPaste;
	Render.Action.Coords[0].X = -1;
	Render.Action.Coords[0].Y = -1;
}
#pragma end_region


#pragma region Brush
void TheMatrix::RotateCopyBrush(int mode)
{
	if (Render.Action.CopyPos.X == Render.Action.CopyPos.Y)
	{
		BackupMatrix();

		switch (mode)
		{
		case kEffectRotateCW:
			for (int x = 0; x <= Render.Action.CopyPos.X; x++)
			{
				for (int y = 0; y <= Render.Action.CopyPos.Y; y++)
				{
					MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[(Render.Action.CopyPos.X - x) * Details.Width + y];
				}
			}
			break;
		case kEffectRotateACW:
			for (int x = 0; x <= Render.Action.CopyPos.X; x++)
			{
				for (int y = 0; y <= Render.Action.CopyPos.Y; y++)
				{
					MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[x * Details.Width + (Render.Action.CopyPos.Y - y)];
				}
			}
			break;
		}

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformEffectOnBrush(int mode)
{
	BackupMatrix();

	switch (mode)
	{
	case kEffectFlip:
		for (int x = 0; x <= Render.Action.CopyPos.X; x++)
		{
			for (int y = 0; y <= Render.Action.CopyPos.Y; y++)
			{
				MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[y * Details.Width + (Render.Action.CopyPos.X - x)];
			}
		}
		break;
	case kEffectMirror:
		for (int y = 0; y <= Render.Action.CopyPos.X; y++)
		{
			for (int x = 0; x <= Render.Action.CopyPos.Y; x++)
			{
				MatrixCopy->Grid[y * Details.Width + x] = MatrixBackup->Grid[(Render.Action.CopyPos.Y - y) * Details.Width + x];
			}
		}
		break;
	case kEffectInvert:
		for (int x = 0; x <= Render.Action.CopyPos.X; x++)
		{
			for (int y = 0; y <= Render.Action.CopyPos.Y; y++)
			{
				switch (Details.ColourMode)
				{
				case MatrixColourMode::kMono:
					MatrixCopy->Grid[y * Details.Width + x] = 1 - MatrixBackup->Grid[y * Details.Width + x];
					break;
				case MatrixColourMode::kBiSequential:
				case MatrixColourMode::kBiBitplanes:
					MatrixCopy->Grid[y * Details.Width + x] = 3 - MatrixBackup->Grid[y * Details.Width + x];
					break;
				case MatrixColourMode::kRGB:
					MatrixCopy->Grid[y * Details.Width + x] = 0xFFFFFF - MatrixBackup->Grid[y * Details.Width + x];
					break;
				case MatrixColourMode::kRGB3BPP:
					MatrixCopy->Grid[y * Details.Width + x] = 0x000004 - MatrixBackup->Grid[y * Details.Width + x];
					break;

				default:
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


#pragma region Gradient_Brush
void TheMatrix::ClearGradient()
{
	Gradient.clear();
}


void TheMatrix::AddGradient(int colour)
{
	Gradient.push_back(colour);
}


int TheMatrix::GradientBrushCount()
{
    return Gradient.size();
}
#pragma end_region


#pragma region Font
void TheMatrix::AddFontCharacter(int ascii, int frame)
{
	const int __FontWidth = 8;
	const int __FontHeight = 8;

	int startY = Render.Action.Coords[0].Y;
	int current_x = Render.Action.Coords[0].X;
	bool canwrite = true;

	int group_id = Data->Layers[CurrentLayer]->Freeform->NextGroupId;
	Data->Layers[CurrentLayer]->Freeform->NextGroupId++;

	for (int x = TextFont->Start[ascii]; x <= TextFont->End[ascii]; x++)
	{
		for (int y = 0; y < 8; y++)
		{
			int outputy = startY - (y * (Render.PixelSizeZ + 1));

			//canwrite = (Render.Action.Coords[0].X >= 0 &&
			//			Render.Action.Coords[0].X < Details.Width &&
			//			y >= 0 && startY - y < Details.Height);

			if (canwrite)
			{
				int data_index = (ascii * __FontWidth * __FontHeight) + (y * __FontWidth + x);

				if (Details.ColourMode == MatrixColourMode::kRGB)
				{
					switch (TextFont->Mode)
					{
					case MatrixColourMode::kNone:
					case MatrixColourMode::kBiSequential:
					case MatrixColourMode::kBiBitplanes:
						break;
					case MatrixColourMode::kMono:
						if (TextFont->Data[data_index] == 1)
						{
							MatrixPixel *mp = new MatrixPixel(current_x, outputy,
															  Data->Layers[CurrentLayer]->Freeform->Frames.size(),
															  Data->Layers[CurrentLayer]->Freeform->Pixels.size(),
															  group_id,
															  Render.Action.Colour);

							Data->Layers[CurrentLayer]->Freeform->Pixels.push_back(mp);
						}
						break;
					case MatrixColourMode::kRGB:
						if (TextFont->Data[data_index] != -1)
						{
							MatrixPixel *mp = new MatrixPixel(current_x, outputy,
															  Data->Layers[CurrentLayer]->Freeform->Frames.size(),
															  Data->Layers[CurrentLayer]->Freeform->Pixels.size(),
															  group_id,
															  TextFont->Data[data_index]);

							Data->Layers[CurrentLayer]->Freeform->Pixels.push_back(mp);
						}
						break;

					default:
						break;
					}
				}
			}
		}

		current_x += Render.PixelSizeZ + 1;
	}

	Render.Action.Coords[0].X = current_x + 3;

    Data->Layers[CurrentLayer]->Freeform->EnsurePixelCoherence();

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::DrawFontCharacter(int ascii, int frame)
{
	const int __FontWidth = 8;
	const int __FontHeight = 8;

	int startY = Render.Action.Coords[0].Y;
	std::wstring temp = L"";
	bool canwrite = true;

	for (int x = TextFont->Start[ascii]; x <= TextFont->End[ascii]; x++)
	{
		for (int y = 0; y < 8; y++)
		{
			int outputx = Render.Action.Coords[0].X;
			int outputy = startY - y;

			if (FontWrap)
			{
				if (outputx > Details.Width - 1)
				{
					outputx = outputx - Details.Width;
					Render.Action.Coords[0].X = outputx;
				}

				if (outputy < 0)
				{
					outputy = outputy + Details.Height;
				}
			}
			else
			{
				canwrite = (Render.Action.Coords[0].X >= 0 &&
					Render.Action.Coords[0].X < Details.Width &&
					y >= 0 && startY - y < Details.Height);
			}

			if (canwrite)
			{
				int data_index = (ascii * __FontWidth * __FontHeight) + (y * __FontWidth + x);

				if (Details.ColourMode == MatrixColourMode::kRGB)
				{
					switch (TextFont->Mode)
					{
					case MatrixColourMode::kNone:
					case MatrixColourMode::kBiSequential:
					case MatrixColourMode::kBiBitplanes:
						break;
					case MatrixColourMode::kMono:
						if (TextFont->Data[data_index] == 1)
						{
							Data->Layers[CurrentLayer]->Cells[frame]->Grid[outputy * Details.Width + outputx] = Render.Action.Colour;
						}
						break;
					case MatrixColourMode::kRGB:
						if (TextFont->Data[data_index] != -1)
						{
							Data->Layers[CurrentLayer]->Cells[frame]->Grid[outputy * Details.Width + outputx] = TextFont->Data[data_index];
						}
						break;

					default:
						break;
					}
				}
				else
				{
					switch (TextFont->Mode)
					{
					case MatrixColourMode::kNone:
					case MatrixColourMode::kBiSequential:
					case MatrixColourMode::kBiBitplanes:
						break;
					case MatrixColourMode::kMono:
						if (TextFont->Data[data_index] == 1)
						{
							Data->Layers[CurrentLayer]->Cells[frame]->Grid[outputy * Details.Width + outputx] = Render.Action.Colour;
						}
						break;
					case MatrixColourMode::kRGB:
						if (TextFont->Data[data_index] != -1)
						{
							Data->Layers[CurrentLayer]->Cells[frame]->Grid[outputy * Details.Width + outputx] = Render.Action.Colour;
						}
						break;

					default:
						break;
					}
				}
			}
		}

		Render.Action.Coords[0].X++;
	}

	Render.Action.Coords[0].X++; 		// adds single column spacing between chars

	if (OnChange) OnChange(this);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}


void TheMatrix::DeleteFontCharacter(int frame)
{
	Render.Action.Coords[0].X--;

	for (int y = Render.Action.Coords[0].Y; y >= Render.Action.Coords[0].Y - 7; y--)
	{
		if (Render.Action.Coords[0].X >= 0 &&
			Render.Action.Coords[0].X < Details.Width &&
			y >= 0 && y < Details.Height)
		{
			Data->Layers[CurrentLayer]->Cells[frame]->Grid[y * Details.Width + Render.Action.Coords[0].X] = 0;
		}
	}

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::LoadTextToolFont(const std::wstring file_name, const std::wstring name)
{
	TextFont->Load(file_name, name);
}
#pragma end_region


#pragma region Frame
void TheMatrix::InsertBlankFrameAt(int insertat)
{
	if (!AutomateMode) Busy = true;

	Data->InsertBlankFrameAt(insertat);

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

	Data->InsertCopyFrameAt(CurrentFrame, insertat);

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
	if (frame == 0 && Data->GetFrameCount() == 1) return;

	CurrentFrame = Data->DeleteFrame(frame, CurrentFrame);

	if (OnNewFrameDisplayed) OnNewFrameDisplayed(this);

	if (OnSizeChange) OnSizeChange(this);

    if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
    }

	PaintBox->Invalidate();
}


void TheMatrix::CopyFromPrevious(int frame_to)
{
	if (frame_to > 0)
	{
		if (Details.DrawMode == MatrixDrawMode::kGrid)
		{
			for (int layer = 0; layer < Data->Layers.size(); layer++)
			{
				std::memcpy(Data->Layers[layer]->Cells[frame_to]->Grid, Data->Layers[layer]->Cells[frame_to - 1]->Grid, Details.Width * Details.Height * sizeof(int));
			}

			CopyCurrentFrameToDrawBuffer();
		}
		else
		{
			for (int layer = 0; layer < Data->Layers.size(); layer++)
			{
				Data->Layers[layer]->Freeform->CopyFromPrevious(frame_to);
			}
        }

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

		Busy = true;
	}

	for (int layer = 0; layer < Data->Layers.size(); layer++)
	{
		std::memcpy(Data->Layers[layer]->Cells[frame_to]->Grid, Data->Layers[layer]->Cells[frame_from]->Grid, Details.Width * Details.Height * sizeof(int));
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

		Data->AddLayer(name);

		Busy = false;

		SetCurrentLayer(Data->Layers.size() - 1);

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

        Data->AddLayerAsCopy(name, copylayer);

		Busy = false;

		SetCurrentLayer(Data->Layers.size() - 1);

		if (OnLayerChange) OnLayerChange(this);

        return true;
	}

	return false;
}


bool TheMatrix::DeleteLayer(int index)
{
	if (Data->Layers.size() > 1)
	{
		if (Details.DrawMode == MatrixDrawMode::kGrid)
		{
			CopyDrawBufferToCurrentFrame();
		}

		Data->DeleteLayer(index);

		if (OnLayerChange) OnLayerChange(this);

		return true;
	}

	return false;
}


void TheMatrix::ClearCurrentLayerAllFrames()
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		DisplayBuffer->Clear(Details.ColourMode, RGBBackground);
	}

	Data->ClearLayerAllFrames(CurrentLayer);

	PaintBox->Invalidate();

	if (OnChange) OnChange(this);
}


void TheMatrix::FlattenAllLayers()
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyDrawBufferToCurrentFrame();

		Busy = true;

		MatrixMerge = new MatrixGrid(Details.Width, Details.Height, Details.ColourMode, RGBBackground);

		for (int l = 0; l < Data->Layers.size(); l++)
		{
			Data->Layers[l]->Visible = true;
		}

		for (int f = 0; f < Data->Layers[kPermanentLayer]->Cells.size(); f++)
		{
			BuildMergedFrame(f, MergeFrameMode::kRetainGridValue);

			std::memcpy(Data->Layers[kPermanentLayer]->Cells[f]->Grid, MatrixMerge->Grid, Details.Width * Details.Height * sizeof(int));
		}

		while (Data->Layers.size() > 1)
		{
			Data->Layers.pop_back();
		}

		delete MatrixMerge;

		CurrentFrame = 0;
		CurrentLayer = 0;

		Busy = false;

		CopyCurrentFrameToDrawBuffer();
	}
	else
	{
        if (Data->Layers.size() <= 1) return;

		while (Data->Layers.size() > 1)
		{
			for (int t = 0; t < Data->Layers.back()->Freeform->Pixels.size(); t++)
			{
				Data->Layers[0]->Freeform->Pixels.push_back(Data->Layers.back()->Freeform->Pixels[t]);
			}

			Data->Layers.pop_back();
		}
	}

	if (OnLayerChange) OnLayerChange(this);
}


bool TheMatrix::IsVisible(int index)
{
	return Data->Layers[index]->Visible;
}


void TheMatrix::SetVisibility(int LayerIndex, bool Visibility)
{
	#if _DEBUG
	if (LayerIndex >= Data->Layers.size())
	{
		std::wstring debug = L"Layer " + std::to_wstring(LayerIndex) + L" outside the valid layer limit of 0 to " + std::to_wstring(Data->Layers.size() - 1);

		ShowMessage(debug.c_str());
	}
	#endif

	Data->Layers[LayerIndex]->Visible = Visibility;

    PaintBox->Invalidate();
}


void TheMatrix::MoveUp(int LayerIndex)
{
	if (LayerIndex == CurrentFrame)
	{
		if (Details.DrawMode == MatrixDrawMode::kGrid)
		{
			CopyCurrentFrameToDrawBuffer();
        }
	}

	Busy = true;

	std::swap(Data->Layers[LayerIndex], Data->Layers[LayerIndex + 1]);

	CurrentLayer = LayerIndex + 1;

	Busy = false;

	PaintBox->Invalidate();

	if (OnLayerChange) OnLayerChange(this);
}


void TheMatrix::MoveDown(int LayerIndex)
{
	if (LayerIndex == CurrentFrame)
	{
		if (Details.DrawMode == MatrixDrawMode::kGrid)
		{
			CopyCurrentFrameToDrawBuffer();
        }
	}

	Busy = true;

	std::swap(Data->Layers[LayerIndex], Data->Layers[LayerIndex - 1]);

	CurrentLayer = LayerIndex - 1;

	Busy = false;

	PaintBox->Invalidate();

	if (OnLayerChange) OnLayerChange(this);
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

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		std::memcpy(Data->Layers[destination]->Cells[frame_to]->Grid, Data->Layers[source]->Cells[frame_from]->Grid, Details.Width * Details.Height * sizeof(int));
	}
	else
	{
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
		switch (Details.ColourMode)
		{
		case MatrixColourMode::kMono:
			LEDColours[t] = LEDColoursSingle[t];
			break;
		case MatrixColourMode::kBiSequential:
		case MatrixColourMode::kBiBitplanes:
			LEDColours[t] = LEDColoursBi[t];
			break;

		default:
			break;
		}
	}

	PaintBox->Invalidate();
}


void TheMatrix::ChangeSelectionColour(int LMB, int MMB, int RMB)
{
	SetMouseButtonColours(LMB, MMB, RMB);

	LEDRGBColours[kMouseLeft]   = LMB;
	LEDRGBColours[kMouseMiddle] = MMB;
	LEDRGBColours[kMouseRight]  = RMB;

	if (OnColourChange) OnColourChange(this);
}


void TheMatrix::GradientFillFrame()
{
	if (!Details.Available) return;

	Data->GradientFillFrame(CurrentLayer, CurrentFrame, Render.Gradient, LEDColours);

	if (OnChange) OnChange(this);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}


void TheMatrix::ChangePixels(int colour_from, int colour_to)
{
	for (int frame = 0; frame < Data->GetFrameCount(); frame++)
	{
		if (Details.DrawMode == MatrixDrawMode::kGrid)
		{
			Data->Layers[CurrentLayer]->Cells[frame]->ChangePixels(colour_from, colour_to);
		}
		else
		{
            Data->Layers[CurrentLayer]->Freeform->ChangePixels(frame, colour_from, colour_to);
		}
	}

	if (OnChange) OnChange(this);

    if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
    }

	PaintBox->Invalidate();
}


void TheMatrix::FadeFirstToLast()
{
	if (Data->GetFrameCount() == 1) return;

    Data->FadeFirstToLast(CurrentLayer);

	PaintBox->Invalidate();
}


// change colours in the current layer of the currently frame only
void TheMatrix::ChangeColourCurrent(int colour_from, int colour_to)
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyDrawBufferToCurrentFrame();

		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->ChangePixels(colour_from, colour_to);

		CopyCurrentFrameToDrawBuffer();
	}
	else
	{
		Data->Layers[CurrentLayer]->Freeform->ChangePixels(CurrentFrame, colour_from, colour_to);
	}

	PaintBox->Invalidate();
}


// change colours in all frames of the current layer
void TheMatrix::ChangeColourCurrentLayer(int colour_from, int colour_to)
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyDrawBufferToCurrentFrame();
	}

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		for (int frame = 0; frame < Data->Layers[CurrentLayer]->Cells.size(); frame++)
		{
			Data->Layers[CurrentLayer]->Cells[frame]->ChangePixels(colour_from, colour_to);
		}
	}
	else
	{
		for (int frame = 0; frame < Data->Layers[CurrentLayer]->Freeform->Frames.size(); frame++)
		{
			Data->Layers[CurrentLayer]->Freeform->ChangePixels(frame, colour_from, colour_to);
		}
	}

    if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
	}

	PaintBox->Invalidate();
}


// change the colours in all layers and all frames
void TheMatrix::ChangeColourAll(int colour_from, int colour_to)
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyDrawBufferToCurrentFrame();
	}

    Data->ChangeColourAll(colour_from, colour_to);

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
	}

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region MatrixIO
std::wstring TheMatrix::RowToString(int frame, int row)
{
	std::wstring s = L"";

	for (int x = 0; x < Details.Width; x++)
	{
		s += IntToHex(Data->Layers[CurrentLayer]->Cells[frame]->Grid[row * Details.Width + x], 6).c_str();

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
						Data->Layers[CurrentLayer]->Cells[frame]->Grid[row * Details.Width + x] = colour;
					}
				}
				else
				{
					Data->Layers[CurrentLayer]->Cells[frame]->Grid[row * Details.Width + x] = colour;
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
#pragma end_region


#pragma region FileIO_Bitmap
// returns false if more than 8 colours (the limit for 3bpp) are found in the source image
bool TheMatrix::ProcessRGB3bppColours(TCanvas* canvas, std::vector<int> &rgb3ppcolours, int width, int height)
{
	for (int x = 0; x < width; x++)
	{
		for (int y = 0; y < height; y++)
		{
			rgb3ppcolours.push_back(canvas->Pixels[x][y]);
		}
	}

	std::sort(rgb3ppcolours.begin(), rgb3ppcolours.end());

	auto last = std::unique(rgb3ppcolours.begin(), rgb3ppcolours.end());
	rgb3ppcolours.erase(last, rgb3ppcolours.end());

	if (rgb3ppcolours.size() > 8)
	{
		std::wstring m = L"Too many colours in image (" + std::to_wstring(rgb3ppcolours.size()) + L"). Exiting :(";

		ShowMessage(m.c_str());

		return false;
	}
	else
	{
		for (int t = 0; t < rgb3ppcolours.size(); t++)
		{
			LEDRGB3BPPColours[t] = rgb3ppcolours[t];
		}

		if (OnNew3bppColours) OnNew3bppColours(this);
	}

	return true;
}


void TheMatrix::ImportFromFrame(TCanvas* canvas, ImportColourMode icm, int width, int height, int frame, int offset, std::vector<int> &rgb3ppcolours)
{
	for (int x = 0; x < width; x++)
	{
		for (int y = 0; y < height; y++)
		{
			switch (icm)
			{
				case ImportColourMode::kMono:
					if (canvas->Pixels[offset + x][y] == clBlack)
					{
						Data->Layers[CurrentLayer]->Cells[frame]->Grid[y * width + x] = 0;
					}
					else
					{
						Data->Layers[CurrentLayer]->Cells[frame]->Grid[y * width + x] = 1;
					}
					break;
				case ImportColourMode::kRGB:
					Data->Layers[CurrentLayer]->Cells[frame]->Grid[y * width + x] = canvas->Pixels[offset + x][y];
					break;
				case ImportColourMode::kRGB3bpp:
					auto it = std::find(rgb3ppcolours.begin(), rgb3ppcolours.end(), canvas->Pixels[offset + x][y]);

					Data->Layers[CurrentLayer]->Cells[frame]->Grid[y * width + x] = it - rgb3ppcolours.begin();
					break;
			}
		}
	}
}


ImportData TheMatrix::ImportFromBMPSingleImage(const std::wstring file_name, int count, int width, int height, ImportColourMode icm, bool createnew)
{
	ImportData import;

	TBitmap *bmp = new TBitmap();
	bmp->LoadFromFile(file_name.c_str());

	if (!bmp->Empty)
	{
		std::vector<int> rgb3ppcolours;

		if (icm == ImportColourMode::kRGB3bpp)
		{
			if (!ProcessRGB3bppColours(bmp->Canvas, rgb3ppcolours, width * count, height))
			{
				return import;
			}
		}

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

			if (Data->Layers[CurrentLayer]->Cells.size() < frame + 1)
			{
				MatrixGrid *matrix = new MatrixGrid(width,height, Details.ColourMode, RGBBackground);
				Data->Layers[CurrentLayer]->Cells.push_back(matrix);
			}

			ImportFromFrame(bmp->Canvas, icm, width, height, frame, wo, rgb3ppcolours);

			if (frame == CurrentFrame)
			{
				CopyCurrentFrameToDrawBuffer();
			}
		}

		// ===========================================================================

        import.ImportOk = true;
		import.NewWidth = width;
		import.NewHeight = height;

		PaintBox->Invalidate();

		Details.Available = true;

		if (OnChange) OnChange(this);

		if (OnLayerChange) OnLayerChange(this);
	}

	delete bmp;

	return import;
}


ImportData TheMatrix::ImportFromBMPMultipleImage(std::wstring pattern, int startframe, int count, int padlength, int fwidth, int fheight, ImportColourMode icm, bool createnew)
{
	ImportData import;

	std::wstring file_name = L"";

	std::vector<int> rgb3ppcolours;

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

		if (icm == ImportColourMode::kRGB3bpp && i == 0)
		{
			if (!ProcessRGB3bppColours(bmp->Canvas, rgb3ppcolours, fwidth, fheight))
			{
				return import;
			}
		}

		if (createnew)
		{
			frame = startframe + i;
		}
		else
		{
			frame = CurrentFrame + i;
		}

		// ===================================================================

		if (frame > Data->Layers[CurrentLayer]->Cells.size() - 1)
		{
			MatrixGrid *matrix = new MatrixGrid(fwidth, fheight, Details.ColourMode, RGBBackground);

			Data->Layers[CurrentLayer]->Cells.push_back(matrix);
		}

		ImportFromFrame(bmp->Canvas, icm, fwidth, fheight, frame, 0, rgb3ppcolours);

		delete bmp;
	}

	// =======================================================================

	import.NewWidth = fwidth;
	import.NewHeight = fheight;
	import.NewFrames = count;

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
	bitmap->Width = Data->GetFrameCount() * Details.Width;
	bitmap->Height = Details.Height;

	try
	{
		TRGBTriple *ptr;

		for (int frame = 0; frame < Data->GetFrameCount(); frame++)
		{
			BuildMergedFrame(frame, MergeFrameMode::kConvertForFileOutput);

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
	else
	{
        return false;
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

		for (int frame = 0; frame < Data->GetFrameCount(); frame++)
		{
			BuildMergedFrame(frame, MergeFrameMode::kConvertForFileOutput);

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
	ClearAllMatrixData(false, 0, 0);

	// ===========================================================================

	ImportData import;

	import.ImportOk        = true;
	import.Source          = -1;
	import.SourceLSB       = -1;
	//  Result.SourceDirection = -1;
	import.ColourMode      = MatrixColourMode::kMono;
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
			import.ImportOk    = false;
			import.ErrorString = GLanguageHandler->Text[kErrorWhileLoadingThisGIF];

            delete lGIF;

			return import;
		}

		TBitmap *lTempFrame = new TBitmap();
		lTempFrame->PixelFormat = pf24bit;

		TGIFRenderer *lGR = new TGIFRenderer(lGIF);
        lGR->Animate = true;

		int gifHeight    = lGIF->Height;
		int gifWidth     = lGIF->Width;

		if (gifWidth > __MaxWidth || gifHeight > __MaxHeight)
		{
			import.ImportOk    = false;
			import.ErrorString = GLanguageHandler->Text[kGIFDimensionsAreTooLarge] + L" " + std::to_wstring(gifWidth) + L" x " + std::to_wstring(gifHeight) + L").";

			delete lGIF;
			delete lGR;
			delete lTempFrame;

			return import;
		}

		Details.Width  = gifWidth;
		Details.Height = gifHeight;

		try
		{
			lTempFrame->SetSize(gifWidth, gifHeight);

			TRGBTriple *ptr;

			lTempFrame->Canvas->Lock();

			for (int t = 0; t < lGIF->Images->Count; t++)
			{
				if (lGIF->Images->Frames[t]->Empty)
				{
					lGR->NextFrame();

					continue;	// ignore bad frames
				}

				for (int layer = 0; layer < Data->Layers.size(); layer++)
				{
					MatrixGrid *m = new MatrixGrid(lGIF->Width, lGIF->Height, MatrixColourMode::kRGB, RGBBackground);

					Data->Layers[layer]->Cells.push_back(m);
				}

				try
				{
					lGR->Draw(lTempFrame->Canvas, lTempFrame->Canvas->ClipRect);

					for (int y = 0; y < lGIF->Height; y++)
					{
						ptr = reinterpret_cast<TRGBTriple *>(lTempFrame->ScanLine[y]);

						for (int x = 0; x < lGIF->Width; x++)
						{
							Data->Layers[CurrentLayer]->Cells.back()->Grid[y * Details.Width + x] = (ptr[x].rgbtBlue << 16) + (ptr[x].rgbtGreen << 8) + (ptr[x].rgbtRed);
						}
					}
				}
				catch(...)
				{

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

	import.ColourMode = MatrixColourMode::kRGB;
	import.NewWidth = Details.Width;
	import.NewHeight = Details.Height;
	import.BackgroundColour = RGBBackground;

	import.MaxFrames = Data->Layers[kPermanentLayer]->Cells.size();
	import.FontMode = false;

	Details.Available = true;

	if (OnLayerChange) OnLayerChange(this);

	PaintBox->Invalidate();

	return import;
}


// if you decide to tweak the export yourself then don't bother with the Embarcadero docs, they are worse
// than useless. open Vcl.Imaging.GIFImg and examine the code to see how things are done!
void TheMatrix::ExportToGIF(const std::wstring file_name, int background, int pixelsize, int pixelshape, int animationspeed)
{
	TGIFImage *lGIF = new TGIFImage();

	lGIF->Animate     = true;
	lGIF->AnimateLoop = glContinously;

	try
	{
		for (int frame = 0; frame < Data->Layers[kPermanentLayer]->Cells.size(); frame++)
		{
			TBitmap *lTempFrame = new TBitmap();

			lTempFrame->Width  = Details.Width * pixelsize;
			lTempFrame->Height = Details.Height * pixelsize;

			lTempFrame->Canvas->Brush->Color = TColor(background);
			lTempFrame->Canvas->FillRect(Rect(0, 0, lTempFrame->Width, lTempFrame->Height));

			BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

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
															  pixelsize - (std::round(pixelsize / kRoundRectCoeff)),
															  pixelsize - (std::round(pixelsize / kRoundRectCoeff)));
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

			delete lTGI;
			delete lTempFrame;
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
ImportData TheMatrix::LoadProject(const std::wstring file_name, ExportOptions &eeo, LoadMode loadmode, int startframe)
{
	if (file_name.find(L".leds2") != std::wstring::npos)
	{
		return LoadProjectFreeform(file_name, eeo, loadmode, startframe);
	}
	else if (file_name.find(L".leds") != std::wstring::npos)
	{
		return LoadProjectGrid(file_name, eeo, loadmode, startframe);
	}

	ImportData id;

	return id;
}


ImportData TheMatrix::LoadProjectGrid(const std::wstring file_name, ExportOptions &eeo, LoadMode loadmode, int startframe)
{
	auto SafeStringToBool = [](const std::wstring s) -> bool
	{
		if (s.empty() || s == L"0")
		{
			return false;
		}

        return true;
	};

	MatrixReadOnly = true;

    Busy = true;

	int importLayer = CurrentLayer;
	int importFrame = CurrentFrame;
	int initialframe = 0;

	switch (loadmode)
	{
	case LoadMode::kNew:
		ClearAllMatrixData(false, 0, 0);

		importFrame = 0;
		importLayer = -1;
		break;
	case LoadMode::kMergeBottomPriority:
	case LoadMode::kMergeTopPriority:
		importFrame = startframe;
		break;
	case LoadMode::kAppend:
		importFrame = Data->Layers[kPermanentLayer]->Cells.size();
		break;
	case LoadMode::kMergeNewLayer:
	{
		importFrame = startframe;

		std::wstring name = ExtractFileName(file_name.c_str()).c_str();

		Data->AddLayerSilent(L"Merge from " + name);

		importLayer = Data->Layers.size() - 1;
		break;
	}
	case LoadMode::kMergeCurrentLayer:
		importLayer = CurrentLayer;
		importFrame = 0;
		break;
	}

	initialframe = importFrame;

	// =======================================================================
	// =======================================================================

	ImportData import;
	import.ImportOk = true;
	import.DrawMode = MatrixDrawMode::kGrid;
	import.ColourMode = MatrixColourMode::kMono;
	import.RGBBrightness = 100;

	import.Colours.HasData = false;

	// clear rest of preview data?
	import.Preview.Enabled = Preview.Active;
	import.Preview.IncrementRadially = false;

	// =======================================================================

	std::wifstream file(file_name);

	if (file)
	{
		std::wstring LayerName = L"";

		FileUtility::FileBlock block = FileUtility::FileBlock::kNone;

		bool fontmode = false;
		int layercount = 0;

		int row = 0;
		MatrixColourMode mode = MatrixColourMode::kMono;
		int colour = 0;
		int palette = 0;
		int importRGBbackground = 0;

		int tempMaxWidth = -1;
		int tempMaxHeight = -1;
		int newwidth = 0;
		int newheight = 0;

		if (loadmode == LoadMode::kNew)
		{
			SetIgnoredPixels(PixelAlive);
		}

		if (loadmode == LoadMode::kAppend)
		{
			for (int i = 0; i < Data->Layers.size(); i++)
			{
				MatrixGrid *m = new MatrixGrid(Details.Width, Details.Height, Details.ColourMode, RGBBackground);

				Data->Layers[i]->Cells.push_back(m);
			}
		}

		// ===========================================================================
		// ===========================================================================

		int line = 1;

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

					switch (FileUtility::LoadDataParameterType(s, block))
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

						block = FileUtility::FileBlock::kHeader;
						break;
					case LoadData::kLoadBlockStartIgnoredPixel:
						block = FileUtility::FileBlock::kIgnoredPixels;

						row = 0;
                        break;
					case LoadData::kLoadBlockBegin:
						row = 0;

						mode = FileUtility::GetMatrixModeFromFileChunk(v[v.length() - 1]);

						if (loadmode == LoadMode::kNew)
						{
							Details.ColourMode = mode;
						}

						block = FileUtility::FileBlock::kMatrixData;
						break;
					case LoadData::kLoadBlockEnd:
						 if (block == FileUtility::FileBlock::kMatrixData)
						 {
							importFrame++;
						 }
						 break;
					case LoadData::kLoadBlockBeginLayout:
						block = FileUtility::FileBlock::kLayer;

						importLayer++;

						importFrame = initialframe;
						break;
					case LoadData::kLoadBlockEndLayout:
						block = FileUtility::FileBlock::kNone;

						switch (loadmode)
						{
						case LoadMode::kNew:
							Details.Height = tempMaxHeight;
							Details.Width  = tempMaxWidth;
							break;

						default:
							break;
						}

						if (importLayer + 1 > Data->Layers.size())
						{
							Data->AddLayerSilent(LayerName);
						}

						layercount = -1;
						break;
					case LoadData::kLoadBlockStartColours:
						block = FileUtility::FileBlock::kColours;

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

					case LoadData::kLoadHeaderBinaryData:
                        eeo.SetBinaryFromFile(v);
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
						if (importLayer == -1)
						{
							importLayer = 0;
                        }

						if (row == 0 && Data->Layers[importLayer]->Cells.size() < importFrame + 1)
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

							default:
								break;
							}

							tempMaxWidth = newwidth;
							tempMaxHeight = newheight;

							MatrixGrid *m = new MatrixGrid(newwidth, newheight, Details.ColourMode, RGBBackground);
							Data->Layers[importLayer]->Cells.push_back(m);
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
									if (mode == MatrixColourMode::kRGB)
									{
										if (Data->Layers[importLayer]->Cells[importFrame]->Grid[row * tempMaxWidth + x] == importRGBbackground)
										{
											Data->Layers[importLayer]->Cells[importFrame]->SafePlot(x, row, Convert::HexToInt(pixel));
										}
									}
									else
									{
										if (Data->Layers[importLayer]->Cells[importFrame]->Grid[row * tempMaxWidth + x] == 0)
										{
											Data->Layers[importLayer]->Cells[importFrame]->SafePlot(x, row, Convert::HexToInt(pixel));
										}
									}
									break;
								case LoadMode::kMergeTopPriority:
									if (mode == MatrixColourMode::kRGB)
									{
										if (Convert::HexToInt(pixel) != importRGBbackground)
										{
											Data->Layers[importLayer]->Cells[importFrame]->SafePlot(x, row, Convert::HexToInt(pixel));
										}
									}
									else
									{
										if (Convert::HexToInt(pixel) != 0)
										{
											Data->Layers[importLayer]->Cells[importFrame]->SafePlot(x, row, Convert::HexToInt(pixel));
										}
									}
									break;

								default:
									Data->Layers[importLayer]->Cells[importFrame]->SafePlot(x, row, GetPixelFrom(Details.ColourMode, mode, Convert::HexToInt(pixel), importRGBbackground));
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
						Data->Layers[importLayer]->Cells[importFrame]->Locked = stoi(v);
						break;

					 // ======================================================================

					case LoadData::kLoadIgnoredPixelData:
					{
						int x     = 0;
						std::wstring pixel = L"";

						for (int i = 0; i < v.length(); i++)
						{
							if (v[i] == L' ' || i == v.length() - 1)
							{
								if (pixel == L"0")
								{
									MatrixIgnoredLayout->Grid[row * tempMaxWidth + x] = PixelAlive;
								}
								else
								{
									MatrixIgnoredLayout->Grid[row * tempMaxWidth + x] = PixelIgnored;
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
						LayerName = v;
						break;
					case LoadData::kLoadLayoutWidth:
						tempMaxWidth  = stoi(v);
						break;
					case LoadData::kLoadLayoutHeight:
						tempMaxHeight = stoi(v);
						break;
					case LoadData::kLoadLayoutLocked:
						Data->Layers[importLayer]->Locked = stoi(v);
						break;

					 // ====================================================================

					case LoadData::kLoadColoursCustom:
						import.Colours.CustomColours[colour] = stoi(v);

						colour++;
						break;
					case LoadData::kLoadColoursDraw0:
						import.Colours.DrawColours[kMouseLeft]   = stoi(v);
						break;
					case LoadData::kLoadColoursDraw1:
						import.Colours.DrawColours[kMouseMiddle] = stoi(v);
						break;
					case LoadData::kLoadColoursDraw2:
						import.Colours.DrawColours[kMouseRight]  = stoi(v);
						break;
					case LoadData::kLoadColoursPaletteHistory:
						import.Colours.PaletteHistory[palette] = stoi(v);
						palette++;
						break;

					default:
						break;
					}
				}
			}
		}

		file.close();

		Data->EnsureLayerCoherence();

		if (loadmode == LoadMode::kNew)
		{
			Details.Height = tempMaxHeight;
			Details.Width  = tempMaxWidth;

			import.ColourMode = mode;
			import.NewWidth         = tempMaxWidth;
			import.NewHeight        = tempMaxHeight;
			import.BackgroundColour = importRGBbackground;
		}

		Details.Available   = true;

       	Data->SetSystem(Details.Width, Details.Height, RGBBackground, Software, Details.DrawMode, Details.ColourMode);

		CurrentFrame = 0;

		Busy = false;

		CopyCurrentFrameToDrawBuffer();

		import.MaxFrames        = Data->Layers[0]->Cells.size() - 1;
		import.FontMode         = fontmode;

		eeo.ExportMode = ExportSource::kAnimationGrid;
	//  except
	//	on E: Exception do {
	//	  Matrix.Available         = false;

	//	  Result.ImportOk    = false;
	//	  Result.ErrorString = GLanguageHandler.Text[kErrorLoadingProject] + ': "' + E.Message + '"';
	//	}
	}

	if (OnLayerChange) OnLayerChange(this);

	PaintBox->Invalidate();

	return import;
}


ImportData TheMatrix::LoadProjectFreeform(const std::wstring file_name, ExportOptions &eeo, LoadMode loadmode, int startframe)
{
	auto SafeStringToBool = [](const std::wstring s) -> bool
	{
		if (s.empty() || s == L"0")
		{
			return false;
		}

        return true;
	};

	MatrixReadOnly = true;

    Busy = true;

	int importLayer = CurrentLayer;
	int importFrame = CurrentFrame;
	int initialframe = 0;

	// =======================================================================

	ClearAllMatrixData(false, 0, 0);

	importFrame = 0;
	importLayer = -1;

	initialframe = importFrame;

	// =======================================================================
	// =======================================================================

	ImportData import;
	import.ImportOk = true;
	import.DrawMode = MatrixDrawMode::kFreeform;
	import.ColourMode = MatrixColourMode::kRGB;
	import.RGBBrightness = 100;

	import.Colours.HasData = false;

	// clear rest of preview data?
	import.Preview.Enabled = Preview.Active;
	import.Preview.IncrementRadially = false;

	// =======================================================================

	std::wifstream file(file_name);

	if (file)
	{
		std::wstring LayerName = L"";

		FileUtility::FileBlock block = FileUtility::FileBlock::kNone;

		bool fontmode = false;
		int layercount = 0;

		MatrixColourMode mode = MatrixColourMode::kMono;
		int colour = 0;
		int palette = 0;
		int importRGBbackground = 0;

		if (loadmode == LoadMode::kNew)
		{
			SetIgnoredPixels(PixelAlive);
		}

		if (loadmode == LoadMode::kAppend)
		{
			for (int i = 0; i < Data->Layers.size(); i++)
			{
				// to do
			}
		}

		// ===========================================================================
		// ===========================================================================

		int line = 1;

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

					switch (FileUtility::LoadDataParameterType(s, block))
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

						block = FileUtility::FileBlock::kHeader;
						break;
					case LoadData::kLoadBlockBegin:
					{
						mode = FileUtility::GetMatrixModeFromFileChunk(v[v.length() - 1]);

						if (loadmode == LoadMode::kNew)
						{
							Details.ColourMode = mode;
						}

						MatrixPixel *mp = new MatrixPixel();
						Data->Layers[importLayer]->Freeform->Pixels.push_back(mp);

						block = FileUtility::FileBlock::kMatrixData;
						break;
                    }
					case LoadData::kLoadBlockEnd:
						 if (block == FileUtility::FileBlock::kMatrixData)
						 {
							importFrame++;
						 }
						 break;
					case LoadData::kLoadBlockBeginLayout:
						block = FileUtility::FileBlock::kLayer;

						importLayer++;

						importFrame = initialframe;
						break;
					case LoadData::kLoadBlockEndLayout:
						block = FileUtility::FileBlock::kNone;

						switch (loadmode)
						{
						case LoadMode::kNew:
							Details.Height = 0;
							Details.Width  = 0;
							break;

						default:
							break;
						}

						if (importLayer + 1 > Data->Layers.size())
						{
							Data->AddLayerSilent(LayerName);
						}

						layercount = -1;
						break;
					case LoadData::kLoadBlockStartColours:
						block = FileUtility::FileBlock::kColours;

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

					case LoadData::kLoadHeaderBinaryData:
                        eeo.SetBinaryFromFile(v);
						break;

					 // ======================================================================

					case LoadData::kLoadLayoutName:
						LayerName = v;
						break;
					case LoadData::kLoadLayoutLocked:
						Data->Layers[importLayer]->Locked = stoi(v);
						break;

					 // ====================================================================

					case LoadData::kLoadColoursCustom:
						import.Colours.CustomColours[colour] = stoi(v);

						colour++;
						break;
					case LoadData::kLoadColoursDraw0:
						import.Colours.DrawColours[kMouseLeft]   = stoi(v);
						break;
					case LoadData::kLoadColoursDraw1:
						import.Colours.DrawColours[kMouseMiddle] = stoi(v);
						break;
					case LoadData::kLoadColoursDraw2:
						import.Colours.DrawColours[kMouseRight]  = stoi(v);
						break;
					case LoadData::kLoadColoursPaletteHistory:
						import.Colours.PaletteHistory[palette] = stoi(v);
						palette++;
						break;

					 // ====================================================================

					case LoadData::kLoadPixelX:
						Data->Layers[importLayer]->Freeform->Pixels.back()->X = stoi(v);
						break;
					case LoadData::kLoadPixelY:
						Data->Layers[importLayer]->Freeform->Pixels.back()->Y = stoi(v);
						break;
					case LoadData::kLoadPixelOrder:
						Data->Layers[importLayer]->Freeform->Pixels.back()->Order = stoi(v);
						break;
					case LoadData::kLoadPixelColour:
						Data->Layers[importLayer]->Freeform->Pixels.back()->Colours.push_back(stoi(v));
						break;
					case LoadData::kLoadPixelGroup:
						Data->Layers[importLayer]->Freeform->Pixels.back()->Group = stoi(v);
						break;

					case LoadData::kLoadBlockStartFrames:
					{
						block = FileUtility::FileBlock::kFrames;

						FreeformFrame *fff = new FreeformFrame();

						Data->Layers[importLayer]->Freeform->Frames.push_back(fff);
						break;
					}
					case LoadData::kLoadFrameLocked:
                        Data->Layers[importLayer]->Freeform->Frames.back()->Locked = stoi(v);

					default:
						break;
					}
				}
			}
		}

		file.close();

		Data->EnsureLayerCoherence();

		if (loadmode == LoadMode::kNew)
		{
			Details.Height = 0;
			Details.Width  = 0;

			import.ColourMode = mode;
			import.NewWidth         = 0;
			import.NewHeight        = 0;
			import.BackgroundColour = importRGBbackground;
		}

		Details.Available   = true;

		auto bounds = Data->GetPixelBounds();

		Data->SetSystem(std::get<0>(bounds), std::get<1>(bounds), RGBBackground, Software, Details.DrawMode, Details.ColourMode);

		CurrentFrame = 0;

		import.MaxFrames        = Data->Layers[0]->Freeform->Frames.size() - 1; // 0 to n-1
		import.FontMode         = fontmode;

		eeo.ExportMode = ExportSource::kAnimationFreeform;

		Busy = false;
	}

	if (OnLayerChange) OnLayerChange(this);

	PaintBox->Invalidate();

	return import;
}


ImportData TheMatrix::ImportLEDMatrixDataSingleFrame(const std::wstring file_name)
{
	BackupMatrix(CurrentLayer, CurrentFrame);

	bool addedSingleFrame = false;
	MatrixColourMode lMatrixMode = MatrixColourMode::kMono;

	FileUtility::FileBlock block = FileUtility::FileBlock::kNone;

	bool fontmode = false;
	int lRGBBackground = -1;

	int lCurrentLayer = 0;

	ImportData import;
	import.Source = -1;
	import.SourceLSB = -1;
	//  Result.SourceDirection = -1;
	import.ColourMode = MatrixColourMode::kMono;

	// ===========================================================================
	// ===========================================================================

	std::wifstream file(file_name);

	if (file)
	{
		int MemSlot = CurrentFrame;
		int Row     = 0;

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

					switch (FileUtility::LoadDataParameterType(s, block))
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

						block = FileUtility::FileBlock::kHeader;
						break;

					case LoadData::kLoadBlockStartIgnoredPixel:
						block = FileUtility::FileBlock::kIgnoredPixels;

						Row = 0;
						break;

					case LoadData::kLoadBlockBegin:
						Row = 0;

						lMatrixMode = FileUtility::GetMatrixModeFromFileChunk(v[v.length() - 1]);

						block = FileUtility::FileBlock::kMatrixData;
						break;

					case LoadData::kLoadBlockEnd:
						if (block == FileUtility::FileBlock::kMatrixData)
						{
							MemSlot++;
						}
						break;
					case LoadData::kLoadBlockBeginLayout:
						block = FileUtility::FileBlock::kLayer;
						break;
					case LoadData::kLoadBlockEndLayout:
                        block = FileUtility::FileBlock::kNone;
						break;

					// =======================================================

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

					// =======================================================

					case LoadData::kLoadMatrixData:
					{
						int	x = 0;
						std::wstring pixel = L"";

						for (int i = 0; i < v.length(); i++)
						{
							if (v[i] == ' ' || i == v.length() - 1)
							{
								switch (lMatrixMode)
								{
								case MatrixColourMode::kMono:
									Data->Layers[lCurrentLayer]->Cells[MemSlot]->Grid[Row * Details.Width + x] = Convert::HexToInt(pixel);
									break;
								case MatrixColourMode::kRGB:
									if (lRGBBackground != -1)
									{
										if (Convert::HexToInt(pixel) == lRGBBackground)
										{
											Data->Layers[lCurrentLayer]->Cells[MemSlot]->Grid[Row * Details.Width + x] = RGBBackground;
										}
									}
									break;
								case MatrixColourMode::kRGB3BPP:
									Data->Layers[lCurrentLayer]->Cells[MemSlot]->Grid[Row * Details.Width + x] = Convert::HexToInt(pixel);
									break;

								default:
									break;
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

					// =======================================================

					case LoadData::kLoadIgnoredPixelData:
					{
						int x = 0;
						std::wstring pixel = L"";

						for (int i = 0; i < v.length(); i++)
						{
							if (v[i] == L' ' || i == v.length() - 1)
							{
								if (pixel == L"0")
								{
									MatrixIgnoredLayout->Grid[Row * Details.Width + x] = PixelAlive;
								}
								else
								{
									MatrixIgnoredLayout->Grid[Row * Details.Width + x] = PixelIgnored;
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
						Data->Layers[lCurrentLayer]->Name = v;
						break;

					default:
						break;
					}
				}
			}
		}

		file.close();
	}

	import.ColourMode = lMatrixMode;

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();

	return import;
}


bool TheMatrix::SaveAnimation(const std::wstring file_name, ImportData &tid, ExportOptions &eeo, ProjectColours &colours)
{
	switch (Details.DrawMode)
	{
	case MatrixDrawMode::kGrid:
		return Data->SaveAnimationGrid(file_name, tid, eeo, colours, Details.Comment, MatrixIgnoredLayout);
	case MatrixDrawMode::kFreeform:
		return Data->SaveAnimationFreeform(file_name, tid, eeo, colours, Details.Comment);
	}

	return false;
}
#pragma end_region


#pragma region MatrixActions
void TheMatrix::PerformEffectController(int mode, int multipleoptionmode)
{
	switch (multipleoptionmode)
	{
	case kMOMCurrentOnly:
		Data->PerformEffect(mode, CurrentLayer, CurrentFrame, Render.Gradient);
		break;
	case kMOMCurrentFrameLayers:
		for (int layer = 0; layer < Data->Layers.size(); layer++)
		{
			Data->PerformEffect(mode, layer, CurrentFrame, Render.Gradient);
		}
		break;
	case kMOMCurrentLayerFrames:
		for (int frame = 0; frame < Data->Layers[CurrentLayer]->Cells.size(); frame++)
		{
			Data->PerformEffect(mode, CurrentLayer, frame, Render.Gradient);
		}
		break;
	case kMOMAll:
		for (int layer = 0; layer < Data->Layers.size(); layer++)
		{
			for (int frame = 0; frame < Data->Layers[layer]->Cells.size(); frame++)
			{
				Data->PerformEffect(mode, layer, frame, Render.Gradient);
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


void TheMatrix::PerformScrollController(int mode, int multipleoptionmode)
{
	switch (multipleoptionmode)
	{
	case kMOMCurrentOnly:
		Data->PerformScroll(mode, CurrentLayer, CurrentFrame);
		break;
	case kMOMCurrentFrameLayers:
		for (int layer = 0; layer < Data->Layers.size(); layer++)
		{
			Data->PerformScroll(mode, layer, CurrentFrame);
		}
		break;
	case kMOMCurrentLayerFrames:
		for (int frame = 0; frame < Data->Layers[CurrentLayer]->Cells.size(); frame++)
		{
			Data->PerformScroll(mode, CurrentLayer, frame);
		}
		break;
	case kMOMAll:
		for (int layer = 0; layer < Data->Layers.size(); layer++)
		{
			for (int frame = 0; frame < Data->Layers[layer]->Cells.size(); frame++)
			{
				Data->PerformScroll(mode, layer, frame);
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


void TheMatrix::PerformWipeOnCurrentFrame(int mode, bool clear)
{
	Data->PerformWipeOnFrame(mode, CurrentLayer, CurrentLayer, clear);

	Data->Layers[CurrentLayer]->Cells[CurrentLayer]->AddToHistory();

	if (!AutomateMode)
	{
		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformRevealOnCurrentFrame(int mode, int colour, int &parameter)
{
	Data->PerformRevealOnFrame(mode, CurrentLayer, CurrentFrame, colour, parameter);

	if (!AutomateMode)
	{
		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformColumnScrollOnCurrentFrame(int mode, int column, bool clear)
{
    Data->PerformColumnScrollOnCurrentFrame(mode, CurrentLayer, CurrentFrame, column, clear);

	if (!AutomateMode)
	{
		CopyCurrentFrameToDrawBuffer();

		if (OnChange) OnChange(this);

		PaintBox->Invalidate();
	}
}


void TheMatrix::PerformRowScrollOnCurrentFrame(int mode, int row, bool clear)
{
    Data->PerformRowScrollOnFrame(mode, CurrentLayer, CurrentFrame, row, clear);

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

	Busy = true;

	Data->RotateMultiOption(mode, multipleoptionmode, CurrentLayer, CurrentFrame);

	Busy = false;

	CopyCurrentFrameToDrawBuffer();

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region ReadOnlyProperties
bool TheMatrix::GetIgnoredPixelsMode()
{
	return IgnoredPixelsMode;
}


SoftwareMode TheMatrix::GetSoftwareMode()
{
	return Software;
}


int TheMatrix::GetAutoPixelSize(int canvas_width, int canvas_height, int gradient)
{
	if (Details.Available)
	{
		if (Details.DrawMode == MatrixDrawMode::kGrid)
		{
			int preview_width = 0;

			if (Preview.Active && !Preview.Popout)
			{
				preview_width = PreviewBox->Width;
			}

			int xc = canvas_width - preview_width - 100;
			int yc = canvas_height - 20;

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
		else    // freeform mode
		{
			return 20;
        }
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


int TheMatrix::GetCurrentPixel()
{
	return CurrentPixel;
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


std::wstring TheMatrix::GetFontName()
{
    return TextFont->Name;
}


void TheMatrix::SetFontWrap(bool mode)
{
	FontWrap = mode;
}


int TheMatrix::GetRandomCoeff()
{
	return RandomCoeff;
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
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		PaintBox->Width  = Details.Width * newpixelsize;
		PaintBox->Height = Details.Height * newpixelsize;
	}
	else
	{
		PaintBox->Width = Canvas->Width;
		PaintBox->Height = Canvas->Height;

		for (int t = 0; t < Data->Layers.size(); t++)
		{
            Data->Layers[t]->Freeform->Relocate(Render.PixelSizeZ, newpixelsize);
		}
	}

	Render.PixelSize = newpixelsize;

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
			PreviewBox->Left = kLeftOffset + (Render.PixelSize * (Details.Width)) + 20;
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

	Preview.Shape = Render.Shape;

	if (Preview.Size <= 2)
	{
		Preview.DisplayShape = PixelShape::kSquare;
	}
	else
	{
		Preview.DisplayShape = Preview.Shape;
	}

	PaintBox->Invalidate();
}


void TheMatrix::ChangeMatrixMode(MatrixDrawMode drawmode, MatrixColourMode newmatrixnode)
{
	Details.DrawMode = drawmode;

	if (Details.Width != -1 || drawmode == MatrixDrawMode::kFreeform)
	{
		Details.ColourMode = newmatrixnode;

		ConfigurePaintboxDrawing();
	}

	Data->SetSystem(Details.Width, Details.Height, RGBBackground, Software, Details.DrawMode, Details.ColourMode);

	// if we're moving to single colour matrix
	// make sure the matrix data fits!

	if (newmatrixnode == MatrixColourMode::kMono)
	{
		for (int layer = 0; layer < Data->Layers.size(); layer++)
		{
			for (int frame = 0; frame < Data->Layers[layer]->Cells.size(); frame++)
			{
				for (int z = 0; z < Details.Width * Details.Height; z++)
				{
					if (Data->Layers[layer]->Cells[frame]->Grid[z] > 0)
					{
						Data->Layers[layer]->Cells[frame]->Grid[z] = 1;
					}
				}
			}
		}
	}

	// ===========================================================================

	if (Details.ColourMode == MatrixColourMode::kRGB)
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
		ClearAllMatrixData(false, 0, 0);
		break;
	case SoftwareMode::kFont:
	{
		ClearAllMatrixData(false, 0, 0);

		for (int t = 0; t < 96; t++)
		{
			MatrixGrid *m = new MatrixGrid(Details.Width, Details.Height, Details.ColourMode, RGBBackground);

			Data->Layers[kPermanentLayer]->Cells.push_back(m);
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
	Render.Action.Parameter = parameter;

	PaintBox->Invalidate();
}


void TheMatrix::SetMirrorMode(MirrorMode newmode)
{
	Mirror = newmode;
}


void TheMatrix::SetAndShowCurrentFrame(int frame)
{
	#if _DEBUG
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		if (frame >= Data->Layers[CurrentLayer]->Cells.size())
		{
			std::wstring debug = L"Frame " + std::to_wstring(frame) + L" outside the valid frame limit of 0 to " + std::to_wstring(Data->Layers[CurrentLayer]->Cells.size() - 1);

			ShowMessage(debug.c_str());
		}
    }
	#endif

	CurrentFrame = frame;

    if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
	}

    PaintBox->Invalidate();

   //	if (OnNewFrameDisplayed) OnNewFrameDisplayed(this);      // interferes with trackbar selection!!!
}


void TheMatrix::RefreshCurrentFrame()
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
	}

	PaintBox->Invalidate();

   //	if (OnNewFrameDisplayed) OnNewFrameDisplayed(this);      // interferes with trackbar selection!!!
}


// if we're currently in a drawing mode's loop then cancel and ensure
// the realtime display of the drawing mode is not copied from the buffer to the frame
void TheMatrix::SetCurrentLayer(int layer)
{
	#if _DEBUG
	if (layer >= Data->Layers.size())
	{
		std::wstring debug = L"Layer " + std::to_wstring(layer) + L" outside the valid layer limit of 0 to " + std::to_wstring(Data->Layers.size() - 1);

		ShowMessage(debug.c_str());
	}
	#endif

	if (Render.Action.Mode != ActionMode::kNone)
	{
		Render.Action.Reset();
	}
	else
	{
		if (Details.DrawMode == MatrixDrawMode::kGrid)
		{
			CopyDrawBufferToCurrentFrame();
        }
    }

	CurrentLayer = layer;

    if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		CopyCurrentFrameToDrawBuffer();
	}

	PaintBox->Invalidate();
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


void TheMatrix::SetGroupOrderDisplay(bool group, bool order)
{
	Render.ShowPixelGroup = group;
	Render.ShowPixelOrder = order;

	if (order || group)
	{
        Data->Layers[CurrentLayer]->Freeform->CalculateContrastColour(CurrentFrame);
	}

	#if _DEBUG
	Render.ShowFrameCount = false;
	#endif

	PaintBox->Invalidate();
}


void TheMatrix::SetIgnoredPixelsMode(bool mode)
{
	IgnoredPixelsMode = mode;

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
	if (ao.Layer < 0 || ao.Layer > Data->Layers.size() - 1) return;

	switch (actionId)
	{
	// == colour cycling =================================================
	case kAutomationColourCyclingLinear:	// linear
	{
		int index = ao.CCTargetIndex;

		for (int x = 0; x < ao.SourceColours.size(); x++)
		{
			Data->Layers[ao.Layer]->Cells[CurrentFrame]->ChangePixels(ao.SourceColours[x],
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

	case kAutomationColourCyclingBounce:	// bounceybouncey
	{
		int index = ao.CCTargetIndex;
		CyclingDirection direction = ao.CCDirection;

		for (int x = 0; x < ao.SourceColours.size(); x++)
		{
			Data->Layers[ao.Layer]->Cells[CurrentFrame]->ChangePixels(ao.SourceColours[x],
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
	case kAutomationMirror:
		Data->PerformEffect(kEffectMirror, ao.Layer, CurrentFrame, Render.Gradient);
		break;
	case kAutomationFlip:
		Data->PerformEffect(kEffectFlip,   ao.Layer, CurrentFrame, Render.Gradient);
		break;
	case kAutomationInvert:
		Data->PerformEffect(kEffectInvert, ao.Layer, CurrentFrame, Render.Gradient);
		break;

	case kAutomationScrollLeft:
		Data->PerformScroll(kEffectScrollLeft,  ao.Layer, CurrentFrame);
		break;
	case kAutomationScrollRight:
		Data->PerformScroll(kEffectScrollRight, ao.Layer, CurrentFrame);
		break;
	case kAutomationScrollUp:
		Data->PerformScroll(kEffectScrollUp,    ao.Layer, CurrentFrame);
		break;
	case kAutomationScrollDown:
		Data->PerformScroll(kEffectScrollDown,  ao.Layer, CurrentFrame);
		break;

	case kAutomationRotateLeft:
		Data->RotateFrame(kEffectRotateACW, ao.Layer, CurrentFrame);
		break;
	case kAutomationRotateRight:
		Data->RotateFrame(kEffectRotateCW,  ao.Layer, CurrentFrame);
		break;

	case kAutomationWipeVertical:
		PerformWipeOnCurrentFrame(kEffectWipeVerticalOut,   ao.EraseBehind);
		break;
	case kAutomationWipeVerticalClear:
		PerformWipeOnCurrentFrame(kEffectWipeVerticalIn,    ao.EraseBehind);
		break;
	case kAutomationWipeHorizontal:
		PerformWipeOnCurrentFrame(kEffectWipeHorizontalOut, ao.EraseBehind);
		break;
	case kAutomationWipeHorizontalClear:
		PerformWipeOnCurrentFrame(kEffectWipeHorizontalIn,  ao.EraseBehind);
		break;

	case kAutomationWipeLeft:
		PerformWipeOnCurrentFrame(kEffectWipeLeftToRight,   ao.EraseBehind);
		break;
	case kAutomationWipeRight:
		PerformWipeOnCurrentFrame(kEffectWipeRightToLeft,   ao.EraseBehind);
		break;
	case kAutomationWipeUp:
		PerformWipeOnCurrentFrame(kEffectWipeUpToDown,      ao.EraseBehind);
		break;
	case kAutomationWipeDown:
		PerformWipeOnCurrentFrame(kEffectWipeDownToUp,      ao.EraseBehind);
		break;

	case kAutomationJiggleLeft :
		if (ao.ProcesingStage < Details.Height)
		{
			for (int x = 0; x <= ao.ProcesingStage % Details.Height; x++)
			{
				PerformRowScrollOnCurrentFrame(kEffectScrollLeft, x, ao.EraseBehind);
			}
		}
		else
		{
			for (int x = 0; x < Details.Height; x++)
			{
				PerformRowScrollOnCurrentFrame(kEffectScrollLeft, x, ao.EraseBehind);
			}
		}
		break;

	case kAutomationJiggleRight:
		if (ao.ProcesingStage < Details.Height)
		{
			for (int x = 0; x <= ao.ProcesingStage % Details.Height; x++)
			{
				PerformRowScrollOnCurrentFrame(kEffectScrollRight, x, ao.EraseBehind);
			}
		}
		else
		{
			for (int x = 0; x < Details.Height; x++)
			{
				PerformRowScrollOnCurrentFrame(kEffectScrollRight, x, ao.EraseBehind);
			}
		}
		break;

	case kAutomationJiggleUp:
		if (ao.ProcesingStage < Details.Width)
		{
			for (int x = 0; x <= ao.ProcesingStage % Details.Width; x++)
			{
				PerformColumnScrollOnCurrentFrame(kEffectScrollUp, x, ao.EraseBehind);
			}
		}
		else
		{
			for (int x = 0; x < Details.Width; x++)
			{
				PerformColumnScrollOnCurrentFrame(kEffectScrollUp, x, ao.EraseBehind);
			}
		}
		break;

	case kAutomationJiggleDown:
		if (ao.ProcesingStage < Details.Width)
		{
			for (int x = 0; x <= ao.ProcesingStage % Details.Width; x++)
			{
				PerformColumnScrollOnCurrentFrame(kEffectScrollDown, x, ao.EraseBehind);
			}
		}
		else
		{
			for (int x = 0; x < Details.Width; x++)
			{
				PerformColumnScrollOnCurrentFrame(kEffectScrollDown, x, ao.EraseBehind);
			}
		}
		break;

		  // Bounce left/right
		  // parameter1 is scroll count
		  // parameter2 is direction: 0 = right, 1 = left
	case kAutomationBounceLeftRight:
		if (ao.Parameter2 == 0)
		{
			Data->PerformScroll(kEffectScrollRight, ao.Layer, CurrentFrame);
		}
		else
		{
			Data->PerformScroll(kEffectScrollLeft, ao.Layer, CurrentFrame);
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

		  // Bounce up/down
		  // parameter1 is scroll count
		  // parameter2 is direction: 0 = right, 1 = left
	case kAutomationBounceUpDown:
		if (ao.Parameter2 == 0)
		{
			Data->PerformScroll(kEffectScrollUp, ao.Layer, CurrentFrame);
		}
		else
		{
			Data->PerformScroll(kEffectScrollDown, ao.Layer, CurrentFrame);
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
	 case kAutomationBrush1EveryFrame:
		for (int x = 0; x < ao.Brushes[0].BrushData.size(); x++)
		{
			StringToRow(false, ao.Brushes[0].BrushData[x], CurrentFrame, x,
						  ao.Brushes[0].TransparentColour,
						  ao.Brushes[0].Transparent);
		}
		break;

	case kAutomationBrush1FirstFrame:
		if (CurrentFrame == ao.FrameStart)
		{
			for (int x = 0; x < ao.Brushes[0].BrushData.size(); x++)
			{
				StringToRow(false, ao.Brushes[0].BrushData[x], CurrentFrame, x,
							 ao.Brushes[0].TransparentColour,
							 ao.Brushes[0].Transparent);
			}
		}
		break;

	case kAutomationBrush2EveryFrame:
		for (int x = 0; x < ao.Brushes[1].BrushData.size(); x++)
		{
			StringToRow(false, ao.Brushes[1].BrushData[x], CurrentFrame, x,
						  ao.Brushes[1].TransparentColour,
						  ao.Brushes[1].Transparent);
		}
		break;

	case kAutomationBrush2FirstFrame:
		if (CurrentFrame == ao.FrameStart)
		{
			for (int x = 0; x < ao.Brushes[1].BrushData.size(); x++)
			{
				StringToRow(false, ao.Brushes[1].BrushData[x], CurrentFrame, x,
					 ao.Brushes[1].TransparentColour,
					 ao.Brushes[1].Transparent);
			}
		}
		break;

          // == split scroll
	case kAutomationScrollLeftRightSplit:
		Data->PerformSplitScroll(kEffectSplitScrollLeftRight, ao.Layer, CurrentFrame);
		break;
	case kAutomationScrollRightLeftSplit:
		Data->PerformSplitScroll(kEffectSplitScrollRightLeft, ao.Layer, CurrentFrame);
		break;
	case kAutomationScrollUpDownSplit:
		Data->PerformSplitScroll(kEffectSplitScrollUpDown,    ao.Layer, CurrentFrame);
		break;
	case kAutomationScrollDownUpSplit:
		Data->PerformSplitScroll(kEffectSplitScrollDownUp,    ao.Layer, CurrentFrame);
		break;

		  // alternate scrolls
	case kAutomationAlternateUpDownScroll:
		Data->PerformAlternateScroll(kEffectAlternateScrollUpDown, ao.Layer, CurrentFrame);
		break;

	case kAutomationRevealLeftRight:
		PerformRevealOnCurrentFrame(kEffectRevealLeftRight, ao.ParameterRevealColour, ao.ParameterReveal);
		break;
	case kAutomationRevealRightLeft:
		PerformRevealOnCurrentFrame(kEffectRevealRightLeft, ao.ParameterRevealColour, ao.ParameterReveal);
		break;
	case kAutomationRevealTopBottom:
		PerformRevealOnCurrentFrame(kEffectRevealTopBottom, ao.ParameterRevealColour, ao.ParameterReveal);
		break;
	case kAutomationRevealBottomTop:
		PerformRevealOnCurrentFrame(kEffectRevealBottomTop, ao.ParameterRevealColour, ao.ParameterReveal);
		break;
	case kAutomationRevealCentreIn:
		PerformRevealOnCurrentFrame(kEffectRevealCentreIn,  ao.ParameterRevealColour, ao.ParameterReveal);
		break;
	case kAutomationRevealCentreOut:
		PerformRevealOnCurrentFrame(kEffectRevealCentreOut, ao.ParameterRevealColour, ao.ParameterReveal);
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

			if (action >= 19 && action <= 22)           // paste brush
			{
				AutomationActionExecute(ao, action);
			}
		}

		for (int frame = ao.FrameStart + 1; frame <= ao.FrameEnd; frame++)
		{
			if (frame >= Data->GetFrameCount())
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
			if (frame > Data->GetFrameCount())
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
			if (frame > Data->GetFrameCount())
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
        break;
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
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		std::memcpy(MatrixCopy->Grid, Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid, Details.Width * Details.Height * sizeof(int));
	}
	else
	{
		PixelFrameColours.clear();

		for (int t = 0; t < Data->Layers[CurrentLayer]->Freeform->Pixels.size(); t++)
		{
			PixelFrameColours.push_back(Data->Layers[CurrentLayer]->Freeform->Pixels[t]->Colours[CurrentFrame]);
		}
    }
}


void TheMatrix::CopyBackupToCurrentFrame()
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		std::memcpy(MatrixCopy->Grid, MatrixBackup->Grid, Details.Width * Details.Height * sizeof(int));
	}
	else
	{
		for (int t = 0; t < PixelFrameColours.size(); t++)
		{
			Data->Layers[CurrentLayer]->Freeform->Pixels[t]->Colours[CurrentFrame] = PixelFrameColours[t];
		}
	}
}


void TheMatrix::PasteSpecial(int mode)
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		Data->PerformScrollOnCopyFrame(CurrentLayer, CurrentFrame, mode, MatrixCopy);

		PasteCurrentFrame();
	}
	else
	{
		// to do
	}
}


void TheMatrix::PasteCurrentFrame()
{
	if (Data->IsThisFrameLocked(CurrentLayer, CurrentFrame) ||
		!Data->Layers[CurrentLayer]->Visible) return;

	BackupMatrix(CurrentLayer, CurrentFrame);

	std::memcpy(Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid, MatrixCopy->Grid, Details.Width * Details.Height * sizeof(int));

	Data->Layers[CurrentLayer]->Cells[CurrentFrame]->AddToHistory();

	CopyCurrentFrameToDrawBuffer();

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region UIScrollbars
// configures the render engine for scrolling the view window
void TheMatrix::ChangeZoomUI(int pixelsize)
{
	TPanel *panel = (TPanel*)Canvas;

	int containerwidth  = panel->Width - 75 - 25;
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

		Render.ViewWindow.X = Details.Width - 1;
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

		Render.ViewWindow.Y = Details.Height - 1;
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


#pragma region UserBuffers
void TheMatrix::ClearUserBuffers()
{
	for (int i = 0; i < 10; i++)
	{
		MatrixUser[i]->Clear(Details.ColourMode, RGBBackground);

		MatrixUserFF[i].clear();
	}
}


void TheMatrix::CopyToUserBuffer(int frame)
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		std::memcpy(static_cast<MatrixGrid*>(MatrixUser[frame])->Grid, Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid, Details.Width * Details.Height * sizeof(int));
	}
	else
	{
		MatrixUserFF[frame].clear();

		for (int t = 0; t < Data->Layers[CurrentLayer]->Freeform->Pixels.size(); t++)
		{
			MatrixUserFF[frame].push_back(Data->Layers[CurrentLayer]->Freeform->Pixels[t]->Colours[CurrentFrame]);
		}
	}
}


void TheMatrix::RestoreFromUserBuffer(int frame)
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		if (Details.ColourMode == MatrixColourMode::kRGB)
		{
			std::memcpy(Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid, static_cast<MatrixGrid*>(MatrixUser[frame])->Grid, Details.Width * Details.Height * sizeof(int));
		}
		else
		{
			for (int z = 0; z < Details.Width * Details.Height; z++)
			{
				if (static_cast<MatrixGrid*>(MatrixUser[frame])->Grid[z] == 1)
				{
					Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid[z] = 1;
				}
				else
				{
					Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid[z] = 0;
				}
			}
		}

		CopyCurrentFrameToDrawBuffer();
	}
	else
	{
		for (int t = 0; t < MatrixUserFF[frame].size(); t++)
		{
			Data->Layers[CurrentLayer]->Freeform->Pixels[t]->Colours[frame] = MatrixUserFF[frame][t];
		}
	}

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}
#pragma end_region


#pragma region UndoRedo
void TheMatrix::Undo()
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Undo();

		CopyCurrentFrameToDrawBuffer();
	}
	else
	{
		Data->Layers[CurrentLayer]->Freeform->Undo();
	}

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::Redo()
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Redo();

		CopyCurrentFrameToDrawBuffer();
	}
	else
	{
		Data->Layers[CurrentLayer]->Freeform->Redo();
	}

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


void TheMatrix::SetFromUndo(int undo)
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		Data->Layers[CurrentLayer]->Cells[CurrentFrame]->SetFromUndo(undo);

		CopyCurrentFrameToDrawBuffer();
	}
	else
	{
		Data->Layers[CurrentLayer]->Freeform->SetFromUndo(undo);
	}

	if (OnChange) OnChange(this);

	PaintBox->Invalidate();
}


bool TheMatrix::CanUndo()
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		return Data->Layers[CurrentLayer]->Cells[CurrentFrame]->HistoryOffset != 0;
	}

	return Data->Layers[CurrentLayer]->Freeform->CanUndo();
}


bool TheMatrix::CanRedo()
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		return Data->Layers[CurrentLayer]->Cells[CurrentFrame]->HistoryOffset != Data->Layers[CurrentLayer]->Cells[CurrentFrame]->History.size() - 1;
	}

	return Data->Layers[CurrentLayer]->Freeform->CanRedo();
}
#pragma end_region


#pragma region Freeform
void TheMatrix::AddPixel(int x, int y)
{
	MatrixPixel *mp = new MatrixPixel(x, y, Data->Layers[CurrentLayer]->Freeform->Frames.size(),
									  Data->Layers[CurrentLayer]->Freeform->Pixels.size(),
									  0x00ff00ff);

	Data->Layers[CurrentLayer]->Freeform->Pixels.push_back(mp);

	PaintBox->Invalidate();
}


void TheMatrix::DeletePixel()
{
	if (CurrentPixel != -1)
	{
		Data->Layers[CurrentLayer]->Freeform->Pixels.erase(Data->Layers[CurrentLayer]->Freeform->Pixels.begin() + CurrentPixel);

        CurrentPixel = -1;
	}
}


void TheMatrix::ClearSelection()
{
	Data->Layers[CurrentLayer]->Freeform->ClearSelection();
}


void TheMatrix::SelectInGroup(int group)
{
	if (Render.Action.Mode == ActionMode::kMovePixel)
	{
		if (group != -1)
		{
			Data->Layers[CurrentLayer]->Freeform->AddGroupToSelection(group);
        }
	}
}


void TheMatrix::PerformFreeformEffect(int direction)
{
	switch (direction)
	{
	case kEffectScrollLeft:
		Data->Layers[CurrentLayer]->Freeform->ShiftColoursLeft(CurrentFrame);
		break;
	case kEffectScrollRight:
		Data->Layers[CurrentLayer]->Freeform->ShiftColoursRight(CurrentFrame);
		break;
	}

	PaintBox->Invalidate();
}


void TheMatrix::AutoOrderPixels(int mode)
{
	Data->Layers[CurrentLayer]->Freeform->AutoOrderPixels(mode);

	PaintBox->Invalidate();
}
#pragma end_region


void TheMatrix::BackupMatrix(int layer, int frame)
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		if (frame >= 0)
		{
			std::memcpy(MatrixBackup->Grid, Data->Layers[layer]->Cells[frame]->Grid, Details.Width * Details.Height * sizeof(int));
		}
		else
		{
			std::memcpy(MatrixBackup->Grid, MatrixCopy->Grid, Details.Width * Details.Height * sizeof(int));
		}
	}
	else
	{
		//to do
	}
}


void TheMatrix::BackupMatrix()
{
	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		std::memcpy(MatrixBackup->Grid, Data->Layers[CurrentLayer]->Cells[CurrentFrame]->Grid, Details.Width * Details.Height * sizeof(int));
	}
	else
	{
		// to do
	}
}


#if _DEBUG
std::wstring TheMatrix::GetPaintBoxDebug()
{
	std::wstring s = std::to_wstring(PaintBox->Left) + L", " + std::to_wstring(PaintBox->Top) + L"; " +
					 std::to_wstring(PaintBox->Width) + L" x " + std::to_wstring(PaintBox->Height) + L" (view: " +
					 std::to_wstring(Render.ViewWindow.X) + L" x " + std::to_wstring(Render.ViewWindow.Y) + L")";

	return s;
}


std::wstring TheMatrix::GetPreviewDebug()
{
	std::wstring s = std::to_wstring(PreviewBox->Left) + L", " + std::to_wstring(PreviewBox->Top) + L"; " +
					 std::to_wstring(PreviewBox->Width) + L" x " + std::to_wstring(PreviewBox->Height) + L". " +
					 std::to_wstring(Preview.Size);

	return s;
}


// generates a very simple test pattern
// very useful when testing :)
void TheMatrix::TestSignal()
{
	if (Details.DrawMode == MatrixDrawMode::kFreeform) return;

    Data->TestSignal(CurrentLayer);

	CopyCurrentFrameToDrawBuffer();

	PaintBox->Invalidate();
}
#endif
