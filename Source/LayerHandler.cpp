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

#include "LayerHandler.h"


LayerHandler::LayerHandler(const std::wstring name)
{
	MatrixBackup = new MatrixGrid(__MaxWidth, __MaxHeight, MatrixColourMode::kNone, RGBBackground);

	Layer *layer = new Layer(name);

	Layers.push_back(layer);
}


LayerHandler::~LayerHandler()
{
	for (int t = 0; t < Layers.size(); t++)
	{
		delete Layers[t];
	}

	delete MatrixBackup;
}


void LayerHandler::SetSystem(int width, int height, int background, SoftwareMode sm, MatrixDrawMode mdm, MatrixColourMode mcm)
{
	Width = width;
	Height = height;
	RGBBackground = background;

	Software = sm;
	DrawMode = mdm;
	ColourMode = mcm;
}


void LayerHandler::BackupMatrix(int layer, int frame)
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		if (frame >= 0)
		{
			std::memcpy(MatrixBackup->Grid, Layers[layer]->Cells[frame]->Grid, Width * Height * sizeof(int));
		}
	}
	else
	{
		//to do
	}
}


bool LayerHandler::IsThisFrameLocked(int layer, int frame)
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		return Layers[layer]->Locked || Layers[layer]->Cells[frame]->Locked;
	}

	return (Layers[layer]->Locked || Layers[layer]->Freeform->Frames[frame]->Locked);
}


int LayerHandler::GetLayerCount()
{
	return Layers.size();
}


// value *should* be the same for all layers, so just return those of layer 0
int LayerHandler::GetFrameCount()
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		return Layers[kPermanentLayer]->Cells.size();
	}

	return Layers[kPermanentLayer]->Freeform->Frames.size();
}


int LayerHandler::GetPixelCount()
{
	int total = 0;

	for (int t = 0; t < Layers.size(); t++)
	{
		if (Layers[t]->Freeform != nullptr)
		{
			total += Layers[t]->Freeform->Pixels.size();
		}
	}

	return total;
}


std::wstring LayerHandler::GetLayerName(int layerindex)
{
	return Layers[layerindex]->Name;
}


void LayerHandler::SetLayerName(const std::wstring name, int layerindex)
{
	Layers[layerindex]->Name = name;
}


bool LayerHandler::AreLayersIdentical(int layer1, int layer2, int frame)
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		for (int z = 0; z < Width * Height; z++)
		{
			if (Layers[layer1]->Cells[frame]->Grid[z] != Layers[layer2]->Cells[frame]->Grid[z])
			{
				return false;
			}
		}
	}
	else
	{
		for (int z = 0; z < Layers[layer1]->Freeform->Frames.size(); z++)
		{
            // to do
		}
	}

	return true;
}


#pragma region LockStatus
bool LayerHandler::IsLocked(int layer, int frame)
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		return Layers[layer]->Cells[frame]->Locked;
	}

	return Layers[layer]->Freeform->Frames[frame]->Locked;
}


void LayerHandler::UnlockLayer(int layer)
{
	Layers[layer]->Locked = false;
}


void LayerHandler::LockLayer(int layer)
{
	Layers[layer]->Locked = true;
}


void LayerHandler::UnLockFrame(int layer, int frame)
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		Layers[layer]->Cells[frame]->Locked = false;
	}
	else
	{
		Layers[layer]->Freeform->Frames[frame]->Locked = false;
	}
}


void LayerHandler::LockFrame(int layer, int frame)
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		Layers[layer]->Cells[frame]->Locked = true;
	}
	else
	{
		Layers[layer]->Freeform->Frames[frame]->Locked = true;
	}
}


void LayerHandler::LockUnLockRange(int layer, int start, int end, bool status)
{
	for (int f = start; f <= end; f++)
	{
		if (DrawMode == MatrixDrawMode::kGrid)
		{
			Layers[layer]->Cells[f]->Locked = status;
		}
		else
		{
			Layers[layer]->Freeform->Frames[f]->Locked = status;
		}
	}
}
#pragma end_region


void LayerHandler::ClearLayerAllFrames(int layer)
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		for (int frame = 0; frame < Layers[layer]->Cells.size(); frame++)
		{
			if (!IsThisFrameLocked(layer, frame))
			{
				Layers[layer]->Cells[frame]->Clear(ColourMode, RGBBackground);

				Layers[layer]->Cells[frame]->AddToHistory();
			}
		}
	}
	else
	{
		for (int frame = 0; frame < Layers[layer]->Freeform->Frames.size(); frame++)
		{
			Layers[layer]->Freeform->Clear(frame, ColourMode, RGBBackground);
		}
	}
}


#pragma region ColourCounting
int LayerHandler::CountColoursFrame(int frame)
{
	std::vector<int> Colours;

	for (int layer = 0; layer < Layers.size(); layer++)
	{
		if (DrawMode == MatrixDrawMode::kGrid)
		{
			for (int z = 0; z < Width * Height; z++)
			{
				if (std::find(Colours.begin(), Colours.end(), Layers[layer]->Cells[frame]->Grid[z]) == Colours.end())
				{
					Colours.push_back(Layers[layer]->Cells[frame]->Grid[z]);
				}
			}
		}
		else
		{
			for (int p = 0; p < Layers[layer]->Freeform->Pixels.size(); p++)
			{
				if (std::find(Colours.begin(), Colours.end(), Layers[layer]->Freeform->Pixels[p]->Colours[frame]) == Colours.end())
				{
					Colours.push_back(Layers[layer]->Freeform->Pixels[p]->Colours[frame]);
				}
			}
		}
	}

	return Colours.size();
}


int LayerHandler::CountColoursAnimation()
{
	std::vector<int> Colours;

	for (int frame = 0; frame < GetFrameCount(); frame++)
	{
		for (int layer = 0; layer < Layers.size(); layer++)
		{
			if (DrawMode == MatrixDrawMode::kGrid)
			{
				for (int z = 0; z < Width * Height; z++)
				{
					if (std::find(Colours.begin(), Colours.end(), Layers[layer]->Cells[frame]->Grid[z]) == Colours.end())
					{
						Colours.push_back(Layers[layer]->Cells[frame]->Grid[z]);
					}
				}
			}
			else
			{
				for (int p = 0; p < Layers[layer]->Freeform->Pixels.size(); p++)
				{
					if (std::find(Colours.begin(), Colours.end(), Layers[layer]->Freeform->Pixels[p]->Colours[frame]) == Colours.end())
					{
						Colours.push_back(Layers[layer]->Freeform->Pixels[p]->Colours[frame]);
					}
				}
			}
		}
	}

	return Colours.size();
}


void LayerHandler::GetFirst32Colours(std::vector<int> &colour_list)
{
	for (int layer = 0; layer < Layers.size(); layer++)
	{
		if (DrawMode == MatrixDrawMode::kGrid)
		{
			for (int frame = 0; frame < Layers[layer]->Cells.size(); frame++)
			{
				for (int z = 0; z < Width * Height; z++)
				{
					if (std::find(colour_list.begin(), colour_list.end(), Layers[layer]->Cells[frame]->Grid[z]) == colour_list.end())
					{
						colour_list.push_back(Layers[layer]->Cells[frame]->Grid[z]);

						if (colour_list.size() >= 32) return;
					}
				}
			}
		}
		else
		{
			for (int p = 0; p < Layers[layer]->Freeform->Pixels.size(); p++)
			{
				for (int frame = 0; frame < Layers[layer]->Freeform->Frames.size(); frame++)
				{
					if (std::find(colour_list.begin(), colour_list.end(), Layers[layer]->Freeform->Pixels[p]->Colours[frame]) == colour_list.end())
					{
						colour_list.push_back(Layers[layer]->Freeform->Pixels[p]->Colours[frame]);

						if (colour_list.size() >= 32) return;
					}
				}
			}
		}
	}
}
#pragma end_region


#pragma region Colours
void LayerHandler::FadeFirstToLast(int layer)
{
	for (int z = 0; z < Height * Width; z++)
	{
		int colstart   = Layers[layer]->Cells[0]->Grid[z];
		int colend     = Layers[layer]->Cells[GetFrameCount() - 1]->Grid[z];

		int gradheight = GetFrameCount();

		int rdy  = (colend & 0x0000FF) - (colstart & 0x0000FF);
		int gdy  = ((colend & 0x00FF00) >> 8) - ((colstart & 0x00FF00) >> 8);
		int bdy  = ((colend & 0xFF0000) >> 16) - ((colstart & 0xFF0000) >> 16);

		double newr = (colstart & 0x0000FF);
		double newg = (colstart & 0x00FF00) >> 8;
		double newb = (colstart & 0xFF0000) >> 16;

		double rdx  = (double)rdy / ((double)gradheight - 1);
		double gdx  = (double)gdy / ((double)gradheight - 1);
		double bdx  = (double)bdy / ((double)gradheight - 1);

		for (int frame = 1; frame < GetFrameCount() - 1; frame++)
		{
			newr  = newr + rdx;
			newg  = newg + gdx;
			newb  = newb + bdx;

			int newri = std::floor(newr);
			int newgi = std::floor(newg);
			int newbi = std::floor(newb);

			Layers[layer]->Cells[frame]->Grid[z] = (newbi << 16) + (newgi << 8) + newri;
		}
	}
}


void LayerHandler::ClearAllFramesGradient(int layer, int mode, MatrixGradient gradient, int LEDColours[6])
{
	for (int frame = 0; frame < Layers[layer]->Cells.size(); frame++)
	{
		if (!IsThisFrameLocked(layer, frame))
		{
			for (int x = 0; x < Width; x++)
			{
				for (int y = 0; y < Height; y++)
				{
					if (mode == 1)
					{
						if (ColourMode == MatrixColourMode::kRGB || ColourMode == MatrixColourMode::kRGB3BPP)
						{
							Layers[layer]->Cells[frame]->Grid[y * Width + x] = gradient.IY[y];
						}
						else
						{
							Layers[layer]->Cells[frame]->Grid[y * Width + x] = LEDColours[gradient.IY[y]];
						}
					}
					else
					{
						if (ColourMode == MatrixColourMode::kRGB || ColourMode == MatrixColourMode::kRGB3BPP)
						{
							Layers[layer]->Cells[frame]->Grid[y * Width + x] = gradient.IX[x];
						}
						else
						{
							Layers[layer]->Cells[frame]->Grid[y * Width + x] = LEDColours[gradient.IX[x]];
						}
					}
				}
			}
		}
	}
}


void LayerHandler::GradientFillFrame(int layer, int frame, MatrixGradient gradient, int LEDColours[6])
{
	if (IsThisFrameLocked(layer, frame)) return;

	for (int x = 0; x < Width; x++)
	{
		for (int y = 0; y < Height; y++)
		{
			if (gradient.Option == GradientOption::kVertical)
			{
				if (ColourMode == MatrixColourMode::kRGB || ColourMode == MatrixColourMode::kRGB3BPP)
				{
					Layers[layer]->Cells[frame]->Grid[y * Width + x] = gradient.IY[y];
				}
				else
				{
					Layers[layer]->Cells[frame]->Grid[y * Width + x] = LEDColours[gradient.IY[y]];
				}
			}
			else
			{
				if (ColourMode == MatrixColourMode::kRGB || ColourMode == MatrixColourMode::kRGB3BPP)
				{
					Layers[layer]->Cells[frame]->Grid[y * Width + x] = gradient.IX[x];
				}
				else
				{
					Layers[layer]->Cells[frame]->Grid[y * Width + x] = LEDColours[gradient.IX[x]];
				}
			}
		}
	}
}


void LayerHandler::ChangeColourAll(int colour_from, int colour_to)
{
	for (int layer = 0; layer < Layers.size(); layer++)
	{
		if (DrawMode == MatrixDrawMode::kGrid)
		{
			for (int frame = 0; frame < Layers[layer]->Cells.size(); frame++)
			{
				Layers[layer]->Cells[frame]->ChangePixels(colour_from, colour_to);
			}
		}
		else
		{
			for (int frame = 0; frame < Layers[layer]->Freeform->Frames.size(); frame++)
			{
				Layers[layer]->Freeform->ChangePixels(frame, colour_from, colour_to);
			}
		}
	}
}
#pragma end_region


void LayerHandler::AddLayer(const std::wstring name)
{
	Layer *layer = new Layer(name);

	Layers.push_back(layer);

	if (DrawMode == MatrixDrawMode::kGrid)
	{
		for (int t = 0; t < Layers[kPermanentLayer]->Cells.size(); t++)
		{
			MatrixGrid *m = new MatrixGrid(Width, Height, ColourMode, RGBBackground);

			Layers.back()->Cells.push_back(m);
		}
	}
	else
	{
		for (int t = 0; t < Layers[kPermanentLayer]->Freeform->Frames.size(); t++)
		{
			FreeformFrame *fff = new FreeformFrame();
			Layers.back()->Freeform->Frames.push_back(fff);
		}
	}
}


void LayerHandler::AddLayerAsCopy(const std::wstring name, int copylayer)
{
	Layer *layer = new Layer(name);

	Layers.push_back(layer);

	if (DrawMode == MatrixDrawMode::kGrid)
	{
		for (int t = 0; t < Layers[kPermanentLayer]->Cells.size(); t++)
		{
			MatrixGrid *m = new MatrixGrid(Width, Height, ColourMode, RGBBackground);

			std::memcpy(m->Grid, Layers[copylayer]->Cells[t]->Grid, Width * Height * sizeof(int));

			Layers.back()->Cells.push_back(m);
		}
	}
	else
	{
		// Data->Layers to do
	}
}


// used by file io to create a layer without triggering rerendering etc.
// this may fail as the layer/frame structure likely not fully configured
bool LayerHandler::AddLayerSilent(const std::wstring name)
{
	Layer *layer = new Layer(name);

	Layers.push_back(layer);

	if (DrawMode == MatrixDrawMode::kGrid)
	{
		for (int t = 0; t < Layers[kPermanentLayer]->Cells.size(); t++)
		{
			MatrixGrid *m = new MatrixGrid(Width, Height, ColourMode, RGBBackground);

			Layers.back()->Cells.push_back(m);
		}
	}
	else
	{
		for (int t = 0; t < Layers[kPermanentLayer]->Freeform->Frames.size(); t++)
		{
			FreeformFrame *fff = new FreeformFrame();
			Layers.back()->Freeform->Frames.push_back(fff);
		}
	}

	return true;
}


void LayerHandler::DeleteLayer(int index)
{
	Layers.erase(Layers.begin() + index);
}


void LayerHandler::WipeAllFrames(int layer)
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		for (int frame = 0; frame < Layers[layer]->Cells.size(); frame++)
		{
			if (!IsThisFrameLocked(layer, frame))
			{
				Layers[layer]->Cells[frame]->Clear(ColourMode, RGBBackground);
			}
		}
	}
	else
	{
		for (int frame = 0; frame < Layers[layer]->Freeform->Frames.size(); frame++)
		{
			if (!IsThisFrameLocked(layer, frame))
			{
				Layers[layer]->Freeform->Clear(frame, ColourMode, RGBBackground);
			}
		}
	}
}


void LayerHandler::WipeAllFramesAllLayers()
{
	for (int layer = 0; layer < Layers.size(); layer++)
	{
		if (DrawMode == MatrixDrawMode::kGrid)
		{
			for (int frame = 0; frame < Layers[layer]->Cells.size(); frame++)
			{
				if (!IsThisFrameLocked(layer, frame))
				{
					Layers[layer]->Cells[frame]->Clear(ColourMode, RGBBackground);
				}
			}
		}
		else
		{
			for (int frame = 0; frame < Layers[layer]->Freeform->Frames.size(); frame++)
			{
				if (!IsThisFrameLocked(layer, frame))
				{
					Layers[layer]->Freeform->Clear(frame, ColourMode, RGBBackground);
				}
			}
		}
	}
}


int LayerHandler::DeleteFrame(int frame, int oldcurrentframe)
{
	int NewCurrentFrame = oldcurrentframe;

	if (DrawMode == MatrixDrawMode::kGrid)
	{
		for (int layer = 0; layer < Layers.size(); layer++)
		{
			Layers[layer]->Cells.erase(Layers[layer]->Cells.begin() + frame);
		}

		if (frame >= Layers[kPermanentLayer]->Cells.size())
		{
			NewCurrentFrame = Layers[kPermanentLayer]->Cells.size() - 1;
		}
	}
	else
	{
		for (int layer = 0; layer < Layers.size(); layer++)
		{
			Layers[layer]->Freeform->DeleteFrame(frame);
		}

		if (frame >= Layers[kPermanentLayer]->Freeform->Frames.size())
		{
			NewCurrentFrame = Layers[kPermanentLayer]->Freeform->Frames.size() - 1;
		}
	}

	return NewCurrentFrame;
}


void LayerHandler::InsertBlankFrameAt(int insertat)
{
	for (int layer = 0; layer < Layers.size(); layer++)
	{
		if (DrawMode == MatrixDrawMode::kGrid)
		{
			MatrixGrid *m = new MatrixGrid(Width, Height, ColourMode, RGBBackground);

			if (insertat >= Layers[layer]->Cells.size())
			{
				Layers[layer]->Cells.push_back(m);
			}
			else
			{
				Layers[layer]->Cells.insert(Layers[layer]->Cells.begin() + insertat, m);
			}
		}
		else
		{
			Layers[layer]->Freeform->InsertBlankFrameAt(insertat, RGBBackground);
		}
	}
}


void LayerHandler::InsertCopyFrameAt(int frame, int insertat)
{
	for (int layer = 0; layer < Layers.size(); layer++)
	{
		if (DrawMode == MatrixDrawMode::kGrid)
		{
			MatrixGrid *m = new MatrixGrid(Width, Height, ColourMode, RGBBackground);

			std::memcpy(m->Grid, Layers[layer]->Cells[frame]->Grid, Width * Height * sizeof(int));

			if (insertat >= Layers[layer]->Cells.size())
			{
				Layers[layer]->Cells.push_back(m);
			}
			else
			{
				Layers[layer]->Cells.insert(Layers[layer]->Cells.begin() + insertat, m);
			}
		}
		else
		{
			Layers[layer]->Freeform->InsertCopyFrameAt(frame, insertat);
		}
	}
}


// ensures that all layers have the same number of frames, crashes will occur
// if this is not the case!
void LayerHandler::EnsureLayerCoherence()
{
	int max = 0;

	if (DrawMode == MatrixDrawMode::kGrid)
	{
		for (int t = 0; t < Layers.size(); t++)
		{
			if (Layers[t]->Cells.size() > max)
			{
				max = Layers[t]->Cells.size();
			}
		}

		for (int t = 0; t < Layers.size(); t++)
		{
			if (Layers[t]->Cells.size() != max)
			{
				for (int frame = Layers[t]->Cells.size() + 1; frame <= max; frame++)
				{
					MatrixGrid *m = new MatrixGrid(Width, Height, ColourMode, RGBBackground);

					Layers[t]->Cells.push_back(m);
				}
			}
		}
	}
	else
	{
		for (int t = 0; t < Layers.size(); t++)
		{
			if (Layers[t]->Freeform->Frames.size() > max)
			{
				max = Layers[t]->Freeform->Frames.size();
			}
		}

		for (int t = 0; t < Layers.size(); t++)
		{
			while (Layers[t]->Freeform->Frames.size() < max)
			{
				FreeformFrame *fff = new FreeformFrame();
				Layers[t]->Freeform->Frames.push_back(fff);
			}

			for (int p = 0; p < Layers[t]->Freeform->Pixels.size(); p++)
			{
				if (Layers[t]->Freeform->Pixels[p]->Colours.size() < max)
				{
					for (int x = 0; x < max - Layers[t]->Freeform->Pixels[p]->Colours.size(); x++)
					{
						Layers[t]->Freeform->Pixels[p]->Colours.push_back(RGBBackground);
					}
				}
			}
		}
	}
}


std::tuple<int, int> LayerHandler::GetPixelBounds()
{
	int right = 0;
	int bottom = 0;

	for (int layer = 0; layer < Layers.size(); layer++)
	{
		for (int pixel = 0; pixel < Layers[layer]->Freeform->Pixels.size(); pixel++)
		{
			if (Layers[layer]->Freeform->Pixels[pixel]->X > right)
			{
				right = Layers[layer]->Freeform->Pixels[pixel]->X;
			}

			if (Layers[layer]->Freeform->Pixels[pixel]->Y > bottom)
			{
				bottom = Layers[layer]->Freeform->Pixels[pixel]->Y;
			}
		}
	}

	return std::make_tuple(right, bottom);
}


int LayerHandler::RightBounds(int layer, int frame)
{
	int bound = 0;

	for (int x = 0; x < Width; x++)
	{
		for (int y = 0; y < Height; y++)
		{
			if (ColourMode == MatrixColourMode::kRGB || ColourMode == MatrixColourMode::kRGB3BPP)
			{
				if (Layers[layer]->Cells[frame]->Grid[y * Width + x] != RGBBackground)
				{
					if (x > bound) bound = x;
				}
			}
			else
			{
				if (Layers[layer]->Cells[frame]->Grid[y * Width + x] == 1)
				{
					if (x > bound) bound = x;
				}
			}
		}
	}

	return bound;
}


int LayerHandler::BottomBounds(int layer, int frame)
{
	int bound = 0;

	for (int x = 0; x < Width; x++)
	{
		for (int y = 0; y < Height; y++)
		{
			if (ColourMode == MatrixColourMode::kRGB || ColourMode == MatrixColourMode::kRGB3BPP)
			{
				if (Layers[layer]->Cells[frame]->Grid[y * Width + x] != RGBBackground)
				{
					if (y > bound) bound = y;
				}
			}
			else
			{
				if (Layers[layer]->Cells[frame]->Grid[y * Width + x] == 1)
				{
					if (y > bound) bound = y;
				}
			}
		}
	}

	return bound;
}


#pragma region Statistics
int LayerHandler::GetUndoCount(int currentlayer, int currentframe)
{
	if (DrawMode == MatrixDrawMode::kGrid)
	{
		return Layers[currentlayer]->Cells[currentframe]->History.size();
	}

	return 0; // to do
}


int LayerHandler::GetTotalUndos()
{
	int total = 0;

	if (DrawMode == MatrixDrawMode::kGrid)
	{
		for (int layer = 0; layer < Layers.size(); layer++)
		{
			for (int frame = 0; frame < Layers[layer]->Cells.size(); frame++)
			{
				total += Layers[layer]->Cells[frame]->History.size();
			}
		}
	}
	else
	{
		// to do
	}

	return total;
}


int LayerHandler::CalculateMemoryUsage()
{
	switch (ColourMode)
	{
	case MatrixColourMode::kRGB:
		if (DrawMode == MatrixDrawMode::kGrid)
		{
			return Width * Height * 4 * GetFrameCount();        // 4 bytes per pixel
		}
		return Layers[0]->Freeform->Frames.size() * 4 * GetFrameCount();  // 4 bytes per pixel
	case MatrixColourMode::kRGB3BPP:
		if (DrawMode == MatrixDrawMode::kGrid)
		{
			return std::ceil((Width * Height * 3 * GetFrameCount()) / 8); // 3 bits per pixel
		}
		return Layers[0]->Freeform->Frames.size() * 3 * GetFrameCount();            // 3 bytes per pixel

	default:
		int a = 0;
		int b = 0;
		int total = 0;

		if (Height >= Width)
		{
			a = std::div(Height + 1, 8).quot;
			b = (Width);
		}
		else
		{
			a = std::div(Width + 1, 8).quot;
			b = (Height);
		}

		if (Software == SoftwareMode::kFont)
		{
			total = (a * b) * (kFontCharacterCount);         // always 96 frames in font mode
		}
		else
		{
			total = (a * b) * (GetFrameCount());
		}

		// if using any of the bicolour modes then double requirements
		if (ColourMode > MatrixColourMode::kMono)
		{
			total *= 2;
		}

		return total;
	}
}


int LayerHandler::DataSizeBytes()
{
	switch (ColourMode)
	{
	case MatrixColourMode::kRGB:
		return 4;
	case MatrixColourMode::kRGB3BPP:
		return std::ceil(Height / 8) * 3;

	default:
		if (Height >= 0 && Height <= 8)
			return 1;
		else if (Height >= 9 && Height <= 16)
			return 2;
		else if (Height >= 17 && Height <= 32)
			return 4;
		else if (Height >= 33 && Height <= 64)
			return 8;
		else
			return 0;
	}
}
#pragma end_region


#pragma region Automation
void LayerHandler::PerformScroll(int mode, int layer, int frame)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
	{
	case kEffectScrollLeft:
		for (int x = 0; x <= Width - 2; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x + 1)];
			}
		}

		for (int y = 0; y < Height; y++)
		{
			Layers[layer]->Cells[frame]->Grid[y * Width + (Width - 1)] = MatrixBackup->Grid[y * Width];
		}
		break;
	case kEffectScrollRight:
		for (int x = 1; x < Width; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				 Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x - 1)];
			}
		}

		for (int y = 0; y < Height; y++)
		{
			Layers[layer]->Cells[frame]->Grid[y * Width] = MatrixBackup->Grid[y * Width + (Width - 1)];
		}
		break;
	case kEffectScrollUp:
		for (int y = 0; y < Height - 1; y++)
		{
			for (int x = 0; x < Width; x++)
			{
				 Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(y + 1) * Width + x];
			}
		}

		for (int x = 0; x < Width; x++)
		{
			Layers[layer]->Cells[frame]->Grid[(Height - 1) * Width + x] = MatrixBackup->Grid[x];
		}
		break;
	case kEffectScrollDown:
		for (int y = 1; y < Height; y++)
		{
			for (int x = 0; x < Width; x++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(y - 1) * Width + x];
			}
		}

		for (int x = 0; x < Width; x++)
		{
			Layers[layer]->Cells[frame]->Grid[x] = MatrixBackup->Grid[(Height - 1) * Width + x];
		}
		break;
	}

	Layers[layer]->Cells[frame]->AddToHistory();
}


void LayerHandler::ScrollRow(int layer, int frame, int mode, int row)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	switch (mode)
	{
	case kEffectScrollRowLeft:
	{
		int pixel = Layers[layer]->Cells[frame]->Grid[row * Width];

		for (int x = 0; x <= Width - 2; x++)
		{
			Layers[layer]->Cells[frame]->Grid[row * Width + x] = Layers[layer]->Cells[frame]->Grid[row * Width + (x + 1)];
		}

		Layers[layer]->Cells[frame]->Grid[row * Width + (Width - 1)] = pixel;
		break;
	}
	case kEffectScrollRowRight:
	{
		int pixel = Layers[layer]->Cells[frame]->Grid[row * Width + (Width - 1)];

		for (int x = Width - 1; x >= 1; x--)
		{
			Layers[layer]->Cells[frame]->Grid[row * Width + x] = Layers[layer]->Cells[frame]->Grid[row * Width + (x - 1)];
		}

		Layers[layer]->Cells[frame]->Grid[row * Width] = pixel;
		break;
	}
	}
}


void LayerHandler::ScrollColumn(int layer, int frame, int mode, int column)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	switch (mode)
	{
	case kEffectScrollColumnUp:
	{
		int pixel = Layers[layer]->Cells[frame]->Grid[column];

		for (int y = 0; y < Height; y++)
		{
			Layers[layer]->Cells[frame]->Grid[y * Width + column] = Layers[layer]->Cells[frame]->Grid[(y + 1) * Width + column];
		}

		Layers[layer]->Cells[frame]->Grid[(Height - 1) * Width + column] = pixel;
		break;
	}
	case kEffectScrollColumnDown:
	{
		int pixel = Layers[layer]->Cells[frame]->Grid[(Height - 1) * Width + column];

		for (int y = Height - 1; y >= 1; y--)
		{
			Layers[layer]->Cells[frame]->Grid[y * Width + column] = Layers[layer]->Cells[frame]->Grid[(y - 1) * Width + column];
		}

		Layers[layer]->Cells[frame]->Grid[column] = pixel;
		break;
	}
	}
}


void LayerHandler::PerformSplitScroll(int mode, int layer, int frame)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	switch (mode)
	{
	case kEffectSplitScrollLeftRight:
	case kEffectSplitScrollRightLeft:
	{
		int mid = std::round(Height / 2) - 1;

		int	a = kEffectScrollRowLeft;
		int b = kEffectScrollRowRight;

		switch (mode)
		{
		case kEffectSplitScrollLeftRight:
			a = kEffectScrollRowLeft;
			b = kEffectScrollRowRight;
			break;
		case kEffectSplitScrollRightLeft:
			a = kEffectScrollRowRight;
			b = kEffectScrollRowLeft;
			break;
		}

		for (int row = 0; row <= mid; row++)
		{
			ScrollRow(layer, frame, a, row);
		}

		for (int row = mid + 1; row < Height; row++)
		{
			ScrollRow(layer, frame, b, row);
		}
		break;
	}
	case kEffectSplitScrollUpDown:
	case kEffectSplitScrollDownUp:
	{
		int mid = std::round(Width / 2) - 1;
		int a = kEffectScrollColumnUp;
		int b = kEffectScrollColumnDown;

		switch (mode)
		{
		case kEffectSplitScrollUpDown:
			a = kEffectScrollColumnUp;
			b = kEffectScrollColumnDown;
			break;
		case kEffectSplitScrollDownUp:
			a = kEffectScrollColumnDown;
			b = kEffectScrollColumnUp;
			break;
		}

		for (int column = 0; column <= mid; column++)
		{
			ScrollColumn(layer, frame, a, column);
		}

		for (int column = mid + 1; column < Width; column++)
		{
			ScrollColumn(layer, frame, b, column);
		}
		break;
	}
	}
}


void LayerHandler::PerformAlternateScroll(int mode, int layer, int frame)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	switch (mode)
	{
	case kEffectAlternateScrollUpDown:
	case kEffectAlternateScrollDownUp:
	{
		int coeff = std::round((double)Width / 4);

		int count = 0;
		int mode  = kEffectScrollColumnUp;

		for (int t = 0; t < Width; t++)
		{
			ScrollColumn(layer, frame, mode, t);

			count++;

			if (count == coeff)
			{
				count = 0;

				if (mode == kEffectScrollColumnUp)
				{
					mode = kEffectScrollColumnDown;
				}
				else
				{
					mode = kEffectScrollColumnUp;
				}
			}
		}
		break;
	}
	}
}


void LayerHandler::RotateFrame(int mode, int layer, int frame)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
	{
	case kEffectRotateCW:
		for (int x = 0; x < Width; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(Width - x - 1) * Width + y];
			}
		}
		break;
	case kEffectRotateACW:
		for (int x = 0; x < Width; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[x * Width + (Height - y - 1)];
			}
		}
		break;
	}

	Layers[layer]->Cells[frame]->AddToHistory();
}


void LayerHandler::RotateFrameAllLayersAnyAngle(double angle, int toframe)
{
	double aradians = (3.1415926535 * angle) / 180;
	int hx = std::round(((double)Width - 1) / 2);
	int hy = std::round(((double)Height - 1) / 2);

	for (int layer = 0; layer < Layers.size(); layer++)
	{
    	if (IsThisFrameLocked(layer, toframe) || !Layers[layer]->Visible) continue;

		Layers[layer]->Cells[toframe]->Clear(ColourMode, RGBBackground);

		for (int x = 0; x < Width; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				int ox = x - hx;
				int oy = y - hy;

				int newx = hx + std::round((ox * std::cos(aradians)) - (oy * std::sin(aradians)));
				int newy = hy + std::round((ox * std::sin(aradians)) + (oy * std::cos(aradians)));

				switch (ColourMode)
				{
				case MatrixColourMode::kRGB:
				case MatrixColourMode::kRGB3BPP:
					if (newx >= 0 && newx < Width && newy >= 0 && newy < Height)
					{
						Layers[layer]->Cells[toframe]->Grid[newy * Width + newx] = MatrixBackup->Grid[y * Width + x];
					}
                    break;

				default:
					if (MatrixBackup->Grid[y * Width + x] > 0)
					{
						if (newx >= 0 && newx < Width && newy >= 0 && newy < Height)
						{
							Layers[layer]->Cells[toframe]->Grid[newy * Width + newx] = MatrixBackup->Grid[y * Width + x];
						}
					}
				}
			}
		}
	}
}


void LayerHandler::RotateFrameAnyAngle(double angle, int layer, int toframe)
{
	if (IsThisFrameLocked(layer, toframe) || !Layers[layer]->Visible) return;

	Layers[layer]->Cells[toframe]->Clear(ColourMode, RGBBackground);

	double aradians = (3.1415926535 * angle) / 180;
	int hx = std::round(((double)Width - 1) / 2);
	int hy = std::round(((double)Height - 1) / 2);

	for (int x = 0; x < Width; x++)
	{
		for (int y = 0; y < Height; y++)
		{
			int ox = x - hx;
			int oy = y - hy;

			int newx = hx + std::round((ox * std::cos(aradians)) - (oy * std::sin(aradians)));
			int newy = hy + std::round((ox * std::sin(aradians)) + (oy * std::cos(aradians)));

			switch (ColourMode)
			{
			case MatrixColourMode::kRGB:
			case MatrixColourMode::kRGB3BPP:
				if (newx >= 0 && newx < Width && newy >= 0 && newy < Height)
				{
					Layers[layer]->Cells[toframe]->Grid[newy * Width + newx] = MatrixBackup->Grid[y * Width + x];
				}
                break;

			default:
				if (MatrixBackup->Grid[y * Width + x] > 0)
				{
					if (newx >= 0 && newx < Width && newy >= 0 && newy < Height)
					{
						Layers[layer]->Cells[toframe]->Grid[newy * Width + newx] = MatrixBackup->Grid[y * Width + x];
					}
				}
			}
		}
	}
}


void LayerHandler::RotateMultiOption(int mode, int multimode, int currentlayer, int currentframe)
{
	switch (multimode)
	{
	case kMOMCurrentOnly:
		RotateFrame(mode, currentlayer, currentframe);
		break;
	case kMOMCurrentFrameLayers:
		for (int layer = 0; layer < Layers.size(); layer++)
		{
			RotateFrame(mode, layer, currentframe);
		}
		break;
	case kMOMCurrentLayerFrames:
		for (int frame = 0; frame < Layers[currentlayer]->Cells.size(); frame++)
		{
			RotateFrame(mode, currentlayer, frame);
		}
		break;
	case kMOMAll:
		for (int layer = 0; layer < Layers.size(); layer++)
		{
			for (int frame = 0; frame < Layers[layer]->Cells.size(); frame++)
			{
				RotateFrame(mode, layer, frame);
			}
		}
		break;
	}
}


void LayerHandler::PerformWipeOnFrame(int mode, int layer, int frame, bool clear)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
    {
	case kEffectWipeVerticalOut:
	{
		int z = std::round((double)Width / 2);

		for  (int x = 0; x <= z - 2; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x + 1)];
			}
		}

		for (int x = Width - 1; x >= z + 1; x--)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x - 1)];
			}
		}

		for (int y = 0; y < Height; y++)
		{
			if (clear)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + (z - 1)] = RGBBackground;
				Layers[layer]->Cells[frame]->Grid[y * Width + z]     = RGBBackground;
			}
			else
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + (z - 1)] = MatrixBackup->Grid[y * Width];
				Layers[layer]->Cells[frame]->Grid[y * Width + z]     = MatrixBackup->Grid[y * Width + (Width - 1)];
			}
		}
		break;
	}
	case kEffectWipeVerticalIn:
	{
		int z = std::round((double)Width / 2);

		for (int x = 1; x <= z - 1; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x - 1)];
			}
		}

		for (int x = Width - 1; x >= z; x--)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x + 1)];
			}
		}

		for (int y = 0; y < Height; y++)
		{
			if (clear)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width] = RGBBackground;
				Layers[layer]->Cells[frame]->Grid[y * Width + (Width - 1)] = RGBBackground;
			}
			else
			{
				Layers[layer]->Cells[frame]->Grid[y * Width] = MatrixBackup->Grid[y * Width + (z - 1)];
				Layers[layer]->Cells[frame]->Grid[y * Width + (Width - 1)] = MatrixBackup->Grid[y * Width + z];
			}
		}
		break;
	}
	case kEffectWipeHorizontalOut:
	{
		int z = std::round((double)Height / 2);

		for (int y = 0; y <= z - 2; y++)
		{
			for (int x = 0; x < Width; x++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(y + 1) * Width + x];
			}
		}

		for (int y = Height - 1; y >= z + 1; y--)
		{
			for (int x = 0; x < Width; x++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(y - 1) * Width + x];
			}
		}

		for (int x = 0; x < Width; x++)
		{
			if (clear)
			{
				Layers[layer]->Cells[frame]->Grid[(z - 1) * Width + x] = RGBBackground;
				Layers[layer]->Cells[frame]->Grid[z * Width + x] = RGBBackground;
			}
			else
			{
				Layers[layer]->Cells[frame]->Grid[(z - 1) * Width + x] = MatrixBackup->Grid[x];
				Layers[layer]->Cells[frame]->Grid[z * Width + x] = MatrixBackup->Grid[(Height - 1) * Width + x];
			}
		}
		break;
	}
	case kEffectWipeHorizontalIn:
	{
		int z = std::round((double)Height / 2);

		for (int y = 1; y < z; y++)
		{
			for (int x = 0; x < Width; x++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(y - 1) * Width + x];
			}
		}

		for (int y = Height - 1; y >= z; y--)
		{
			for (int x = 0; x < Width; x++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(y + 1) * Width + x];
			}
		}

		for (int x = 0; x < Width; x++)
		{
			if (clear)
			{
				Layers[layer]->Cells[frame]->Grid[x] = RGBBackground;
				Layers[layer]->Cells[frame]->Grid[(Height - 1) * Width + x] = RGBBackground;
			}
			else
			{
				Layers[layer]->Cells[frame]->Grid[x]                = MatrixBackup->Grid[(z - 1) * Width + x];
				Layers[layer]->Cells[frame]->Grid[(Height - 1) * Width + x] = MatrixBackup->Grid[z * Width + x];
			}
		}
		break;
	}
	case kEffectWipeLeftToRight:
	{
		for (int x = 0; x <= Width - 2; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x + 1)];
			}
		}

		for (int y = 0; y < Height; y++)
		{
			if (clear)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + (Width - 1)] = RGBBackground;
			}
			else
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + (Width - 1)] = MatrixBackup->Grid[y * Width];
			}
		}
		break;
	}
	case kEffectWipeRightToLeft:
	{
		for (int x = 1; x < Width; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x - 1)];
			}
		}

		for (int y = 0; y < Height; y++)
		{
			if (clear)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width] = RGBBackground;
			}
			else
			{
				Layers[layer]->Cells[frame]->Grid[y * Width] = MatrixBackup->Grid[y * Width + (Width - 1)];
			}
		}
		break;
	}
	case kEffectWipeUpToDown:
	{
		for (int y = 0; y <= Height - 2; y++)
		{
			for (int x = 0; x < Width; x++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(y + 1) * Width + x];
			}
		}

		for (int x = 0; x < Width; x++)
		{
			if (clear)
			{
				Layers[layer]->Cells[frame]->Grid[(Height - 1) * Width + x] = RGBBackground;
			}
			else
			{
				Layers[layer]->Cells[frame]->Grid[(Height - 1) * Width + x] = MatrixBackup->Grid[x];
			}
		}
		break;
	}
	case kEffectWipeDownToUp:
	{
		for (int y = 1; y < Height; y++)
		{
			for (int x = 0; x < Width; x++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(y - 1) * Width + x];
			}
		}

		for (int x = 0; x < Width; x++)
		{
			if (clear)
			{
				Layers[layer]->Cells[frame]->Grid[x] = RGBBackground;
			}
			else
			{
				Layers[layer]->Cells[frame]->Grid[x] = MatrixBackup->Grid[(Height - 1) * Width + x];
			}
		}
		break;
	}
	}
}


void LayerHandler::PerformRevealOnFrame(int mode, int layer, int frame, int colour, int &parameter)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
	{
	case kEffectRevealLeftRight:
		if (parameter < Width)
		{
			for (int x = parameter; x < Width; x++)
			{
				for (int y = 0; y < Height; y++)
				{
					Layers[layer]->Cells[frame]->Grid[y * Width + x] = colour;
				}
			}

			parameter++;
		}
		break;
	case kEffectRevealRightLeft:
		if (parameter >= 0)
		{
			for (int x = parameter; x >= 0; x--)
			{
				for (int y = 0; y < Height; y++)
				{
					Layers[layer]->Cells[frame]->Grid[y * Width + x] = colour;
				}
			}

			parameter--;
		}
		break;
	case kEffectRevealTopBottom:
		if (parameter < Height)
		{
			for (int y = parameter; y < Height; y++)
			{
				for (int x = 0; x < Width; x++)
				{
					Layers[layer]->Cells[frame]->Grid[y * Width + x] = colour;
				}
			}

			parameter++;
		}
		break;
	case kEffectRevealBottomTop:
		if (parameter >= 0)
		{
			for (int y = parameter; y >= 0; y--)
			{
				for (int x = 0; x < Width; x++)
				{
					Layers[layer]->Cells[frame]->Grid[y * Width + x] = colour;
				}
			}

			parameter--;
		}
		break;
	case kEffectRevealCentreIn:
	case kEffectRevealCentreOut:
		break;
	}

	Layers[layer]->Cells[frame]->AddToHistory();
}


void LayerHandler::PerformColumnScrollOnCurrentFrame(int mode, int layer, int frame, int column, bool clear)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
	{
	case kEffectScrollUp:
		for (int y = 0; y <= Height - 2; y++)
		{
			Layers[layer]->Cells[frame]->Grid[y * Width + column] = MatrixBackup->Grid[(y + 1) * Width + column];
		}

		if (clear)
		{
			Layers[layer]->Cells[frame]->Grid[(Height - 1) * Width + column] = 0;
		}
		else
		{
			Layers[layer]->Cells[frame]->Grid[(Height - 1) * Width + column] = MatrixBackup->Grid[column];
		}
		break;
	case kEffectScrollDown:
		for (int y = 1; y < Height; y++)
		{
			Layers[layer]->Cells[frame]->Grid[y * Width + column] = MatrixBackup->Grid[(y - 1) * Width + column];
		}

		if (clear)
		{
			Layers[layer]->Cells[frame]->Grid[column] = 0;
		}
		else
		{
			Layers[layer]->Cells[frame]->Grid[column] = MatrixBackup->Grid[(Height - 1) * Width + column];
		}
		break;
	}

	Layers[layer]->Cells[frame]->AddToHistory();
}


void LayerHandler::PerformRowScrollOnFrame(int mode, int layer, int frame, int row, bool clear)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
	{
	case kEffectScrollLeft:
		for (int x = 0; x < Width - 1; x++)
		{
			Layers[layer]->Cells[frame]->Grid[row * Width + x] = MatrixBackup->Grid[row * Width + (x + 1)];
		}

		if (clear)
		{
			Layers[layer]->Cells[frame]->Grid[row * Width + (Width - 1)] = 0;
		}
		else
		{
			Layers[layer]->Cells[frame]->Grid[row * Width + (Width - 1)] = MatrixBackup->Grid[row];
		}
		break;
	case kEffectScrollRight:
		for (int x = 1; x < Width; x++)
		{
			Layers[layer]->Cells[frame]->Grid[row * Width + x] = MatrixBackup->Grid[row * Width + (x - 1)];
		}

		if (clear)
		{
			Layers[layer]->Cells[frame]->Grid[row] = 0;
		}
		else
		{
			Layers[layer]->Cells[frame]->Grid[row] = MatrixBackup->Grid[row * Width + (Width - 1)];
		}
		break;
	}

	Layers[layer]->Cells[frame]->AddToHistory();
}


void LayerHandler::PerformScrollOnCopyFrame(int layer, int frame, int mode, MatrixGrid *matrixcopy)
{
	BackupMatrix(layer, frame);

	switch (mode)
	{
	case kEffectScrollLeft:
		for (int x = 0; x <= Width - 2; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				matrixcopy->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x + 1)];
			}
		}

		for (int y = 0; y < Height; y++)
		{
			matrixcopy->Grid[y * Width + (Width - 1)] = MatrixBackup->Grid[y * Width];
		}
		break;
	case kEffectScrollRight:
		for (int x = 1; x < Width; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				matrixcopy->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (x - 1)];
			}
		}

		for (int y = 0; y < Height; y++)
		{
			matrixcopy->Grid[y * Width] = MatrixBackup->Grid[y * Width + (Width - 1)];
		}
		break;
	case kEffectScrollUp:
		for (int y = 0; y <= Height - 2; y++)
		{
			for (int x = 0; x < Width; x++)
			{
				matrixcopy->Grid[y * Width + x] = MatrixBackup->Grid[(y + 1) * Width + x];
			}
		}

		for (int x = 0; x < Width; x++)
		{
			matrixcopy->Grid[(Height - 1) * Width + x] = MatrixBackup->Grid[x];
		}
		break;
	case kEffectScrollDown:
		for (int y = 1; y < Height; y++)
		{
			for (int x = 0; x < Width; x++)
			{
				matrixcopy->Grid[y * Width + x] = MatrixBackup->Grid[(y - 1) * Width + x];
			}
		}

		for (int x = 0; x < Width; x++)
		{
			matrixcopy->Grid[x] = MatrixBackup->Grid[(Height - 1) * Width + x];
		}
		break;
	}
}


void LayerHandler::PerformEffect(int mode, int layer, int frame, MatrixGradient gradient)
{
	if (IsThisFrameLocked(layer, frame) || !Layers[layer]->Visible) return;

	BackupMatrix(layer, frame);

	switch (mode)
	{
	case kEffectFlip:
		for (int y = 0; y < Height; y++)
		{
			for (int x = 0; x < Width; x++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[(Height - y - 1) * Width + x];
			}
		}
		break;
	case kEffectMirror:
		for (int x = 0; x < Width; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				Layers[layer]->Cells[frame]->Grid[y * Width + x] = MatrixBackup->Grid[y * Width + (Width - x - 1)];
			}
		}
		break;
	case kEffectInvert:
		for (int z = 0; z < Width * Height; z++)
		{
			switch (ColourMode)
			{
			case MatrixColourMode::kMono:
				Layers[layer]->Cells[frame]->Grid[z] = 1 - Layers[layer]->Cells[frame]->Grid[z];
				break;
			case MatrixColourMode::kBiSequential:
			case MatrixColourMode::kBiBitplanes:
				Layers[layer]->Cells[frame]->Grid[z] = 3 - Layers[layer]->Cells[frame]->Grid[z];
				break;
			case MatrixColourMode::kRGB:
				Layers[layer]->Cells[frame]->Grid[z] = 0xFFFFFF - Layers[layer]->Cells[frame]->Grid[z];
				break;
			case MatrixColourMode::kRGB3BPP:
				Layers[layer]->Cells[frame]->Grid[z] = 0x4 - Layers[layer]->Cells[frame]->Grid[z];
				break;

			default:
				break;
			}
		}
		break;
	case kEffectGradientAll:
		for (int x = 0; x < Width; x++)
		{
			for (int y = 0; y < Height; y++)
			{
				if (gradient.Option == GradientOption::kVertical && gradient.IY[y] != 0)
				{
					if (Layers[layer]->Cells[frame]->Grid[y * Width + x] != 0)
					{
						Layers[layer]->Cells[frame]->Grid[y * Width + x] = gradient.IY[y];
					}
				}
			}
		}
		break;
	}

	Layers[layer]->Cells[frame]->AddToHistory();
}
#pragma end_region


#pragma region File_IO
bool LayerHandler::SaveAnimationGrid(const std::wstring file_name, ImportData &tid, ExportOptions &eeo, ProjectColours &colours,
	const std::wstring comment, MatrixIgnored *ignoredlayout)
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
		file << Formatting::to_utf8(kAnimCommentF +            comment + L"\n");
		file << Formatting::to_utf8(kAnimRGBBackgroundF +      std::to_wstring(RGBBackground) + L"\n");
		file << Formatting::to_utf8(kAnimFrameRangeF +         std::to_wstring(tid.StartFrame) + L"," + std::to_wstring(tid.EndFrame) + L"\n");
		file << Formatting::to_utf8(kAnimLayerCountF +         std::to_wstring(Layers.size()) + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		if (tid.ColourMode == MatrixColourMode::kRGB)
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

			file << Formatting::to_utf8(kAnimColoursLeftF +   std::to_wstring(colours.DrawColours[kMouseLeft]) + L"\n");
			file << Formatting::to_utf8(kAnimColoursMiddleF + std::to_wstring(colours.DrawColours[kMouseMiddle]) + L"\n");
			file << Formatting::to_utf8(kAnimColoursRightF +  std::to_wstring(colours.DrawColours[kMouseRight]) + L"\n");

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		// ===================================================================

		for (int layer = 0; layer < Layers.size(); layer++)
		{
			file << Formatting::to_utf8(L"[" + kFileHeaderLayer + L"\n");
			file << Formatting::to_utf8(kAnimLayerNameF +   Layers[layer]->Name + L"\n");
			file << Formatting::to_utf8(kAnimLayerWidthF +  std::to_wstring(Width) + L"\n");
			file << Formatting::to_utf8(kAnimLayerHeightF + std::to_wstring(Height) + L"\n");
			file << Formatting::to_utf8(kAnimLayerLockedF + std::to_wstring(Layers[layer]->Locked) + L"\n");
			file << Formatting::to_utf8(L"]\n");

			// ===============================================================

			for (int frame = tid.StartFrame; frame <= tid.EndFrame; frame++)
			{
				switch (tid.ColourMode)
				{
				case MatrixColourMode::kMono:
					file << Formatting::to_utf8(L"{" + kFilePrefixMono + L"\n");
					break;
				case MatrixColourMode::kBiSequential:
					file << Formatting::to_utf8(L"{" + kFilePrefixBiSequential + L"\n");
					break;
				case MatrixColourMode::kBiBitplanes:
					file << Formatting::to_utf8(L"{" + kFilePrefixBiBitPlanes + L"\n");
					break;
				case MatrixColourMode::kRGB:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB + L"\n");
					break;
				case MatrixColourMode::kRGB3BPP:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB3BPP + L"\n");
					break;

				default:
					break;
				}

				file << Formatting::to_utf8(kAnimWidthF  + std::to_wstring(Width) + L"\n");
				file << Formatting::to_utf8(kAnimHeightF + std::to_wstring(Height) + L"\n");

				for (int y = 0; y < Height; y++)
				{
					std::wstring s = L"";

					for (int x = 0; x < Width; x++)
					{
						s += IntToHex(Layers[layer]->Cells[frame]->Grid[y * Width + x], 6) + L" ";
					}

					file << Formatting::to_utf8(kAnimRowDataF + s + L"\n");
				}

				file << Formatting::to_utf8(kAnimFrameLockedF + std::to_wstring(Layers[layer]->Cells[frame]->Locked) + L"\n");

				file << Formatting::to_utf8(kDataBlockEndS + L"\n");
			}
		}


		// ===========================================================================

		file << Formatting::to_utf8(L"{" + kFileHeaderIgnoredPixel + L"\n");

		for (int y = 0; y < Height; y++)
		{
			std::wstring s = L"";

			for (int x = 0; x < Width; x++)
			{
				s += std::to_wstring(ignoredlayout->Grid[y * Width + x]) + L" ";
			}

			file << Formatting::to_utf8(kAnimIgnoredPixelDataF + s + L"\n");
		}

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		file.close();

		return true;
	}

	return false;
}


bool LayerHandler::SaveAnimationFreeform(const std::wstring file_name, ImportData &tid, ExportOptions &eeo, ProjectColours &colours, const std::wstring comment)
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
		file << Formatting::to_utf8(kAnimCommentF +            comment + L"\n");
		file << Formatting::to_utf8(kAnimRGBBackgroundF +      std::to_wstring(RGBBackground) + L"\n");
		file << Formatting::to_utf8(kAnimFrameRangeF +         std::to_wstring(tid.StartFrame) + L"," + std::to_wstring(tid.EndFrame) + L"\n");
		file << Formatting::to_utf8(kAnimLayerCountF +         std::to_wstring(Layers.size()) + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		if (tid.ColourMode == MatrixColourMode::kRGB)
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

			file << Formatting::to_utf8(kAnimColoursLeftF +   std::to_wstring(colours.DrawColours[kMouseLeft]) + L"\n");
			file << Formatting::to_utf8(kAnimColoursMiddleF + std::to_wstring(colours.DrawColours[kMouseMiddle]) + L"\n");
			file << Formatting::to_utf8(kAnimColoursRightF +  std::to_wstring(colours.DrawColours[kMouseRight]) + L"\n");

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		// ===================================================================

		for (int layer = 0; layer < Layers.size(); layer++)
		{
			file << Formatting::to_utf8(L"[" + kFileHeaderFreeformLayer + L"\n");
			file << Formatting::to_utf8(kAnimLayerNameF +   Layers[layer]->Name + L"\n");
			file << Formatting::to_utf8(kAnimLayerLockedF + std::to_wstring(Layers[layer]->Locked) + L"\n");
			file << Formatting::to_utf8(L"]\n");

			// ===============================================================

			for (int pixel = 0; pixel < Layers[layer]->Freeform->Pixels.size(); pixel++)
			{
				switch (tid.ColourMode)
				{
				case MatrixColourMode::kRGB:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB + L"\n");
					break;
				case MatrixColourMode::kRGB3BPP:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB3BPP + L"\n");
					break;

				default:
					break;
				}

				file << Formatting::to_utf8(kAnimPixelXF + std::to_wstring(Layers[layer]->Freeform->Pixels[pixel]->X) + L"\n");
				file << Formatting::to_utf8(kAnimPixelYF + std::to_wstring(Layers[layer]->Freeform->Pixels[pixel]->Y) + L"\n");
				file << Formatting::to_utf8(kAnimPixelOrderF + std::to_wstring(Layers[layer]->Freeform->Pixels[pixel]->Order) + L"\n");
				file << Formatting::to_utf8(kAnimPixelGroupF + std::to_wstring(Layers[layer]->Freeform->Pixels[pixel]->Group) + L"\n");

				for (int colour = 0; colour < Layers[layer]->Freeform->Pixels[pixel]->Colours.size(); colour++)
				{
					file << Formatting::to_utf8(kAnimPixelColourF + std::to_wstring(Layers[layer]->Freeform->Pixels[pixel]->Colours[colour]) + L"\n");
				}

				file << Formatting::to_utf8(kDataBlockEndS + L"\n");
			}

			for (int frame = tid.StartFrame; frame <= tid.EndFrame; frame++)
			{
				file << Formatting::to_utf8(L"{" + kFileHeaderFreeformFrame + L"\n");
				file << Formatting::to_utf8(kAnimFrameLockedF + std::to_wstring(Layers[layer]->Freeform->Frames[frame]->Locked) + L"\n");
				file << Formatting::to_utf8(kDataBlockEndS + L"\n");
			}
		}

		// ===========================================================================

		file.close();

		return true;
	}

	return false;
}


bool LayerHandler::SaveFont(const std::wstring file_name, ImportData &tid, ExportOptions &eeo, const std::wstring comment, MatrixIgnored *ignoredlayout)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kFileHeaderFontHeader + L"\n");

		file << Formatting::to_utf8(kAnimPreviewEnabledF +     std::to_wstring(tid.Preview.Enabled) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewSizeF +        std::to_wstring(tid.Preview.Size) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewViewF +        std::to_wstring(tid.Preview.ViewToInt()) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewVoidF +        std::to_wstring(tid.Preview.Void) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewOffsetF +      std::to_wstring(tid.Preview.Offset) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewDirectionF +   std::to_wstring(tid.Preview.OffsetDirection) + L"\n");
		file << Formatting::to_utf8(kAnimPreviewIncRadiallyF + std::to_wstring(tid.Preview.IncrementRadially) + L"\n");

		eeo.SaveToFile(file);

		file << Formatting::to_utf8(kAnimAutomationFileNameF + tid.AutomationFileName + L"\n");
		file << Formatting::to_utf8(kAnimCommentF +            comment + L"\n");
		file << Formatting::to_utf8(kAnimRGBBackgroundF +      std::to_wstring(RGBBackground) + L"\n");
		file << Formatting::to_utf8(kAnimFrameRangeF +         std::to_wstring(tid.StartFrame) + L"," + std::to_wstring(tid.EndFrame) + L"\n");
		file << Formatting::to_utf8(kAnimLayerCountF +         std::to_wstring(Layers.size()) + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		for (int layer = 0; layer < Layers.size(); layer++)
		{
			file << Formatting::to_utf8(L"[" + kFileHeaderLayer + L"\n");
			file << Formatting::to_utf8(kAnimLayerNameF +   Layers[layer]->Name + L"\n");
			file << Formatting::to_utf8(kAnimLayerWidthF +  std::to_wstring(Width) + L"\n");
			file << Formatting::to_utf8(kAnimLayerHeightF + std::to_wstring(Height) + L"\n");
			file << Formatting::to_utf8(kAnimLayerLockedF + std::to_wstring(Layers[layer]->Locked) + L"\n");
			file << Formatting::to_utf8(L"]\n");

			for (int i = 1; i <= kFontCharacterCount; i++)
			{
				switch (tid.ColourMode)
				{
				case MatrixColourMode::kMono:
					file << Formatting::to_utf8(L"{" + kFilePrefixMono + L"\n");
					break;
				case MatrixColourMode::kBiSequential:
					file << Formatting::to_utf8(L"{" + kFilePrefixBiSequential + L"\n");
					break;
				case MatrixColourMode::kBiBitplanes:
					file << Formatting::to_utf8(L"{" + kFilePrefixBiBitPlanes + L"\n");
					break;
				case MatrixColourMode::kRGB:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB + L"\n");
					break;
				case MatrixColourMode::kRGB3BPP:
					file << Formatting::to_utf8(L"{" + kFilePrefixRGB3BPP + L"\n");
					break;

				default:
					break;
				}

				file << Formatting::to_utf8(kAnimWidthF +  std::to_wstring(Width) + L"\n");
				file << Formatting::to_utf8(kAnimHeightF + std::to_wstring(Height) + L"\n");

				for (int y = 0; y < Height; y++)
				{
					std::wstring s = L"";

					for (int x = 0; x < Width; x++)
					{
						s += IntToHex(Layers[layer]->Cells[i]->Grid[y * Width + x], 6) + L" ";
					}

					file << Formatting::to_utf8(kAnimRowDataF + s + L"\n");
				}

				file << Formatting::to_utf8(kDataBlockEndS + L"\n");
			}
		}

		// ===========================================================================

		file << Formatting::to_utf8(L"{" + kFileHeaderIgnoredPixel + L"\n");

		for (int y = 0; y < Height; y++)
		{
			std::wstring s = L"";

			for (int x = 0; x < Width; x++)
			{
				s += std::to_wstring(ignoredlayout->Grid[y * Width + x]) + L" ";
			}

			file << Formatting::to_utf8(kAnimIgnoredPixelDataF + s + L"\n");
		}

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		file.close();

		return true;
	}

	return false;
}


bool LayerHandler::SaveSingleFrame(const std::wstring file_name, ImportData tid, int frame, const std::wstring comment, MatrixIgnored *ignoredlayout)
{
	std::ofstream file(file_name);

	if (file)
	{
		switch (tid.ColourMode)
		{
		case MatrixColourMode::kMono:
			file << Formatting::to_utf8(L"{" + kFramePrefixMono + L"\n");
			break;
		case MatrixColourMode::kBiSequential:
			file << Formatting::to_utf8(L"{" + kFramePrefixBiSequential + L"\n");
			break;
		case MatrixColourMode::kBiBitplanes:
			file << Formatting::to_utf8(L"{" + kFramePrefixBiBitPlanes + L"\n");
			break;
		case MatrixColourMode::kRGB:
			file << Formatting::to_utf8(L"{" + kFramePrefixRGB + L"\n");
			break;
		case MatrixColourMode::kRGB3BPP:
			file << Formatting::to_utf8(L"{" + kFramePrefixRGB3BPP + L"\n");
			break;

		default:
			break;
		}

		file << Formatting::to_utf8(kAnimWidthF + std::to_wstring(Width) + L"\n");
		file << Formatting::to_utf8(kAnimHeightF + std::to_wstring(Height) + L"\n");
		file << Formatting::to_utf8(kAnimCommentF + comment + L"\n");
		file << Formatting::to_utf8(kAnimRGBBackgroundF + std::to_wstring(RGBBackground) + L"\n");

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===========================================================================

		for (int layer = 0; layer < Layers.size(); layer++)
		{
			file << Formatting::to_utf8(L"[" + kFileHeaderLayer + L"\n");
			file << Formatting::to_utf8(kAnimLayerNameF +   Layers[layer]->Name + L"\n");
			file << Formatting::to_utf8(kAnimLayerWidthF +  std::to_wstring(Width) + L"\n");
			file << Formatting::to_utf8(kAnimLayerHeightF + std::to_wstring(Height) + L"\n");
			file << Formatting::to_utf8(L"]\n");

			file << Formatting::to_utf8(kDataBlockStartS + L"\n");

			for (int y = 0; y < Height; y++)
			{
				std::wstring s = L"";

				for (int x = 0; x < Width; x++)
				{
					s += IntToHex(Layers[layer]->Cells[frame]->Grid[y * Width + x], 6) + L" ";
				}

				file << Formatting::to_utf8(kAnimRowDataF + s + L"\n");
			}

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		// ===========================================================================

		file << Formatting::to_utf8(L"{" + kFileHeaderIgnoredPixel + L"\n");

		for (int y = 0; y < Height; y++)
		{
			std::wstring s = L"";

			for (int x = 0; x < Width; x++)
			{
				s += std::to_wstring(ignoredlayout->Grid[y * Width + x]) + L" ";
			}

			file << Formatting::to_utf8(kAnimIgnoredPixelDataF + s + L"\n");
		}

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		// ===================================================================

		file.close();

		return true;
	}

	return false;
}


bool LayerHandler::SaveAsTextToolFont(const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		for (int t = 1; t < kFontCharacterCount; t++)
		{
			std::wstring s = L"";

			for (int x = 0; x < Width; x++)
			{
				int mydata = 0;

				for (int y = 0; y < Height; y++)
				{
					if (Layers[kPermanentLayer]->Cells[t]->Grid[y * Width + x] == 1)
					{
						mydata = mydata + (kPowers[Height - y - 1]);
					}
				}

				if (x != Width - 1)
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

		return true;
	}

	return false;
}


bool LayerHandler::SaveAsRGBFont(const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kFileHeaderFontRGB + L"\n");
		file << Formatting::to_utf8(kRGBFontWidthF  + std::to_wstring(Width) + L"\n");
		file << Formatting::to_utf8(kRGBFontHeightF + std::to_wstring(Height) + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		for (int t = 0; t < kFontCharacterCount; t++)
		{
			file << Formatting::to_utf8(L"{" + kFontPrefixChar + L"\n");

			for (int x = 0; x < Width; x++)
			{
				std::wstring mydata = L"";

				for (int y = 0; y < Height; y++)
				{
					if (Layers[kPermanentLayer]->Cells[t]->Grid[y * Width + x] != RGBBackground)
					{
						mydata += IntToHex(Layers[kPermanentLayer]->Cells[t]->Grid[y * Width + x], 6).c_str();

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

		return true;
	}

	return false;
}
#pragma end_region


#pragma region Debug
// generates a very simple test pattern
void LayerHandler::TestSignal(int layer)
{
	int y = 0;

	for (int x = 0; x < Width; x++)
	{
		switch (ColourMode)
		{
		case MatrixColourMode::kMono:
		case MatrixColourMode::kBiSequential:
		case MatrixColourMode::kBiBitplanes:
			Layers[layer]->Cells[0]->Grid[y * Width + x] = 1;
			break;
		case MatrixColourMode::kRGB:
			Layers[layer]->Cells[0]->Grid[y * Width + x] = 0x0044ff;
			break;
		}

		if (y < Height - 1)
		{
			y++;
		}
		else
		{
			y = 0;
		}
	}

	switch (ColourMode)
	{
	case MatrixColourMode::kMono:
	case MatrixColourMode::kBiSequential:
	case MatrixColourMode::kBiBitplanes:
		Layers[layer]->Cells[0]->Grid[0] = 1;
		Layers[layer]->Cells[0]->Grid[Width - 1] = 1;
		Layers[layer]->Cells[0]->Grid[(Height - 1) * Width] = 1;
		Layers[layer]->Cells[0]->Grid[(Height - 1) * Width + (Width - 1)] = 1;
		break;
	case MatrixColourMode::kRGB:
		Layers[layer]->Cells[0]->Grid[0] = 0x00ff44;
		Layers[layer]->Cells[0]->Grid[Width - 1] = 0x00ff44;
		Layers[layer]->Cells[0]->Grid[(Height - 1) * Width] = 0x00ff44;
		Layers[layer]->Cells[0]->Grid[(Height - 1) * Width + (Width - 1)] = 0x00ff44;
		break;
	}
}
#pragma end_region
