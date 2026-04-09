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


LayerHandler::LayerHandler()
{
	Layer *layer = new Layer(GLanguageHandler->Text[kBottomLayer]);

	Layers.push_back(layer);
}


LayerHandler::~LayerHandler()
{
}


/*
bool LayerHandler::AddLayerSilent(const std::wstring name)
{
	Layer *layer = new Layer(name);

	Layers.push_back(layer);

	if (Details.DrawMode == MatrixDrawMode::kGrid)
	{
		for (int t = 0; t < Layers[kPermanentLayer]->Cells.size(); t++)
		{
			MatrixGrid *m = new MatrixGrid(Details.Width, Details.Height, Details.ColourMode, RGBBackground);

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
}*/
