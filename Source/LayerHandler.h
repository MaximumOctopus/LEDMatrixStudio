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

#pragma once

#include <tuple>
#include <vector>

#include "Colours.h"
#include "Gradient.h"
#include "ImportData.h"
#include "Layer.h"
#include "MatrixIgnored.h"


class LayerHandler
{

	MatrixGrid *MatrixBackup;

	void BackupMatrix(int, int);

public:

	int Width = 0;
	int Height = 0;
	int RGBBackground = 0x000000;

	SoftwareMode Software = SoftwareMode::kAnimation;
	MatrixDrawMode DrawMode = MatrixDrawMode::kGrid;
	MatrixColourMode ColourMode = MatrixColourMode::kNone;

	std::vector<Layer*> Layers;

	LayerHandler(const std::wstring);
	~LayerHandler();

    void SetSystem(int, int, int, SoftwareMode, MatrixDrawMode, MatrixColourMode);

	bool IsThisFrameLocked(int, int);

	int GetLayerCount();
	int GetFrameCount();
	int GetPixelCount();

	std::wstring GetLayerName(int);
	void SetLayerName(const std::wstring, int);

    bool AreLayersIdentical(int, int, int);

	bool IsLocked(int, int);
	void UnlockLayer(int);
	void LockLayer(int);
	void UnLockFrame(int, int);
	void LockFrame(int, int);
	void LockUnLockRange(int, int, int, bool);

	void ClearLayerAllFrames(int);

	// colour counting
	int CountColoursFrame(int);
	int CountColoursAnimation();
	void GetFirst32Colours(std::vector<int> &);

	// colours
	void FadeFirstToLast(int);
	void ChangeColourAll(int, int);
	void ClearAllFramesGradient(int, int, MatrixGradient, int[6]);
	void GradientFillFrame(int, int, MatrixGradient, int[6]);

	void AddLayer(const std::wstring);
	bool AddLayerSilent(const std::wstring);
	void AddLayerAsCopy(const std::wstring, int);

	void DeleteLayer(int);

    int DeleteFrame(int, int);

	void WipeAllFrames(int);
	void WipeAllFramesAllLayers();

	void InsertBlankFrameAt(int);
	void InsertCopyFrameAt(int, int);

	void EnsureLayerCoherence();

	std::tuple<int, int> GetPixelBounds();
	int RightBounds(int, int);
	int BottomBounds(int, int);

	// == statistics
	int GetUndoCount(int, int);
	int GetTotalUndos();
	int CalculateMemoryUsage();
	int DataSizeBytes();

	// == automation etc.
	void PerformScroll(int, int, int);
	void ScrollRow(int, int, int, int);
	void ScrollColumn(int, int, int, int);

	void PerformSplitScroll(int, int, int);
	void PerformAlternateScroll(int, int, int);

	void RotateFrame(int, int, int);
	void RotateFrameAllLayersAnyAngle(double, int);
	void RotateFrameAnyAngle(double, int, int);
	void RotateMultiOption(int, int, int, int);

	void PerformWipeOnFrame(int, int, int, bool);
	void PerformRevealOnFrame(int, int, int, int, int&);
	void PerformColumnScrollOnCurrentFrame(int, int, int, int, bool);
	void PerformRowScrollOnFrame(int, int, int, int, bool);

    void PerformScrollOnCopyFrame(int, int, int, MatrixGrid *);

	void PerformEffect(int, int, int, MatrixGradient);

	// == file i/o
	bool SaveAnimationGrid(const std::wstring, ImportData &, ExportOptions &, ProjectColours &, const std::wstring, MatrixIgnored*);
	bool SaveAnimationFreeform(const std::wstring, ImportData &, ExportOptions &, ProjectColours &, const std::wstring);
	bool SaveFont(const std::wstring, ImportData &, ExportOptions &, const std::wstring, MatrixIgnored *);
	bool SaveSingleFrame(const std::wstring, ImportData, int, const std::wstring, MatrixIgnored *);
	bool SaveAsTextToolFont(const std::wstring);
	bool SaveAsRGBFont(const std::wstring);

	// == debug
	void TestSignal(int layer);
};
