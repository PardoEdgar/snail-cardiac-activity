// --- Analysis for multiple regions (4 regions of heart) ---

dir = ""; //add directory path
outDir = ""; //add directory path to save data

videoNums = newArray(); //// Add individual IDs to analysis (e.g. newArray(1,2,3))


for (v = 0; v < videoNums.length; v++) {
    n = videoNums[v];
    videoPath = dir + "name" +  n + ".mp4"; //add video name; n = Individual ID
    print("Processing video: name " + n);//add video name
    run("Movie (FFMPEG)...", "open=[" + videoPath + "] first_frame=0 last_frame=-1");
		run("8-bit");
		run("Fire");
	    nPixels = 0;
        mean = 0;
        min = 0;
        max = 0;
        std = 0;
		// Get intensity values from the stack
		getRawStatistics(nPixels, mean, min, max, std);

//  Define maximum intensity threshold
umbralMean = 30;

// If the maximum intensity is less than or equal to the threshold, apply adjustments
if (mean <= 30) {
  run("Window/Level...");
    run("Enhance Contrast", "saturated=0.35");
    setMinAndMax(-3, 140);
    run("Apply LUT", "stack");   }
else {
   print("No adjustment is applied. The maximum intensity is " + mean);
}

// Parameters for defining four heart regions
nRegions = 4;
rectHeight = 100;
spacing = 1; 
startY = 16;
startX = 96;
rectWidth = 150;
nFrames = nSlices;

// Save accumulated changes per region
ChangesPerRegion = newArray(nRegions);
ReferencePoints = newArray(nRegions);

for (s = 0; s < nRegions; s++) {
    y = startY + s * (rectHeight + spacing);
    Profiles = newArray(nFrames * rectWidth);

    // Extract profiles for the current region
    for (i = 0; i < nFrames; i++) {
        setSlice(i + 1);
        makeRectangle(startX, y, rectWidth, rectHeight);
        roiManager("Add");
        roiManager("Select", roiManager("Count") - 1);
        roiManager("Rename", "rectangle");
        roiManager("Update");
        
        prof = getProfile(); // vertical mean intensity
        
        for (j = 0; j < rectWidth; j++) {
            Profiles[i * rectWidth + j] = prof[j];
        }
    }

    // Calculate accumulated changes by X-position in this section
    accumulatedChanges = newArray(rectWidth);
    for (j = 0; j < rectWidth; j++) {
        accumulatedChanges[j] = 0;
        for (i = 1; i < nFrames; i++) {
            idx1 = i * rectWidth + j;
            idx0 = (i - 1) * rectWidth + j;
            change = abs(Profiles[idx1] - Profiles[idx0]);
            accumulatedChanges[j] += change;
        }
    }

    // Find maximun accumulated change
    maxChange = -1;
    for (j = 0; j < rectWidth; j++) {
        if (accumulatedChanges[j] > maxChange) {
            maxChange = accumulatedChanges[j];
        }
    }

    // Find the first point from the left with change ≥85% of the maximum
    changeThreshold = 0.85 * maxChange;
    referencePoint = 0;
    for (j = 0; j < rectWidth; j++) {
        if (accumulatedChanges[j] >= changeThreshold) {
            referencePoint = j - 1;
            break;
        }
    }

    // Save values per region
    ChangesPerRegion[s] = maxChange;
    ReferencePoints[s] = startX + referencePoint;
}

// Find the region with the greatest change 
maxGlobalChange = -1;
maxRegion = -1;
for (s = 0; s < nRegions; s++) {
    if (ChangesPerRegion[s] > maxGlobalChange) {
        maxGlobalChange = ChangesPerRegion[s];
        realpoint = ReferencePoints[s];
        maxRegion = s;
    }
}

print("region with greatest change: " + maxRegion);
print("Real point with greatest change: " + realpoint);

roiManager("Reset");

// Draw oval for each region
for (s = 0; s < nRegions; s++) {
    ycenter = startY + s * (rectHeight + spacing) + rectHeight / 2;  
    pointX = ReferencePoints[s];

    setSlice(1);

    if (pointX - startX > 50) {
        // Fixed-size oval
        makeOval(startX, ycenter - 25, 50, 50);
        roiManager("Add");
        roiManager("Select", roiManager("Count") - 1);
        roiManager("Rename", "oval_" + (s+1));
        roiManager("Update");
        

    } else {
       
        // Oval with width equal to pointX - startX
        makeOval(startX, ycenter - 25, pointX - startX, 50);
        roiManager("Add");
        roiManager("Select", roiManager("Count") - 1);
        roiManager("Rename", "oval_" + (s+1));
        roiManager("Update");
    }
}
   roiManager("Select All");
   roiManager("Multi Measure");

// save csv for each video
        savePath = outDir + "name" + n +  ".csv"; //add video name
        saveAs("Results", savePath);
        roiManager("Reset");
        run("Close All");
}
