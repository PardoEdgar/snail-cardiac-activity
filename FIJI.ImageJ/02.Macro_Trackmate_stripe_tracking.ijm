waitForUser(
"First, open the example video (Moving_snail_example in folder: Data) with the FFmpeg plugin for tracking,\n"+
"then continue running this macro.\n"+
"Click OK to proceed after opening the video")
makeRectangle(567, 17, 303, 100);
run("Duplicate...", "title=processed duplicate");
setSlice(125);
makeLine(32, 57, 263, 68);
run("Set Scale...", "distance=229.0546 known=5 unit=mm");
run("Select None");
run("Invert", "stack");
run("Minimum...", "radius=2 stack");
run("Maximum...", "radius=2 stack");
run("8-bit");
waitForUser("Pre-processing ready. TrackMate will open.");
run("TrackMate");
waitForUser(
"TRACKMATE WORKFLOW:\n\n" +
"1. Run StarDist detector preview\n" +
"2. Run detection (StarDist)\n" +
"3. Filter spots:\n" +
"   - Quality: automatic\n" +
"   - Mean intensity > 126.88\n" +
"   - Area > 0.84\n\n" +
"4. Run LAP tracker (default parameters)\n" +
"5. Filter tracks:\n" +
"   - Duration > 86.5 frames\n\n" +
"6. Review Track Scheme:\n" +
"   - Keep continuous detections\n" +
"   - Remove noise or detections with extreme shape changes\n\n" +
"7. Export:\n" +
"   - Spots statistics (CSV)\n" +
"   - Tracks statistics (CSV)\n" +
"   - XML\n"
);
close();

