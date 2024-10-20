 ===============================================================================
 =                                                                             =
 =  LED Matrix Studio v0.20.27                                                 =
 =                                                                             =
 =  October 20th 2024                                                           =
 =                                                                             =
 =  www.freshney.org // paul@freshney.org                                      =
 =                                                                             = 
 =  https://maximumoctopus.hashnode.dev/                                       =
 =                                                                             =
 =  Application and Source Code:                                               =
 =      https://github.com/MaximumOctopus/LEDMatrixStudio                      =
 =                                                                             =
 ===============================================================================

 Hello!
 
 Thanks for downloading the latest version of my LED Matrix Studio.

 Several years ago I started playing with Picaxe and Arduino 
 microcontrollers and one of the first things I bought was an 8x8
 LED matrix. In the passing couple of years I've bought lots and lots
 of LED boards! I love LEDs, I think it's an addiction!

 After working out the bit-patterns on a piece of paper a couple of
 times I did what any programmer would do: I wrote a simple 
 program to work out the bit-patterns for me. That simple program
 morphed in to three separate programs, each designed for slightly 
 different things. 

 On the 10th of June 2012 I decided to start from scratch and
 build an all-purpose application with every feature I (and others)
 could ever want.

 On the 11th of October 2023 I finally got around to starting the process of rewriting
 the application in C++. The Delphi version is now deprecated, and all future
 versions will be C++.
 
 I've designed it to be as easy to use as possible. Click New and select
 the size and type of matrix you need.

 Incidentally, if you think there is an option missing then please email
 me and I'll make sure it gets added to a future release.
 
 There is a special Sure Electronics 24x16 mode that outputs the column
 data in the order that the device requires. This is one of my favourite
 matrix devices, hence why I have this mode.
 
 Please email me if you have any special requests for other devices. 

 Features:
 
 - Supports a matrix with up to 1024x1024 pixels!
 - Supports as many frames as you have memory!
   100k frames of RGB 64x64 requires ~3.7GB
   100k frames of RGB 16x16 requires ~370MB
   (if you are really this many frames then get in touch and I'll look at adding frame compression and memory optimisation)
 - Support for standard square/rectangular matrices, plus circular and hollow squares
 - Support for unlimited layers (2 will double RAM requirements, 3 will triple RAM requirements, etc.)
 - Supports single colour, bi-colour, 3 bits per pixel RGB, and full 24bit RGB matrices
 - Supports exporting RGB matrices in RGB565 format. Uses two bytes per colour instead of 3 or 4.
 - Seven sizes of display "pixel" allows for use on almost any resolution PC
   Plus auto-size which automatically resizes to fit the application window
 - Square, round, and rounded square "pixels" to mirror real LEDs
 - Five brush sizes (1 pixel, 2x2, 3x3, 4x4, and 5x5 pixels)
 - Export data in every combination imaginable!
 - Export as code or binary
 - Export directly to code templates (see \codetemplates folder)
 - Select either binary, decimal or hex output (with $ or 0x prefix)
 - Select either normal, curly, or square brackets
 - Many different draw modes
     simple click to toggle on/off (left mouse button)
     Freehand draw mode
     Filled rectangles
     Empty rectangles
     Filled circle
     Empty circle
     Line from A->B
     Multi-draw (all frames simultaneously)
     Text (using customisable fonts, two included: 5x7 and 3x5)
     Random colour
     Gradient (draw using the middle button)
     Gradient brush
     Flood fill
 - Flip, mirror, invert, rotate, scroll a matrix
 - Animation support, unlimited number of frames
 - Import any animated GIF (up to 256x256)
 - Export animation to GIF
 - Automatic animation creation (see Tools->Automate)
 - Font designer mode (single colour or RGB)
 - Unlimited per-frame undo/redo
 - Save/Load native format
 - Unlimited "presets", predefined sizes and formats
 - Export animation or single frame
 - 10 separate user buffers/scratchpads
 - Auto-save option
 - Import from bitmap image (one or more frames)
 - Export to bitmap 
 - Preview mode, view the image at x1, x2, x3, x4, x5, x6 and up to x50 pixel size.
   Preview as displayed, or map the pixels radially, or on to a semi-circle or a
   three-quarter circle arc. 
 - Preview inline with the edited image or within a separate window
   (ideal for ultrawide monitors or dual screen use)
 - Open source, download from the address at the top of this document

 If you find a bug or have a feature request then *please* email me.

 Look in the /help/ folder for more information.

 Many thanks,

 Paul A Freshney (paul@freshney.org)

========================================================================

 LED Matrix Studio has been developed with C++ Builder Community Edition 11.3.
 The source code can be found at the link at the top of this document.

 If you wish to compile the product, or make your own changes, then download
 C++ Builder from here:
   
    https://www.embarcadero.com/products/cbuilder/starter

========================================================================

 Credits:

   All coding       : Paul A Freshney
   Development Cats : Rutherford, Freeman, and Maxwell
   Thanks           : Manoj, Dennis, Lasa, Nitesh, Lorenz, Greg, Andrew,
                      Apostolos, David, Peter, Zoltan, and Gary

   Dedicated to Dad, Julie, Adam, and Dyanne.


 Help wanted!

 Have you created a fantastic animation, font, graphic or a preset
 for an LED matrix device?

 Please consider sending it to me so that I may include it with
 future updates.

== Updates for 0.20.27 beta ============================================

- Removed 256x256 pixel limit, increased to 1024x1024.
  Be warned that a matrix that size will be very unwieldy! Improvements incoming...
- Fixed issue with horizontal scrollbar not working with matrices too big for the display 
- Fixed a couple of minor issues with the brush tool
- A few tweaks to Custom Brush usability 
- Fixed a couple of issues with jerky drawing with middle and right mouse buttons.

== Updates for 0.20.26 beta ============================================

- Fixed bugs with RGB3bpp export (binary and code)
- Fixed RGB3bpp colour selections not working
- Removed all references to "Dead Pixels", these are now know throughout the application 
  as Ignored Pixels, as this makes much more sense.
- Fixed a bug which caused Ignored Pixels to be displayed when using an RGB matrix project
  (they should be hidden from view)
- Added the abiltity to "import from bitmap" with 3-bits per pixel images
- Major refactoring of Export code
  Working towards a rewrite of all Export code at some point soon
- Added a Reset to Default option on the Export dialog
- A couple of minor bugs fixed

== Updates for 0.20.25 beta ============================================

- Fixed Optimise (Tools->Optimise data)
  Designed for RGB projects, can decrease RAM/ROM usage by a huge amount
  by replacing the matrix data with a streamlined look-up table.

== Updates for 0.20.24 beta ============================================

Fixed a bug which broke Binary export. It's all working now!
Fixed a bug which caused binary export to ignore all but the bottom layer.
This is a legacy bug that goes back years...

== Updates for 0.20.23 beta ============================================

Fixed a few issues with Automations (Wipe, Reveal, and Bounce).
A couple of minor tweaks, bug fixes, and refactoring.

== Updates for 0.20.22 beta ============================================

A few minor tweaks and bug fixes.

== Updates for 0.20.20 beta ============================================

First official public build of the C++ rewrite.

This update is built from an entirely new code base. Delphi has gone, 
replaced with over 41000 lines of C++. It's open source too, there's
a link at the top of the page.

There are many improvements, and fixes, too many to list, but
being a new code base means there could also be some new bugs.

If you find a bug or any kind of issue that needs looking at then 
PLEASE! PLEASE! send me an email.

========================================================================
========================================================================

  Legacy Delphi version updates below

========================================================================
== Updates for 0.10.10 beta ============================================
======================================================================== 

Added: RGB565 mode. Select it from the Export options window when using
       an RGB matrix.
Added: Text wrappiing. Toggle it via the right mouse button menu 
       available on the text tool selection (looks like a stylised A).
Fixed: A massive bug that affects single colour matrices when the export
       source (column or row) is greater than 64 pixels, and the output
       is set to 8 bits.

A few minor tweaks to the code.

The export code is rather messy now. I'm going to refactor (at a minimum)
or rewrite certain sections, plus tidy the code for the next release. 

========================================================================
== Updates for 0.10.9 beta =============================================
======================================================================== 

Added: Animation speed setting in GIF Export
       This is currently a global setting; per-frame delay will come in 
       a future update.
 
Fixed: Crash when selecting right triangle pattern.
Fixed: Incorrect "scan direction" value when loading matrix preset.
Fixed: A couple of minor issues with the gradient brush and gradient modes.

========================================================================
== Updates for 0.10.8 beta =============================================
======================================================================== 
 
Added: Simple export toolbar. Retains the functionality from the
       recently removed simple export GUI components. While limited in
       options, it's much quicker than using the Export dialog, but
       can still supply data in a format useful in many applications.

========================================================================
== Updates for 0.10.7.1 beta ===========================================
========================================================================

Fixed a bug when opening the automate dialog.

========================================================================
== Updates for 0.10.7 beta =============================================
========================================================================

- Added: Language support across the entire application.
         Comes with English (and lolcat) only for now.
   	     Langauge files are in \language\. If you can translate them
	     then please let me know.

I've moved the source code to github for those that want an easier way
to access it (much better than hosting it all in a zip file).
 
========================================================================
== Updates for 0.10.6 beta =============================================
========================================================================

- Added: Popup menu to the main canvas. Change the background colour is 
         the only option for now, more will come.

- Fixed: Some issues with importing bitmap. Rewritten some logic.

- Fixed: Help file not opening from Help menu.

Updated the help files.	

(I've noticed on many occasions that I often update files very close to 
a year (or multiple) since the last update. This is always a coincidence,
but happens so often that it's something that always makes me think. Well,
I'm writing this on the 28th of April, and about to update the main help file.
You may not have seen it, but that's a bug that's fixed (oops). The last
update to the help file was April 29th 2020. Weird, eh?!)

========================================================================
== Updates for 0.10.5.1 beta ===========================================
========================================================================

- Added: Export Window preview display now only shows the first 512 (default)
         lines of the output file. This will be a performance boost for
         anyone with large animations. You will no longer have to wait
         for the entire animation to be displayed. This will not effect
         saving (but will make copying to clipboard slower).

         Scrolling down the preview page with a wheel mouse will cause 
         the next 512 lines of output to be appended to the preview.

         The default value of 512 can be altered in preferences.

========================================================================
== Updates for 0.10.5 beta =============================================
========================================================================

- Fixed: Export presets now correctly set all parameters

- Fixed: GIF export now includes an option for background colour

- Added: Horizontal/vertical scrolling when the matrix is too big to fit
         on screen

- Added: Drag and drop a file on to the main window to open it.

Row and column data removed. Column/data panel removed. Why? These were
legacy features that date back to the first ever iteration of the tool.

I don't think they're that useful and they made the gui more confusing 
than necessary.

========================================================================
== Updates for 0.10.4.8 beta ===========================================
========================================================================

- Added: A new language option for export: C/C++ (FastLED)
         For all you FastLED users who want your data formatted in a
         slightly different way. One frame per frame variable.

- Fixed: Some minor issues

Tidied and tweaked the code.


========================================================================
== Updates for 0.10.4.7 beta ===========================================
========================================================================

- Fixed: Loading bug with 3BPP files.
- Fixed: Zero'd data when using Export Code. Brightness was set to zero!

- Added: It's now possible to export GIFs at any size multiple of the
         original.

========================================================================
== Updates for 0.10.4.6 beta ===========================================
========================================================================

- Fixed: Rare bug when saving files

- Added: Export auto-build on startup limiter. Change from the Preferences
         window. Sets the maximum number of pixels allowed in the animation
         that will trigger an auto-build when the Export window is opened.
         Default is 100k. Large animations will still take a few seconds
         (or more) to build on slower machines, but it takes away that
         annoying initial delay when opening the Export window.

         The bottleneck is in the display component that I'm using; 95%+
         of the delay is caused by copying the cached data to the text box.
         
         Creating the code is almost instant! It's high on the to-do list...

========================================================================
== Updates for 0.10.4.5 beta ===========================================
========================================================================

- Fixed: rgb brightness can be set to 0% if not found in the matrix file

========================================================================
== Updates for 0.10.4.4 beta ===========================================
========================================================================

- Fixed: a few bugs with Export. Row/Column selection would fail if
         the boundaries weren't set to the frame size.

========================================================================
== Updates for 0.10.4.3 beta ===========================================
========================================================================

- Fixed: bug stopping Export Code from working. sorry about that.

========================================================================
== Updates for 0.10.4.2 beta ===========================================
========================================================================

- Fixed: Exported gifs now loop correctly
- Fixed: Dead pixel system. Overhaul and improvements, see below
         Renamed to "Ignored pixels"
- Fixed: Draw on all frames simultaneously
- Fixed: Colour dialogs now show the correct colour being changed
- Added: More keyboard shortcuts
- Added: Keyboard shortcut document, available via Help menu
- Added: Pre-defined matrix shapes, be gone oh boring rectangles
         Circular and empty boxes, more coming soon.
- Added: Ability to save and load ignored pixel shapes
- Added: Colour-cycle automation can now be applied to every n frame
- Added: Lock layer option to Layer Toolbar
- Added: RGB brightness option to Export (0-100%)
- Added: Ability to select off, data, grid numbering for column and
         row labels (data not available in RGB mode)
 
========================================================================
== Updates for 0.10.4.1 beta ===========================================
========================================================================

- Fixed: Moved patterns to tools toolbar.
- Fixed: Moved draw colours to a separate toolbar
- Fixed: Removed options to toggle tool/drawing colours toolbars. This is
         now handled automatically based on current matrix style
- Fixed: Transparent areas on the Colour Change dialog :)	
- Fixed: Automate "Wipe" being incorrectly disabled	 
- Fixed: Undo/redo now works correctly
 
========================================================================
== Updates for 0.10.4 beta =============================================
======================================================================== 

- AAAdded: Layers! Photoshop-like layer system (currently beta, save often!)
- Added: New layer menu and layer toolbar
- Added: Gradient brush!
- Added: Gradient toolbar (find it on the palette toolbar)
- Added: Flood fill :) (finally!)
- Added: Palette colours are saved with all project files
- Added: Hex and RGB int values to colour cycling section of Automation
- Added: First 32 colours (of your anim) to new selector in colour cycling section
- Added: Int values to RGB value display in main window
- Added: Merge can now merge into any starting frame (collated the merge
         options into a single dialog)
- Added: First 32 colours of the animation added to a selector for
         colour change modes.
- Added: Three new patterns: pyramid, left triangle, right triangle.		 
- Added: Recent files list on File menu. Keeps track of last 20 opened files
- Fixed: Gif import now 10x faster
- Fixed: After using font mode to type text, the next drawing mode will
         think its starting location has already been selected
- Fixed: An issue where a pasted frame cannot be undone
- Fixed: Colour cycling automation
- Fixed: Binary output blank when using "bottom to top" ordering
- Fixed: Preferences not setting colours correctly for bi/mono display mode
         Separated out the two modes.
- Fixed: Lots of minor UI tweaks and minor usability fixes.

========================================================================
== Updates for 0.10.3 beta =============================================
========================================================================

- Added: Export animation to GIF.
- Added: New mode: RGB 3 bits per pixel (3BPP). Allows for up 7 colours
         plus black. Requires much less storage than full RGB, at
	 the cost of much fewer colours. An 8x8 matrix will use 24 bytes
	 instead of 256 bytes for full RGB!
- Added: Colour menu, two new options to get unique colour count from 
         current frame and entire animation.		 
- Fixed: A couple of issues with gradients
- Fixed: A few minor tweaks and fixes
 
========================================================================
== Updates for 0.10.2 beta =============================================
======================================================================== 
 
- Added: GIF import! Import any animated GIF no larger than 256x256
         pixels.
- Added: Rewritten the display engine so that all drawing functions can
         be seen in real-time in the preview display. 
- Added: Four more Automate modes: Reveals the animation below by wiping
         from left/right/top or bottom. 
- Fixed: "Clear all frames gradient"		
- Fixed: Optimise on Export dialog 
- Fixed: Project file name is set to none if all frames are cleared
         (with or without gradient) so that you don't accidentally
	 overwrite your nice project with a blank one!
- Fixed: Current Preview settings are saved, and will be set when
         creating a New project.		 
 
========================================================================
== Updates for 0.10.1b beta ============================================
========================================================================  
 
Quick update to fix a couple of minor bugs.

- Fixed: Append sometimes doesn't append anything!
- Fixed: Bug where it's possible to cause an infinite loop when changing
         the distance value of some of the new patterns!
 
========================================================================
== Updates for 0.10.1 beta =============================================
======================================================================== 

- Improved the help file and added many missing items and helpful things!

- Fixed: Memory usage issues. Rewritten matrix object creation
         and reduced memory usage for the average user by more than 99%
         It'll really only be noticeable to those with hundreds or
         thousands of frames.
- Fixed: Preview settings are now saved with each saved animation
- Fixed: Copy multiple now won't let you copy beyond the currently
         available frame limit!
- Fixed: Play with continue from the current frame, and not reset the frame
         position to one.	
- Fixed: Displays updates correctly after copy/paste with rgb matrix		 
- Fixed: Bug that disabled the delete frame button when there were two frames
         left.
- Fixed: Bug where the background pixel colour wasn't saved in project files
         (or set properly at all!)
- Fixed: A bug where Export Animation to Bitmap would fail if there was a
         full stop in the folder path.
- Fixed: Gradient mode now works (and load/save too)
- Fixed: A couple of minor issues with the New Brush window. Add colour button
         never enabled. Brush would always be limited to 8x8 no matter what was
	 entered in the box. Now the dimensions of the new brush can be as big
	 as the current matrix.	 
- Added: All draw modes now work with gradient mode enabled!
- Added: Colour shades. Select any colour in RGB mode and get access to 
         16 shades of that colour.          		 
- Added: Separate preview window. That's right, you can now move the preview
         display to a separate window. Awesome if you have an ultrawide
	 monitor or dual screens.	 
- Added: Append to animation
- Added: Merge in to animation (combines to saved projects into a single animation)
         Two modes, for giving priority to top or bottom animation
         Photoshop-style layering is coming this year...
- Added: More palette history colours (RGB mode)
- Added: Fixed primary colour palette (RGB mode)
- Added: Ability to lock a frame to prevent editing (and new menu options
         to lock all, unlock all, and toggle a range)
- Added: Alternate background colours for editing and preview
         (black, grey, white)		 
- Added: Background pixel colour option to New Project window
- Added: Now includes 32 and 64 bit versions. If you're creating huge
         (1000s of frames or more) animations then 64 bit will make things
         a bit easier.
- Added: Import from multiple bitmaps. One frame per bitmap file.
- Added: Pattern toolbar with a selection of easy-to-use patterns.
         Includes mirror drawing, and spiral/pretty patterns.
	     Patterns best used in radial or semi-circle preview modes.
- Added: New Colour menu with the option to change an RGB colour
         to another colour, either across a single frame or the
		 entire animation.

- Known issue: A couple of menu items might disappear or appear blank.
               This happens infrequently and at random. If you see it,
			   then let me know! I've moved some menu options around
			   (and to new top level menus), I'm hoping this fixes it...

========================================================================
== Updates for 0.9.6 beta ==============================================
======================================================================== 

- Switched to a dark theme! 
- Added: Performance improvements for large animations when Exporting or
         Generating code
- Fixed: A couple of bugs when deleting frames or clearing all frames.

========================================================================
== Updates for 0.9.5 beta ==============================================
======================================================================== 
 
- Added: Delete multiple frames 
         (on the bottom toolbar next to delete current frame)
- Added: Rotational offset for radial/semi-circle preview modes
- Added: New pixel mode: rounded rectangle!
- Fixed: Mouse-over pixel details now fixed for RGB mode (bottom status bar)
 
========================================================================
== Updates for 0.9.4 beta ==============================================
========================================================================

- Added: Copy multiple to edit menu (frames must exist!)
- Added: BRG format export mode
- Added: Export to multiple bitmaps
- Other stuff I've added over the past few months and haven't released (and forgotten)

========================================================================
== Updates for 0.9.1 beta ==============================================
========================================================================

A massive update, and hence a new version number!

- Added: I'm back on twitter, link in Help menu!
         https://twitter.com/sp00kyb001
		 Keep up-to-date with application updates and cat pictures

- Fixed: Default LSB position in Export is right (as it should be!)
- Added: Optimise mode. From tools.
- Added: RGB palette colour selector/slider with history
- Added: Preview in semi-circle
- Added: Preview in inverted semi-circle
- Added: Preview in "3/4" radial (keyhole-ish)
- Added: Pop-up menu to preview display (right mouse button to view)
- Added: Several new preview sizes up to 20x
- Added: 25 pixel wide "void" for radial and semi-circle preview
- Added: Flip/mirror/invert/rotate clockwise/rotate anti-clockwise 
         functions to brush. 
		 Create a brush as normal and use the key combinations
		 found in Edit->Brush actions.
- Added: Create a custom brush
         (2nd button from the left on the tools toolbar)
- Added: Fill the brush with one of several pre-made patterns/gradients
- Added: Ability to paste a brush to every frame. Paste with transparency
         or overwrite pixels. See Edit->Brush actions for more details
- Added: Generate Code button to main toolbar
         This feature is going to get a lot of love over the next few
		 weeks... watch this space.
		 It uses the content in <install>\codetemplates
- Added: Lots of new features to Automate. Tools->Automate or F6
- Added: Many new functions to Automate. Added Post Processing too :)
- Added: Colour-cycling to Automate. Select source colours, and the 
         target colour you wish to cycle through.
- Added: Automate now has the ability to paste up to 2 different brushes to every
         frame, or just the first frame. Paste them with transparency if required.
		 e.g. Use one as a background as one as a foreground.
 
- Plus a lots of minor tweaks

== Updates for 0.8.16 beta =============================================

- Fixed: Default LSB position in Export is right (as it should be!)
- Added: Optimise mode. From tools.
- Added: Preview in semi-circle
- Added: Several new preview sizes up to 20x
- Added: 25 pixel wide "void" for radial and semi-circle preview
- Plus a few minor tweaks

== Updates for 0.8.15 beta =============================================

- Fixed a few export bugs
  Right-to-left columns and bottom-to-top rows not working
  Too many commas
  Too many $ :(   

== Updates for 0.8.14 beta =============================================

- Added number format (decimal, binary, hex) option to RGB output
- Added Pascal code format output (beta, to do :))
- Few minor tweaks

== Updates for 0.8.13 beta =============================================

- Fixed crash when trying to delete the last frame
- Tweaks to the automate mode
- Added support for 256x256 matrix size. Only use this is you have the
  space (you may need to disable preview display)
- Few other tweaks
- Added new preview mode: radial. Maps the pixels around a circle.
  Change the size of the "void" in the circle from the View menu
- Added Export button to the top toolbar
- Added a new help document \docs\en\Help.pdf
- Lots of minor tweaks

== Updates for 0.8.12 beta =============================================

- Fixed Font mode. A some point in the last few versions I broke it :(

== Updates for 0.8.11 beta =============================================

- Added more playback speed options, and a custom option
- Added a simple animation processing system (tools->automate)
- Added the ability to paste with a shift up/down/left/right
  ALT+arrow_key to shift the image as you paste!
- Fixed a couple of minor export bugs

== Updates for 0.8.9 beta ==============================================

- Fixed autosave bugs
  Not setting correct value on load
  Incorrect file name (...hh_ss instead of hh_mm :)
  Out of bounds error when saving

== Updates for 0.8.8 beta ==============================================

- Fixed an RGB copy/paste bug
- Fixed couple of minor Preview display bugs


== Updates for 0.8.7 beta ==============================================

- Fixed a bug where drawing in RGB mode with a horizontal gradient 
  wouldn't work properly.
- Fixed a bug where drawing shapes could crash in RGB mode :(
- Tidied up the source code a bit :)

== Updates for 0.8.6 beta ==============================================

- Added binary export option (raw data, no formatting of any kind)
- Added full frame-based history (undo and redo) with infinite levels!
- Fixed an output bug (RGB column mode)
- Added drop down list of popular options to Frame Count selector on
  "New Project" window
- Added Preview details to settings (load and save on startup)
- Other minor tweaks

== Updates for 0.8.5 beta ==============================================

- Fixed a bug with circle draw modes
- Fixed a bug which caused the "middle mouse button" to appear in
  single colour mode

== Updates for 0.8.4 beta ==============================================

- Added Gradient mode reminder graphic, shows that middle mouse button
  draws gradient
- Fixed unchecked "Clear All" setting in new project not doing anything
- Other minor fixes and tweaks

== Updates for 0.8.3 beta ==============================================

- Added 128x128 pixel matrix support (BETA!)
- Couple minor fixes and tweaks (rotate any angle + others)

== Updates for 0.8.2 beta ==============================================

- Added filled circle draw mode
- Other minor fixes

== Updates for 0.8.1 beta ==============================================

- Added RGB font support
- Removed animation frame limit! Add as many as you like
- Improved memory usage
- Added horizontal gradient mode

== Updates for 0.8.0 beta ==============================================

- Fixed Presets not appearing/working
- Fixed Bi-colour mode
- Added RGB mode
- Added Code Templates (see doc in folder)
- Added Randomness selection to random drawing mode (RGB only!)
- Added "dead pixels", pixels that can't be drawn on, or exported
- Added new "Alternate up/down" modes for display boards that need it
  (various 8x32 RGB boards on eBay)
- Added optimise option. Works well on RGB or 16/32 bit outputs. Capable
  of giving good compression of simple-ish data with almost no extra
  microcontroller overhead.  
- Plus lots of other tweaks and fixes

== Updates for 0.7.16 ==================================================

- Complete rewrite of the matrix rendering engine
  (everyone should see a performance increase!)
- Drawing, freehand/tools now follow standard conventions 
  Left click = "ON", right click = "OFF"
- New/Open/Save buttons on top toolbar
- New/Open/Save logic is much better (more logical)
- Added a new output option "Microchip" (dt ...... ; comment)
- Added "Are you sure you want to quit?" message
- Added "Comment" field for each matrix (Edit -> Edit Comment)
  (is included with exported data)
- Added circle drawing tool
- Added multi-draw tool (draws on every frame simultaneously!)
- Added new brush sizes, 2x2 and 3x3 pixels
- Added "Auto" pixel size mode
- Added a font viewer (View->Font Viewer)
- Drawing tools now update in realtime
- Copy shows bounding box
- Lots of little fixes and improvements

== Updates for 0.7.15 ==================================================

- Fixed a bug with export options profiles not loading properly
- Fixed a bug with export options "binary output" mode
- Added more details to export options output
- Added x4 preview mode
- Added Donate button :0

== Updates for 0.7.14 ==================================================

- Fixed a bug in the Font Mode data loader
- Added a preview of the current matrix: x1, x2 and x3 pixel magnification

== Updates for 0.7.13 ==================================================

- Fixed the loader :(

== Updates for 0.7.12 ==================================================

- Much improved export functionality
- User memories can be exported in the same way as normal matrix data
- Removed separate Row/Column toolbars, replaced with a single toolbar
- Fixed a couple of minor bugs

== Updates for 0.7.11b =================================================

- Fixed a bug with row display showing MSB/LSB wrong way around
- Fixed a number of bugs with export
- Added a couple of Python export formats
- Changed the default start char of font mode from 33 to 32

== Updates for 0.7.10 ==================================================

- Fixed a bug that would multiply the amount of data when saving
  animations with some settings
- Added "combine nibbles" option, useful for displays with a width or
  length of 4 pixels (like the Orion4x4 grid board)
- Hex and bracket option settings are saved in each animation file now
- Added option to use above when loading or use the current application
  settings (View->Use format embedded in save files)
- Added ability to change the start ASCII code in font mode
  (default is 33)

== Updates for 0.7.9d ==================================================

- Fix when editing text in column/row boxes with 0x prefix

== Updates for 0.7.9c ==================================================

- Couple of minor fixes

== Updates for 0.7.9b ==================================================

- Fixed a bug which wouldn't select no hex prefix
- Changed a couple of button/control tooltips

== Updates for 0.7.9 ===================================================

- Added option of circular or square pixels

== Updates for 0.7.8 ===================================================

- Fixed a bug that caused the grid to hide behind the top tool bars on
  systems using large fonts

== Updates for 0.7.7 ===================================================

- Added a "gradient" mode for bicolour matrices
- Added a "random" colour drawing mode for bicolour matrices
- Minor bug fixes etc.

== Updates for 0.7.6b ==================================================

- Fixed bug that stopped some files loading :(

== Updates for 0.7.6 ===================================================

- Added bicolour matrix support
- Added optimise mode (beta at moment)
- Added mousewheel up/down to select frame
- Minor modifications to the GUI and other minor updates
- Fixed a few minor bugs

== Updates for 0.7.5 ===================================================

- Added Auto Save option (2/5/10 minute intervals)
  Saves to \saves\_autosave.leds
- Added new icon!
- Added import from bitmap image option
- Increased max frame limit to 500
- Increased max frame size to 64x64
- Column and Row "data boxes" now accept hex values ($ and 0x)
- Fixed a couple of minor bugs

== Updates for 0.7.4 ===================================================

- Added Padding option for hex values
- Fixed a couple of minor bugs (load/save location is now remembered)
- Added option to toggle matrix grid

== Updates for 0.7.3 ===================================================

- Fixed a bug which outputs the 24x16 data incorrectly
- Fixed a bug which makes the hex output settings not correctly
  set on startup
- Fixed a bug which stops the update checker connecting to my website
  (for some reason it's become very picky over agent strings?!)

== Updates for 0.7.2 ===================================================

- Fixed a few minor bugs
- Added flip/invert/mirror all frames option on menu
- Added import option. Imports a single frame from a saved matrix to the
  currently selected frame.
- Increased frame limit to 200.
- Other minor tweaks!

== Updates for 0.7.1 ===================================================

- Fixed a few minor bugs
- Added rotate by any angle feature. Well, actually multiples of 5'.
  Select the angle, and number of frames and the software will generate
  them for you. Each new frame being x degrees further rotated than
  the preceeding frame. Works best with square grids...
- Now shows which user buffers have content by the appearance of a little
  icon beside the menu item
- Customisable animation playback speed, press the right mouse button
  over the play button.
- Other minor tweaks!

========================================================================
========================================================================