dEXIF is Copyright © 2001 - 2006 Gerry McGuire. All rights reserved.
See the copyright details specified in Copyright.txt

Written and tested in Delphi 7.0

Version History:

v1.03d - Bug fixes: fix the last fix! Oops! (20-Apr-2006)   
v1.03c - Bug fixes: stop recursive EXIF directory entries   
  [Thanks to Ronald Ruijs] (18-Apr-2006)
  * Programs delivered complete - no runtime libraries needed
v1.03b - Bug fixes [Thanks to Anatoliy Kovalenko] (29-Jan-2006)
  * Fix problem with extended Canon tags being listed twice
  * Fix comment problem with corrupting Digitizing Date Time
  * Restore ClearSections( ) call
v1.03a - Bug fixes
  * Avoid recursive entries in some malformed image files
  * Add support for Casio2 Makernote structure
v1.02a - IPTC Enhancements
  * Add placeholders for undefined tags.  See iptc view for sample code.
  * Add remaining tags for IPTC.  [Thanks Wolfgang Wendefeuer]
  * Add time zone information for time tags.
v1.01a - Bug Fixes
  * Fixed a problem with Photoshop 7.0 and PS Elements 2.0 which resulted
    in the wrong EXIF segment being read.
  * Enhanced the display of custom Canon tags.  Custom sub-tags are entered
    into the standard tag array (but with a tag value of 0).
  * Default numeric formatting use function pointers (so they can be
    easily overridden). 
v1.00a - First Release
  * Update and write integer and string fields [Thanks Hans-David Alkenius]
      The routines WriteThruInt() and WriteThruString() will write
      their respective values to the buffer layer.  These changes
      will be written if the WriteEXIFJpeg() method is called.  Both 16 
      and 32 bit integers will be written in the correct endian order.
  * Fix TIFF decode for larger files.  Since many TIFF files have differing
      specifications for the size of the header, and reading the entire file
      each time will impact performance, there is a variable TiffHeaderSize
      (default about 256k) that will determine how much of a tiff file is
      read while processing the EXIF segment.
  * Wider variation of valid EXIF segments [Thanks Jarle Aasland]
  * DexifDecode flag now works properly  

V0.99b - Update:
  * Separator for IPTC strings is now variable DexifDelim [Thanks Ronald Ruijs]
  * By popular demand: extractThumbnailJpeg() extracts embedded thumbnails

V0.99a - Update:
  * Fix and add new tags for newer Nikon models (Exif 2.2)
  * Remove need for 'assignable variable constant' compiler option.
  
V0.98f - Update:
  * Add tags as defined in: http://lxr.php.net/source/php4/ext/exif/exif.c.
  * Add desc generation for primary tag arrays: simplifies tag maintenance
  * More minor tweaks and bug fixes - Thanks again to all contributors!

V0.98e - Update:
  * Exif Flash tag updated - more details and fixed bug. [Thanks Fast Trax]
  * FocalLengthin35mmFilm now calculated for cameras that do not 
    include this specific 2.2 tag [Thanks M. Schwaiger]
  * Add units display (added FormatS field to data structure)
  * Updated Olympus E20 Makernotes tag decoding
  * Lots of minor tweaks and bug fixes - Thanks again to all contributors!

V0.98d - Update:
  * EXIF 2.2 tags added.
  * Return multi-part ITPC tags as stringlist [Thanks Jarle]
  * Lots of minor tweaks and bug fixes - Thanks to all contributors!

V0.98c - Update:
  * Re-enable dExifNoJpeg conditional directive
  * IPTC module uses first section if multiple are defined. [Thanks Jarle!]
  * ThumbNails handled separately - see DexView example 

V0.98b - Update:
  * Convert TFileStream to TStream objects
  * Fix exception error for some files (Delphi's try... catch fails!)
  * Repair reading of TIFF data
[Thanks to Erik Ludden] 

V0.98a - Rewrite: 
  * Conversion to object model

V0.96a - Update:
  * Add code to detect EXIF thumbnails and routine to remove 
    thumbnail (see removeThumbnail).
  * Add callback to interpret Canon MakerNote.
  * Add code to recognize .tiff, .jpeg, and .jpe as valid
    extensions. [Thanks Jarle]
  * EXIF date/time tags can be written via OverwriteDateTime().
  * Given a time difference, Date/Time tags can be adjusted via
    AdjDateTime() procedure.
  * Added several misc. EXIF tags.
  * Use TFileStreams rather than BlockRead on legacy file 
    procedures to read file data.

V0.95b - Update:
  * Adjusted some internal routines to eliminate range check
    exceptions.
[Thanks to Jan Derk]

V0.95a - Update:
  * Add support for tag translation into non English languages.
    IPTCReadTransfile() and IPTCReadTransfile().  Write can be
    accessed from the demo's system menu.  A sample nonsense 
    file has been provided - rename to TransIn.Txt to use.
  * Added helper functions for Date and Time tags.  Photoshop
    requires Date be in 'YYYYMMDD' format.  The Time is set to
    'HHNNSS'.

V0.94a - Update:
  * Add method to extract comment section.
  * Documentation: Added examples to ProgrammersGuide.txt
  * Added hosekeeping methods to dEXIF: ClearSections() resets
    internal buffers, ProcessFile returns a boolean success and
    GetCommentSegment() extracts the data from the comment 
    section.

V0.93c - Update:
  * dIPTC now writes Photoshop readable files (previous versions
    could read Photoshop data but did not write them in a format
    Photoshop could process.
  * Added an IPTC AppendToTag() method which appends text to an
    exisiting tag rather than overwrite the previous contents.
  * Documentation: started a ProgrammerGuide - examples so far
    demonstrate dIPTC calls only.
[Thanks to Jarle Aasland]

V0.93b - Update:
  * Refined IPTC object model, add default property, dynamic array
  * Add file read/write routines
  * Add decode capability for irrationals
  * Several bug fixes
  * User can overwrite EXIF comment. [timageinfo.setEXIFcomment()]
[Jarle Aasland and Martin Larsen provided ideas and feedback]

V0.92f - Bug Fixes - IPTC index problem
  * Fix image trimming [by 20 bytes]
  * Add decode separator constant
  * Add CDATA fields to XML output
  * Adjust constant names to avoid collision
[Thanks Martin]

V0.92c - Update:
  * Added XML generator to dITPC, add button to example
  * Add IPTC indicator to dEXIF example

V0.92b - Update:
  * Fixed problems when writing both exif and itpc sections.
  * Fixed problem with JFIF header
  * Improved IPTCdata class: added initialization and
    search routines.

V0.92a - Update:
  * Added write capability for IPTC data and image comments
    IPTCView updated to allow editing and selection of tags.
    This demo is a little crude: will be refined in future
    vesions.
  * IPTC is now an object - upgraders beware!  A default object
    is created.
  * EXIF write routine fixed.  It strips older information
    that borland's TJPEG component leaves in if image is not
    expanded/compressed.
  * JFIF header correctly written if no EXIF segment present
 [Martin Larsen contributed some ideas - thanks!]

V0.91b - Update:
  * Fix miscalculation in GPS formatting

V0.91a - Update:
  * New Feature - decode GPS info, add GPS tag table.
  * GPS format can be set at run-time.  (Search on "GPSFormat".)
  * Can now display multiple values per tag - these are separated
    by a user definable value: dExifDataSep.
  * Tags can optionally be decoded (i.e. for flash, 0=Off, 1=On).
  * Tags can be processed by run time callback functions: customize
    the output.
 [Thanks to Takeo Shinohara and Martin Larsen for their contributions]

V0.9b - Update:
  * If dExifNoJpeg is defined, then dEXIF will not use the Borland
    jpeg unit and the jpeg write funtions will not be defined.
    SaveEXIF() which writes the buffers to a stream will still be
    defined.
  * EXIF buffers > 32k can be read.  There is currently a 64k limit.
  * A boolean flag has been added to the Jpeg write routines.  If set
    the output will be adjusted to the actual image dimensions,
    otherwise the original dimensions will be written (photoshop
    behavior).
  * Filemode is now correctly reset.
  * TAG_DATETIME_ORIGINAL will be also be used as the file date for
    reporting.
  * toString() and toLongString() will return empty strings if
    no EXIF data is present.
[Thanks to Jarle Aasland and Claude Rieth for prompt feedback.]

Release Notes (version: 0.9 - beta)  (new features - **)

  * dEXIF provides a fairly fast way to extract EXIF information
    from jpeg image files.  It can read and parse a EXIF header
    in about 5 msec (200/second) on a 400 mhz system.

  ** dEXIF can write (size adjusted) EXIF data to a jpeg file.
    This uses the JPEG module shipped with Borland's Delphi product.
    The IPTC and Comment sections will also be written (if present).

  ** dEXIF can read EXIF data from a TIFF file.  Since Borland does
    not include a TIFF module, dEXIF does not directly support
    writing TIFF files although the techniques used to write jpegs
    can be adapted.

  ** dEXIF can read IPTC (Photoshop) metadata contained in a JPEG
    file.  A sample program is included that uses dEXIF to extract
    only the IPTC data and builds a simple entry form.

  * dEXIF provides a simple way to extend manufacturer specific
    information through the msData module.  Manufacturer information
    is provided for Canon, Casio, Fuji, Nikon, and Olympus cameras.
    If you add other manufactures please send a copy of the
    specification to mailto://mcguirez.hotmail.com so that it may
    be included in future releases.

  * dEXIF is a Delphi port of a EXIF module written by
    Matthias Wandel.  The exif.c module is distributed as part of
    the program source for JHEAD.EXE.

  * dEXIF and the demostration program(s) are released as open
    source projects and are free for private or commercial use as
    detailed in the included copyright text file.

  * Futures releases and information as well as some basic
    technical support can be found at http://mcguirez.homestead.com.

Bug fixes:

  * Reading EXIF data from a read only file produced an error.

  * Reading EXIF data from files that contain both an EXIF and JPEG
    section prevented the processing of all data.


Thanks to all that have helped contribute toward this open source
effort.  Extra thanks go out to:

  Jarle Aasland - Helped with read-only bug, identified multi-section
                bug and provided IPTC information.
  Lucas Pierrick - Provided Epson 850Z MakerNote information.
  Wes Peterson at LexCraft Data Services - provided information and
                inspiration for TIFF based extensions.
  Earl F. Glynn - Basis for dIPTC module and also for making this
                module available on:

http://www.efg2.com/Lab/Library/Delphi/Graphics/FileFormatsAndConversion.htm

 Future Plans:

  * dEXIF currently retains much of the flavor of the original 'C'
    code (global variables etc.).  Future releases will more thoroughly
    integrate a more Delphi-like object data model.  The dIPTC module
    demonstrates a cleaner Delphi-based structure.

  * A full blown Delphi VCL control.  This will permit EXIF use from a
    much simpler level.

  * You decide.  Several of the current features are the result of
    discussion with you - the implementers - as to what you need in
    your apps.
