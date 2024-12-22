{*
 *  URUWorks
 *
 *  The contents of this file are used with permission, subject to
 *  the Mozilla Public License Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/2.0.html
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2023 URUWorks, uruworks@gmail.com.
 *  Copyright (C) 2023 CM630.
 *}

unit procLocalize;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Translations, LCLTranslator;

resourcestring

  //CommonControls
  lngbtnClose = 'Close';
  lngbtnCancel = 'Cancel';
  lngbtnSave = 'Save';
  lngbtnLoad = 'Open';
  lngbtnAdd = 'Add';
  lngbtnRemove = 'Remove';
  lngbtnUpdate = 'Update';

  //CommonStrings
  lngOpenFile = 'Open file';
  lngSaveFile = 'Save file';
  lngAllSupportedFiles = 'All supported files';
  lngProjectFile = 'Subtitle Translation Project';
  lngUnableToLoad = 'Error loading file, unrecognized format.';
  lngFileNotFound = 'File cannot be found.';
  lngSaveSubtitleError = 'Error trying to save the file.';
  lngFileChanged = 'File has not been saved.';
  lngTranslationFileChanged = 'Translation file has not been saved.';
  lngAskToSave = '“%s”' + LineEnding + LineEnding + 'Do you wish to save it?';
  lngNoName = 'no name';
  lngWriteDenieded = 'Write denied. Please check folder permissions.';
  lngBackupSaved = 'Backup saved.';
  lngNoEntriesMarked = 'There are no marked entries.';
  lngPromptForDeleteLines = 'The selected lines will be deleted.';
  lngShiftTimeMore = 'Shift %dms more';
  lngShiftTimeLess = 'Shift %dms less';
  lngTranscriptionModeExit = 'Exiting transcription mode will lose any unsaved changes.';
  lngSourceModeWarning = 'Switching to source mode will change the format to “Tero Subtitler”, otherwise unsaved translations will be lost.';
  lngSubtitleHasErrorsToFix = 'The current subtitle set contains errors that are recommended to be resolved, but it can be saved.';
  lngSubtitleSpecificationError = 'The current subtitles do not meet the specifications of the format to be saved.';
  lngSubtitleSpecificationErrorFPS = 'Current frame rate does not match.';
  lngSubtitleSpecificationErrorBR = 'There are entries which exceed the line break.';
  lngSubtitleSpecificationErrorAlign = 'There are entries with vertical alignment which are not allowed.';
  lngTextFormatted = 'Export formatted text?';
  lngNoInternetConnection = 'No Internet connection available';
  lngFailedToDownload = 'No content downloaded, missing file or no Internet connection.';
  lngDownloadCanceled = 'Download canceled by user.';
  lngAskToExtractAudio = 'Audio waveform was not found, do you want to extract it?';
  lngExtractAppError = 'Unable to find “%s”. Check in settings if it is configured correctly.';
  lngExtracting = 'Extracting...';
  lngTranscribing = 'Transcribing...';
  lngDetectDialogSegmentsWarning = 'Attempting to detect dialogue segments deletes all entries from the list.';
  lngEntry = 'Entry %d';
  lnglibMPVError = 'Unable to find the required libraries to run the embedded mpv player. Please download them.';
  lnglibMPVglError = 'Failed to initialize mpv GL context.';
  lnglibMPVVersionError = 'Could not initialize libmpv. Check if the right library was installed.';
  lngSpellCheckFinished = 'Spell check finished.';
  lngDetect = 'Auto-detect';
  lngAddNote = 'Add note';
  lngOpenVideoFromURL = 'Open video from';
  lngURL = 'URL';
  lngSupportedSites = 'Supported sites';
  lngWebVideoUnsupported = 'The video format is not supported for playback in the web browser.';
  lngNewShortCutPreset = 'Enter a name for your shortcuts preset';
  lngNewShortCutPresetName = 'Name';
  lngFolderNotEmpty = 'The folder is not empty.';
  lngContinueAnyway = 'Continue?';
  lngEntryStartedAt = 'Entry started at %s';
  lngEntryEndedAt = 'Entry ended in %s';
  lngFailed = 'Failed.';
  lngSuccess = 'Success.';
  lngNewVersionFound = 'A new version was found.';
  lngSeeChangeList = 'Do you want to see the changelog?';
  lngNoNewVersion = 'You''re using the latest version.';
  lngErrorExecuting = 'Error executing “%s”.';
  lngFeatureNotAvailableFromURL = 'Function not available for online video.';
  lngMultipleTracksDetected = 'Multiple tracks detected';
  lngSelectTrackToUse = 'Select the track to use:';
  lngSelectFPSToUse = 'Select FPS to use:';
  lngSelectEncodingToUse = 'Select the Encoding to use:';
  lngSelectSheetToUse = 'Select sheet to use:';
  lngWriteStatus = 'Writing %d/%d';
  lngShortCutInUse = 'Shortcut currently used in “%s”.';
  lngAdditionalParams = 'Additional params:';
  lngLine = 'Line:';
  lngFileSavedSuccessfully = 'File saved successfully.';
  lngOperationFailed = 'Job %d/%d fail with code %d.';
  lngOperationCancelled = 'Operation cancelled.';
  lngOperationCompleted = 'Operation completed.';
  lngVideoGenerationFailed = 'Video generation failed.';
  lngVideoErrorAOInitFailed = 'Initializing the audio output failed.';
  lngVideoErrorVOInitFailed = 'Initializing the video output failed.';
  lngVideoErrorNothingToPlay = 'There was no audio or video stream to play.';
  lngOpenContainingFolder = 'Open containing folder';
  lngAdditionalOption = 'Additional option';
  lngHHMMSSZZZ = 'Media [HH:MM:SS.ZZZ]';
  lngHHMMSSFF = 'SMPTE [HH:MM:SS:FF]';

  //AppStrings
  lngasTextChars = 'Text (%s characters at %.2f CPS):';
  lngasText = 'Text';
  lngasTranslationChars = 'Translation (%s characters at %.2f CPS):';
  lngasTranslation = 'Translation';
  lngasLineSelected = '%d lines selected / %d';
  lngasMarked = 'Marked entry';
  lngasBadValues = 'Bad values';
  lngasTimeTooLong = 'Duration too long';
  lngasTimeTooShort = 'Duration too short';
  lngasGapTooShort = 'Gap too short';
  lngasMaxCPS = 'Too many CPS';
  lngasOverlappingWithPrev = 'Overlapping with previous entry';
  lngasOverlappingWithNext = 'Overlapping with following entry';
  lngasFixTags = 'Invalid or incomplete tags';
  lngasEmpty = 'Empty entry';
  lngasUnbreak = 'Line breaks less than %d characters';
  lngasUnnecessarySpaces = 'Unnecessary spaces';
  lngasUnnecessaryDots = 'Unnecessary dots';
  lngasRepeatedChars = 'Repeated character';
  lngasProhibitedChars = 'Contains a prohibited character';
  lngasHearingImpaired = 'Hearing impaired';
  lngasBreakLongLines = 'Line(s) too long';
  lngasRepeatedEntry = 'Repeated entry';
  lngasEllipsesSingleSmartCharacter = 'Ellipses dots instead of Unicode character';
  lngasMaxLines = 'Maximum number of lines exceeded';
  lngasOCR = 'OCR error';
  lngasWaveformText = 'Click here to use timeline';
  lngasVideoLen = '%s (%s FPS)';
  lngasVideoLenSMPTE = '%s (%s FPS/SMPTE)';
  lngasReadingVideoFile = 'Reading video file...';
  lngasCustom = 'Custom';

  //AboutStrings
  lngabVersion = 'version %s'+ LineEnding + LineEnding + 'Compiled at %s on %s' + LineEnding + 'Compiler version: %s' + LineEnding + 'Target CPU: %s';

  //AdjustSubtitleStrings
  lngIndex = '#';
  lngOldTime = 'Old time';
  lngNewTime = 'New time';

  //AutomaticDurationsStrings
  lngNewDurationAll = 'New duration in all cases';
  lngNewDurationGreater = 'New duration is greater than original';
  lngNewDurationSmaller = 'New duration is smaller than original';

  //BatchConvertStrings
  lngbcFileName = 'File name';
  lngbcFileFormat = 'Format';
  lngbcFileState = 'Status';

  //CustomFormatStrings
  lngcfsTime = 'Time';
  lngcfsFrames = 'Frames';

  //FixSubtitlesStrings
  lngfsetUnnecessarySpaces = 'Remove unnecessary spaces';
  lngfsetUnnecessaryDots = 'Remove unnecessary dots';
  lngfsetFixTags = 'Fix invalid tags';
  lngfsetTimeTooShort = 'Fix durations too short';
  lngfsetTimeTooLong = 'Fix durations too long';
  lngfsetOverlapping = 'Fix overlapping display times';
  lngfsetBadValues = 'Fix bad timing values';
  lngfsetUnbreak = 'Remove line breaks if %d characters not exceeded';
  lngfsetBreakLongLines = 'Break overlong lines';
  lngfsetEmpty = 'Remove empty entries';
  lngfsetEllipsesSingleSmartCharacter = 'Convert ellipsis dots to Unicode character';
  lngfsetProhibitedChars = 'Remove entries with %s';
  lngfsetHearingImpaired = 'Remove text for hearing impaired';
  lngfsetRepeatedEntry = 'Remove repeated entries';
  lngfsetRepeatedChars = 'Remove repeated characters';
  lngfsetIncompleteHyphenText = 'Remove incomplete hyphenated line';
  lngfsetSpaceOfOpeningHyphen = 'Spacing of opening hyphen';
  lngfsetRemoveSpacesWithinBrackets = 'Remove spaces within brackets';
  lngfsetFixInterrobang = 'Fix interrobang: !? -> ?!';
  lngfsetOCR = 'Fix common OCR errors';
  lngfsetSnapToShotChanges = 'Snap times to shot changes';
  lngfsetSnapToShotChangesInCue = 'Snap In Cue to shot change';
  lngfsetSnapToShotChangesOutCue = 'Snap Out Cue to shot change';
  lngfsetSnapToShotChangesInCueAway = 'Snap In Cue away from shot change';
  lngfsetChaining = 'Chaining entries';
  lngfsetCleanupTags = 'Cleanup tags';
  lngfsIndex = '#';
  lngfsAction = 'Action';
  lngfsCurrent = 'Current';
  lngfsAfter = 'After';
  lngfsAddSpacing = 'Add spacing';
  lngfsRemoveSpacing = 'Remove spacing';
  lngfsTotalErrors = 'Total errors: %d (displayed: %d)';

  //FormatPropertiesStrings
  lngfoCenter = 'Center';
  lngfoLeft = 'Left';
  lngfoRight = 'Right';
  lngfoUndefined = 'Undefined';
  lngfoOpenSubtitling = 'Open subtitling';
  lngfoLevel1Teletext = 'Level-1 teletext';
  lngfoLevel2Teletext = 'Level-2 teletext';
  lngfoLatin = 'Latin';
  lngfoLatinCyrillic = 'Latin/Cyrillic';
  lngfoLatinArabic = 'Latin/Arabic';
  lngfoLatinGreek = 'Latin/Greek';
  lngfoLatinHebrew = 'Latin/Hebrew';

  //HeaderStrings
  lnghIndex = '#/Gap';
  lnghTimes = 'Times';
  lnghDuration = 'Duration';
  lnghStyleAndActor = 'Style/Actor';
  lnghText = 'Text';
  lnghTranslation = 'Translation';
  lnghCPS = 'CPS';
  lnghWPM = 'WPM';
  lnghCPL = 'CPL';

  //ModifySelectionStrings
  lngmsContains = 'Contains';
  lngmsStartsWith = 'Starts with';
  lngmsEndsWith = 'Ends with';

  //MultipleReplaceStrings
  lngmrFind = 'Find';
  lngmrReplaceWith = 'Replace with';
  lngmrRegExpr = 'R. Expression';

  //QualityCheckStrings
  lngqcIndex = '#';
  lngqcRule = 'Broken rule';
  lngqcCPS = 'CPS (characters-per-second)';
  lngqcWPM = 'WPM (words-per-minute)';
  lngqcCPL = 'CPL (characters-per-line)';
  lngqcMaximumLines = 'Maximum lines';
  lngqcMinimumDuration = 'Minimum duration';
  lngqcMaximumDuration = 'Maximum duration';
  lngqcGAP = 'Gap Minimum duration';

  //RestoreBackupStrings
  lngrbDateTime = 'Date / Time';
  lngrbFilename = 'File name';

  //SettingsStrings
  lngssGeneral = 'General';
  lngssConventions = 'Conventions';
  lngssAppearance = 'Appearance';
  lngssShortcuts = 'Shortcuts';
  lngssMPV = 'Video player';
  lngssTools = 'Tools';
  lngssFileTypeAssociations = 'File types';
  lngssAutoMode = 'Automatic';
  lngssLightMode = 'Light mode';
  lngssDarkMode = 'Dark mode';
  lngssListMode = 'List';
  lngssBlockMode = 'Block';
  lngssFrames = 'Frames';
  lngssMilliseconds = 'Milliseconds';
  lngssShortCutNone = 'None';

  //ShotChangesStrings
  lngscFrames = 'Frames';
  lngscSeconds = 'Seconds';
  lngscMilliseconds = 'Milliseconds';
  lngscHHMMSSZZZ = 'HH:MM:SS.ZZZ';
  lngscHHMMSSFF = 'HH:MM:SS:FF';
  lngscEDL = 'Edit Decision Lists';
  lngscShotChanges = 'Text files';
  lngscXML = 'XML';

  //ShortCutCategoryStrings
  lngscEdit = 'Edit';
  lngscEditor = 'Editor';
  lngscFile = 'File';
  lngscFind = 'Find';
  lngscHelp = 'Help';
  lngscSelect = 'Select';
  lngscEntries = 'Entries';
  lngscText = 'Text';
  lngscTimings = 'Timings';
  lngscTools = 'Tools';
  lngscVideo = 'Video';
  lngscVideoFilters = 'Video filters';
  lngscAudioFilters = 'Audio filters';
  lngscView = 'View';
  lngscWaveform = 'Waveform';

  //SpellCheckStrings
  lngspCurrentLine = '# %d';

  //Statistics
  lngstNumberOfEntries = 'Number of entries: %d';
  lngstNumberOfWords = 'Number of words: %d';
  lngstNumberOfCharacters = 'Number of characters: %d';
  lngstEntriesWithoutText = 'Entries without text: %d';
  lngstTotalDuration = 'Total duration: %s';
  lngstTotalCharactersPerSecond = 'Total characters per second: %.2f';
  lngstShortestLine = 'Shortest line: %d (#%d)';
  lngstLongestLine = 'Longest line: %d (#%d)';
  lngstLineLengthAverage = 'Line length average: %d';
  lngstShortestDuration = 'Shortest duration: %s (#%d)';
  lngstLongestDuration = 'Longest duration: %s (#%d)';
  lngstDurationAverage = 'Duration average: %s';
  lngstShortestCharactersPerSecond = 'Shortest characters per second: %.2f (#%d)';
  lngstLongestCharactersPerSecond = 'Longest characters per second: %.2f (#%d)';
  lngstCharactersPerSecondAverage = 'Characters per second average: %.2f';
  lngstShortestWordsPerMinute = 'Shortest words per minute: %.2f (#%d)';
  lngstLongestWordsPerMinute = 'Longest words per minute: %.2f (#%d)';
  lngstWordsPerMinuteAverage = 'Words per minute average: %.2f';
  lngstShortestGap = 'Shortest gap: %s (#%d)';
  lngstLongestGap = 'Longest gap: %s (#%d)';
  lngstGapAverage = 'Gap average: %s';

  //StreamExtractor
  lngseID = 'ID';
  lngseKind = 'Type';
  lngseLanguage = 'Language';
  lngseCodec = 'Codec';
  lngseOutput = 'Output';
  lngseAnalyzingFile = 'Analyzing file...';

  //TimeExpanderStrings/CompareStrings
  lngteExpand = 'Expand';
  lngteReduce = 'Reduce';
  lngteTime = 'Time';
  lngteText = 'Text';
  lngteIndex = 'Index';
  lngteInitialTime = 'Initial time';
  lngteFinalTime = 'Final time';

  //TranslationMemoryStrings
  lngOriginal = 'Original';
  lngTranslated = 'Translated';
  lngPercent = 'Percent';

  //VideoDubbingStrings
  lngvdVoice = 'Voice';
  lngvdGeneratingTTS = 'Generating text to speech...';
  lngvdGeneratingVideo = 'Generating video dubbing...';
  lngvdQuotaExceeded = 'We couldn''t process this request because you''ve exceeded your quota.';

  //VideoInfoStrings
  lngviFile = 'File: %s';
  lngviDuration = 'Duration: %s (%d frames)';
  lngviFPS = 'FPS: %s';
  lngviResolution = 'Resolution: %dx%d';
  lngviVideoTracks = 'Video tracks: %d';
  lngviAudioTracks = 'Audio tracks: %d';
  lngviSubtitleTracks = 'Subtitle tracks: %d';

  //WizardStrings
  lngwizLanguage = 'Language';

  //TipStrings
  lngtTip1 = 'Tip: Use <%s/%s> to go to previous/next entry';
  lngtTip2 = 'Tip: Use <%s> for a web word reference';
  lngtTip3 = 'Tip: Use <%s> dock/undock video window';
  lngtTip4 = 'Tip: Use <%s> dock/undock waveform window';

function AppNameD: String;
function LocalizeCategory(const aCategory: String): String;
function GetGUILangIndex(const aLngList: TStrings; LangId: String): Integer;
function GetPOLanguage(const AFileName: String): String;
function GetPOLanguageNameFromID(const LangID: String): String;

procedure SetGUILanguage;

// -----------------------------------------------------------------------------

implementation

uses
  UWSystem.Globalization, ActnList, procTypes, procConfig, formMain;

// -----------------------------------------------------------------------------

//Returns the app name + a dot
function AppNameD: String;
begin
  Result := ExtractFileNameOnly(Application.Exename) + '.';
end;

// -----------------------------------------------------------------------------

function ResStringDefault(aResString: PString): String;
var
  resstr: PResourceStringRecord;
begin
  resstr := PResourceStringRecord(PPtrUInt(aResString)-1);
  Result := resstr^.DefaultValue;
end;

// -----------------------------------------------------------------------------

function LocalizeCategory(const aCategory: String): String;
begin
  Result := aCategory;

  if aCategory = ResStringDefault(@lngscEdit) then Exit(lngscEdit);
  if aCategory = ResStringDefault(@lngscEditor) then Exit(lngscEditor);
  if aCategory = ResStringDefault(@lngscFile) then Exit(lngscFile);
  if aCategory = ResStringDefault(@lngscFind) then Exit(lngscFind);
  if aCategory = ResStringDefault(@lngscHelp) then Exit(lngscHelp);
  if aCategory = ResStringDefault(@lngscSelect) then Exit(lngscSelect);
  if aCategory = ResStringDefault(@lngscEntries) then Exit(lngscEntries);
  if aCategory = ResStringDefault(@lngscText) then Exit(lngscText);
  if aCategory = ResStringDefault(@lngscTimings) then Exit(lngscTimings);
  if aCategory = ResStringDefault(@lngscTools) then Exit(lngscTools);
  if aCategory = ResStringDefault(@lngscVideo) then Exit(lngscVideo);
  if aCategory = ResStringDefault(@lngscVideoFilters) then Exit(lngscVideoFilters);
  if aCategory = ResStringDefault(@lngscAudioFilters) then Exit(lngscAudioFilters);
  if aCategory = ResStringDefault(@lngscView) then Exit(lngscView);
  if aCategory = ResStringDefault(@lngscWaveform) then Exit(lngscWaveform);
end;

// -----------------------------------------------------------------------------

function GetGUILangIndex(const aLngList: TStrings; LangId: String): Integer;
var
  langs: TStringArray;
  i: Integer;
begin
  Result := -1;
  for i := 0 to aLngList.Count-1 do
  begin
    langs := aLngList.Strings[i].Split([';']);
    if (Length(langs) > 0) and (LangId = langs[0]) then
      Exit(i);
  end;
end;

// -----------------------------------------------------------------------------

function GetPOLanguage(const AFileName: String): String;
var
  pofile: TPoFile;
  L: TStringList;
  i: Integer;
  sa: TStringArray;
begin
  Result := '';
  L := TStringList.Create;
  try
    pofile := TPoFile.Create(AFileName);
    try
      L.Text := pofile.Header.Translation;
    finally
      pofile.Free;
    end;

    for i := 0 to L.Count - 1 do
    begin
      sa := L[i].Split(':');
      if (Length(sa) = 2) then
        case sa[0] of
          'Language' : begin
                         Result := Trim(sa[1]);
                         Break;
                       end;
        end;
    end;
  finally
    L.Free;
  end;
end;

// -----------------------------------------------------------------------------

function GetPOLanguageNameFromID(const LangID: String): String;
begin
  Result := GetCultureDisplayName(LangID.Replace('_', '-'));
end;

// -----------------------------------------------------------------------------

procedure SetGUILanguage;
var
  i: Integer;
begin
  for i := 0 to frmMain.ActionList.ActionCount-1 do
    with TAction(frmMain.ActionList.Actions[i]) do
      Hint := '';

  SetDefaultLang(AppOptions.GUILanguage, LanguageFolder);
  UpdateCommonActionString;

  for i := 0 to frmMain.ActionList.ActionCount-1 do
    with TAction(frmMain.ActionList.Actions[i]) do
      if Hint <> '' then
        Hint := Caption + LineEnding + LineEnding + Hint
      else
        Hint := Caption;
end;

// -----------------------------------------------------------------------------

end.
