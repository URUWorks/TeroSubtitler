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
 *}

unit procTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLType, UWSubtitleAPI, UWSubtitleAPI.Formats,
  BGRABitmap, procConventions, UWSubtitleAPI.TMX, UWSubtitleAPI.TBX, procMRU,
  ATSynEdit;

const

  ProgramName       = 'Tero Subtitler';
  ProgramWebsite    = 'https://uruworks.net';
  ProgramHelpURL    = 'https://help.uruworks.net';
  ProgramUpdateURL  = 'https://raw.githubusercontent.com/URUWorks/TeroSubtitler/main/version.txt';
  ProgramReleaseURL = 'https://github.com/URUWorks/TeroSubtitler/releases';
  UWDonateURL       = 'https://www.paypal.me/URUWorks';
  RootCfg           = 'tero';

var

  DefTimeFormat: String     = 'hh:mm:ss.zzz';
  DefDurationFormat: String = 'mm:ss.zzz';

const

  DefFramesFormat   = 'hh:mm:ss:ff';
  DefFPS            = 23.976;
  DefFPSList        : array[0..15] of Single = (8, 10, 12, 15, 16, 20, 23.976, 24, 25, 29.97, 30, 48, 50, 59.94, 60, 75);

  TVideoExts : array[0..21] of String =
  (
    '.avi', '.mp4', '.mpg', '.mpeg', '.mkv', '.webm', '.flv', '.ogv', '.ogg',
    '.mov', '.qt', '.wmv', '.rm', '.rmvb', '.asf', '.m4v', '.m4p', '.mpv',
    '.mpe', '.nsv', '.divx', '.vob'
  );

  TVideoWebExts : array[0..2] of String =
  (
    '.mp4', '.ogg', '.webm'
  );

  TAudioExts : array[0..14] of String =
  (
    '.mp3', '.aac', '.flac', '.ac3', '.wma', '.ogg', '.mpa', '.m4a', '.m4b',
    '.ape', '.aiff', '.mka', '.opus', '.adts', '.wav'
  );

  TWaveformAudioExts : array[0..1] of String =
  (
    '.wav', '.peak'
  );

  TShotChangeExts : array[0..2] of String =
  (
    '.edl', '.shotchanges', '.xml'
  );

  TProjectExt = '.stp';

  TBluRayExt = '.sup';

  TUnicodeChars : array[0..8] of TUTF8Char =
  (
    '♪', '♫', '©', '…', '‘', '’', '“', '”', '⹀' //, '♥'
  );

  tst_StartTag  = '{';
  tst_EndTag    = '}';
  tst_Bold      = 'b';
  tst_Italic    = 'i';
  tst_Underline = 'u';
  tst_Strikeout = 's';
  tst_Color     = 'c';

  UC_BOOKMARK = '🗭'; //WideChar(#$2691);

  TAG_CONTROL_NORMAL        = 0;
  TAG_CONTROL_UPDATE        = 1;
  TAG_CONTROL_INITIALTIME   = 2;
  TAG_CONTROL_FINALTIME     = 3;
  TAG_CONTROL_DURATION      = 4;
  TAG_CONTROL_PAUSE         = 5;
  TAG_CONTROL_TEXT          = 6;
  TAG_CONTROL_TRANSLATION   = 7;

  TAG_ACTION_ALWAYSENABLED  = 0;
  TAG_ACTION_BYVAL          = 1;
  TAG_ACTION_DISABLE        = 2;
  TAG_ACTION_VIDEO          = 4;
  TAG_ACTION_AUDIO          = 8;
  TAG_ACTION_TRANSCRIPTION  = 16;

  RegExprTime = '([0-1]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](:[0-9][0-9]?|.[0-9][0-9][0-9])';
  URL_WordReference = 'https://www.wordreference.com/redirect/translation.aspx?w=%s&dict=enen';

  {$IFDEF MSWINDOWS}
  URL_FFMPEG  = 'https://github.com/URUWorks/additional-files/raw/main/ffmpeg/ffmpeg_win64.zip';
  URL_LIBMPV  = 'https://github.com/URUWorks/additional-files/raw/main/libmpv/libmpv2_win64.zip';
  URL_YTDLP   = 'https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp.exe';
  URL_WHISPER = 'https://github.com/URUWorks/additional-files/raw/main/whisper/whisper_win64.zip';
  URL_FASTERWHISPER     = 'https://github.com/URUWorks/additional-files/raw/main/whisper/fasterwhisper_win64.zip';
  URL_FASTERWHISPERCUDA = 'https://github.com/Purfview/whisper-standalone-win/releases/download/libs/cuBLAS.and.cuDNN_win_v2.zip';

  FFMPEG_EXE           = 'ffmpeg.exe';
  //FFMPEG_Params        = '-i "%input" -vn -ac 1 -ar 44100 -map 0:a:%trackid -acodec pcm_s16le -y "%output"';
  FFMPEG_Params        = '-i "%input" -vn -ac 1 -ar 44100 -map 0:a:%trackid -acodec pcm_s16le -af "highpass=f=300,asendcmd=0.0 afftdn sn start,asendcmd=1.5 afftdn sn stop,afftdn=nf=-20,dialoguenhance,lowpass=f=3000" -y "%output"';
  FFMPEG_SCParams      = '-hide_banner -i "%input" -vf "select=''gt(scene,%value)'',showinfo" -f null -';
  FFMPEG_VideoEncoding = '-i "%input" -vf "%extrasubtitles=''%subtitle'':force_style=''%style''" -s %widthx%height %videosettings %audiosettings -y -hide_banner "%output"';

  YTDLP_EXE = 'yt-dlp.exe';

  WHISPER_EXE      = 'main.exe';
  WHISPER_Params   = '-m "%model" -l %lang -osrt -of "%output" -f "%input"';
  WHISPER_ffParams = '-i "%input" -ar 16000 -ac 1 -map 0:a:%trackid -c:a pcm_s16le -y "%output"';

  FASTERWHISPER_EXE    = 'whisper-faster.exe';
  FASTERWHISPER_Params = '"%input" --output_dir "%output" --model %model --model_dir "%binpath" --output_format srt --beep_off';

  SCENEDETECT_EXE      = 'scenedetect.exe';
  SCENEDETECT_SCParams = '-i "%input" list-scenes -f "%output" -q';
  {$ENDIF}
  {$IFDEF LINUX}
  URL_FFMPEG = '';
  URL_LIBMPV = '';
  URL_YTDLP  = 'https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp_linux_aarch64';
  URL_WHISPER = 'https://github.com/URUWorks/additional-files/raw/main/whisper/whisper_linux.zip';
  URL_FASTERWHISPER     = 'https://github.com/URUWorks/additional-files/raw/main/whisper/fasterwhisper_linux.zip';
  URL_FASTERWHISPERCUDA = 'https://github.com/Purfview/whisper-standalone-win/releases/download/libs/cuBLAS.and.cuDNN_linux.zip';

  FFMPEG_EXE           = 'ffmpeg';
  FFMPEG_Params        = '-i %input -vn -ac 1 -ar 44100 -map 0:a:%trackid -acodec pcm_s16le -y %output';
  //FFMPEG_Params        = '-i %input -vn -ac 1 -ar 44100 -map 0:a:%trackid -acodec pcm_s16le -af "highpass=f=300,asendcmd=0.0 afftdn sn start,asendcmd=1.5 afftdn sn stop,afftdn=nf=-20,dialoguenhance,lowpass=f=3000" -y %output';
  FFMPEG_SCParams      = '-hide_banner -i %input -vf select=''gt(scene,%value)'',showinfo -f null -';
  FFMPEG_VideoEncoding = '-i %input -vf %extrasubtitles=''%subtitle'':force_style=''%style'' -s %widthx%height %videosettings %audiosettings -y -hide_banner %output';

  YTDLP_EXE = 'yt-dlp_linux_aarch64';

  WHISPER_EXE      = 'main';
  WHISPER_Params   = '-m %model -l %lang -pp -osrt -of %output -f %input';
  WHISPER_ffParams = '-i %input -ar 16000 -ac 1 -map 0:a:%trackid -c:a pcm_s16le -y %output';

  FASTERWHISPER_EXE    = 'whisper-faster';
  FASTERWHISPER_Params = '%input --output_dir %output --model %model --model_dir %binpath --output_format srt --beep_off';

  SCENEDETECT_EXE      = 'scenedetect';
  SCENEDETECT_SCParams = '-i %input list-scenes -f %output -q';
  {$ENDIF}
  {$IFDEF DARWIN}
  URL_FFMPEG = 'https://github.com/URUWorks/additional-files/raw/main/ffmpeg/ffmpeg_macos64.zip';
  URL_LIBMPV = 'https://github.com/URUWorks/additional-files/raw/main/libmpv/libmpv_macos.zip';
  URL_YTDLP  = 'https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp_macos.zip';
  URL_WHISPER = 'https://github.com/URUWorks/additional-files/raw/main/whisper/whisper_macos_%cpu.zip';
  URL_FASTERWHISPER     = 'https://github.com/URUWorks/additional-files/raw/main/whisper/fasterwhisper_macos_%cpu.zip';
  //URL_FASTERWHISPERCUDA = '';

  FFMPEG_EXE           = 'ffmpeg';
  FFMPEG_Params        = '-i %input -vn -ac 1 -ar 44100 -map 0:a:%trackid -acodec pcm_s16le -y %output';
  //FFMPEG_Params        = '-i %input -vn -ac 1 -ar 44100 -map 0:a:%trackid -acodec pcm_s16le -af "highpass=f=300,asendcmd=0.0 afftdn sn start,asendcmd=1.5 afftdn sn stop,afftdn=nf=-20,dialoguenhance,lowpass=f=3000" -y %output';
  FFMPEG_SCParams      = '-hide_banner -i %input -vf select=''gt(scene,%value)'',showinfo -f null -';
  FFMPEG_VideoEncoding = '-i %input -vf %extrasubtitles=''%subtitle'':force_style=''%style'' -s %widthx%height %videosettings %audiosettings -y -hide_banner %output';

  YTDLP_EXE = 'yt-dlp_macos';

  WHISPER_EXE      = 'main';
  WHISPER_Params   = '-m %model -l %lang -pp -osrt -of %output -f %input';
  WHISPER_ffParams = '-i %input -ar 16000 -ac 1 -map 0:a:%trackid -c:a pcm_s16le -y %output';

  FASTERWHISPER_EXE    = 'whisper-faster';
  FASTERWHISPER_Params = '%input --output_dir %output --model %model --model_dir %binpath --output_format srt --beep_off';

  SCENEDETECT_EXE      = 'scenedetect';
  SCENEDETECT_SCParams = '-i %input list-scenes -f %output -q';
  {$ENDIF}

type

  { Workspace }

  TWorkMode = (wmTime, wmFrames);
  TViewMode = (vmList, vmSource, vmTranscription);
  TMediaPlayMode = (mpmAll, mpmSelection, mpmFromSelection, mpmBeforeSelection, mpmAfterSelection);

  TTranscription = record
    Memo: TATSynEdit;
  end;

  TWorkspace = record
    ViewMode       : TViewMode;
    WorkMode       : TWorkMode;
    TranslatorMode : Boolean;
    SMPTE          : Boolean;
    FPS            : record        // used for conversions
                       DefFPS    : Single;
                       InputFPS  : Single;
                       OutputFPS : Single;
                     end;
    DefEncoding    : Integer;
    DefFormat      : TUWSubtitleFormats;
    MediaPlayMode  : TMediaPlayMode;
    Transcription  : TTranscription;
    Layout         : Byte;
  end;

  { VST Proc }

  TVSTDoLoopProc      = procedure(const Item: PUWSubtitleItem; const Index: Integer);
  TVSTDoLoopProcCB    = procedure(const CurrentItem, TotalCount: Integer; var Cancel: Boolean);
  TVSTDoLoopSelection = (dlAll, dlSelected, dlCurrentToLast, dlMarked, dlRange);

  { VST Options }

  TVSTDrawMode = (dmList, dmBlock);  // dmList = classic list, dmBlock = new style

  TVSTOptions = record
    BackgroundBlock  : TBGRABitmap;  // VSTBeforeCellPaint_Block
    RepaintBckgBlock : Boolean;      // VSTResize
    DrawMode         : TVSTDrawMode;
    DrawErrors       : Boolean;      // VSTBeforeCellPaint
    DrawTags         : Boolean;
  end;

  { TMPVOptions }

  TMPVOptions = record
    SubtitleHandleByMPV    : Boolean; // mpv handles the subtitle
    KeepVideoOpen          : Boolean;
    TextColor              : String;
    TextBorderColor        : String;
    TextShadowColor        : String;
    UseTextShadowColor     : Boolean;
    TextBackgroundColor    : String;
    UseTextBackgroundColor : Boolean;
    TextPosition           : String;
    TextSize               : Integer;
    TextShadowOffset       : Integer;
    AutoStartPlaying       : Boolean;
    SubtitleToShow         : TSubtitleMode;
    SeekTime               : Integer;
    FrameStep              : Byte;
    AdditionalOptions      : array of String;
    UnDockData             : record  // procUnDockVideoControls
                               FileName        : String;
                               CurrentPosition : Integer;
                               Paused          : Boolean;
                               WAVELoaded      : Boolean;
                             end;
    Volume                 : record
                               Percent : Byte;
                               Mute    : Boolean;
                            end;
  end;

  { TWAVEOptions }

  TWAVEOptions = record
    LoopCount : Byte;
  end;

  { TTools }

  TWhisperEngine = (WhisperCPP, FasterWhisper);

  TTools = record
    FFmpeg                       : String;
    FFmpeg_ParamsForAudioExtract : String;
    FFmpeg_ParamsForShotChanges  : String;
    FFmpeg_ParamsForWhisper      : String;

    PySceneDetect        : String;
    PySceneDetect_Params : String;

    WhisperCPP            : String;
    WhisperCPP_Params     : String;
    WhisperCPP_Additional : String;

    FasterWhisper            : String;
    FasterWhisper_Params     : String;
    FasterWhisper_Additional : String;

    YTDLP : String;

    WhisperEngine : TWhisperEngine;
  end;

  { TAdjustSubtitles }

  TAdjustItem = record
    SubtitleIndex  : Integer;
    NewInitialTime : Cardinal;
  end;

  TAdjustSubtitles = array of TAdjustItem;

  { TTextEffect }

  TTextEffect = (fxFlash, fxTypewriter);

  { SubtitleInfo }

  TSubtitleInfo = record
    Text,
    Translation  : record
                     FileName : String;
                     Format   : TUWSubtitleFormats;
                     Changed  : Boolean;
                   end;

    LastSubtitle : record
                     Selected    : Integer;  // GetSubtitleTextAtTime
                     InitialTime : Cardinal; // actMediaStartSubtitle
                     Color       : TColor;   // ApplyFontColor
                     ShowIndex   : Integer;  // GetSubtitleTextAtTime
    end;
  end;

  { AppOptions }

  TAppOptions = record
    CommonErrors           : TSubtitleErrorTypeSet;
    Conventions            : TProfileItem;
    ShiftTimeMS            : Cardinal;
    DefChangePlayRate      : Byte;
    DialogSegmentThreshold : Cardinal;
    AutoBackupSeconds      : Cardinal;
    AutoLengthChar         : Cardinal;
    AutoLengthWord         : Cardinal;
    AutoLengthLine         : Cardinal;
    ExpandMs               : Cardinal;
    ExpandChar             : Cardinal;
    ExpandLen              : Cardinal;
    ShowWelcomeAtStartup   : Boolean;
    UseOwnFileDialog       : Boolean;
    AutoCheckErrors        : Boolean;
    AskForDeleteLines      : Boolean;
    AskForInputFPS         : Boolean;
    AskForInputEncoding    : Boolean;
    CheckErrorsBeforeSave  : Boolean;
    TextToFind             : String;
    LastUnicodeChar        : TUTF8Char;
    WebSearchURL           : String;
    GUILanguage            : String;
    HunspellLanguage       : String;
    ShortCutPreset         : String;
    UnicodeChars           : array of TUTF8Char;
  end;

var
  Subtitles     : TUWSubtitles;  // SubtitleAPI power!
  SubtitleInfo  : TSubtitleInfo;
  Workspace     : TWorkspace;
  VSTOptions    : TVSTOptions;
  MPVOptions    : TMPVOptions;
  WAVEOptions   : TWAVEOptions;
  Tools         : TTools;
  AppOptions    : TAppOptions;
  MRU           : TMRU;
  TMX           : TUWTMX;
  TBX           : TUWTBX;
  LastTickCount : Cardinal;     // Source memo change

implementation

end.

