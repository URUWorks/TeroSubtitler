# ![TeroSubtitler](share/terosubtitler_logo.png)
[![License: MPL 2.0](https://img.shields.io/badge/License-MPL_2.0-brightgreen.svg)](https://opensource.org/licenses/MPL-2.0)
[![TS release](https://img.shields.io/github/v/release/URUWorks/TeroSubtitler?style=badge&sort=semver)](https://github.com/URUWorks/TeroSubtitler/releases)
[![TS downloads](https://img.shields.io/github/downloads/URUWorks/TeroSubtitler/total.svg)](https://github.com/URUWorks/TeroSubtitler/releases)
[![Handbook](https://img.shields.io/badge/Handbook-EN%20%7C%20ES-1764b8)](https://handbook.uruworks.net)
[![discord](https://img.shields.io/badge/Discord-blue)](https://discord.gg/EqzXEa8pHK)
[![ko-fi](https://img.shields.io/badge/Ko--fi-F16061?logo=ko-fi&logoColor=white)](https://ko-fi.com/uruworks)
[![Donate](https://img.shields.io/badge/Donate-PayPal-blue.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=uruworks@gmail.com&lc=US&item_name=URUWorks+Tero+Subtitler&no_note=0&cn=&currency_code=USD&amount=10&tax=0&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted)
[![Github-Sponsors](https://img.shields.io/badge/Sponsor-30363D?&logo=GitHub-Sponsors)](https://github.com/sponsors/URUWorks)

Tero Subtitler is an open source, cross-platform, and free subtitle editing software for Windows, macOS and Linux.

- **Download:** [latest release](https://github.com/URUWorks/TeroSubtitler/releases) (Windows installer or portable, macOS, Linux)
- **User manual:** [handbook.uruworks.net](https://handbook.uruworks.net), in English and Spanish
- **Community:** [Discord](https://discord.gg/EqzXEa8pHK) and [GitHub Discussions](https://github.com/URUWorks/TeroSubtitler/discussions)

## Features

- User-friendly and multi-language interface
- Fully fledged editing with SMPTE and Media modes
- Support for 45+ subtitle formats (SubRip, WebVTT, ASS, TTML, EBU STL, Netflix and iTunes Timed Text, DCinema, spreadsheets...)
- Multi-level undo/redo
- Search and replace
- Auto-backup
- Source, Translator and Transcription modes
- Translation memory and terminology
- Audiovisual preview for many formats
- Timeline with waveform visualizer and shot change detection
- Tools for manipulation (frame/video rate conversion), formatting (fonts and alignment), quality control (spell check), analysis (comparing subtitle sets), translation (including auto), transcription (including auto), validation (industry and customizable profiles), and automation (correcting errors and breaches)
- Text to speech (export subtitles to audio)
- Import/Export Blu-ray SUP format
- Generate blank video
- Generate video with hardcoded subtitles
- Video dubbing
- Batch conversion
- And much more

See the [handbook](https://handbook.uruworks.net) for a full guide to every menu and tool.

### [mpv](https://mpv.io/) playback engine

Powerful multimedia playback engine.

### [FFmpeg](https://ffmpeg.org/)

Advanced audio/video manipulation tool.

### [yt-dlp](https://github.com/yt-dlp/yt-dlp)

For opening videos via URL.

### [whisper.cpp](https://github.com/URUWorks/UW_WhisperCPP#uw_whispercpp) / [Faster-Whisper](https://github.com/URUWorks/UW_FasterWhisper#uw_fasterwhisper)

For the automatic transcription of audio to subtitles.

### [Google Translate](https://translate.google.com/)

Automatic translation.

### [ElevenLabs TTS](https://elevenlabs.io/)

Text to speech and video dubbing.

### [PySceneDetect](https://www.scenedetect.com/)

Optional shot change detector.

### [Tesseract](https://github.com/URUWorks/UW_Tesseract#uw_tesseract)

Open source OCR engine, used to import Blu-ray SUP subtitles.

## Compiling

[Lazarus IDE](https://www.lazarus-ide.org/).

### Internal packages required to compile

- [UW Blu-ray PGS Parser](https://github.com/URUWorks/UW_BlurayPGSParser)
- [UW Common units](https://github.com/URUWorks/TeroSubtitler/tree/main/TeroSubtitler/common)
- [UW Subtitle API](https://github.com/URUWorks/TeroSubtitler/tree/main/TeroSubtitler/subtitleapi)
- [UW Tero controls](https://github.com/URUWorks/TeroSubtitler/tree/main/TeroSubtitler/controls)

### External packages required to compile

- [ATSynEdit](https://wiki.freepascal.org/ATSynEdit) ([EncConv](https://github.com/Alexey-T/EncConv), [ATFlatControls](https://github.com/Alexey-T/ATFlatControls))
- [BGRABitmap](https://wiki.freepascal.org/BGRABitmap)
- [FPSpreadsheet](https://wiki.lazarus.freepascal.org/FPSpreadsheet)
- [UW MPVPlayer](https://github.com/URUWorks/UW_MPVPlayer)

## Contributions

We appreciate anyone who can offer skills towards the project (additions and refinements), and there is a desire for volunteers who can assist in the following roles/tasks:
- App designer (improving Tero Subtitler's UI and UX)
- Coders (resolving issues and developing features)
- Localization (reaching more people)
- Documentation (keeping the [handbook](https://handbook.uruworks.net) up to date and translating it)
- Subtitling consultant (for QC and keeping abreast of industry developments)
- Video producer (for social media)
- Web designer (for URUWorks)

We encourage public discussions (ideally in English) to avoid the doubling of work and reach agreements with others:
- If there are questions or proposals, please create a topic (after searching for existing ones) in Discussions before heading to Issues.
- Users can join our Discord channel for queries and discussions (using the relevant category).

## History

URUWorks' subtitling solution began as Subtitle Workshop. In making the distinction from the other project with the same name, the organization is retiring that repository in favor of Tero Subtitler.

## Donations

We appreciate any donation, no matter how small, to help cover the costs of developing the software that URUWorks maintains. Not only will you help them continue to be free and improved, but it implies to the members of URUWorks that their work is valued and appreciated, thus encouraging the investment of time and resources as part of our mission to provide all user bases with better apps and support.
 
[![PayPal](https://img.shields.io/badge/PayPal-00457C?style=for-the-badge&logo=paypal&logoColor=white)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=uruworks@gmail.com&lc=US&item_name=URUWorks+Tero+Subtitler&no_note=0&cn=&currency_code=USD&amount=10&tax=0&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted)
[![Ko-Fi](https://img.shields.io/badge/Ko--fi-F16061?style=for-the-badge&logo=ko-fi&logoColor=white)](https://ko-fi.com/uruworks)

## Special thanks 

Circular,
Rafael López Sánchez,
Scott Napier,
Serenella Massidda,
Martin Keary,
Casper Jeukendrup,
Pedro Caires,
and all users of our software!
