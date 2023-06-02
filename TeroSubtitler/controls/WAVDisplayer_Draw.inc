{*
 *  URUWorks Waveform Displayer Control
 *
 *  Copyright (C) 2021-2023 URUWorks, uruworks@gmail.com.
 *
 *  Based on the great work of:
 * -----------------------------------------------------------------------------
 *  VisualSubSync
 * -----------------------------------------------------------------------------
 *  Copyright (C) 2003 Christophe Paris
 * -----------------------------------------------------------------------------
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *  http://www.gnu.org/copyleft/gpl.html
 * -----------------------------------------------------------------------------
 *}

// -----------------------------------------------------------------------------

{ DrawAlphaRect }

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawAlphaRect(const ABitmap:{$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const x1, x2 : Integer; const y1, y2 : Integer; const AColor: {$IFDEF USEBGRABITMAP}TBGRAPixel{$ELSE}TColor{$ENDIF});
var
  r : TRect;
begin
  r.Left   := x1;
  r.Right  := x2;
  r.Top    := y1;
  r.Bottom := y2;

  {$IFDEF USEBGRABITMAP}
  ABitmap.FillRect(r, ColorToBGRA(AColor, 10), dmDrawWithTransparency)
  {$ELSE}
  laz.VTGraphics.AlphaBlend(ABitmap.Canvas.Handle, ABitmap.Canvas.Handle, r,
    Point(0, 0), bmConstantAlphaAndColor, 40, AColor);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawAlphaRectByTimes(const ABitmap:{$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const t1, t2 : Integer; const y1, y2 : Integer; const AColor: {$IFDEF USEBGRABITMAP}TBGRAPixel{$ELSE}TColor{$ENDIF});
var
  x1, x2 : Integer;
begin
  x1 := TimeToPixel(t1 - FPositionMS);
  x2 := TimeToPixel(t2 - FPositionMS);

  Constrain(x1, 0, Width);
  Constrain(x2, 0, Width);
  DrawAlphaRect(ABitmap, x1, x2, y1, y2, AColor);
end;

// -----------------------------------------------------------------------------

{ DrawWave }

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawWave(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const TryOptimize: Boolean = False);
var
  x, y1, y2            : Integer;
  x1_update, x2_update,
  x_optim              : Integer;
  x_scaled             : Cardinal;
  PeaksPerPixelScaled  : Double;
  StartPositionInPeaks : Double;
  Middle               : Integer;
  PeakMax, PeakMin     : Integer;
  i                    : Integer;
  Rect1                : TRect;
  RectHeight           : Integer;
begin
  {$IFDEF USEBGRABITMAP}
  ABitmap.Fill(CustomColors.Background);
  {$ELSE}
  ABitmap.Canvas.Brush.Color := CustomColors.Background;
  ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);
  {$ENDIF}
  DrawGridLines(ABitmap);

  if not FPeakDataLoaded then Exit;

  with ABitmap.{$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF} do
  begin
    PeaksPerPixelScaled := (((FPageSizeMS / 1000.0) * FWaveFormat.nSamplesPerSec) / FSamplesPerPeak) / Width;
    StartPositionInPeaks := ((FPositionMS / 1000.0) * FWaveFormat.nSamplesPerSec) / FSamplesPerPeak;

    x1_update := 0;
    x2_update := Width;

    if TryOptimize and (FOldPageSizeMs = FPageSizeMs) then
    begin
      // Calculate intersection between old and new view, and update only the new part
      if (FPositionMs > FOldPositionMs) then
      begin
        x_optim   := TimeToPixel(FPositionMs - FOldPositionMs);
        x2_update := Width;
        x1_update := x2_update - x_optim;
        Constrain(x1_update, 0, Width);

        if (x1_update <> 0) then
          CopyRect(Rect(0, 0, x1_update, Height), ABitmap{$IFNDEF USEBGRABITMAP}.Canvas{$ENDIF}, Rect(x_optim, 0, x1_update, Height));
      end
      else
      begin
        x_optim   := TimeToPixel(FOldPositionMs - FPositionMs);
        x1_update := 0;
        x2_update := x_optim;
        Constrain(x2_update, 0, Width);

        if (x2_update <> Width) then
          CopyRect(Rect(x_optim, 0, Width-x_optim, Height), ABitmap{$IFNDEF USEBGRABITMAP}.Canvas{$ENDIF}, Rect(0, 0, Width-x_optim, Height));
      end;
    end;

    Rect1        := ClientRect;
    Rect1.Left   := x1_update;
    Rect1.Right  := x2_update;
    Rect1.Bottom := Rect1.Bottom - FScrollBar.Height;

    RectHeight := Rect1.Bottom - Rect1.Top;
    Middle     := Rect1.Top + (RectHeight div 2);

    // Wave
    Pen.{$IFDEF USEBGRABITMAP}BGRAColor{$ELSE}Color{$ENDIF} := CustomColors.Waveform;
    Pen.Style := psSolid;
    Pen.Mode  := pmCopy;

    for x := x1_update to x2_update do
    begin
      x_scaled := Round((x * PeaksPerPixelScaled) + StartPositionInPeaks);

      if (x_scaled >= FPeakTabSize) then
        x_scaled := FPeakTabSize - 1;

      // calculate peak from x_scaled to next x_scaled
      PeakMax := FPeakTab[x_scaled].Max;
      PeakMin := FPeakTab[x_scaled].Min;
      for i := x_scaled+1 to Min(Round(((x+1)*PeaksPerPixelScaled)+StartPositionInPeaks), FPeakTabSize)-1 do
      begin
        if FPeakTab[i].Max > PeakMax then
          PeakMax := FPeakTab[i].Max;

        if FPeakTab[i].Min < PeakMin then
          PeakMin := FPeakTab[i].Min;
      end;

      y1 := Round((((PeakMax * FVerticalScaling) / 100) * RectHeight) / 65536);
      y2 := Round((((PeakMin * FVerticalScaling) / 100) * RectHeight) / 65536);

      Line(x, Middle-y1, x, Middle-y2);
    end;

    // zero line
    //Pen.BGRAColor := CustomColors.Waveform;
    Line(0, Middle, Width, Middle);
  end;
end;

// -----------------------------------------------------------------------------

{ DrawTimeLine }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawTimeLine(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  PosRect      : TRect;
  PosString    : String;
  p, x, x1, x2 : Integer;
begin
  if not IsTimeLineEnabled then Exit;

  with ABitmap do
  begin
    // Set the text font
    {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Color := CustomColors.Text;
    {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;
    //CanvasBGRA.Font.Name := 'Tahoma';
    {$IFDEF USEBGRABITMAP}
    CanvasBGRA.Font.Height := 10;
    {$ELSE}
    Canvas.Font.Size := 7;
    {$ENDIF}
    FTimeLineHeight := {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.TextHeight('0');

    PosRect        := ClientRect;
    PosRect.Bottom := PosRect.Bottom - FScrollBar.Height;
    PosRect.Top    := PosRect.Bottom - FTimeLineHeight;

    // Draw background
    {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Brush.Style := bsClear;
    p := (FPositionMS div FStepMs * FStepMs);
    while (p < FPositionMS + FPageSizeMS) do
    begin
      // Draw main division
      x := TimeToPixel(p - FPositionMS);
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, PosRect.Top +2, x, PosRect.Bottom);
      if not FFPSTimeMode then
        PosString := TimeMSToShortString(p, FStepLog)
      else
        PosString := TimeMSToShortStringFrames(p, 1, FFPS);
      // Calculate text coordinate
      x1 := x + 3; //x - (CanvasBGRA.TextWidth(PosString) div 2);
      // Draw text
      {$IFDEF USEBGRABITMAP}CanvasBGRA.Font.BGRAColor{$ELSE}Canvas.Font.Color{$ENDIF} := CustomColors.Text;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.TextOut(x1, PosRect.Top, PosString);
      // Draw subdivision?
      if FDrawSubdivision then
      begin
        x2 := x + TimeToPixel(FStepMs div 2);
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x2, PosRect.Bottom -4, x2, PosRect.Bottom);
      end;
      p := p + FStepMs;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ DrawItemsCanvas }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawItemsCanvas(const ACompleteDraw: Boolean = False);
var
  y1, y2: Integer;
begin
  if not Assigned(FSubtitles) then Exit;

  if ACompleteDraw then
  begin
    // Draw WAVE
    {$IFDEF USEBGRABITMAP}
    FBackBufferItems.Assign(FBackBufferWAVE);
    {$ELSE}
    FBackBufferItems.Canvas.Draw(0, 0, FBackBufferWAVE);
    {$ENDIF}
    // Draw Subtitle Items
    //FBackBufferItems.FontStyle := [TFontStyle.fsBold];
    y1 := 0;
    y2 := GetWAVECanvasHeight;
    DrawMinimumBlank(FBackBufferItems, y1, y2);
    DrawSceneChange(FBackBufferItems, y1, y2);
    DrawSubtitleItem(FBackBufferItems, y1, y2);

    {$IFDEF USEBGRABITMAP}
    FBackBuffer.Assign(FBackBufferItems);
    {$ELSE}
    FBackBuffer.Canvas.Draw(0, 0, FBackBufferItems);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF USEBGRABITMAP}
    FBackBuffer.Assign(FBackBufferItems);
    {$ELSE}
    FBackBuffer.Canvas.Draw(0, 0, FBackBufferItems);
    {$ENDIF}
  end;

  {$IFDEF USEBGRABITMAP}
  FBackBuffer.Rectangle(FBackBuffer.CanvasBGRA.ClipRect, BGRA(40, 40, 40)); // Some border
  {$ELSE}
  FBackBuffer.Canvas.Pen.Color := RGBToColor(40, 40, 40);
  FBackBuffer.Canvas.Brush.Style := bsClear;
  FBackBuffer.Canvas.Rectangle(FBackBuffer.Canvas.ClipRect); // Some border
  {$ENDIF}
end;

//------------------------------------------------------------------------------

{ DrawSubtitleItem }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawSubtitleItem(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);

  {procedure _TextOut(const X, Y: Integer; const S: String);
  begin
    with ABitmap.Canvas do
    begin
      Font.Size := 8;
      Font.Color := CustomColors.Text;
      TextOut(X, Y, S);
    end;
  end;}

  procedure _TextOut(const R: TRect; const S, N: String; ALayout: TTextLayout);
  begin
    with ABitmap.{$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF} do
    begin
      Font.Height := 14;
      Font.Style := [fsBold];
      {$IFDEF USEBGRABITMAP}Font.BGRAColor{$ELSE}Font.Color{$ENDIF} := CustomColors.Text;
      FTS.Layout := ALayout;

      if N <> '' then
        TextRect(R, R.Left, R.Top, S + sLineBreak + sLineBreak + WideChar(#$2691) + ' ' + N, FTS)
      else
        TextRect(R, R.Left, R.Top, S, FTS);
    end;
  end;

const
  TextMargin = 4;

var
  i        : Integer;
  r        : TUWSubtitleItem;
  x1, x2,
  y1, y2   : Integer;
  yy1, yy2 : Integer;
  ShowStart, ShowStop, FullHLines : Boolean;
  CustomDrawRect : TRect;
//  sTime     : String;
begin
  with ABitmap do
  begin
    FTS.Alignment := taLeftJustify;
    y1  := ATop;
    y2  := ABottom - FScrollBar.Height;
    yy1 := ATop;
    yy2 := ABottom;

    for i := 0 to FSubtitles.Count-1 do
    begin
      r  := FSubtitles[i];
      x1 := -1;
      x2 := -1;

      if (r.InitialTime >= FPositionMS) and (r.InitialTime <= FPositionMS + FPageSizeMS) then
        x1 := TimeToPixel(r.InitialTime - FPositionMS);

      if (r.FinalTime >= FPositionMS) and (r.FinalTime <= FPositionMS + FPageSizeMS) then
        x2 := TimeToPixel(r.FinalTime - FPositionMS);

      if x1 > Width then Break;

      ShowStart  := (x1 <> -1);
      ShowStop   := (x2 <> -1) and (x2 <> x1);
      FullHLines := (r.InitialTime < FPositionMS) and (r.FinalTime > FPositionMS + FPageSizeMS);

      //Canvas.Pen.Mode  := pmCopy;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 2;

      // Paint start time
      if ShowStart then
      begin
        //if (FDynamicEditMode = demInitial) and (FDynamicSelSub = @r) then
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Style := psSolid;
        //else
          //Canvas.Pen.Style := psDot;

        {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.ItemIT;
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x1, yy1, x1, yy2);

        {z := x2;
        if z = -1 then z := FBackBuffer.Width-1;
        if (z - x1) > 70 then
        begin
          Canvas.Brush.Style := bsClear;
          sTime := TrimTimeString(TimeToString(r.InitialTime, 'h:mm:ss.zzz'));
          _TextOut(x1+4, y2, sTime);
        end;}
      end;

      // Paint stop time
      if ShowStop then
      begin
        //if (FDynamicEditMode = demFinal) and (FDynamicSelSub = @r) then
        //  Canvas.Pen.Style := psSolid
        //else
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Style := psDash;

        {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.ItemFT;
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x2, yy1, x2, yy2);

        {z := x1;
        if z = -1 then z := 0;
        if (x2 - z) > 140 then
        begin
          Canvas.Brush.Style := bsClear;
          sTime := TrimTimeString(TimeToString(r.FinalTime, 'h:mm:ss.zzz'));
          _TextOut(x2-4-Canvas.TextWidth(sTime), y2, sTime);
        end;}
      end;

      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      // Draw the top and bottom horizontal lines
      if FullHLines then
      begin
        x1 := 0;
        x2 := Width-1;
      end;

      if ((x1 <> -1) or (x2 <> -1)) then
      begin
        if (x1 = -1) then
          x1 := 0
        else if (x2 = -1) then
          x2 := Width-1;

        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Style := psSolid;

        // Draw top h-line
        //Canvas.Line(x1, y1, x2, y1);
        // Draw bottom h-line
        //Canvas.Line(x1, y2, x2, y2);

        {$IFDEF USEBGRABITMAP}
        ABitmap.FillRect(x1, y1, x2, yy2, CustomColors.Item, dmDrawWithTransparency);
        {$ELSE}
        DrawAlphaRect(ABitmap, x1, x2, y1, yy2, CustomColors.Item);
        {$ENDIF}

        // Custom draw
        if ((x2 - x1) > 10) then
        begin
          CustomDrawRect.Top    := y1+TextMargin;
          CustomDrawRect.Left   := x1+TextMargin;
          CustomDrawRect.Right  := x2-TextMargin;
          CustomDrawRect.Bottom := y2-TextMargin;

          if Assigned(FOnCustomDrawSubtitleItem) then
            FOnCustomDrawSubtitleItem(Self, Canvas, i, r, CustomDrawRect)
          else
          begin
            case Subtitles[i].Align of
              2: FTS.Alignment := taCenter;
              3: FTS.Alignment := taRightJustify;
            else
              FTS.Alignment := taLeftJustify;
            end;
            _TextOut(CustomDrawRect, RemoveTSTags(Subtitles[i].Text), Subtitles[i].Notes, tlTop);

            FTS.Alignment := taLeftJustify;
            //_TextOut(CustomDrawRect, '#' + IntToStr(i+1) +'  ' + TimeToString(Subtitles.Duration[i], 'h:mm:ss.zzz', FFPS, True), '', tlBottom);

            if not FFPSTimeMode then
              _TextOut(CustomDrawRect, '#' + IntToStr(i+1) + '  ' + TimeMSToShortString(Subtitles.Duration[i], 1), '', tlBottom)
            else
              _TextOut(CustomDrawRect, '#' + IntToStr(i+1) + '  ' + TimeMSToShortStringFrames(Subtitles.Duration[i], 1, FFPS), '', tlBottom);
          end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ DrawSelection }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawSelection(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  x1, x2       : Integer;
  CanvasHeight : Integer;
  SelRect      : TRect;
begin
  CanvasHeight := GetWAVECanvasHeight;

  // Selection
  if (FDynamicSelection.FinalTime > 0) then
    with ABitmap do
    begin
      //Brush.Style := bsSolid;
      //Pen.Style   := psDot;
      //Pen.Color   := clWhite;
      //Pen.Width   := 1;

      x1 := TimeToPixel(FDynamicSelection.InitialTime - FPositionMS);
      x2 := TimeToPixel(FDynamicSelection.FinalTime - FPositionMS);

      if (x1 = x2) then
      begin
        // points are on each other and in the display Subtitle
        if (FDynamicSelection.InitialTime >= FPositionMS) and
           (FDynamicSelection.InitialTime <= FPositionMS + FPageSizeMS) then
        begin
          {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x1, 0, x1, CanvasHeight);
        end;
      end
      else
      begin
        Constrain(x1, 0, Width);
        Constrain(x2, 0, Width);
        if (x1 <> x2) then
        begin
          SelRect           := ClientRect;
          SelRect.Left      := x1;
          SelRect.Right     := x2+1;
          SelRect.Bottom    := SelRect.Bottom - FScrollBar.Height;

          if FSelectedSubtitle = NIL then
            {$IFDEF USEBGRABITMAP}
            ABitmap.FillRect(SelRect, CustomColors.Item, dmDrawWithTransparency)
            {$ELSE}
            DrawAlphaRect(ABitmap, SelRect.Left, SelRect.Right, SelRect.Top, SelRect.Bottom, CustomColors.Item)
            {$ENDIF}
          else
            {$IFDEF USEBGRABITMAP}
            ABitmap.FillRect(SelRect, CustomColors.ItemSelected, dmDrawWithTransparency);
            {$ELSE}
            DrawAlphaRect(ABitmap, SelRect.Left, SelRect.Right, SelRect.Top, SelRect.Bottom, CustomColors.ItemSelected);
            {$ENDIF}
        end;
      end;
    end;
end;

//------------------------------------------------------------------------------

{ DrawCursor }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawCursor(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  x: Integer;
begin
  // Cursor
  if (FCursorMS >= FPositionMS) and (FCursorMS <= FPositionMS + FPageSizeMS) then
    with ABitmap do
    begin
      //Canvas.Pen.Style := psSolid;
      //Canvas.Pen.Mode  := pmCopy;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Color := CustomColors.Cursor;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      x := TimeToPixel(FCursorMS - FPositionMS);
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, 0, x, GetWAVECanvasHeight);
    end;
end;

//------------------------------------------------------------------------------

{ DrawPlayCursor }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawPlayCursor(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  x: Integer;
begin
  // Play Cursor
  if (FPlayCursorMS >= FPositionMS) and (FPlayCursorMS <= FPositionMS + FPageSizeMS) then
    with ABitmap do
    begin
      //Canvas.Pen.Style := psSolid; //psDot;
      //Canvas.Pen.Mode  := pmCopy;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Style := psDot;
      {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.PlayCursor;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      x := TimeToPixel(FPlayCursorMS - FPositionMS);
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, 0, x, GetWAVECanvasHeight);
    end;
end;

//------------------------------------------------------------------------------

{ DrawGridLines }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawGridLines(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  i, p, x, x1: Integer;
begin
  with ABitmap do
  begin
    //Canvas.Pen.Style := psSolid;
    //Canvas.Pen.Mode  := pmCopy;
    {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.GridLine;
    {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

    if not IsTimeLineEnabled then
    begin
      i := 0;
      while i < {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Width do
      begin
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(i, 0, i, GetWAVECanvasHeight);
        inc(i, 30);
      end;
    end
    else
    begin
      p := (FPositionMS div FStepMs * FStepMs);
      while (p < FPositionMS + FPageSizeMS) do
      begin
        // Draw main division
        x := TimeToPixel(p - FPositionMS);
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, 0, x, GetWAVECanvasHeight);

        x1 := x + TimeToPixel(FStepMs div 2);
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x1, 0, x1, GetWAVECanvasHeight);

        p := p + FStepMs;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ DrawSceneChange }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawSceneChange(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);
var
  x, i, SceneChange : Integer;
begin
  if FSceneChangeEnabled then
    with ABitmap do
    begin
      //CanvasBGRA.Brush.Style := bsSolid;
      //CanvasBGRA.Pen.Style   := psSolid;
      {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.SceneChange;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width     := 1;

      for i := Low(FSceneChangeList) to High(FSceneChangeList) do
      begin
        SceneChange := FSceneChangeList[i];
        if (SceneChange >= FPositionMS) and (SceneChange <= FPositionMs + FPageSizeMS) then
        begin
          x := TimeToPixel(SceneChange - FPositionMS);
          {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, ATop, x, ABottom);
        end;
      end;
    end;
end;

//------------------------------------------------------------------------------

{ DrawMinimumBlank }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawMinimumBlank(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);
begin
  if FDrawGAP then
    with ABitmap do
    begin
      //CanvasBGRA.Brush.Style := bsSolid;
      //CanvasBGRA.Pen.Style   := psSolid;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      if (FMinBlankInfo1.Exists) then
        DrawAlphaRectByTimes(ABitmap, FMinBlankInfo1.GetStart(FGAP),
          FMinBlankInfo1.GetStop(FGAP), ATop, ABottom, clWhite);

      if (FMinBlankInfo2.Exists) then
        DrawAlphaRectByTimes(ABitmap, FMinBlankInfo2.GetStart(FGAP),
          FMinBlankInfo2.GetStop(FGAP), ATop, ABottom, clWhite);
    end;
end;

//------------------------------------------------------------------------------

