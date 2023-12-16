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

// -----------------------------------------------------------------------------

{ PrepareVSTDraw }

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTDrawInitialize(const ADrawMode: TVSTDrawMode);
var
  TextHeight: Integer;
begin
  VSTOptions.BackgroundBlock := NIL;
  VSTOptions.DrawMode := ADrawMode;

  VST.BeginUpdate;
  try
    VST.Header.Columns.Clear;
    VST.DefaultText := '';

    TextHeight := VST.Canvas.Font.GetTextHeight('W');
    if ADrawMode = dmList then
    begin
      VST.DefaultNodeHeight := (TextHeight*4);
      VST.Header.Height     := TextHeight+4;
      VST.Header.Options    := VST.Header.Options + [hoVisible];
      VST.TreeOptions.PaintOptions := VST.TreeOptions.PaintOptions + [toShowHorzGridLines];
      VST.OnGetText         := @VSTGetText;
      VST.OnBeforeCellPaint := @VSTBeforeCellPaint;
      VST.OnAfterItemPaint  := @VSTAfterItemPaint;
      VST.OnDrawText        := @VSTDrawText;

      VSTAddColumn(VST, lnghIndex, 70, taCenter, actShowColumnNumber.Checked);
      VSTAddColumn(VST, lnghTimes, 90, taCenter, actShowColumnTimes.Checked);
      VSTAddColumn(VST, lnghDuration, 70, taCenter, actShowColumnDuration.Checked);
      VSTAddColumn(VST, lnghStyleAndActor, 90, taCenter, actShowColumnStyleAndActor.Checked);
      VSTAddColumn(VST, lnghText, 200, taLeftJustify);
      VSTAddColumn(VST, lnghTranslation, 200, taLeftJustify, actTranslatorMode.Checked);
      VSTAddColumn(VST, lnghCPS, 50, taCenter, actShowColumnCPS.Checked);
      VSTAddColumn(VST, lnghWPM, 50, taCenter, actShowColumnWPM.Checked);
      VSTAddColumn(VST, lnghCPL, 50, taCenter, actShowColumnCPL.Checked);
    end
    else
    begin // dmBlock
      VST.DefaultNodeHeight := (TextHeight*6);
      VST.Header.Options    := VST.Header.Options - [hoVisible];
      VST.TreeOptions.PaintOptions := VST.TreeOptions.PaintOptions - [toShowHorzGridLines];
      VST.OnBeforeCellPaint := @VSTBeforeCellPaint_Block;
      VST.OnGetText         := NIL;
      VST.OnDrawText        := NIL;
    end;

    VSTSetNodeHeight(VST, VST.DefaultNodeHeight);
    VST.UpdateRanges;
  finally
    VST.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTDrawErrors(TargetCanvas: TCanvas; CellRect: TRect; AIndex: Integer; AX: Integer = 4; AW: Integer = 18; ALeftMargin: Integer = 4; ABottomMargin: Integer = 18);

  procedure DrawErrorIcon(const AImageList: TImageList; const AErrorType: TSubtitleErrorType);
  begin
    AImageList.Draw(TargetCanvas, CellRect.Left+AX, CellRect.Bottom-ABottomMargin, Integer(AErrorType))
  end;

var
  ET: TSubtitleErrorType;
begin
  if VSTOptions.DrawErrors then
  begin
    AX := ALeftMargin;
    for ET in Subtitles[AIndex].ErrorType do
    begin
      if ColorThemeInstance.GetRealColorMode = cmDark then
        DrawErrorIcon(ImageList_ET_Dark, ET)
      else
        DrawErrorIcon(ImageList_ET_Default, ET);

      Inc(AX, AW);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTDrawValidateIcon(TargetCanvas: TCanvas; CellRect: TRect; AX, AY, AIndex: Integer);
var
  idx: Integer;
begin
  if (Subtitles[AIndex].Data > 0) then
    idx := 72
  else
    idx := 73;

  if ColorThemeInstance.GetRealColorMode = cmDark then
    ImageList_Dark.Draw(TargetCanvas, AX, AY, idx)
  else
    ImageList_Default.Draw(TargetCanvas, AX, AY, idx);
end;

// -----------------------------------------------------------------------------

{ Classic Draw (with columns) }

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
begin
  // I take values directly in "DrawText" because from here clip the var string
  CellText := ' ';
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  // row background color
  if (Column = 5) or (Node^.Index mod 2 = 0) then
    VSTPaintCell(TargetCanvas, CellRect, ColorThemeInstance.Colors.Form);

  // selection color
  if vsSelected in Node^.States then
    VSTPaintCell(TargetCanvas, CellRect, ColorThemeInstance.Colors.Highlight);

  // marked
  if (Column = 0) and Subtitles[Node^.Index].Marked then
    VSTPaintCell(TargetCanvas, Rect(CellRect.Left, CellRect.Top, CellRect.Left+3, CellRect.Bottom), clBlue);

  // validate icon
  if (Column = 5) and Workspace.TranslatorMode and TMX.Ready then
    VSTDrawValidateIcon(TargetCanvas, CellRect, CellRect.Right-18, 2, Node^.Index);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTAfterItemPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
begin
  // icon errors
  if VSTOptions.DrawErrors then
    VSTDrawErrors(TargetCanvas, ItemRect, Node^.Index);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);

var
  TS : TTextStyle;
  R : TRect;
  c : Cardinal;
  TextHeight : Integer;
  oldColor : TColor;
  iArray : TIntegerDynArray;
  i : Integer;

  procedure DrawCPS(const Original: Boolean);
  var
    cps: Double;
  begin
    if Original then
      cps := Subtitles.TextCPS[Node^.Index, AppOptions.Conventions.CPSLineLenStrategy]
    else
      cps := Subtitles.TranslationCPS[Node^.Index, AppOptions.Conventions.CPSLineLenStrategy];

    c := TargetCanvas.Font.Color;
    if cps > AppOptions.Conventions.MaxCPS then
      TargetCanvas.Font.Color := clRed;

    TargetCanvas.TextRect(R, R.Left, R.Top, Format('%.2f', [cps], AppOptions.FormatSettings), TS);
    TargetCanvas.Font.Color := c;
  end;

  procedure DrawWPM(const Original: Boolean);
  var
    wpm: Double;
  begin
    if Original then
      wpm := Subtitles.TextWPM[Node^.Index]
    else
      wpm := Subtitles.TranslationWPM[Node^.Index];

    TargetCanvas.TextRect(R, R.Left, R.Top, Format('%.2f', [wpm], AppOptions.FormatSettings), TS);
  end;

begin
  DefaultDraw := False;
  TargetCanvas.Brush.Style := bsClear;

  if vsSelected in Node^.States then
    TargetCanvas.Font.Color := ColorThemeInstance.Colors.HighlightText
  else
    TargetCanvas.Font.Color := ColorThemeInstance.Colors.Text;

  FillByte(TS, SizeOf(TTextStyle), 0);
  TS.EndEllipsis := True;
  TS.RightToLeft := Application.IsRightToLeft;
  R := CellRect;
  TextHeight := VST.Canvas.Font.GetTextHeight('W');

  case Column of
    0: begin
         TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
         TS.Layout := tlTop;
         // Note
         if not Subtitles[Node^.Index].Notes.IsEmpty then
         begin
           c := TargetCanvas.Font.Color;
           TargetCanvas.Font.Color := clRed;
           TS.Alignment := taLeftJustify;
           TargetCanvas.TextRect(R, R.Left, R.Top, UC_BOOKMARK, TS);
           TargetCanvas.Font.Color := c;
         end;
         // #
         TS.Alignment := taRightJustify;
         TargetCanvas.TextRect(R, R.Left, R.Top, '#' + (Node^.Index+1).ToString, TS);
         // Pause
         if Node^.Index < (VST.RootNodeCount-1) then
         begin
           R.Top := TextHeight;
           if (etPauseTooShort in Subtitles[Node^.Index].ErrorType) then TargetCanvas.Font.Color := clRed;
           TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
           TargetCanvas.TextRect(R, R.Left, R.Top, GetPauseTimeStr(Node^.Index, True), TS);
         end;
       end;
    1: begin
         // Times
         oldColor := TargetCanvas.Font.Color;
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         if (etBadValues in Subtitles[Node^.Index].ErrorType) then
           TargetCanvas.Font.Color := clRed;

         if (etOverlapping in Subtitles[Node^.Index].ErrorType) then
         begin
           if (etOverlappingWithPrev in Subtitles[Node^.Index].ErrorType) then
             TargetCanvas.Font.Color := clRed;
           TargetCanvas.TextRect(R, R.Left, R.Top, GetInitialTimeStr(Node^.Index), TS);
           TargetCanvas.Font.Color := oldColor;

           if (etOverlappingWithNext in Subtitles[Node^.Index].ErrorType) then
             TargetCanvas.Font.Color := clRed;
           R.Top := TextHeight;
           TargetCanvas.TextRect(R, R.Left, R.Top, GetFinalTimeStr(Node^.Index), TS);
           TargetCanvas.Font.Color := oldColor;
         end
         else
           TargetCanvas.TextRect(R, R.Left, R.Top, GetInitialTimeStr(Node^.Index) + sLineBreak + GetFinalTimeStr(Node^.Index), TS);
       end;
    2: begin
         // Duration
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         if (etTimeTooLong in Subtitles[Node^.Index].ErrorType) then TargetCanvas.Font.Color := clRed;
         TargetCanvas.TextRect(R, R.Left, R.Top, GetDurationTimeStr(Node^.Index, True), TS);
       end;
    3: begin
         // Style & Actor
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Style, TS);
         R.Top := TextHeight;
         //TargetCanvas.Font.Style := TargetCanvas.Font.Style + [TFontStyle.fsBold];
         TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Actor, TS);
         //TargetCanvas.Font.Style := TargetCanvas.Font.Style - [TFontStyle.fsBold];
       end;
    4: begin
         // Text
         if VSTOptions.DrawTags then
           DrawASSText(TargetCanvas, R, Subtitles[Node^.Index].Text, TS.RightToLeft)
         else
         begin
           TS.Alignment := taLeftJustify;
           TS.Layout := tlTop;
           //TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
           TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Text, TS);
         end;
       end;
    5: begin
         // Translation
         R.Right := (R.Right - 16);
         if VSTOptions.DrawTags then
           DrawASSText(TargetCanvas, R, Subtitles[Node^.Index].Translation, TS.RightToLeft)
         else
         begin
           TS.Alignment := taLeftJustify;
           TS.Layout := tlTop;
           //TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
           TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Translation, TS);
         end;
       end;
    6: begin
         // CPS
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         DrawCPS(True);
         if WorkSpace.TranslatorMode then
         begin
           R.Top := TextHeight;
           DrawCPS(False);
         end;
       end;
    7: begin
         // WPM
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         DrawWPM(True);
         if WorkSpace.TranslatorMode then
         begin
           R.Top := TextHeight;
           DrawWPM(False);
         end;
       end;
    8: begin
         // CPL
         TS.Alignment := taRightJustify;
         TS.Layout := tlTop;
         if (etBreakLongLines in Subtitles[Node^.Index].ErrorType) then
         begin
           c := TargetCanvas.Font.Color;

           if WorkSpace.TranslatorMode then
           begin
             iArray := GetLengthForEachLineIntArray(Subtitles[Node^.Index].Translation, sLineBreak, '');
             for i := 0 to High(iArray) do
             begin
               if iArray[i] > AppOptions.Conventions.CPL then
                 TargetCanvas.Font.Color := clRed;

               TargetCanvas.TextRect(R, R.Left, R.Top, iArray[i].ToString, TS);
               R.Top := TextHeight;
               TargetCanvas.Font.Color := c;
             end;
             TS.Alignment := taLeftJustify;
             R := CellRect;
           end;

           iArray := GetLengthForEachLineIntArray(Subtitles[Node^.Index].Text, sLineBreak, '');
           for i := 0 to High(iArray) do
           begin
             if iArray[i] > AppOptions.Conventions.CPL then
               TargetCanvas.Font.Color := clRed;

             TargetCanvas.TextRect(R, R.Left, R.Top, iArray[i].ToString, TS);
             R.Top := TextHeight;
             TargetCanvas.Font.Color := c;
           end;

           SetLength(iArray, 0);
         end
         else
         begin
           if WorkSpace.TranslatorMode then
           begin
             TargetCanvas.TextRect(R, R.Left, R.Top, GetLengthForEachLine(Subtitles[Node^.Index].Translation, sLineBreak, ''), TS);
             TS.Alignment := taLeftJustify;
           end;
           TargetCanvas.TextRect(R, R.Left, R.Top, GetLengthForEachLine(Subtitles[Node^.Index].Text, sLineBreak, ''), TS);
         end;
       end;
  end;
end;

// -----------------------------------------------------------------------------

{ Block Draw (without columns) }

// -----------------------------------------------------------------------------

procedure DrawBGRABlock(var ABMP: TBGRABitmap; const ACanvas: TCanvas);
const
  shadow = 2;
  dist   = 3;
  rxy    = 2;
begin
  if not Assigned(ABMP) then
    ABMP := TBGRABitmap.Create(ACanvas.Width, ACanvas.Height, ColorToBGRA(ColorThemeInstance.Colors.Window))
  else
  begin
    ABMP.SetSize(ACanvas.Width, ACanvas.Height);
    ABMP.Fill(cssWhite);
  end;

  ABMP.RoundRectAntialias(dist+shadow, dist+shadow,
    ABMP.Width-dist, ABMP.Height-dist, rxy, rxy,
    BGRA(60, 60, 60), 1, BGRA(60, 60, 60), [rrTopLeftSquare, rrTopRightSquare]);

  BGRAreplace(ABMP, ABMP.FilterBlurRadial(3, 3, rbFast));

  ABMP.RoundRectAntialias(dist, dist, ACanvas.Width-dist-shadow, ACanvas.Height-dist-shadow,
    rxy, rxy, ColorThemeInstance.Colors.ItemBackground, 2, ColorThemeInstance.Colors.ItemBackground, [rrTopLeftSquare, rrTopRightSquare]);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTBeforeCellPaint_Block(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);

const
  margin =3;

var
  R, rr : TRect;
  cy    : Integer;
  tl    : TTextStyle;
  w, h  : Integer;

  procedure DrawCPS(const Original: Boolean);
  var
    cps: Double;
  begin
    if Original then
      cps := Subtitles.TextCPS[Node^.Index, AppOptions.Conventions.CPSLineLenStrategy]
    else
      cps := Subtitles.TranslationCPS[Node^.Index, AppOptions.Conventions.CPSLineLenStrategy];

    TargetCanvas.TextRect(rr, rr.Left, rr.Top, Format('%.2f', [cps], AppOptions.FormatSettings), TL);
  end;

  procedure DrawWPM(const Original: Boolean);
  var
    wpm: Double;
  begin
    if Original then
      wpm := Subtitles.TextWPM[Node^.Index]
    else
      wpm := Subtitles.TranslationWPM[Node^.Index];

    TargetCanvas.TextRect(rr, rr.Left, rr.Top, Format('%.2f', [wpm], AppOptions.FormatSettings), TL);
  end;

begin
  if not Assigned(VSTOptions.BackgroundBlock) or (VSTOptions.RepaintBckgBlock) then
  begin
    DrawBGRABlock(VSTOptions.BackgroundBlock, TargetCanvas);
    VSTOptions.RepaintBckgBlock := False;
  end;

  VSTOptions.BackgroundBlock.Draw(TargetCanvas, 0, 0);

  // header
  if vsSelected in Node^.States then
  begin
    TargetCanvas.Brush.Color := ColorThemeInstance.Colors.ItemSelHeader;
    TargetCanvas.Font.Color  := ColorThemeInstance.Colors.HighlightText;
  end
  else
  begin
    TargetCanvas.Brush.Color := ColorThemeInstance.Colors.ItemHeader;
    TargetCanvas.Font.Color  := ColorThemeInstance.Colors.Text;
  end;

  w := 0;
  h := 0;
  TargetCanvas.Font.GetTextSize('W', w, h);
  cy := h + (margin*3);
  R := ContentRect;
  R.Left := R.Left -1 ;
  R.Right := R.Right - margin - 2;
  R.Top := R.Top+margin;
  R.Bottom := cy;
  TargetCanvas.FillRect(R);

  TargetCanvas.Brush.Style := bsClear;
  TargetCanvas.Pen.Color   := ColorThemeInstance.Colors.Text;
  TargetCanvas.Font.Style  := [fsBold];
  R.Left := R.Left + 4 ;

  tl.Layout     := tlCenter;
  tl.Alignment  := taCenter;
  tl.SingleLine := True;

  // marked
  if Subtitles[Node^.Index].Marked then
    VSTPaintCell(TargetCanvas, Rect(CellRect.Left, CellRect.Top+3, CellRect.Left+3, CellRect.Bottom-3), clBlue);
  // #
  if actShowColumnNumber.Checked then
  begin
    rr := Rect(R.Left - Margin + 2, R.Top , R.Left + w * 2, R.Bottom);
    TargetCanvas.TextRect(rr, rr.Left, rr.Top, '#' + (Node^.Index+1).ToString, tl);
  end;
  // IT
  if actShowColumnTimes.Checked then
  begin
    rr.Left := rr.Right + 25;
    rr.Right := rr.Left + 80;
    TargetCanvas.TextRect(rr, rr.Left, rr.Top, GetInitialTimeStr(Node^.Index), tl);
  end;
  // D
  if actShowColumnDuration.Checked then
  begin
    rr.Left := rr.Right + 25;
    rr.Right := rr.Left + 50;
    TargetCanvas.TextRect(rr, rr.Left, rr.Top, GetDurationTimeStr(Node^.Index, True), tl);
  end;
  // FT
  if actShowColumnTimes.Checked then
  begin
    rr.Left := rr.Right + 25;
    rr.Right := rr.Left + 80;
    TargetCanvas.TextRect(rr, rr.Left, rr.Top, GetInitialTimeStr(Node^.Index), tl);
  end;
  // CPS
  if actShowColumnCPS.Checked then
  begin
    rr.Left := rr.Right + 25;
    rr.Right := rr.Left + 40;
    DrawCPS(True);
  end;
  // WPM
  if actShowColumnWPM.Checked then
  begin
    rr.Left := rr.Right + 25;
    rr.Right := rr.Left + 40;
    DrawWPM(True);
  end;
  // CPL
  if actShowColumnCPL.Checked then
  begin
    rr.Left := rr.Right + 25;
    rr.Right := rr.Left + 40;
    TargetCanvas.TextRect(rr, rr.Left, rr.Top, GetLengthForEachLine(Subtitles[Node^.Index].Text), tl);
  end;
  // Text
  TargetCanvas.Font.Color  := ColorThemeInstance.Colors.Text;
  tl.Layout     := tlTop;
  tl.Alignment  := taLeftJustify;
  tl.SingleLine := False;
  ContentRect.Left := ContentRect.Left + margin;
  ContentRect.Top  := cy;
  TargetCanvas.TextRect(ContentRect, ContentRect.Left, ContentRect.Top, Subtitles[Node^.Index].Text, tl);
  // Icon Errors
  if VSTOptions.DrawErrors then
    VSTDrawErrors(TargetCanvas, CellRect, Node^.Index, 4, 18, 6, 24);
  // Note
  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  if not Subtitles[Node^.Index].Notes.IsEmpty then
  begin
    rr := R;
    rr.Right -= 4;
    TargetCanvas.Font.Color := clRed;
    tl.Alignment := taRightJustify;
    TargetCanvas.TextRect(rr, r.Left, r.Top, UC_BOOKMARK, tl);
  end;

end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTResize(Sender: TObject);
var
  i, NewWidth: Integer;
begin
  if VSTOptions.DrawMode = dmBlock then
    VSTOptions.RepaintBckgBlock := True
  else
  begin
    if VST.Header.Columns.Count = 0 then Exit;

    NewWidth := 0;
    for i := 0 to VST.Header.Columns.Count-1 do // All except "Text / Translation"
      if not (i in [4..5]) and (coVisible in VST.Header.Columns[i].Options) then
        NewWidth := NewWidth + VST.Header.Columns[i].Width;

    NewWidth := (VST.Width-NewWidth) - (GetSystemMetrics(SM_CXVSCROLL)+5);
    if actTranslatorMode.Checked then NewWidth := NewWidth div 2;

    for i := 4 to 5 do // Text & Translation
      VST.Header.Columns[i].Width := NewWidth;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if ColorThemeInstance.GetRealColorMode = cmDark then
    Elements := [hpeBackground];
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if (hpeBackground in Elements) then
  begin
    PaintInfo.TargetCanvas.Brush.Color := ColorThemeInstance.Colors.Window;

    if Assigned(PaintInfo.Column) then
      DrawFrameControl(PaintInfo.TargetCanvas.Handle, PaintInfo.PaintRectangle, DFC_BUTTON, DFCS_FLAT or DFCS_ADJUSTRECT);

    PaintInfo.TargetCanvas.FillRect(PaintInfo.PaintRectangle);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);

  procedure SetHintText(const S: String);
  begin
    if HintText = '' then
      HintText := S
    else
      HintText += sLineBreak + S;
  end;

begin
  LineBreakStyle := hlbForceSingleLine;
  HintText := '';
  with Subtitles[Node^.Index] do
  begin
    if ErrorType <> [] then
    begin
      if etBadValues           in ErrorType then SetHintText(lngasBadValues);
      if etTimeTooLong         in ErrorType then SetHintText(lngasTimeTooLong);
      if etTimeTooShort        in ErrorType then SetHintText(lngasTimeTooShort);
      if etPauseTooShort       in ErrorType then SetHintText(lngasGapTooShort);
      if etMaxCPS              in ErrorType then SetHintText(lngasMaxCPS);
      if etOverlappingWithPrev in ErrorType then SetHintText(lngasOverlappingWithPrev);
      if etOverlappingWithNext in ErrorType then SetHintText(lngasOverlappingWithNext);
      if etFixTags             in ErrorType then SetHintText(lngfsetFixTags);
      if etEmpty               in ErrorType then SetHintText(lngfsetEmpty);
      if etUnbreak             in ErrorType then SetHintText(Format(lngfsetUnbreak, [AppOptions.Conventions.CPL]));
      if etUnnecessarySpaces   in ErrorType then SetHintText(lngasUnnecessarySpaces);
      if etUnnecessaryDots     in ErrorType then SetHintText(lngasUnnecessaryDots);
      if etRepeatedChars       in ErrorType then SetHintText(lngasRepeatedChars);
      if etProhibitedChars     in ErrorType then SetHintText(lngasProhibitedChars);
      if etHearingImpaired     in ErrorType then SetHintText(lngasHearingImpaired);
      if etBreakLongLines      in ErrorType then SetHintText(lngasBreakLongLines);
      if etRepeatedSubtitle    in ErrorType then SetHintText(lngasRepeatedSubtitle);
      if etEllipsesSingleSmartCharacter in ErrorType then SetHintText(lngasEllipsesSingleSmartCharacter);
      if etMaxLines            in ErrorType then SetHintText(lngasMaxLines);
      if etOCR                 in ErrorType then SetHintText(lngasOCR);
    end;

    if not Notes.IsEmpty then
      SetHintText(sLineBreak+Notes);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTDblClick(Sender: TObject);
var
  it: Integer;
begin
  if WAVE.IsPeakDataLoaded then
    WAVE.SelectSubtitle(VSTFocusedNode(VST), True, True);

  it := Subtitles.ItemPointer[VSTFocusedNode(VST)]^.InitialTime;
  MPV.SetMediaPosInMs(it);

  if MPV.IsPaused then
  begin
    lblMediaTime.Tag     := TAG_CONTROL_UPDATE;
    lblMediaTime.Caption := GetTimeStr(it);
  end;

  FocusMemo;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Node: PVirtualNode;
begin
  Node := VST.GetNodeAt(MousePos);
  if Assigned(Node) then
  begin
    if not VST.Selected[Node] then
    begin
      VST.ClearSelection;
      VST.Selected[Node] := True;
    end;
  end
  else
    Handled := True;
end;

// -----------------------------------------------------------------------------

