(*
  LazSolutions, Notifier OS Classes
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSNotifierOS;

{$I lazsolutions.inc}

interface

uses
  LSForms, LResources, Classes, Forms, StdCtrls, ExtCtrls, LCLType, LCLIntf,
  Graphics, Controls, CustomTimer, Clipbrd;

type

  TLSNotifyDuration = 1..61;

  { TLSNotifyPosition }

  TLSNotifyPosition = (npTopLeft, npTopRight, npBottomRight, npBottomLeft);

  { TLSNotifierOS }

  TLSNotifierOS = class(THintWindow)
  private
    FClick: TNotifyEvent;
    FBorderColor: TColor;
    FActiveForm: TCustomForm;
    FTheme: TGraphic;
    FDuration: Byte;
    FMargin: Integer;
    FAnimation: Boolean;
    FPosition: TLSNotifyPosition;
    FTitleLabel: TLabel;
    FMsgLabel: TLabel;
    FCloseButton: TImage;
    FImage: TImage;
    FAnimationTimer: TCustomTimer;
    FAnimationCount: Integer;
  protected
    procedure DoAnimate(ASender: TObject);
    procedure DoCloseButton(ASender: TObject);
    procedure DoClose(var ACloseAction: TCloseAction); override;
    procedure DoHintClick(ASender: TObject);
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    class procedure Execute(AOwner: TComponent;
      const ATitle, AMsg: string; const AGraphic: TGraphic;
      const APosition: TLSNotifyPosition = npBottomRight;
      const AAnimation: Boolean = True; const ADuration: TLSNotifyDuration = 3;
      const AMargin: SmallInt = 10; const AColor: TColor = clWhite;
      const ABorderColor: TColor = clSkyBlue;
      const ATextColor: TColor = clWindowText; const AClick: TNotifyEvent = nil);
    class procedure Execute(AOwner: TComponent; var ATheme: TGraphic;
      const ATitle, AMsg: string; const AGraphic: TGraphic;
      const APosition: TLSNotifyPosition = npBottomRight;
      const AAnimation: Boolean = True; const ADuration: TLSNotifyDuration = 3;
      const AMargin: SmallInt = 10; const ATextColor: TColor = clWindowText;
      const AClick: TNotifyEvent = nil);
    procedure AdjustNotifierOS(const ATitle, AMsg: string; const AGraphic,
      ATheme: TGraphic; const APosition: TLSNotifyPosition;
      const AAnimation: Boolean; const ADuration: TLSNotifyDuration;
      const AMargin: SmallInt; const AColor, ABorderColor, ATextColor: TColor;
      const AClick: TNotifyEvent);
  end;

implementation

{ TLSNotifierOS }

constructor TLSNotifierOS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompStyle := csHintWindow + csForm;
  Caption := '';
  Height := 80;
  Width := 270;
  { Animation }
  FAnimationTimer := TCustomTimer.Create(Self);
  FAnimationTimer.OnTimer := @DoAnimate;
  FAnimationTimer.Interval := 10;
  FAnimationCount := 0;
  { Image }
  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Center := True;
  FImage.OnClick := @DoHintClick;
  { TitleLabel }
  FTitleLabel := TLabel.Create(Self);
  FTitleLabel.Parent := Self;
  FTitleLabel.Font.Style := [fsBold];
  FTitleLabel.ShowAccelChar := False;
  FTitleLabel.OnClick := @DoHintClick;
  { MsgLabel }
  FMsgLabel := TLabel.Create(Self);
  FMsgLabel.Parent := Self;
  FMsgLabel.AutoSize := False;
  FMsgLabel.ShowAccelChar := False;
  FMsgLabel.WordWrap := True;
  FMsgLabel.OnClick := @DoHintClick;
  { CloseButton }
  FCloseButton := TImage.Create(Self);
  FCloseButton.AutoSize := True;
  FCloseButton.Cursor := crHandPoint;
  FCloseButton.Picture.LoadFromLazarusResource('LSNotifierOSCloseBtn');
  FCloseButton.Parent := Self;
  FCloseButton.OnClick := @DoCloseButton;
end;

destructor TLSNotifierOS.Destroy;
begin
  if Assigned(FActiveForm) then
    FActiveForm.BringToFront;
  inherited Destroy;
end;

procedure TLSNotifierOS.Paint;
begin
  if Assigned(FTheme) then
    Canvas.StretchDraw(ClientRect, FTheme)
  else
  begin
{$IFDEF LCLQt}
    Canvas.Brush.Style := bsSolid;
{$ENDIF}
    Canvas.Pen.Color := FBorderColor;
    Canvas.Rectangle(ClientRect);
  end;
end;

{$HINTS OFF}
procedure TLSNotifierOS.DoAnimate(ASender: TObject);
var
  VMoveAdjust: Integer;
begin
  if FAnimation then
  begin
    VMoveAdjust := 10;
    case FPosition of
      npTopLeft, npTopRight:
        begin
          if ClientOrigin.Y < LSGetWorkAreaRect.Top + FMargin then
            Top := Top + VMoveAdjust
          else
            if FDuration > 60 then
              FAnimationTimer.Enabled := False;
        end;
      npBottomRight, npBottomLeft:
        begin
          if ClientOrigin.Y > LSGetWorkAreaRect.Bottom - Height - FMargin then
            Top := Top - VMoveAdjust
          else
            if FDuration > 60 then
              FAnimationTimer.Enabled := False;
        end;
    end;
  end;
  if FAnimationCount >= FDuration * 100 then
  begin
    if not MouseEntered then
      Close;
  end
  else
    Inc(FAnimationCount);
end;
{$HINTS ON}

{$HINTS OFF}
procedure TLSNotifierOS.DoCloseButton(ASender: TObject);
begin
  Close;
end;
{$HINTS ON}

procedure TLSNotifierOS.DoClose(var ACloseAction: TCloseAction);
begin
{$IFDEF LCLQt}
  ReleaseHandle;
{$ENDIF}
  inherited;
  ACloseAction := caFree;
end;

procedure TLSNotifierOS.DoHintClick(ASender: TObject);
begin
  if Assigned(FClick) then
    FClick(ASender);
end;

procedure TLSNotifierOS.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  if ((ssCtrl in AShift) and (AKey = VK_C)) then
    Clipboard.AsText := FTitleLabel.Caption + LineEnding + LineEnding +
      FMsgLabel.Caption;
  inherited KeyDown(AKey, AShift);
end;

class procedure TLSNotifierOS.Execute(AOwner: TComponent; const ATitle,
  AMsg: string; const AGraphic: TGraphic; const APosition: TLSNotifyPosition;
  const AAnimation: Boolean; const ADuration: TLSNotifyDuration;
  const AMargin: SmallInt; const AColor: TColor; const ABorderColor: TColor;
  const ATextColor: TColor; const AClick: TNotifyEvent);
var
  VHint: TLSNotifierOS;
begin
  VHint := TLSNotifierOS.Create(AOwner);
  VHint.AdjustNotifierOS(ATitle, AMsg, AGraphic, nil, APosition, AAnimation,
    ADuration, AMargin, AColor, ABorderColor, ATextColor, AClick);
  VHint.Visible := True;
end;

class procedure TLSNotifierOS.Execute(AOwner: TComponent; var ATheme: TGraphic;
  const ATitle, AMsg: string; const AGraphic: TGraphic;
  const APosition: TLSNotifyPosition; const AAnimation: Boolean;
  const ADuration: TLSNotifyDuration; const AMargin: SmallInt;
  const ATextColor: TColor; const AClick: TNotifyEvent);
var
  VHint: TLSNotifierOS;
begin
  VHint := TLSNotifierOS.Create(AOwner);
  VHint.AdjustNotifierOS(ATitle, AMsg, AGraphic, ATheme, APosition, AAnimation,
    ADuration, AMargin, clDefault, clDefault, ATextColor, AClick);
  VHint.Visible := True;
end;

procedure TLSNotifierOS.AdjustNotifierOS(const ATitle, AMsg: string;
  const AGraphic, ATheme: TGraphic; const APosition: TLSNotifyPosition;
  const AAnimation: Boolean; const ADuration: TLSNotifyDuration;
  const AMargin: SmallInt; const AColor, ABorderColor, ATextColor: TColor;
  const AClick: TNotifyEvent);
var
  VRect: TRect;
begin
  case APosition of
    npTopLeft:
    begin
      VRect.Left := LSGetWorkAreaRect.Left + AMargin;
      if AAnimation then
        VRect.Top := LSGetWorkAreaRect.Top - Height - AMargin
      else
        VRect.Top := LSGetWorkAreaRect.Top + AMargin;
    end;
    npTopRight:
    begin
      VRect.Left := LSGetWorkAreaRect.Right - Width - AMargin;
      if AAnimation then
        VRect.Top := LSGetWorkAreaRect.Top - Height - AMargin
      else
        VRect.Top := LSGetWorkAreaRect.Top + AMargin;
    end;
    npBottomRight:
    begin
      VRect.Left := LSGetWorkAreaRect.Right - Width - AMargin;
      if AAnimation then
        VRect.Top := LSGetWorkAreaRect.Bottom + Height
      else
        VRect.Top := LSGetWorkAreaRect.Bottom - Height - AMargin;
    end;
    npBottomLeft:
    begin
      VRect.Left := LSGetWorkAreaRect.Left + AMargin;
      if AAnimation then
        VRect.Top := LSGetWorkAreaRect.Bottom + Height
      else
        VRect.Top := LSGetWorkAreaRect.Bottom - Height - AMargin;
    end;
  end;
  VRect.Right := VRect.Left + Width;
  VRect.Bottom := VRect.Top + Height;
  Color := AColor;
  Top := VRect.Top;
  Left := VRect.Left;
  OnClick := @DoHintClick;
  FActiveForm := Screen.ActiveCustomForm;
  FAnimation := AAnimation;
  FBorderColor := ABorderColor;
  FClick := AClick;
  FDuration := ADuration;
  FMargin := AMargin;
  FPosition := APosition;
  FTheme := ATheme;
  FCloseButton.Anchors := [akRight, akTop];
  FCloseButton.AnchorSideTop.Control := Self;
  FCloseButton.BorderSpacing.Top := 2;
  FCloseButton.AnchorSideRight.Control := Self;
  FCloseButton.AnchorSideRight.Side := asrRight;
  FCloseButton.BorderSpacing.Right := 2;
  FImage.Align := alLeft;
  FImage.AutoSize := True;
  FImage.Center := True;
  FImage.BorderSpacing.Around := 2;
  FImage.Picture.Graphic := AGraphic;
  FTitleLabel.Anchors := [akLeft, akRight, akTop];
  FTitleLabel.AnchorSideTop.Control := Self;
  FTitleLabel.BorderSpacing.Top := 3;
  FTitleLabel.AnchorSideRight.Control := FCloseButton;
  FTitleLabel.BorderSpacing.Right := 2;
  FMsgLabel.Anchors := [akBottom, akLeft, akRight, akTop];
  FMsgLabel.AnchorSideTop.Control := FTitleLabel;
  FMsgLabel.AnchorSideTop.Side := asrBottom;
  FMsgLabel.BorderSpacing.Top := 2;
  FMsgLabel.AnchorSideBottom.Control := Self;
  FMsgLabel.AnchorSideBottom.Side := asrBottom;
  FMsgLabel.BorderSpacing.Bottom := 2;
  FMsgLabel.AnchorSideRight.Control := Self;
  FMsgLabel.AnchorSideRight.Side := asrRight;
  FMsgLabel.BorderSpacing.Right := 2;
  if Assigned(AGraphic) then
  begin
    FTitleLabel.AnchorSideLeft.Control := FImage;
    FTitleLabel.AnchorSideLeft.Side := asrRight;
    FMsgLabel.AnchorSideLeft.Control := FImage;
    FMsgLabel.AnchorSideLeft.Side := asrRight;
  end
  else
  begin
    FTitleLabel.AnchorSideLeft.Control := Self;
    FTitleLabel.BorderSpacing.Left := 3;
    FMsgLabel.AnchorSideLeft.Control := Self;
    FMsgLabel.BorderSpacing.Left := 3;
    FImage.Hide;
  end;
  FTitleLabel.Caption := ATitle;
  FTitleLabel.Font.Color := ATextColor;
  FMsgLabel.Caption := AMsg;
  FMsgLabel.Font.Color := ATextColor;
end;

initialization
  {$I LSNotifierOSCloseBtn.lrs}

end.

