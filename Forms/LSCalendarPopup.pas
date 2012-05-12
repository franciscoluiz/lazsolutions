(*
  LazSolutions, Calendar popup unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSCalendarPopup;

{$mode objfpc}{$H+}

interface

uses
  LSConsts, LSMessages, Classes, SysUtils, Forms, Controls, Calendar, ExtCtrls,
  Buttons, LCLType;

type

  TLSCalendarPopupCloseEvent = procedure(ASender: TObject;
    const ADateTime: TDateTime) of object;

  { TLSCalendarPopupForm }

  TLSCalendarPopupForm = class(TForm)
    OKBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    DateCalendar: TCalendar;
    BottomPanel: TPanel;
    procedure CancelBitBtnClick(Sender: TObject);
    procedure DateCalendarClick(Sender: TObject);
    procedure DateCalendarDblClick(Sender: TObject);
    procedure DateCalendarKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure OKBitBtnClick(Sender: TObject);
  private
    FCalendarPopupClose: TLSCalendarPopupCloseEvent;
    FCloseWithDblClick: Boolean;
  protected
    procedure DoCalendarPopupClose; virtual;
  public
    class procedure Execute(var ADateTime: TDateTime;
      const APopupOrigin: TPoint; const ACloseWithDblClick, AShowBtns,
      AShowBtnsCaptions, AShowBtnsGlyphs: Boolean;
      const ACalendarDisplaySettings: TDisplaySettings;
      const ACalendarOnClose: TLSCalendarPopupCloseEvent);
  end;

implementation

{$R *.lfm}

{ TLSCalendarPopupForm }

{$HINTS OFF}
procedure TLSCalendarPopupForm.DateCalendarClick(Sender: TObject);
begin
  if not FCloseWithDblClick then
    DoCalendarPopupClose;
end;

procedure TLSCalendarPopupForm.CancelBitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TLSCalendarPopupForm.DateCalendarDblClick(Sender: TObject);
begin
  if FCloseWithDblClick then
    DoCalendarPopupClose;
end;

procedure TLSCalendarPopupForm.DateCalendarKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  VHandled: Boolean;
begin
  if Shift = [] then
  begin
    VHandled := True;
    case Key of
      VK_ESCAPE: Close;
      VK_RETURN, VK_SPACE: DoCalendarPopupClose;
    else
      VHandled := False;
    end;
    if VHandled then
      Key := 0;
  end;
end;

procedure TLSCalendarPopupForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TLSCalendarPopupForm.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TLSCalendarPopupForm.OKBitBtnClick(Sender: TObject);
begin
  DoCalendarPopupClose;
end;
{$HINTS ON}

procedure TLSCalendarPopupForm.DoCalendarPopupClose;
var
  VPoint: TPoint;
begin
  VPoint := DateCalendar.ScreenToClient(Mouse.CursorPos);
  if DateCalendar.HitTest(VPoint) in [cpNoWhere, cpDate] then
  begin
    Close;
    FCalendarPopupClose(Self, DateCalendar.DateTime);
  end;
end;

class procedure TLSCalendarPopupForm.Execute(var ADateTime: TDateTime;
  const APopupOrigin: TPoint; const ACloseWithDblClick, AShowBtns,
  AShowBtnsCaptions, AShowBtnsGlyphs: Boolean;
  const ACalendarDisplaySettings: TDisplaySettings;
  const ACalendarOnClose: TLSCalendarPopupCloseEvent);
var
  VBounds: TRect;
begin
  with Self.Create(nil) do
  begin
    VBounds := Screen.MonitorFromPoint(APopupOrigin).BoundsRect;
    if APopupOrigin.X + Width > VBounds.Right then
      Left := VBounds.Right - Width
    else
      Left := APopupOrigin.X;
    if APopupOrigin.Y + Height > VBounds.Bottom then
      Top := VBounds.Bottom - Height
    else
      Top := APopupOrigin.Y;
    FCalendarPopupClose := ACalendarOnClose;
    FCloseWithDblClick := ACloseWithDblClick;
    DateCalendar.DisplaySettings := ACalendarDisplaySettings;
    if ADateTime <> CLSConstsNullDate then
      DateCalendar.DateTime := ADateTime
    else
      DateCalendar.DateTime := Date;
    BottomPanel.Visible := AShowBtns;
    if AShowBtnsCaptions then
    begin
      OKBitBtn.Caption := SLSDateEditPopupOKButton;
      CancelBitBtn.Caption := SLSDateEditPopupCancelButton;
    end;
    if not AShowBtnsGlyphs then
    begin
      OKBitBtn.Glyph := nil;
      CancelBitBtn.Glyph := nil;
    end;
    Show;
  end;
end;

end.

