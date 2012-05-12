(*
  LazSolutions, Forms unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSForms;

{$I lazsolutions.inc}

interface

uses
  LSUtils, SysUtils, Classes, Forms, Controls, LCLType;

type

  { TLSEnterAsTAB }

  TLSEnterAsTAB = class
  protected
    FPerformTAB: Boolean;
    FSkipped: array of TWinControlClass;
    procedure DoKeyDown(ASender: TObject; var AKey: Word; AShift: TShiftState);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Prepare(const ASkipped: array of TWinControlClass;
      const APerformTAB: Boolean);
  end;

{ Get your work area rect. }
function LSGetWorkAreaRect(const AHandle: THandle = 0): TRect;
{ Run once application. }
function LSRunOnce(const AHalt: Boolean = True): Boolean;
{ Enter as TAB. }
procedure LSRegisterEnterAsTAB(const ASkipped: array of TWinControlClass;
  const APerformTAB: Boolean = False);

implementation

var
  _LSEnterAsTAB: TLSEnterAsTAB;

function LSEnterAsTABInstance: TLSEnterAsTAB;
begin
  if not Assigned(_LSEnterAsTAB) then
    _LSEnterAsTAB := TLSEnterAsTAB.Create;
  Result := _LSEnterAsTAB;
end;

function LSGetWorkAreaRect(const AHandle: THandle): TRect;
begin
  if AHandle = 0 then
    Result := Screen.MonitorFromWindow(Application.MainForm.Handle).WorkAreaRect
  else
    Result := Screen.MonitorFromWindow(AHandle).WorkAreaRect;
end;

function LSRunOnce(const AHalt: Boolean): Boolean;
var
  VProcess: string;
begin
  VProcess := ExtractFileName(Application.ExeName);
  Result := LSProcessIsRunning(VProcess, True);
  if Result and AHalt then
    Halt;
end;

procedure LSRegisterEnterAsTAB(const ASkipped: array of TWinControlClass;
  const APerformTAB: Boolean);
begin
  LSEnterAsTABInstance.Prepare(ASkipped, APerformTAB);
end;

{ TLSEnterAsTAB }

constructor TLSEnterAsTAB.Create;
begin
  Application.AddOnKeyDownHandler(@DoKeyDown);
end;

destructor TLSEnterAsTAB.Destroy;
begin
  Application.RemoveOnKeyDownHandler(@DoKeyDown);
  inherited Destroy;
end;

procedure TLSEnterAsTAB.Prepare(const ASkipped: array of TWinControlClass;
  const APerformTAB: Boolean);
var
  I, L: Integer;
begin
  FPerformTAB := APerformTAB;
  L := Length(ASkipped);
  SetLength(FSkipped, L);
  for I := 0 to Pred(L) do
    FSkipped[I] := ASkipped[I];
end;

{$HINTS OFF}
procedure TLSEnterAsTAB.DoKeyDown(ASender: TObject; var AKey: Word;
  AShift: TShiftState);

  function _IsSkipped(const AWinControl: TWinControl): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Pred(Length(FSkipped)) do
    begin
      Result := AWinControl is FSkipped[I];
      if Result then
        Break;
    end;
  end;

var
  VWinControl: TWinControl;
begin
  if AKey = VK_RETURN then
  begin
    if _IsSkipped(Screen.ActiveControl) then
      Exit;
    if FPerformTAB then
      Screen.ActiveControl.PerformTab(True)
    else
    begin
      VWinControl := Screen.ActiveControl.Parent;
      if Assigned(VWinControl) then
        while Assigned(VWinControl.Parent) do
          VWinControl := VWinControl.Parent;
        begin
          VWinControl.SelectNext(Screen.ActiveControl, True, True);
          AKey := VK_UNKNOWN;
        end;
    end;
  end;
end;
{$HINTS ON}

initialization

finalization
  if Assigned(_LSEnterAsTAB) then
    _LSEnterAsTAB.Free;

end.

