(*
  LazSolutions, RegExList unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSRegExList;

{$I lazsolutions.inc}

interface

uses
  LSCommonIntf, LSConsts, LSRegEx, Classes, SysUtils;

type

  ELSRegExError = class(Exception);

  TLSRegExs = class;

  TLSRegExChangeHandler = procedure(ASender: TObject;
    var ANewName, ANewDefMsg: string) of object;

  { TLSRegEx }

  TLSRegEx = class(TCollectionItem)
  private
    FActive: Boolean;
    FAuthor: string;
    FDefaultMessage: string;
    FDescription: string;
    FExpression: string;
    FMatch: Integer;
    FModifiers: TLSRegExModifiers;
    FName: string;
    FOwner: TLSRegExs;
    procedure SetDefaultMessage(AValue: string);
    procedure SetName(AValue: string);
  public
    constructor Create(ACollection: TCollection); override;
    function GetUniqueName(AName: string): string; virtual;
    function Execute(const AString: string): string; virtual;
  published
    property Active: Boolean read FActive write FActive;
    property Author: string read FAuthor write FAuthor;
    property Description: string read FDescription write FDescription;
    property Expression: string read FExpression write FExpression;
    property DefaultMessage: string read FDefaultMessage write SetDefaultMessage;
    property Match: Integer read FMatch write FMatch default 0;
    property Modifiers: TLSRegExModifiers read FModifiers
      write FModifiers default [rmModifierI];
    property Name: string read FName write SetName;
  end;

  { TLSRegExs }

  TLSRegExs = class(TOwnedCollection)
  private
    FChangeHandler: TLSRegExChangeHandler;
    function GetItem(AIndex: Integer): TLSRegEx;
    procedure SetItem(AIndex: Integer; AValue: TLSRegEx);
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function Add: TLSRegEx; virtual;
    function FindByName(const AName: string): TLSRegEx; virtual;
    function IndexOf(const AName: string): Integer; virtual;
    property Items[AIndex: Integer]: TLSRegEx read GetItem write SetItem; default;
    property ChangeHandler: TLSRegExChangeHandler read FChangeHandler
      write FChangeHandler;
  end;

  { TLSCustomRegExList }

  TLSCustomRegExList = class(TComponent, ILSAboutComponent)
  private
    FItems: TLSRegExs;
    function GetAbout: string;
    function GetTip: string;
    procedure SetAbout(AValue: string);
    procedure SetTip(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Count: Integer; virtual;
    function FindByName(const AName: string): TLSRegEx; virtual;
    property About: string read GetAbout write SetAbout stored False;
    property Items: TLSRegExs read FItems write FItems;
    property Tip: string read GetTip write SetTip stored False;
  end;

  { TLSRegExList }

  TLSRegExList = class(TLSCustomRegExList)
  published
    property About stored False;
    property Items;
    property Tip stored False;
  end;

const
  LS_REGEX_DISABLED = 'RegEx disabled';
  LS_REGEX_EMPTY = 'Empty RegEx';

implementation

{ TLSRegEx }

constructor TLSRegEx.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FOwner := ACollection as TLSRegExs;
  SetName(GetUniqueName(FName));
  FMatch := 0;
  FModifiers := [rmModifierI];
end;

function TLSRegEx.GetUniqueName(AName: string): string;
var
  I: Integer;
begin
  Result := AName;
  if AName = '' then
    AName := DisplayName;
  I := 0;
  repeat
    Inc(I);
    Result := AName + IntToStr(I);
  until FOwner.IndexOf(Result) < 0;
end;

procedure TLSRegEx.SetName(AValue: string);
begin
  if FName <> AValue then
  begin
    if not IsValidIdent(AValue) then
      raise ELSRegExError.CreateFmt(CLSInvalidName, [AValue]);
    if Assigned(FOwner) and (FOwner.IndexOf(AValue) > -1) then
      raise ELSRegExError.Createfmt(CLSDuplicateName, [AValue]);
    FName := AValue;
  end;
  if Assigned(FOwner.ChangeHandler) then
    FOwner.ChangeHandler(Self, AValue, FDefaultMessage);
end;

procedure TLSRegEx.SetDefaultMessage(AValue: string);
begin
  if AValue <> FDefaultMessage then
    FDefaultMessage := AValue;
  if Assigned(FOwner.ChangeHandler) then
    FOwner.ChangeHandler(Self, FName, AValue);
end;

function TLSRegEx.Execute(const AString: string): string;
begin
  if not FActive then
  begin
    Result := LS_REGEX_DISABLED;
    Exit;
  end;
  if FExpression = '' then
  begin
    Result := LS_REGEX_EMPTY;
    Exit;
  end;
  Result := LSExtractStringUsingRegEx(AString, FExpression, FMatch, FModifiers);
end;

{ TLSRegExs }

constructor TLSRegExs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TLSRegEx);
end;

destructor TLSRegExs.Destroy;
begin
  FChangeHandler := nil;
  inherited Destroy;
end;

function TLSRegExs.GetItem(AIndex: Integer): TLSRegEx;
begin
  Result := inherited GetItem(AIndex) as TLSRegEx;
end;

procedure TLSRegExs.SetItem(AIndex: Integer; AValue: TLSRegEx);
begin
  inherited SetItem(AIndex, AValue);
end;

function TLSRegExs.Add: TLSRegEx;
begin
  Result := inherited Add as TLSRegEx;
end;

function TLSRegExs.IndexOf(const AName: string): Integer;
begin
  Result := Pred(Count);
  while (Result >= 0) and (AnsiCompareText(AName, Items[Result].Name) <> 0) do
    Dec(Result);
end;

function TLSRegExs.FindByName(const AName: string): TLSRegEx;
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I >= 0 then
    Result := Items[I]
  else
    Result := nil;
end;

{ TLSCustomRegExList }

constructor TLSCustomRegExList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TLSRegExs.Create(Self);
end;

destructor TLSCustomRegExList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TLSCustomRegExList.Count: Integer;
begin
  Result := FItems.Count;
end;

function TLSCustomRegExList.FindByName(const AName: string): TLSRegEx;
begin
  Result := FItems.FindByName(AName);
end;

function TLSCustomRegExList.GetAbout: string;
begin
  Result := '';
end;

function TLSCustomRegExList.GetTip: string;
begin
  Result := '';
end;

{$HINTS OFF}
procedure TLSCustomRegExList.SetAbout(AValue: string);
begin
end;
{$HINTS ON}

{$HINTS OFF}
procedure TLSCustomRegExList.SetTip(AValue: string);
begin
end;
{$HINTS ON}

end.

