(*
  LazSolutions, Exception utils unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSExceptionUtils;

{$I lazsolutions.inc}

interface

uses
  LSMessages, SysUtils;

type

  { JSON exceptions }

  ELSJSONEmptyDatabaseError = class(Exception);
  ELSJSONEmptyArrayError = class(Exception);
  ELSJSONEmptyObjectError = class(Exception);
  ELSJSONIsNotArrayError = class(Exception);
  ELSJSONIsNotObjectError = class(Exception);

{ Dump exception call stack.
  (see: http://wiki.lazarus.freepascal.org/Logging_exceptions) }
function LSDumpExceptionCallStack(const ACGIApp: Boolean = False): string;
{ JSON exception errors. }
procedure LSShowJSONEmptyDatabaseError(const AClassName: ShortString);
procedure LSShowJSONIsNotArrayError(const AClassName, AGotClass: ShortString);
procedure LSShowJSONIsNotObjectError(const AClassName, AGotClass: ShortString);
procedure LSShowJSONEmptyArrayError(const AClassName: ShortString);
procedure LSShowJSONEmptyObjectError(const AClassName: ShortString);

implementation

function LSDumpExceptionCallStack(const ACGIApp: Boolean): string;
var
  I: Integer;
  VFrames: PPointer;
begin
  VFrames := ExceptFrames;
  Result := BackTraceStrFunc(ExceptAddr);
  if ACGIApp then
    Result += '<br />'
  else
    Result += LineEnding;
  for I := 0 to Pred(ExceptFrameCount) do
  begin
    Result += BackTraceStrFunc(VFrames[I]);
    if ACGIApp then
      Result += '<br />'
    else
      Result += LineEnding;
  end;
end;

procedure LSShowJSONEmptyDatabaseError(const AClassName: ShortString);
begin
  raise ELSJSONEmptyDatabaseError.CreateFmt(
    SLSJSONEmptyDatabaseError, [AClassName]);
end;

procedure LSShowJSONIsNotArrayError(const AClassName, AGotClass: ShortString);
begin
  raise ELSJSONIsNotArrayError.CreateFmt(
    SLSJSONIsNotArrayError, [AClassName, AGotClass]);
end;

procedure LSShowJSONIsNotObjectError(const AClassName, AGotClass: ShortString);
begin
  raise ELSJSONIsNotObjectError.CreateFmt(
    SLSJSONIsNotObjectError, [AClassName, AGotClass]);
end;

procedure LSShowJSONEmptyArrayError(const AClassName: ShortString);
begin
  raise ELSJSONEmptyArrayError.CreateFmt(SLSJSONEmptyArrayError, [AClassName]);
end;

procedure LSShowJSONEmptyObjectError(const AClassName: ShortString);
begin
  raise ELSJSONEmptyObjectError.CreateFmt(SLSJSONEmptyObjectError, [AClassName]);
end;

end.

