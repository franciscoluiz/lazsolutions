(*
  LazSolutions, Hashs unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSHashs;

{$I lazsolutions.inc}

interface

uses
  LSUtils, MD5, SHA1, SysUtils;

type
  TLSPwsGenAmout = 2..10;

{ Generate MD5 string from a string. (see: http://en.wikipedia.org/wiki/MD5) }
function LSMD5(const AString: string): string;
{ Generate MD5 string from a file. (see: http://en.wikipedia.org/wiki/Md5sum) }
function LSMD5Sum(const AFileName: TFileName): string;
{ Generate SHA1 string from a string. (see: http://en.wikipedia.org/wiki/SHA-1) }
function LSSHA1(const AString: string): string;
{ Generate SHA1 string from a file. (see: http://en.wikipedia.org/wiki/Sha1sum ) }
function LSSHA1Sum(const AFileName: TFileName): string;
{ Password generator. }
function LSPasswordGenerator(const AAmount: TLSPwsGenAmout = 6;
  const AAZ: Boolean = True; const A_az: Boolean = False;
  const ASymbols: Boolean = False; const ANumbers: Boolean = True): string;
{ MD5 password generator. }
function LSMD5PasswordGenerator(const AUpperCase: Boolean = True): string;
{ SHA1 password generator. }
function LSSHA1PasswordGenerator(const AUpperCase: Boolean = True): string;

implementation

function LSMD5(const AString: string): string;
begin
  Result := MD5Print(MD5String(AString));
end;

function LSMD5Sum(const AFileName: TFileName): string;
begin
  Result := MD5Print(MD5File(AFileName));
end;

function LSSHA1(const AString: string): string;
begin
  Result := SHA1Print(SHA1String(AString));
end;

function LSSHA1Sum(const AFileName: TFileName): string;
begin
  Result := SHA1Print(SHA1File(AFileName));
end;

function LSPasswordGenerator(const AAmount: TLSPwsGenAmout; const AAZ: Boolean;
  const A_az: Boolean; const ASymbols: Boolean;
  const ANumbers: Boolean): string;
const
  CNumbers = '0123456789';
  C_az = 'abcdefghijklmnopqrstuvwxyz';
  CAZ = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  CSymbols = '!"#$%&*(){}[]<>=+-\|/,.:;?@^_~`''';
var
  I, J, K: Integer;
  VString: string = '';
begin
  Result := '';
  if ANumbers then
    VString += CNumbers;
  if A_az then
    VString += C_az;
  if AAZ then
    VString += CAZ;
  if ASymbols then
    VString += CSymbols;
  K := Length(VString);
  if K = 0 then
    Exit;
  for I := 1 to AAmount do
  begin
    J := 1 + Random(K);
    Result += VString[J];
  end;
end;

function LSMD5PasswordGenerator(const AUpperCase: Boolean = True): string;
begin
  Result := LSMD5(LSUUID);
  if AUpperCase then
    Result := UpperCase(Result);
end;

function LSSHA1PasswordGenerator(const AUpperCase: Boolean = True): string;
begin
  Result := LSSHA1(LSUUID);
  if AUpperCase then
    Result := UpperCase(Result);
end;

initialization
  Randomize;

end.

