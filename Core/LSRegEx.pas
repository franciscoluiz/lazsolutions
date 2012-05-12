(*
  LazSolutions, Regular expression (RegEx) unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSRegEx;

{$I lazsolutions.inc}

interface

uses
  Classes, SynRegExpr;

type

  { TLSRegExModifiers }

  TLSRegExModifiers = set of (rmModifierI, rmModifierR, rmModifierS,
    rmModifierG, rmModifierM, rmModifierX);

{ Extract string using regular expression. }
function LSExtractStringUsingRegEx(
  const AString, AExpression: string; const AMatch: Integer = 0;
  const ALSRegExModifiers: TLSRegExModifiers = [rmModifierI]): string;
{ Extract strings using regular expression. }
procedure LSExtractStringsUsingRegEx(var AStrings: TStrings;
  const AString, AExpression: string; const AMatch: Integer = 0;
  const ALSRegExModifiers: TLSRegExModifiers = [rmModifierI]);
{ Replace string using regular expression. }
function LSReplaceStringUsingRegEx(const AExpression, AString, AReplace: string;
  const ALSRegExModifiers: TLSRegExModifiers = [rmModifierI]): string;

implementation

function LSExtractStringUsingRegEx(const AString, AExpression: string;
  const AMatch: Integer; const ALSRegExModifiers: TLSRegExModifiers): string;
var
  VRegExpr: TRegExpr;
begin
  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.ModifierI := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierR := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierS := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierG := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierM := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierX := rmModifierI in ALSRegExModifiers;
    VRegExpr.Expression := AExpression;
    if VRegExpr.Exec(AString) then
      Result := VRegExpr.Match[AMatch];
  finally
    VRegExpr.Free;
  end;
end;

procedure LSExtractStringsUsingRegEx(var AStrings: TStrings; const AString,
  AExpression: string; const AMatch: Integer;
  const ALSRegExModifiers: TLSRegExModifiers);
var
  VRegExpr: TRegExpr;
begin
  VRegExpr := TRegExpr.Create;
  try
    AStrings.Clear;
    VRegExpr.ModifierI := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierR := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierS := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierG := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierM := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierX := rmModifierI in ALSRegExModifiers;
    VRegExpr.Expression := AExpression;
    if VRegExpr.Exec(AString) then
      repeat
        AStrings.Add(VRegExpr.Match[AMatch]);
      until not VRegExpr.ExecNext;
  finally
    VRegExpr.Free;
  end;
end;

function LSReplaceStringUsingRegEx(const AExpression, AString, AReplace: string;
  const ALSRegExModifiers: TLSRegExModifiers): string;
var
  VRegExpr: TRegExpr;
begin
  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.ModifierI := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierR := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierS := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierG := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierM := rmModifierI in ALSRegExModifiers;
    VRegExpr.ModifierX := rmModifierI in ALSRegExModifiers;
    VRegExpr.Expression := AExpression;
    Result := VRegExpr.Replace(AString, AReplace, True);
  finally
    VRegExpr.Free;
  end;
end;

end.

