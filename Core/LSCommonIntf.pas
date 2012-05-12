(*
  LazSolutions, Common interfaces unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSCommonIntf;

{$I lazsolutions.inc}

interface

uses
  Classes;

type

  { ILSAboutComponent }

  ILSAboutComponent = interface
    ['{889A1F61-66B6-486F-A54D-F21C3CFDF734}']
    function GetAbout: string;
    procedure SetAbout(AValue: string);
    property About: string read GetAbout write SetAbout stored False;
  end;

  { ILSValidationComponent }

  ILSValidationComponent = interface(ILSAboutComponent)
    ['{7107DC46-A75D-4C0C-826D-37FA833F6381}']
    function Validate: Boolean;
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation);
  end;

implementation

end.

