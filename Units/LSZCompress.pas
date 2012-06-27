(*
  LazSolutions, (Un)Compress unit
  Copyright (C) 2011-2013 Silvio Clecio.

  http://silvioprog.com.br

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LSZCompress;

{$I lazsolutions.inc}

interface

uses
  Classes, SysUtils, ZStream;

type
  TLSCompressionLevel = (
    clNone,    { Do not use compression, just copy data. }
    clFastest, { Use fast (but less) compression. }
    clDefault, { Use default compression. }
    clMax      { Use maximum compression. }
    );

{ Compress a stream. }
procedure LSZCompressStream(var AInStream, AOutStream: TStream;
  const ALSCompressionLevel: TLSCompressionLevel = clDefault);
{ UnCompress a stream. }
procedure LSZUnCompressStream(var AInStream, AOutStream: TStream);
{ Compress a string. }
function LSZCompressString(const AString: string;
  const ALSCompressionLevel: TLSCompressionLevel = clDefault): string;
{ UnCompress a string. }
function LSZUnCompressString(const AString: string): string;
{ Compress a file. }
procedure LSZCompressFile(const AFileIn, AFileOut: TFileName;
  const ALSCompressionLevel: TLSCompressionLevel = clDefault);
{ UnCompress a file. }
procedure LSZUnCompressFile(const AFileIn, AFileOut: TFileName);

implementation

procedure LSZCompressStream(var AInStream, AOutStream: TStream;
  const ALSCompressionLevel: TLSCompressionLevel = clDefault);
begin
  with TCompressionStream.Create(TCompressionlevel(ALSCompressionLevel),
      AOutStream) do
    try
      CopyFrom(AInStream, AInStream.Size);
    finally
      Free;
    end;
end;

procedure LSZUnCompressStream(var AInStream, AOutStream: TStream);
const
  CBufferSize = 4096;
var
  VCount: Integer;
  VDecompressionStream: TDecompressionStream;
  VBuffer: array[0..CBufferSize - 1] of Byte;
begin
  VBuffer[0] := 0;
  VDecompressionStream := TDecompressionStream.Create(AInStream);
  try
    while True do
    begin
      VCount := VDecompressionStream.Read(VBuffer, CBufferSize);
      if VCount <> 0 then
        AOutStream.WriteBuffer(VBuffer, VCount)
      else
        Break;
    end;
  finally
    VDecompressionStream.Free;
  end;
end;

function LSZCompressString(const AString: string;
  const ALSCompressionLevel: TLSCompressionLevel = clDefault): string;
var
  VInStream, VOutStream: TStream;
begin
  Result := '';
  VInStream := TMemoryStream.Create;
  VOutStream := TMemoryStream.Create;
  try
    VInStream.Write(Pointer(AString)^, Length(AString));
    VInStream.Position := 0;
    LSZCompressStream(VInStream, VOutStream, ALSCompressionLevel);
    VOutStream.Position := 0;
    SetLength(Result, VOutStream.Size);
    VOutStream.Read(Pointer(Result)^, VOutStream.Size);
  finally
    VInStream.Free;
    VOutStream.Free;
  end;
end;

function LSZUnCompressString(const AString: string): string;
var
  VInStream, VOutStream: TStream;
begin
  Result := '';
  VInStream := TMemoryStream.Create;
  VOutStream := TMemoryStream.Create;
  try
    VInStream.Write(Pointer(AString)^, Length(AString));
    VInStream.Position := 0;
    LSZUnCompressStream(VInStream, VOutStream);
    VOutStream.Position := 0;
    SetLength(Result, VOutStream.Size);
    VOutStream.Read(Pointer(Result)^, VOutStream.Size);
  finally
    VInStream.Free;
    VOutStream.Free;
  end;
end;

procedure LSZCompressFile(const AFileIn, AFileOut: TFileName;
  const ALSCompressionLevel: TLSCompressionLevel);
var
  VInStream, VOutStream: TMemoryStream;
begin
  VInStream := TMemoryStream.Create;
  VOutStream := TMemoryStream.Create;
  try
    VInStream.LoadFromFile(AFileIn);
    with TCompressionStream.Create(TCompressionlevel(ALSCompressionLevel),
        VOutStream) do
      try
        CopyFrom(VInStream, VInStream.Size);
      finally
        Free;
      end;
    VOutStream.SaveToFile(AFileOut);
  finally
    VOutStream.Free;
    VInStream.Free;
  end;
end;

procedure LSZUnCompressFile(const AFileIn, AFileOut: TFileName);
var
  VInStream, VOutStream: TMemoryStream;
begin
  VInStream := TMemoryStream.Create;
  VOutStream := TMemoryStream.Create;
  try
    VInStream.LoadFromFile(AFileIn);
    LSZUnCompressStream(TStream(VInStream), TStream(VOutStream));
    VOutStream.SaveToFile(AFileOut);
  finally
    VInStream.Free;
    VOutStream.Free;
  end;
end;

end.

