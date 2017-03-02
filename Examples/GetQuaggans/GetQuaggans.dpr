{
    GW2DelphiAPI - An API port for Guild Wars 2 written in Delphi ( Object-Pascal )
    Copyright (C) 2017  Thimo Braker

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}
program GetQuaggans;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  GW2DelphiAPI in '..\..\src\GW2DelphiAPI.pas';

var
  fGW2API:              TGW2API;          // Main API class object
  fParams:              TUrlParams;       // Array of parameter objects
  fQuagganIDs:          TStringArray;     // Array of strings
  fQuagganArray:        TGW2QuagganArray; // Array of Quaggan class objects
  fQuaggan:             TGW2Quaggan;      // Single Quaggan class object
  fStrValue, fBuildStr: string;

begin
  try
    WriteLn('GW2 Delphi API Version: ' + CONST_VERSION);
    fGW2API := TGW2API.Create();

    WriteLn;
    WriteLn('All Quaggan IDs: ');
    fQuagganIDs := fGW2API.Misc.GetQuagganIDs(fGW2API.WebHandler);
    fBuildStr   := '';

    for fStrValue in fQuagganIDs do
      if fBuildStr = '' then
        fBuildStr := fStrValue
      else
        fBuildStr := fBuildStr + ', ' + fStrValue;

    WriteLn(fBuildStr);

    WriteLn;
    WriteLn;
    WriteLn('Quaggans:');
    SetLength(fParams, 1);
    fParams[0].Name  := 'ids';
    fParams[0].Value := fQuagganIDs[0] + ',' +
                        fQuagganIDs[1] + ',' +
                        fQuagganIDs[2];
    fQuagganArray    := fGW2API.Misc.GetQuaggans(fGW2API.WebHandler, fParams);

    for fQuaggan in fQuagganArray do
    begin
      WriteLn(#9 + 'ID: '  + fQuaggan.id);
      WriteLn(#9 + 'URL: ' + fQuaggan.url);
      WriteLn;
    end;

    FreeAndNil(fGW2API);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  WriteLn('Press the Enter key to continue...');
  ReadLn;
end.
