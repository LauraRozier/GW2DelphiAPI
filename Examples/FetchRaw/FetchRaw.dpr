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
program FetchRaw;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  GW2DelphiAPI in '..\..\src\GW2DelphiAPI.pas';

var
  fGW2API:        TGW2API;
  fAPIRequestUrl: string;
  fAPIReply:      string;

begin
  try
    WriteLn('GW2 Delphi API Version: ' + CONST_VERSION);
    // Create the API object
    fGW2API := TGW2API.Create;

    // Create some space
    WriteLn(sLineBreak);
    // Build the request URL string
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv1];
    // Fetch the server's answer for API v1
    fAPIReply := fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl);
    WriteLn('Raw endpoint response API v1:' + sLineBreak + fAPIReply);

    // Create some space
    WriteLn(sLineBreak);
    // Build the request URL string again
    fAPIRequestUrl := CONST_API_URL_BASE + CONST_API_Versions[APIv2];
    // Fetch the server's answer for API v2
    fAPIReply := fGW2API.WebHandler.FetchRawEndpoint(fAPIRequestUrl);
    WriteLn('Raw endpoint response API v2:' + sLineBreak + fAPIReply);

    FreeAndNil(fGW2API);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  WriteLn('Press the Enter key to continue...');
  ReadLn;
end.
